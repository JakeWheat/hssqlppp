

> {- | Functions to convert sql asts to valid SQL source code. Includes
>    a function - 'printSqlAnn' - to output the annotations from a tree
>    in comments in the outputted SQL source.
>
>    Produces sort of readable code, but mainly just written to produce
>    reparsable text. Could do with some work to make the outputted text
>    layout better.
> -}
> {-# LANGUAGE PatternGuards #-}
> module Database.HsSqlPpp.Pretty (
>                       --convert a sql ast to text
>                       printStatements
>                      ,printStatementsAnn
>                      ,printQueryExpr
>                       --convert a single expression parse node to text
>                      ,printScalarExpr
>                      ,printQueryExprNice
>                      )
>     where
>
> import Text.PrettyPrint
> --import Data.Char
> --import Data.List
> import Data.Maybe
>
> import Database.HsSqlPpp.Ast -- hiding (ncStr)
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Utils.Utils

--------------------------------------------------------------------------------

Public functions

> -- | convert an ast back to valid SQL source, it's also almost human readable.
> printStatements :: StatementList -> String
> printStatements = printStatementsAnn (const "")
>
> -- | convert the ast back to valid source, and convert any annotations to
> -- text using the function provided and interpolate the output of
> -- this function(inside comments) with the SQL source.
> printStatementsAnn :: (Annotation -> String) -> StatementList -> String
> printStatementsAnn f ast = render $ vcat (map (statement False True f) ast) <> text "\n"
>

> printQueryExpr :: QueryExpr -> String
> printQueryExpr ast = render (queryExpr False True True Nothing ast <> statementEnd True)

> -- | Testing function, pretty print an expression
> printScalarExpr :: ScalarExpr -> String
> printScalarExpr = render . scalExpr False


> -- | Try harder to make the output human readable, not necessary correct
> -- sql output at the moment
> printQueryExprNice :: QueryExpr -> String
> printQueryExprNice ast = render (queryExpr True True True Nothing ast <> statementEnd True)


-------------------------------------------------------------------------------

Conversion routines - convert Sql asts into Docs

> -- Statements
>
> statement :: Bool -> Bool -> (Annotation -> String) -> Statement -> Doc
> statement _nice _se _ca (AntiStatement s) = text $ "$(" ++ s ++ ")"
>
> -- selects
>
> statement nice se ca (QueryStatement ann s) =
>   annot ca ann <+>
>   queryExpr nice True True Nothing s <> statementEnd se
>
> --dml
>
> statement nice se pa (Insert ann tb atts idata rt) =
>   annot pa ann <+>
>   text "insert into" <+> name tb
>   <+> ifNotEmpty (parens . sepCsvMap nmc) atts
>   $+$ queryExpr nice True True Nothing idata
>   $+$ returning nice rt
>   <> statementEnd se
>
> statement nice se ca (Update ann tb scs fr wh rt) =
>    annot ca ann <+>
>    text "update" <+> name tb <+> text "set"
>    <+> sepCsvMap (set nice) scs
>    <+> ifNotEmpty (\_ -> text "from" <+> sepCsvMap (tref nice) fr) fr
>    <+> whr nice wh
>    $+$ returning nice rt <> statementEnd se
>
> statement nice se ca (Delete ann tbl us wh rt) =
>    annot ca ann <+>
>    text "delete from" <+> name tbl
>    <+> ifNotEmpty (\_ -> text "using" <+> sepCsvMap (tref nice) us) us
>    <+> whr nice wh
>    $+$ returning nice rt
>    <> statementEnd se
>
> statement _nice se ca (Truncate ann names ri casc) =
>     annot ca ann <+>
>     text "truncate"
>     <+> sepCsvMap name names
>     <+> text (case ri of
>                       RestartIdentity -> "restart identity"
>                       ContinueIdentity -> "continue identity")
>     <+> cascade casc
>     <> statementEnd se
>
> -- ddl
>
> statement nice se ca (CreateTable ann tbl atts cns) =
>     annot ca ann <+>
>     text "create table"
>     <+> name tbl <+> lparen
>     $+$ nest 2 (vcat (csv (map attrDef atts ++ map (constraint nice) cns)))
>     $+$ rparen <> statementEnd se
>     where
>       attrDef (AttributeDef _ n t def cons) =
>         nmc n <+> typeName t
>         <+> maybePrint (\e -> text "default" <+> scalExpr nice e) def
>         <+> hsep (map cCons cons)
>       cCons (NullConstraint _ cn) =
>         mname cn <+> text "null"
>       cCons (NotNullConstraint _ cn) =
>         mname cn <+> text "not null"
>       cCons (RowCheckConstraint _ cn ew) =
>         mname cn <+> text "check" <+> parens (scalExpr nice ew)
>       cCons (RowUniqueConstraint _ cn) =
>         mname cn <+> text "unique"
>       cCons (RowPrimaryKeyConstraint _ cn) =
>         mname cn <+> text "primary key"
>       cCons (RowReferenceConstraint _ cn tb att ondel onupd) =
>         mname cn <+> text "references" <+> name tb
>         <+> maybePrint (parens . nmc) att
>         <+> text "on delete" <+> cascade ondel
>         <+> text "on update" <+> cascade onupd
>
> statement nice se ca (AlterTable ann nm act) =
>     annot ca ann <+>
>     text "alter table" <+> name nm
>     <+> hcatCsvMap alterAction act <> statementEnd se
>     where
>       alterAction (AlterColumnDefault _ cnm def) =
>           text "alter column" <+> nmc cnm
>           <+> text "set default" <+> scalExpr nice def
>       alterAction (AddConstraint _ con) =
>           text "add " <+> constraint nice con
>
> statement _nice se ca (CreateSequence ann nm incr _ _ start cache) =
>     annot ca ann <+>
>     text "create sequence" <+> name nm <+>
>     text "increment" <+> text (show incr) <+>
>     text "no minvalue" <+>
>     text "no maxvalue" <+>
>     text "start" <+> text (show start) <+>
>     text "cache" <+> text (show cache) <> statementEnd se
>
> statement _nice se ca (AlterSequence ann nm o) =
>     annot ca ann <+>
>     text "alter sequence" <+> name nm
>     <+> text "owned by" <+> name o <> statementEnd se
>
> statement nice se ca (CreateTableAs ann t sel) =
>     annot ca ann <+>
>     text "create table"
>     <+> name t <+> text "as"
>     $+$ queryExpr nice True True Nothing sel
>     <> statementEnd se
>
> statement nice se ca (CreateFunction ann nm args retType rep lang body vol) =
>     annot ca ann <+>
>     text ("create " ++ (case rep of
>                          Replace -> "or replace "
>                          _ -> "") ++ "function")
>     <+> name nm
>     <+> parens (sepCsvMap paramDefn args)
>     <+> text "returns" <+> typeName retType <+> text "as" <+> text "$$"
>     $+$ functionBody body
>     $+$ text "$$" <+> text "language"
>     <+> text (case lang of
>                         Sql -> "sql"
>                         Plpgsql -> "plpgsql")
>     <+> text (case vol of
>                        Volatile -> "volatile"
>                        Stable -> "stable"
>                        Immutable -> "immutable")
>     <> statementEnd se
>     where
>       functionBody (SqlFnBody ann1 sts) =
>         annot ca ann1 <+>
>         nestedStatements nice ca sts
>       functionBody (PlpgsqlFnBody ann1 blk) =
>           annot ca ann1 <+>
>           statement nice True ca blk
>       paramDefn (ParamDef _ n t) = nmc n <+> typeName t
>       paramDefn  (ParamDefTp _ t) = typeName t
>
> statement nice se ca (Block ann lb decls sts) =
>   annot ca ann <+>
>   label lb <>
>   ifNotEmpty (\l -> text "declare"
>                   $+$ nest 2 (vcat $ map varDefn l)) decls
>   $+$ text "begin"
>   $+$ nestedStatements nice ca sts
>   $+$ text "end" <> statementEnd se
>   where
>       varDefn (VarDef _ n t v) =
>         nmc n <+> typeName t
>         <+> maybePrint (\x -> text ":=" <+> scalExpr nice x) v <> semi
>       varDefn (VarAlias _ n n1) =
>         nmc n <+> text "alias for" <+> name n1 <> semi
>       varDefn (ParamAlias _ n p) =
>         nmc n <+> text "alias for $" <> text (show p) <> semi
>
>
> statement nice se ca (CreateView ann nm cols sel) =
>     annot ca ann <+>
>     text "create view" <+> name nm
>     <> case cols of
>          Nothing -> empty
>          Just cs -> parens (sepCsvMap nmc cs)
>     <+> text "as"
>     $+$ nest 2 (queryExpr nice True True Nothing sel) <> statementEnd se
>
> statement nice se ca (CreateDomain ann nm tp n ex) =
>     annot ca ann <+>
>     text "create domain" <+> name nm <+> text "as"
>     <+> typeName tp <+> cname <+> checkExp ex <> statementEnd se
>     where
>       checkExp = maybePrint (\e -> text "check" <+> parens (scalExpr nice e))
>       cname = if n == ""
>                then empty
>                else text "constraint" <+> text n
>
> statement _nice se ca (DropFunction ann ifE fns casc) =
>   annot ca ann <+>
>   text "drop function"
>   <+> ifExists ifE
>   <+> sepCsvMap doFunction fns
>   <+> cascade casc
>   <> statementEnd se
>   where
>     doFunction (nm,types) =
>       name nm <> parens (sepCsvMap typeName types)
>
> statement _nice se ca (DropSomething ann dropType ifE names casc) =
>     annot ca ann <+>
>     text "drop"
>     <+> text (case dropType of
>                 Table -> "table"
>                 View -> "view"
>                 Domain -> "domain"
>                 Type -> "type")
>     <+> ifExists ifE
>     <+> sepCsvMap name names
>     <+> cascade casc
>     <> statementEnd se
>
> statement _nice se ca (CreateType ann nm atts) =
>     annot ca ann <+>
>     text "create type" <+> name nm <+> text "as" <+> lparen
>     $+$ nest 2 (vcat (csv
>           (map (\(TypeAttDef _ n t) -> nmc n <+> typeName t)  atts)))
>     $+$ rparen <> statementEnd se
>
> statement _nice se ca (CreateLanguage ann nm) =
>     annot ca ann <+>
>     text "create language" <+> text nm <> statementEnd se
>
> statement nice se ca (CreateTrigger ann nm wh events tbl firing fnName fnArgs) =
>     annot ca ann <+>
>     text "create trigger" <+> nmc nm
>     <+> text (case wh of
>                       TriggerBefore -> "before"
>                       TriggerAfter -> "after")
>     <+> evs
>     <+> text "on" <+> name tbl
>     <+> text "for" <+> text (case firing of
>                                         EachRow -> "row"
>                                         EachStatement -> "statement")
>     <+> text "execute procedure" <+> name fnName
>     <> parens (sepCsvMap (scalExpr nice) fnArgs) <> statementEnd se
>     where
>       evs = sep $ punctuate (text " or ") $ map
>             (text . (\e -> case e of
>                                 TInsert -> "insert"
>                                 TUpdate -> "update"
>                                 TDelete -> "delete"
>                                 AntiTriggerEvent s -> "$(" ++ s ++ ")")) events
>
> -- plpgsql
>
> statement _nice se ca (NullStatement ann) =
>   annot ca ann <+> text "null" <> statementEnd se
> statement _nice se ca (ExitStatement ann lb) =
>   annot ca ann <+> text "exit"
>     <+> maybe empty text lb <> statementEnd se
>

> statement nice se ca (Into ann str is (QueryStatement _ q)) =
>   annot ca ann <+>
>   queryExpr nice True True (Just (str,is)) q <> statementEnd se


> statement nice se ca (Into ann str into st) =
>   annot ca ann <+>
>   statement nice False ca st
>   <+> text "into"
>   <> (if str
>       then empty <+> text "strict"
>       else empty)
>   <+> sepCsvMap name into
>   <> statementEnd se
>   --fixme, should be insert,update,delete,execute

> statement nice se ca (Assignment ann nm val) =
>     annot ca ann <+>
>     name nm <+> text ":=" <+> scalExpr nice val <> statementEnd se
>
> statement nice se ca (Return ann ex) =
>     annot ca ann <+>
>     text "return" <+> maybePrint (scalExpr nice) ex <> statementEnd se
>
> statement nice se ca (ReturnNext ann ex) =
>     annot ca ann <+>
>     text "return" <+> text "next" <+> scalExpr nice ex <> statementEnd se
>
> statement nice se ca (ReturnQuery ann sel) =
>     annot ca ann <+>
>     text "return" <+> text "query"
>     <+> queryExpr nice True True Nothing sel <> statementEnd se
>
> statement nice se ca (Raise ann rt st exps) =
>     annot ca ann <+>
>     text "raise"
>     <+> case rt of
>                 RNotice -> text "notice"
>                 RException -> text "exception"
>                 RError -> text "error"
>     <+> scalExpr nice (StringLit emptyAnnotation st)
>     <> ifNotEmpty (\e -> comma <+> csvExp nice e) exps
>     <> statementEnd se
>
> statement nice se ca (ForQueryStatement ann lb i sel stmts) =
>     annot ca ann <+>
>     label lb <>
>     text "for" <+> nmc i <+> text "in"
>     <+> queryExpr nice True True Nothing sel <+> text "loop"
>     $+$ nestedStatements nice ca stmts
>     $+$ text "end loop" <> statementEnd se
>
> statement nice se ca (ForIntegerStatement ann lb var st en stmts) =
>     annot ca ann <+>
>     label lb <>
>     text "for" <+> nmc var <+> text "in"
>     <+> scalExpr nice st <+> text ".." <+> scalExpr nice en <+> text "loop"
>     $+$ nestedStatements nice ca stmts
>     $+$ text "end loop" <> statementEnd se
>
> statement nice se ca (WhileStatement ann lb ex stmts) =
>     annot ca ann <+>
>     label lb <>
>     text "while" <+> scalExpr nice ex <+> text "loop"
>     $+$ nestedStatements nice ca stmts
>     $+$ text "end loop" <> statementEnd se
> statement nice se ca (LoopStatement ann lb stmts) =
>     annot ca ann <+>
>     label lb <>
>     text "loop"
>     $+$ nestedStatements nice ca stmts
>     $+$ text "end loop" <> statementEnd se
>
> statement _nice se ca (ContinueStatement ann lb) =
>     annot ca ann <+> text "continue"
>       <+> maybe empty text lb <> statementEnd se
> statement nice se ca (Perform ann f@(App _ _ _)) =
>     annot ca ann <+>
>     text "perform" <+> scalExpr nice f <> statementEnd se
> statement _ _ _ (Perform _ x) =
>    error $ "internal error: statement not supported for " ++ show x
>
> statement _nice se ca (Copy ann tb cols src) =
>     annot ca ann <+>
>     text "copy" <+> name tb
>     <+> ifNotEmpty (parens . sepCsvMap nmc) cols
>     <+> text "from"
>     <+> case src of
>                  CopyFilename s -> quotes $ text s <> statementEnd se
>                  Stdin -> text "stdin" <> text ";"
>
> statement _ _ ca (CopyData ann s) =
>     annot ca ann <+>
>     text s <> text "\\." <> newline
>
> statement nice se ca (If ann conds els) =
>    annot ca ann <+>
>    text "if" <+> constraintd (head conds)
>    $+$ vcat (map (\c -> text "elseif" <+> constraintd c) $ tail conds)
>    $+$ ifNotEmpty (\e -> text "else" $+$ nestedStatements nice ca e) els
>    $+$ text "end if" <> statementEnd se
>     where
>       constraintd (ex, sts) = scalExpr nice ex <+> text "then"
>                            $+$ nestedStatements nice ca sts
> statement nice se ca (Execute ann s) =
>     annot ca ann <+>
>     text "execute" <+> scalExpr nice s <> statementEnd se
>
>
> statement nice se ca (CaseStatementSimple ann c conds els) =
>     annot ca ann <+>
>     text "case" <+> scalExpr nice c
>     $+$ nest 2 (
>                 vcat (map (uncurry whenStatement) conds)
>                 $+$ elseStatement els
>                 ) $+$ text "end case" <> statementEnd se
>     where
>       whenStatement ex sts = text "when" <+> sepCsvMap (scalExpr nice) ex
>                           <+> text "then" $+$ nestedStatements nice ca sts
>       elseStatement = ifNotEmpty (\s -> text "else"
>                                      $+$ nestedStatements nice ca s)
> statement nice se ca (CaseStatement ann conds els) =
>     annot ca ann <+>
>     text "case"
>     $+$ nest 2 (
>                 vcat (map (uncurry whenStatement) conds)
>                 $+$ elseStatement els
>                 ) $+$ text "end case" <> statementEnd se
>     where
>       whenStatement ex sts = text "when" <+> sepCsvMap (scalExpr nice) ex
>                           <+> text "then" $+$ nestedStatements nice ca sts
>       elseStatement = ifNotEmpty (\s -> text "else"
>                                      $+$ nestedStatements nice ca s)

>
> -- misc
>
> statement _nice se _ (Set _ n vs) =
>   text "set" <+> text n <+> text "="
>   <+> sepCsvMap (text . dv) vs <> statementEnd se
>   where
>     dv (SetStr _ s) = "'" ++ s ++ "'"
>     dv (SetId _ i) = i
>     dv (SetNum _ nm) = show nm
>
> statement _nice se _ (Notify _ n) =
>   text "notify" <+> text n  <> statementEnd se
>
> statementEnd :: Bool -> Doc
> statementEnd b = if b
>                  then semi <> newline
>                  else empty

-------------------------------------------------------------------------------

Statement components

> -- selects
>
> queryExpr :: Bool -> Bool -> Bool -> Maybe (Bool,[Name]) -> QueryExpr -> Doc
> queryExpr nice writeSelect _ intoi (Select _ dis l tb wh grp hav
>                                     order lim off) =
>   (text (if writeSelect then "select" else "")
>          <+> (case dis of
>                  Dupes -> empty
>                  Distinct -> text "distinct"))
>   $+$ nest 2 (vcat $ catMaybes
>   [fmap (\(str,is) -> text "into"
>                       <+> (if str
>                            then text "strict"
>                            else empty)
>                       <+> sepCsvMap name is) intoi
>   ,Just $ nest 2 $ selectList nice l
>   ,Just $ if null tb
>           then empty
>           else text "from" $+$ nest 2 (sepCsvMap (tref nice) tb)
>   ,Just $ whr nice wh
>   ,case grp of
>      [] -> Nothing
>      g -> Just $ text "group by" $+$ nest 2 (sepCsvMap (scalExpr nice) g)
>   ,flip fmap hav $ \h -> text "having" $+$ nest 2 (scalExpr nice h)
>   ,Just $ orderBy nice order
>   ,flip fmap lim $ \lm -> text "limit" <+> scalExpr nice lm
>   ,flip fmap off $ \offs -> text "offset" <+> scalExpr nice offs
>   ])
>
> queryExpr nice writeSelect topLev _ (CombineQueryExpr _ tp s1 s2) =
>   let p = queryExpr nice writeSelect False Nothing  s1
>           $+$ (case tp of
>                        Except -> text "except"
>                        Union -> text "union"
>                        UnionAll -> text "union" <+> text "all"
>                        Intersect -> text "intersect")
>           $+$ queryExpr nice True False Nothing s2
>   in if topLev then p else parens p
> queryExpr nice _ _ _ (Values _ expss) =
>   text "values" $$ nest 2 (vcat $ csv $ map (parens . csvExp nice) expss)
> queryExpr nice _ _ _ (WithQueryExpr _ wqs ex) =
>   text "with" $$ nest 2 (vcat $ csv $ map pwq wqs)
>        $+$ queryExpr nice True False Nothing ex
>   where
>     pwq (WithQuery _ nm cs ex1) =
>       nmc nm <> case cs of
>                    Nothing -> empty
>                    Just cs' -> parens $ sepCsvMap nmc cs'
>       <+> text "as"
>       <+> parens (queryExpr nice True False Nothing ex1)

> name :: Name -> Doc
> name (Name _ ns) = nmcs ns

> nmcs :: [NameComponent] -> Doc
> nmcs ns = hcat $ punctuate (text ".") $ map nmc ns

> nmc :: NameComponent -> Doc
> nmc (Nmc ns) = text ns
> nmc (QNmc ns) = doubleQuotes $ text ns

>
> tref :: Bool -> TableRef -> Doc
> {-tref nice (Tref _ f@(SQIdentifier _ t) (TableAlias _ ta))
>   | nice, last t == ta = name f
>   -- slightly bad hack:
> tref nice (Tref _ f@(SQIdentifier _ t) (FullAlias _ ta _))
>   | nice, last t == ta = name f-}

> tref nice (Tref _ f a) = name f <+> trefAlias nice a
> tref nice (JoinTref _ t1 nat jt t2 ex a) =
>         parens (tref nice t1
>         $+$ (case nat of
>                       Natural -> text "natural"
>                       Unnatural -> empty)
>         <+> text (case jt of
>                           Inner -> "inner"
>                           Cross -> "cross"
>                           LeftOuter -> "left outer"
>                           RightOuter -> "right outer"
>                           FullOuter -> "full outer")
>         <+> text "join"
>         <+> tref nice t2
>         <+> maybePrint (nest 2 . joinScalarExpr) ex
>         <+> trefAlias nice a)
>         where
>           joinScalarExpr (JoinOn _ e) = text "on" <+> scalExpr nice e
>           joinScalarExpr (JoinUsing _ ids) =
>               text "using" <+> parens (sepCsvMap nmc ids)
>
> tref nice (SubTref _ sub alias) =
>         parens (queryExpr nice True True Nothing sub)
>         <+> text "as" <+> trefAlias nice alias
> tref nice (FunTref _ f@(App _ _ _) a) = scalExpr nice f <+> trefAlias nice a
> tref _nice (FunTref _ x _) =
>       error $ "internal error: node not supported in function tref: "
>             ++ show x
>
> trefAlias :: Bool -> TableAlias -> Doc
> trefAlias _ (NoAlias _) = empty
> trefAlias _ (TableAlias _ t) = nmc t
> -- hack this out for now. When the type checking is fixed, can try
> -- to eliminate unneeded aliases?
> trefAlias nice (FullAlias _ t s) =
>   nmc t <> (if nice
>              then empty
>              else parens (sepCsvMap nmc s))

> direction :: Direction -> Doc
> direction d = text $ case d of
>                           Asc -> "asc"
>                           Desc -> "desc"
>
> whr :: Bool -> Maybe ScalarExpr -> Doc
> whr nice (Just ex) = text "where" $+$ nest 2 (scalExpr nice ex)
> whr _ Nothing = empty
>
> selectList :: Bool -> SelectList -> Doc
> selectList nice (SelectList _ ex) =
>   sepCsvMap selectItem ex
>   -- <+> ifNotEmpty (\i -> text "into" <+> hcatCsvMap scalExpr i) into
>   where
>     -- try to avoid printing alias if not necessary
>     selectItem (SelectItem _ ex1@(QIdentifier _ is) nm) | nice, last is == nm = scalExprSl nice ex1
>     selectItem (SelectItem _ ex1@(Identifier _ i) nm) | nice, i == nm = scalExprSl nice ex1
>     selectItem (SelectItem _ ex1 nm) = scalExprSl nice ex1 <+> text "as" <+> nmc nm
>     selectItem (SelExp _ e) = scalExprSl nice e
>
> cascade :: Cascade -> Doc
> cascade casc = text $ case casc of
>                                  Cascade -> "cascade"
>                                  Restrict -> "restrict"
> -- ddl
>
> constraint :: Bool -> Constraint -> Doc
> constraint _nice (UniqueConstraint _ n c) =
>         mname n <+> text "unique"
>         <+> parens (sepCsvMap nmc c)
> constraint _nice (PrimaryKeyConstraint _ n p) =
>         mname n <+>
>         text "primary key"
>         <+> parens (sepCsvMap nmc p)
> constraint nice (CheckConstraint _ n c) =
>         mname n <+> text "check" <+> parens (scalExpr nice c)
> constraint _nice (ReferenceConstraint _ n at tb rat ondel onupd) =
>         mname n <+>
>         text "foreign key" <+> parens (sepCsvMap nmc at)
>         <+> text "references" <+> name tb
>         <+> ifNotEmpty (parens . sepCsvMap nmc) rat
>         <+> text "on update" <+> cascade onupd
>         <+> text "on delete" <+> cascade ondel
>
> mname :: String -> Doc
> mname n = if n == ""
>           then empty
>           else text "constraint" <+> text n
>
> returning :: Bool -> Maybe SelectList -> Doc
> returning nice l = case l of
>                 Nothing -> empty
>                 Just ls -> nest 2 (text "returning" <+> selectList nice ls)
>
> ifExists :: IfExists -> Doc
> ifExists i = case i of
>                         Require -> empty
>                         IfExists -> text "if exists"
>
> -- plpgsql
>
> nestedStatements :: Bool -> (Annotation -> String) -> StatementList -> Doc
> nestedStatements nice pa = nest 2 . vcat . map (statement nice True pa)
>
> typeName :: TypeName -> Doc
> typeName (SimpleTypeName _ s) = name s
> typeName (PrecTypeName _ s i) = name s <> parens(integer i)
> typeName (Prec2TypeName _ s i i1) = name s <> parens (sepCsv [integer i, integer i1])
> typeName (ArrayTypeName _ t) = typeName t <> text "[]"
> typeName (SetOfTypeName _ t) = text "setof" <+> typeName t
>
> -- expressions
>
> scalExpr :: Bool -> ScalarExpr -> Doc
> scalExpr _ (AntiScalarExpr s) = text $ "$(" ++ s ++ ")"
> scalExpr _ (Star _) = text "*"
> scalExpr _ (QStar _ i) = nmc i <> text ".*"

> scalExpr _ (Identifier _ i) = nmc i
>   {-if quotesNeeded
>      then text $ "\"" ++ i ++ "\""
>      else text i
>   where
>     --needs some work - quotes needed if contains invalid unquoted
>     --chars, or maybe if matches keyword or similar
>     quotesNeeded = case i of
>                      x:_ | not (isLetter x || x `elem` "_*") -> True
>                      _ | all okChar i -> False
>                        | otherwise -> True
>                    where
>                      okChar x =isAlphaNum x || x `elem` "*_."-}
> scalExpr _nice (QIdentifier _a [i1, i]) = parens (nmc i1) <> text "." <> nmc i
> scalExpr _nice (QIdentifier _a _) = error "only supports 2 part qualified identifers atm"
> --scalExpr nice (QIdentifier a e i) = parens (scalExpr nice e) <> text "." <> scalExpr nice (Identifier a i)

> --scalExpr (PIdentifier _ i) = parens $ scalExpr i
> scalExpr _ (NumberLit _ n) = text n
> scalExpr _ (StringLit _ s) = -- needs some thought about using $$?
>                           text "'" <> text replaceQuotes <> text "'"
>                           where
>                             replaceQuotes = replace "'" "''" s {-if tag == "'"
>                                               then replace "'" "''" s
>                                               else s-}
>
> scalExpr nice (App _ n es) =
>     --check for special operators
>    case getTName n of
>      Just "!and" | nice, [a,b] <- es -> doLeftAnds a b
>      Just "!arrayctor" -> text "array" <> brackets (csvExp nice es)
>      Just "!between" -> scalExpr nice (head es) <+> text "between"
>                    <+> parens (scalExpr nice (es !! 1))
>                   <+> text "and"
>                   <+> parens (scalExpr nice (es !! 2))
>      Just "!substring" -> text "substring"
>                      <> parens (scalExpr nice (head es)
>                                 <+> text "from" <+> scalExpr nice (es !! 1)
>                                 <+> text "for" <+> scalExpr nice (es !! 2))
>      Just "!arraysub" -> case es of
>                        (Identifier _ i : es1) -> nmc i
>                                                  <> brackets (csvExp nice es1)
>                        _ -> parens (scalExpr nice (head es))
>                             <> brackets (csvExp nice (tail es))
>      Just "!rowctor" -> text "row" <> parens (sepCsvMap (scalExpr nice) es)
>      Just "."   -- special case to avoid ws around '.'. Don't know if this is important
>            -- or just cosmetic
>          | [a,b] <- es -> parens (scalExpr nice a) <> text "." <> scalExpr nice b
>      Just n' | ncs <- nameComponents n
>              , isOperatorName ncs ->
>         case forceRight (getOperatorType defaultTemplate1Catalog ncs) of
>                           BinaryOp ->
>                               let e1d = scalExpr nice (head es)
>                                   opd = text $ filterKeyword n'
>                                   e2d = scalExpr nice (es !! 1)
>                               in parens (if n' `elem` ["!and", "!or"]
>                                          then vcat [e1d, opd <+> e2d]
>                                          else e1d <+> opd <+> e2d)
>                           PrefixOp -> parens (text (if n' == "u-"
>                                                     then "-"
>                                                     else filterKeyword n')
>                                                <+> parens (scalExpr nice (head es)))
>                           PostfixOp -> parens (scalExpr nice (head es)
>                                        <+> text (filterKeyword n'))
>      _ -> name n <> parens (csvExp nice es)
>    where
>      filterKeyword t = case t of
>                          "!and" -> "and"
>                          "!or" -> "or"
>                          "!not" -> "not"
>                          "!isnull" -> "is null"
>                          "!isnotnull" -> "is not null"
>                          "!like" -> "like"
>                          "!notlike" -> "not like"
>                          x -> x
>      -- try to write a series of ands in a vertical line with slightly less parens
>      doLeftAnds a b = let as = and' a
>                       in vcat ((scalExpr nice (head as)
>                                 : map (\x -> text "and" <+> scalExpr nice x) (tail as))
>                                ++ [text "and" <+> scalExpr nice b])
>      and' a = case a of
>                 App _ f [x,y] | Just "!and" <- getTName f -> and' x ++ and' y
>                 _ -> [a]
>
> scalExpr _ (BooleanLit _ b) = bool b
> scalExpr nice (InPredicate _ att t lst) =
>   scalExpr nice att <+> (if not t then text "not" else empty) <+> text "in"
>   <+> parens (case lst of
>                        InList _ expr -> csvExp nice expr
>                        InQueryExpr _ sel -> queryExpr nice True True Nothing sel)
> scalExpr nice (LiftApp _ op flav args) =
>   scalExpr nice (head args) <+> name op
>   <+> text (case flav of
>               LiftAny -> "any"
>               LiftAll -> "all")
>   <+> parens (scalExpr nice $ head $ tail args)
> scalExpr nice (ScalarSubQuery _ s) = parens (queryExpr nice True True Nothing s)
> scalExpr _ (NullLit _) = text "null"
> scalExpr nice (WindowApp _ fn part order frm) =
>   scalExpr nice fn <+> text "over"
>   <+> parens (if hp || ho
>               then (if hp
>                     then text "partition by" <+> csvExp nice part
>                     else empty)
>                     <+> orderBy nice order
>                     <+> frameStuff
>               else empty)
>   where
>     hp = not (null part)
>     ho = not (null order)
>     frameStuff = case frm of
>                 FrameUnboundedPreceding -> text "range unbounded preceding"
>                 FrameUnboundedFull -> text "range between unbounded preceding and unbounded following"
>                 FrameRowsUnboundedPreceding -> text "rows unbounded preceding"
>
> scalExpr nice (AggregateApp _ d (App _ fn es) o) =
>   name fn <> parens ((case d of
>                         Dupes -> text "all"
>                         Distinct -> text "distinct")
>                      <+> csvExp nice es
>                      <+> orderBy nice o)
> scalExpr _ (AggregateApp _ _ _ _) = error "bad syntax for aggregate function"
> scalExpr nice (Case _ whens els) =
>   text "case"
>   $+$ nest 2 (vcat (map whn whens)
>               $+$ maybePrint (\e -> text "else" <+> scalExpr nice e) els)
>   $+$ text "end"
>       where
>         whn (ex1, ex2) =
>             text "when" <+> sepCsvMap (scalExpr nice) ex1
>             <+> text "then" <+> scalExpr nice ex2
>
> scalExpr nice (CaseSimple _ val whens els) =
>   text "case" <+> scalExpr nice val
>   $+$ nest 2 (vcat (map whn whens)
>               $+$ maybePrint (\e -> text "else" <+> scalExpr nice e) els)
>   $+$ text "end"
>       where
>         whn (ex1, ex2) =
>             text "when" <+> sepCsvMap (scalExpr nice) ex1
>             <+> text "then" <+> scalExpr nice ex2
>
> scalExpr _ (PositionalArg _ a) = text "$" <> integer a
> scalExpr _ (Placeholder _) = text "?"
> scalExpr nice (Exists _ s) =
>   text "exists" <+> parens (queryExpr nice True True Nothing s)
> scalExpr nice (Cast _ ex t) = text "cast" <> parens (scalExpr nice ex
>                                              <+> text "as"
>                                              <+> typeName t)
> scalExpr nice (TypedStringLit a t s) =
>   typeName t <+> scalExpr nice (StringLit a s)
> scalExpr nice (Interval a v f p) =
>   text "interval" <+> scalExpr nice (StringLit a v)
>   <+> intervalField <+> precision
>   where
>     intervalField =
>       text $ case f of
>                     IntervalYear -> "year"
>                     IntervalMonth -> "month"
>                     IntervalDay -> "day"
>                     IntervalHour -> "hour"
>                     IntervalMinute -> "minute"
>                     IntervalSecond -> "second"
>                     IntervalYearToMonth -> "year to month"
>                     IntervalDayToHour -> "day to hour"
>                     IntervalDayToMinute -> "day to minute"
>                     IntervalDayToSecond -> "day to second"
>                     IntervalHourToMinute -> "hour to minute"
>                     IntervalHourToSecond -> "hour to second"
>                     IntervalMinuteToSecond -> "minute to second"
>     precision = case p of
>                  Nothing -> empty
>                  Just i -> parens (int i)
> scalExpr nice (Extract _ f e) =
>   text "extract"
>   <> parens (text field <+> text "from" <+> scalExpr nice e)
>   where
>     field =
>       case f of
>              ExtractCentury -> "century"
>              ExtractDay -> "day"
>              ExtractDecade -> "decade"
>              ExtractDow -> "dow"
>              ExtractDoy -> "doy"
>              ExtractEpoch -> "epoch"
>              ExtractHour -> "hour"
>              ExtractIsodow -> "isodow"
>              ExtractIsoyear -> "isoyear"
>              ExtractMicroseconds -> "microseconds"
>              ExtractMillennium -> "millennium"
>              ExtractMilliseconds -> "milliseconds"
>              ExtractMinute -> "minute"
>              ExtractMonth -> "month"
>              ExtractQuarter -> "quarter"
>              ExtractSecond -> "second"
>              ExtractTimezone -> "timezone"
>              ExtractTimezoneHour -> "timezone_hour"
>              ExtractTimezoneMinute -> "timezone_minute"
>              ExtractWeek -> "week"
>              ExtractYear -> "year"


> scalExprSl :: Bool ->  ScalarExpr -> Doc
> scalExprSl nice (App _ f es) | Just "." <- getTName f
>                                 , [a@(Identifier _ _), b] <- es =
>   parens (scalExprSl nice a) <> text "." <> scalExprSl nice b
> scalExprSl nice x = scalExpr nice x

>
> set :: Bool -> SetClause -> Doc
> set nice (SetClause _ a e) =
>    -- (App _ "=" [Identifier _ a, e]) =
>   nmc a <+> text "=" <+> scalExpr nice e
> {-set nice (App _ "=" [a, b]) | (App _ "!rowctor" is1) <- a
>                                      ,(App _ "!rowctor" is2) <- b =
>   rsNoRow is1 <+> text "=" <+> rsNoRow is2
>   where
>     rsNoRow is = parens (sepCsvMap (scalExpr nice) is)
> set _ a = error $ "bad expression in set in update: " ++ show a-}
> set nice (MultiSetClause _ is (App _ f es)) | Just "!rowctor" <- getTName f =
>   parens (sepCsvMap nmc is) <+> text "="
>   <+> parens (sepCsvMap (scalExpr nice) es)
> set _ a = error $ "bad expression in set in update: " ++ show a
>
> --utils
>
> -- convert a list of expressions to horizontal csv
>
> csvExp :: Bool -> [ScalarExpr] -> Doc
> csvExp nice = hcatCsvMap (scalExpr nice)
>
> maybePrint :: (t -> Doc) -> Maybe t -> Doc
> maybePrint f c =
>     case c of
>       Nothing -> empty
>       Just a -> f a
>
> csv :: [Doc] -> [Doc]
> csv = punctuate comma
>
> hcatCsv :: [Doc] -> Doc
> hcatCsv = hcat . csv

> sepCsv :: [Doc] -> Doc
> sepCsv = sep . csv
>
> ifNotEmpty :: ([a] -> Doc) -> [a] -> Doc
> ifNotEmpty c l = if null l then empty else c l
>
> hcatCsvMap :: (a -> Doc) -> [a] -> Doc
> hcatCsvMap ex = hcatCsv . map ex

> sepCsvMap :: (a -> Doc) -> [a] -> Doc
> sepCsvMap ex = sepCsv . map ex

> orderBy :: Bool -> [(ScalarExpr,Direction)] -> Doc
> orderBy _ [] = empty
> orderBy nice os =
>   text "order by"
>   $+$ nest 2 (sepCsvMap (\(oe,od) -> scalExpr nice oe
>                                      <+> direction od) os)

> --vcatCsvMap :: (a -> Doc) -> [a] -> Doc
> --vcatCsvMap ex = vcat . csv . map ex

>
> bool :: Bool -> Doc
> bool b = if b then text "true" else text "false"
>
> newline :: Doc
> newline = text "\n"
>
> annot :: (Annotation -> String) -> Annotation -> Doc
> annot ca a = let s = ca a
>               in if s == ""
>                    then empty
>                    else text "/*\n" <+> text s
>                         <+> text "*/\n"

> label :: Maybe String -> Doc
> label =
>   maybe empty (\l -> text "<<"
>                      <+> text l
>                      <+> text ">>" <> text "\n")

util: to be removed when outputting names is fixed

> getTName :: Name -> Maybe String
> getTName (Name _ [n]) = Just $ ncStr n
> getTName _ = Nothing

