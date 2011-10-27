

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
> import Data.Char
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
> printStatementsAnn f ast = render $ vcat (map (convStatement False True f) ast) <> text "\n"
>

> printQueryExpr :: QueryExpr -> String
> printQueryExpr ast = render (convQueryExpr False True True ast <> statementEnd True)

> -- | Testing function, pretty print an expression
> printScalarExpr :: ScalarExpr -> String
> printScalarExpr = render . convExp False


> -- | Try harder to make the output human readable, not necessary correct
> -- sql output at the moment
> printQueryExprNice :: QueryExpr -> String
> printQueryExprNice ast = render (convQueryExpr True True True ast <> statementEnd True)


-------------------------------------------------------------------------------

Conversion routines - convert Sql asts into Docs

> -- Statements
>
> convStatement :: Bool -> Bool -> (Annotation -> String) -> Statement -> Doc
>
> -- selects
>
> convStatement nice se ca (QueryStatement ann s) =
>   convPa ca ann <+>
>   convQueryExpr nice True True s <> statementEnd se
>
> --dml
>
> convStatement nice se pa (Insert ann tb atts idata rt) =
>   convPa pa ann <+>
>   text "insert into" <+> convName tb
>   <+> ifNotEmpty (parens . sepCsvMap convNC) atts
>   $+$ convQueryExpr nice True True idata
>   $+$ convReturning nice rt
>   <> statementEnd se
>
> convStatement nice se ca (Update ann tb scs fr wh rt) =
>    convPa ca ann <+>
>    text "update" <+> convName tb <+> text "set"
>    <+> sepCsvMap (convSet nice) scs
>    <+> ifNotEmpty (\_ -> text "from" <+> sepCsvMap (convTref nice) fr) fr
>    <+> convWhere nice wh
>    $+$ convReturning nice rt <> statementEnd se
>
> convStatement nice se ca (Delete ann tbl us wh rt) =
>    convPa ca ann <+>
>    text "delete from" <+> convName tbl
>    <+> ifNotEmpty (\_ -> text "using" <+> sepCsvMap (convTref nice) us) us
>    <+> convWhere nice wh
>    $+$ convReturning nice rt
>    <> statementEnd se
>
> convStatement _nice se ca (Truncate ann names ri casc) =
>     convPa ca ann <+>
>     text "truncate"
>     <+> sepCsvMap convName names
>     <+> text (case ri of
>                       RestartIdentity -> "restart identity"
>                       ContinueIdentity -> "continue identity")
>     <+> convCasc casc
>     <> statementEnd se
>
> -- ddl
>
> convStatement nice se ca (CreateTable ann tbl atts cns) =
>     convPa ca ann <+>
>     text "create table"
>     <+> convName tbl <+> lparen
>     $+$ nest 2 (vcat (csv (map convAttDef atts ++ map (convCon nice) cns)))
>     $+$ rparen <> statementEnd se
>     where
>       convAttDef (AttributeDef _ n t def cons) =
>         convNC n <+> convTypeName t
>         <+> maybeConv (\e -> text "default" <+> convExp nice e) def
>         <+> hsep (map cCons cons)
>       cCons (NullConstraint _ cn) =
>         mname cn <+> text "null"
>       cCons (NotNullConstraint _ cn) =
>         mname cn <+> text "not null"
>       cCons (RowCheckConstraint _ cn ew) =
>         mname cn <+> text "check" <+> parens (convExp nice ew)
>       cCons (RowUniqueConstraint _ cn) =
>         mname cn <+> text "unique"
>       cCons (RowPrimaryKeyConstraint _ cn) =
>         mname cn <+> text "primary key"
>       cCons (RowReferenceConstraint _ cn tb att ondel onupd) =
>         mname cn <+> text "references" <+> convName tb
>         <+> maybeConv (parens . convNC) att
>         <+> text "on delete" <+> convCasc ondel
>         <+> text "on update" <+> convCasc onupd
>
> convStatement nice se ca (AlterTable ann name act) =
>     convPa ca ann <+>
>     text "alter table" <+> convName name
>     <+> hcatCsvMap convAct act <> statementEnd se
>     where
>       convAct (AlterColumnDefault _ nm def) =
>           text "alter column" <+> convNC nm
>           <+> text "set default" <+> convExp nice def
>       convAct (AddConstraint _ con) =
>           text "add " <+> convCon nice con
>
> convStatement _nice se ca (CreateSequence ann nm incr _ _ start cache) =
>     convPa ca ann <+>
>     text "create sequence" <+> convName nm <+>
>     text "increment" <+> text (show incr) <+>
>     text "no minvalue" <+>
>     text "no maxvalue" <+>
>     text "start" <+> text (show start) <+>
>     text "cache" <+> text (show cache) <> statementEnd se
>
> convStatement _nice se ca (AlterSequence ann nm o) =
>     convPa ca ann <+>
>     text "alter sequence" <+> convName nm
>     <+> text "owned by" <+> convName o <> statementEnd se
>
> convStatement nice se ca (CreateTableAs ann t sel) =
>     convPa ca ann <+>
>     text "create table"
>     <+> convName t <+> text "as"
>     $+$ convQueryExpr nice True True sel
>     <> statementEnd se
>
> convStatement nice se ca (CreateFunction ann name args retType rep lang body vol) =
>     convPa ca ann <+>
>     text ("create " ++ (case rep of
>                          Replace -> "or replace "
>                          _ -> "") ++ "function")
>     <+> convName name
>     <+> parens (sepCsvMap convParamDef args)
>     <+> text "returns" <+> convTypeName retType <+> text "as" <+> text "$$"
>     $+$ convFnBody body
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
>       convFnBody (SqlFnBody ann1 sts) =
>         convPa ca ann1 <+>
>         convNestedStatements nice ca sts
>       convFnBody (PlpgsqlFnBody ann1 blk) =
>           convPa ca ann1 <+>
>           convStatement nice True ca blk
>       convParamDef (ParamDef _ n t) = convNC n <+> convTypeName t
>       convParamDef  (ParamDefTp _ t) = convTypeName t
>
> convStatement nice se ca (Block ann lb decls sts) =
>   convPa ca ann <+>
>   convLabel lb <>
>   ifNotEmpty (\l -> text "declare"
>                   $+$ nest 2 (vcat $ map convVarDef l)) decls
>   $+$ text "begin"
>   $+$ convNestedStatements nice ca sts
>   $+$ text "end" <> statementEnd se
>   where
>       convVarDef (VarDef _ n t v) =
>         convNC n <+> convTypeName t
>         <+> maybeConv (\x -> text ":=" <+> convExp nice x) v <> semi
>       convVarDef (VarAlias _ n n1) =
>         convNC n <+> text "alias for" <+> convName n1 <> semi
>       convVarDef (ParamAlias _ n p) =
>         convNC n <+> text "alias for $" <> text (show p) <> semi
>
>
> convStatement nice se ca (CreateView ann name cols sel) =
>     convPa ca ann <+>
>     text "create view" <+> convName name
>     <> case cols of
>          Nothing -> empty
>          Just cs -> parens (sepCsvMap convNC cs)
>     <+> text "as"
>     $+$ nest 2 (convQueryExpr nice True True sel) <> statementEnd se
>
> convStatement nice se ca (CreateDomain ann name tp n ex) =
>     convPa ca ann <+>
>     text "create domain" <+> convName name <+> text "as"
>     <+> convTypeName tp <+> cname <+> checkExp ex <> statementEnd se
>     where
>       checkExp = maybeConv (\e -> text "check" <+> parens (convExp nice e))
>       cname = if n == ""
>                then empty
>                else text "constraint" <+> text n
>
> convStatement _nice se ca (DropFunction ann ifExists fns casc) =
>   convPa ca ann <+>
>   text "drop function"
>   <+> convIfExists ifExists
>   <+> sepCsvMap doFunction fns
>   <+> convCasc casc
>   <> statementEnd se
>   where
>     doFunction (name,types) =
>       convName name <> parens (sepCsvMap convTypeName types)
>
> convStatement _nice se ca (DropSomething ann dropType ifExists names casc) =
>     convPa ca ann <+>
>     text "drop"
>     <+> text (case dropType of
>                 Table -> "table"
>                 View -> "view"
>                 Domain -> "domain"
>                 Type -> "type")
>     <+> convIfExists ifExists
>     <+> sepCsvMap convName names
>     <+> convCasc casc
>     <> statementEnd se
>
> convStatement _nice se ca (CreateType ann name atts) =
>     convPa ca ann <+>
>     text "create type" <+> convName name <+> text "as" <+> lparen
>     $+$ nest 2 (vcat (csv
>           (map (\(TypeAttDef _ n t) -> convNC n <+> convTypeName t)  atts)))
>     $+$ rparen <> statementEnd se
>
> convStatement _nice se ca (CreateLanguage ann name) =
>     convPa ca ann <+>
>     text "create language" <+> text name <> statementEnd se
>
> convStatement nice se ca (CreateTrigger ann name wh events tbl firing fnName fnArgs) =
>     convPa ca ann <+>
>     text "create trigger" <+> convNC name
>     <+> text (case wh of
>                       TriggerBefore -> "before"
>                       TriggerAfter -> "after")
>     <+> evs
>     <+> text "on" <+> convName tbl
>     <+> text "for" <+> text (case firing of
>                                         EachRow -> "row"
>                                         EachStatement -> "statement")
>     <+> text "execute procedure" <+> convName fnName
>     <> parens (sepCsvMap (convExp nice) fnArgs) <> statementEnd se
>     where
>       evs = sep $ punctuate (text " or ") $ map
>             (text . (\e -> case e of
>                                 TInsert -> "insert"
>                                 TUpdate -> "update"
>                                 TDelete -> "delete")) events
>
> -- plpgsql
>
> convStatement _nice se ca (NullStatement ann) =
>   convPa ca ann <+> text "null" <> statementEnd se
> convStatement _nice se ca (ExitStatement ann lb) =
>   convPa ca ann <+> text "exit"
>     <+> maybe empty text lb <> statementEnd se
>

> convStatement _ _se _ca (Into _ann _str _into (QueryStatement _annq _s)) = error "no select into"

> convStatement nice se ca (Into ann str into st) =
>   convPa ca ann <+>
>   convStatement nice False ca st
>   <+> text "into"
>   <> (if str
>       then empty <+> text "strict"
>       else empty)
>   <+> sepCsvMap convName into
>   <> statementEnd se
>   --fixme, should be insert,update,delete,execute

> convStatement nice se ca (Assignment ann name val) =
>     convPa ca ann <+>
>     convExp nice name <+> text ":=" <+> convExp nice val <> statementEnd se
>
> convStatement nice se ca (Return ann ex) =
>     convPa ca ann <+>
>     text "return" <+> maybeConv (convExp nice) ex <> statementEnd se
>
> convStatement nice se ca (ReturnNext ann ex) =
>     convPa ca ann <+>
>     text "return" <+> text "next" <+> convExp nice ex <> statementEnd se
>
> convStatement nice se ca (ReturnQuery ann sel) =
>     convPa ca ann <+>
>     text "return" <+> text "query"
>     <+> convQueryExpr nice True True sel <> statementEnd se
>
> convStatement nice se ca (Raise ann rt st exps) =
>     convPa ca ann <+>
>     text "raise"
>     <+> case rt of
>                 RNotice -> text "notice"
>                 RException -> text "exception"
>                 RError -> text "error"
>     <+> convExp nice (StringLit emptyAnnotation st)
>     <> ifNotEmpty (\e -> comma <+> csvExp nice e) exps
>     <> statementEnd se
>
> convStatement nice se ca (ForQueryStatement ann lb i sel stmts) =
>     convPa ca ann <+>
>     convLabel lb <>
>     text "for" <+> convExp nice i <+> text "in"
>     <+> convQueryExpr nice True True sel <+> text "loop"
>     $+$ convNestedStatements nice ca stmts
>     $+$ text "end loop" <> statementEnd se
>
> convStatement nice se ca (ForIntegerStatement ann lb var st en stmts) =
>     convPa ca ann <+>
>     convLabel lb <>
>     text "for" <+> convExp nice var <+> text "in"
>     <+> convExp nice st <+> text ".." <+> convExp nice en <+> text "loop"
>     $+$ convNestedStatements nice ca stmts
>     $+$ text "end loop" <> statementEnd se
>
> convStatement nice se ca (WhileStatement ann lb ex stmts) =
>     convPa ca ann <+>
>     convLabel lb <>
>     text "while" <+> convExp nice ex <+> text "loop"
>     $+$ convNestedStatements nice ca stmts
>     $+$ text "end loop" <> statementEnd se
> convStatement nice se ca (LoopStatement ann lb stmts) =
>     convPa ca ann <+>
>     convLabel lb <>
>     text "loop"
>     $+$ convNestedStatements nice ca stmts
>     $+$ text "end loop" <> statementEnd se
>
> convStatement _nice se ca (ContinueStatement ann lb) =
>     convPa ca ann <+> text "continue"
>       <+> maybe empty text lb <> statementEnd se
> convStatement nice se ca (Perform ann f@(FunCall _ _ _)) =
>     convPa ca ann <+>
>     text "perform" <+> convExp nice f <> statementEnd se
> convStatement _ _ _ (Perform _ x) =
>    error $ "internal error: convStatement not supported for " ++ show x
>
> convStatement _nice se ca (Copy ann tb cols src) =
>     convPa ca ann <+>
>     text "copy" <+> convName tb
>     <+> ifNotEmpty (parens . sepCsvMap convNC) cols
>     <+> text "from"
>     <+> case src of
>                  CopyFilename s -> quotes $ text s <> statementEnd se
>                  Stdin -> text "stdin" <> text ";"
>
> convStatement _ _ ca (CopyData ann s) =
>     convPa ca ann <+>
>     text s <> text "\\." <> newline
>
> convStatement nice se ca (If ann conds els) =
>    convPa ca ann <+>
>    text "if" <+> convCond (head conds)
>    $+$ vcat (map (\c -> text "elseif" <+> convCond c) $ tail conds)
>    $+$ ifNotEmpty (\e -> text "else" $+$ convNestedStatements nice ca e) els
>    $+$ text "end if" <> statementEnd se
>     where
>       convCond (ex, sts) = convExp nice ex <+> text "then"
>                            $+$ convNestedStatements nice ca sts
> convStatement nice se ca (Execute ann s) =
>     convPa ca ann <+>
>     text "execute" <+> convExp nice s <> statementEnd se
>
>
> convStatement nice se ca (CaseStatementSimple ann c conds els) =
>     convPa ca ann <+>
>     text "case" <+> convExp nice c
>     $+$ nest 2 (
>                 vcat (map (uncurry convWhenSt) conds)
>                 $+$ convElseSt els
>                 ) $+$ text "end case" <> statementEnd se
>     where
>       convWhenSt ex sts = text "when" <+> sepCsvMap (convExp nice) ex
>                           <+> text "then" $+$ convNestedStatements nice ca sts
>       convElseSt = ifNotEmpty (\s -> text "else"
>                                      $+$ convNestedStatements nice ca s)
> convStatement nice se ca (CaseStatement ann conds els) =
>     convPa ca ann <+>
>     text "case"
>     $+$ nest 2 (
>                 vcat (map (uncurry convWhenSt) conds)
>                 $+$ convElseSt els
>                 ) $+$ text "end case" <> statementEnd se
>     where
>       convWhenSt ex sts = text "when" <+> sepCsvMap (convExp nice) ex
>                           <+> text "then" $+$ convNestedStatements nice ca sts
>       convElseSt = ifNotEmpty (\s -> text "else"
>                                      $+$ convNestedStatements nice ca s)

>
> -- misc
>
> convStatement _nice se _ (Set _ n vs) =
>   text "set" <+> text n <+> text "="
>   <+> sepCsvMap (text . dv) vs <> statementEnd se
>   where
>     dv (SetStr _ s) = "'" ++ s ++ "'"
>     dv (SetId _ i) = i
>     dv (SetNum _ nm) = show nm
>
> convStatement _nice se _ (Notify _ n) =
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
> convQueryExpr :: Bool -> Bool -> Bool -> QueryExpr -> Doc
> convQueryExpr nice writeSelect _ (Select _ dis l tb wh grp hav
>                                 order lim off) =
>   (text (if writeSelect then "select" else "")
>          <+> (case dis of
>                  Dupes -> empty
>                  Distinct -> text "distinct"))
>   $+$ nest 2 (vcat $ catMaybes
>   [Just $ nest 2 $ convSelList nice l
>   ,Just $ if null tb
>           then empty
>           else text "from" $+$ nest 2 (sepCsvMap (convTref nice) tb)
>   ,Just $ convWhere nice wh
>   ,case grp of
>      [] -> Nothing
>      g -> Just $ text "group by" $+$ nest 2 (sepCsvMap (convExp nice) g)
>   ,flip fmap hav $ \h -> text "having" $+$ nest 2 (convExp nice h)
>   ,Just $ convOrderBy nice order
>   ,flip fmap lim $ \lm -> text "limit" <+> convExp nice lm
>   ,flip fmap off $ \offs -> text "offset" <+> convExp nice offs
>   ])
>
> convQueryExpr nice writeSelect topLev (CombineQueryExpr _ tp s1 s2) =
>   let p = convQueryExpr nice writeSelect False s1
>           $+$ (case tp of
>                        Except -> text "except"
>                        Union -> text "union"
>                        UnionAll -> text "union" <+> text "all"
>                        Intersect -> text "intersect")
>           $+$ convQueryExpr nice True False s2
>   in if topLev then p else parens p
> convQueryExpr nice _ _ (Values _ expss) =
>   text "values" $$ nest 2 (vcat $ csv $ map (parens . csvExp nice) expss)
> convQueryExpr nice _ _ (WithQueryExpr _ wqs ex) =
>   text "with" $$ nest 2 (vcat $ csv $ map pwq wqs)
>        $+$ convQueryExpr nice True False ex
>   where
>     pwq (WithQuery _ nm cs ex1) =
>       convNC nm <> case cs of
>                    Nothing -> empty
>                    Just cs' -> parens $ sepCsvMap convNC cs'
>       <+> text "as"
>       <+> parens (convQueryExpr nice True False ex1)

> convName :: Name -> Doc
> convName (Name _ ns) = convNcs ns

> convNcs :: [NameComponent] -> Doc
> convNcs ns = hcat $ punctuate (text ".") $ map convNC ns

> convNC :: NameComponent -> Doc
> convNC (Nmc ns) = text ns

>
> convTref :: Bool -> TableRef -> Doc
> {-convTref nice (Tref _ f@(SQIdentifier _ t) (TableAlias _ ta))
>   | nice, last t == ta = convName f
>   -- slightly bad hack:
> convTref nice (Tref _ f@(SQIdentifier _ t) (FullAlias _ ta _))
>   | nice, last t == ta = convName f-}

> convTref nice (Tref _ f a) = convName f <+> convTrefAlias nice a
> convTref nice (JoinTref _ t1 nat jt t2 ex a) =
>         parens (convTref nice t1
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
>         <+> convTref nice t2
>         <+> maybeConv (nest 2 . convJoinScalarExpr) ex
>         <+> convTrefAlias nice a)
>         where
>           convJoinScalarExpr (JoinOn _ e) = text "on" <+> convExp nice e
>           convJoinScalarExpr (JoinUsing _ ids) =
>               text "using" <+> parens (sepCsvMap convNC ids)
>
> convTref nice (SubTref _ sub alias) =
>         parens (convQueryExpr nice True True sub)
>         <+> text "as" <+> convTrefAlias nice alias
> convTref nice (FunTref _ f@(FunCall _ _ _) a) = convExp nice f <+> convTrefAlias nice a
> convTref _nice (FunTref _ x _) =
>       error $ "internal error: node not supported in function tref: "
>             ++ show x
>
> convTrefAlias :: Bool -> TableAlias -> Doc
> convTrefAlias _ (NoAlias _) = empty
> convTrefAlias _ (TableAlias _ t) = convNC t
> -- hack this out for now. When the type checking is fixed, can try
> -- to eliminate unneeded aliases?
> convTrefAlias nice (FullAlias _ t s) =
>   convNC t <> (if nice
>              then empty
>              else parens (sepCsvMap convNC s))

> convDir :: Direction -> Doc
> convDir d = text $ case d of
>                           Asc -> "asc"
>                           Desc -> "desc"
>
> convWhere :: Bool -> Maybe ScalarExpr -> Doc
> convWhere nice (Just ex) = text "where" $+$ nest 2 (convExp nice ex)
> convWhere _ Nothing = empty
>
> convSelList :: Bool -> SelectList -> Doc
> convSelList nice (SelectList _ ex) =
>   sepCsvMap convSelItem ex
>   -- <+> ifNotEmpty (\i -> text "into" <+> hcatCsvMap convExp i) into
>   where
>     -- try to avoid printing alias if not necessary
>     convSelItem (SelectItem _ ex1@(QIdentifier _ is) nm) | nice, last is == nm = convExpSl nice ex1
>     convSelItem (SelectItem _ ex1@(Identifier _ i) nm) | nice, i == nm = convExpSl nice ex1
>     convSelItem (SelectItem _ ex1 nm) = convExpSl nice ex1 <+> text "as" <+> convNC nm
>     convSelItem (SelExp _ e) = convExpSl nice e
>
> convCasc :: Cascade -> Doc
> convCasc casc = text $ case casc of
>                                  Cascade -> "cascade"
>                                  Restrict -> "restrict"
> -- ddl
>
> convCon :: Bool -> Constraint -> Doc
> convCon _nice (UniqueConstraint _ n c) =
>         mname n <+> text "unique"
>         <+> parens (sepCsvMap convNC c)
> convCon _nice (PrimaryKeyConstraint _ n p) =
>         mname n <+>
>         text "primary key"
>         <+> parens (sepCsvMap convNC p)
> convCon nice (CheckConstraint _ n c) =
>         mname n <+> text "check" <+> parens (convExp nice c)
> convCon _nice (ReferenceConstraint _ n at tb rat ondel onupd) =
>         mname n <+>
>         text "foreign key" <+> parens (sepCsvMap convNC at)
>         <+> text "references" <+> convName tb
>         <+> ifNotEmpty (parens . sepCsvMap convNC) rat
>         <+> text "on update" <+> convCasc onupd
>         <+> text "on delete" <+> convCasc ondel
>
> mname :: String -> Doc
> mname n = if n == ""
>           then empty
>           else text "constraint" <+> text n
>
> convReturning :: Bool -> Maybe SelectList -> Doc
> convReturning nice l = case l of
>                 Nothing -> empty
>                 Just ls -> nest 2 (text "returning" <+> convSelList nice ls)
>
> convIfExists :: IfExists -> Doc
> convIfExists i = case i of
>                         Require -> empty
>                         IfExists -> text "if exists"
>
> -- plpgsql
>
> convNestedStatements :: Bool -> (Annotation -> String) -> StatementList -> Doc
> convNestedStatements nice pa = nest 2 . vcat . map (convStatement nice True pa)
>
> convTypeName :: TypeName -> Doc
> convTypeName (SimpleTypeName _ s) = text s
> convTypeName (PrecTypeName _ s i) = text s <> parens(integer i)
> convTypeName (Prec2TypeName _ s i i1) = text s <> parens (sepCsv [integer i, integer i1])
> convTypeName (ArrayTypeName _ t) = convTypeName t <> text "[]"
> convTypeName (SetOfTypeName _ t) = text "setof" <+> convTypeName t
>
> -- expressions
>
> convExp :: Bool -> ScalarExpr -> Doc
> convExp _ (Star _) = text "*"
> convExp _ (QStar _ i) = convNC i <> text ".*"

> convExp _ (Identifier _ i) = convNC i
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
> convExp nice (QIdentifier a [i1, i]) = parens (convNC i1) <> text "." <> convNC i
> --convExp nice (QIdentifier a e i) = parens (convExp nice e) <> text "." <> convExp nice (Identifier a i)

> --convExp (PIdentifier _ i) = parens $ convExp i
> convExp _ (NumberLit _ n) = text n
> convExp _ (StringLit _ s) = -- needs some thought about using $$?
>                           text "'" <> text replaceQuotes <> text "'"
>                           where
>                             replaceQuotes = replace "'" "''" s {-if tag == "'"
>                                               then replace "'" "''" s
>                                               else s-}
>
> convExp nice (FunCall _ n es) =
>     --check for special operators
>    case getTName n of
>      Just "!and" | nice, [a,b] <- es -> doLeftAnds a b
>      Just "!arrayctor" -> text "array" <> brackets (csvExp nice es)
>      Just "!between" -> convExp nice (head es) <+> text "between"
>                    <+> parens (convExp nice (es !! 1))
>                   <+> text "and"
>                   <+> parens (convExp nice (es !! 2))
>      Just "!substring" -> text "substring"
>                      <> parens (convExp nice (head es)
>                                 <+> text "from" <+> convExp nice (es !! 1)
>                                 <+> text "for" <+> convExp nice (es !! 2))
>      Just "!arraysub" -> case es of
>                        (Identifier _ i : es1) -> convNC i
>                                                  <> brackets (csvExp nice es1)
>                        _ -> parens (convExp nice (head es))
>                             <> brackets (csvExp nice (tail es))
>      Just "!rowctor" -> text "row" <> parens (sepCsvMap (convExp nice) es)
>      Just "."   -- special case to avoid ws around '.'. Don't know if this is important
>            -- or just cosmetic
>          | [a,b] <- es -> parens (convExp nice a) <> text "." <> convExp nice b
>      Just n' | isOperatorName n' ->
>         case forceRight (getOperatorType defaultTemplate1Catalog n') of
>                           BinaryOp ->
>                               let e1d = convExp nice (head es)
>                                   opd = text $ filterKeyword n'
>                                   e2d = convExp nice (es !! 1)
>                               in parens (if n' `elem` ["!and", "!or"]
>                                          then vcat [e1d, opd <+> e2d]
>                                          else e1d <+> opd <+> e2d)
>                           PrefixOp -> parens (text (if n' == "u-"
>                                                     then "-"
>                                                     else filterKeyword n')
>                                                <+> parens (convExp nice (head es)))
>                           PostfixOp -> parens (convExp nice (head es)
>                                        <+> text (filterKeyword n'))
>      _ | otherwise -> convName n <> parens (csvExp nice es)
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
>                       in vcat ((convExp nice (head as)
>                                 : map (\x -> text "and" <+> convExp nice x) (tail as))
>                                ++ [text "and" <+> convExp nice b])
>      and' a = case a of
>                 FunCall _ f [x,y] | Just "!and" <- getTName f -> and' x ++ and' y
>                 _ -> [a]
>
> convExp _ (BooleanLit _ b) = bool b
> convExp nice (InPredicate _ att t lst) =
>   convExp nice att <+> (if not t then text "not" else empty) <+> text "in"
>   <+> parens (case lst of
>                        InList _ expr -> csvExp nice expr
>                        InQueryExpr _ sel -> convQueryExpr nice True True sel)
> convExp nice (LiftOperator _ op flav args) =
>   convExp nice (head args) <+> text op
>   <+> text (case flav of
>               LiftAny -> "any"
>               LiftAll -> "all")
>   <+> parens (convExp nice $ head $ tail args)
> convExp nice (ScalarSubQuery _ s) = parens (convQueryExpr nice True True s)
> convExp _ (NullLit _) = text "null"
> convExp nice (WindowFn _ fn part order frm) =
>   convExp nice fn <+> text "over"
>   <+> parens (if hp || ho
>               then (if hp
>                     then text "partition by" <+> csvExp nice part
>                     else empty)
>                     <+> convOrderBy nice order
>                     <+> convFrm
>               else empty)
>   where
>     hp = not (null part)
>     ho = not (null order)
>     convFrm = case frm of
>                 FrameUnboundedPreceding -> text "range unbounded preceding"
>                 FrameUnboundedFull -> text "range between unbounded preceding and unbounded following"
>                 FrameRowsUnboundedPreceding -> text "rows unbounded preceding"
>
> convExp nice (AggregateFn _ d (FunCall _ fn es) o) =
>   convName fn <> parens ((case d of
>                         Dupes -> text "all"
>                         Distinct -> text "distinct")
>                      <+> csvExp nice es
>                      <+> convOrderBy nice o)
> convExp _ (AggregateFn _ _ _ _) = error $ "bad syntax for aggregate function"
> convExp nice (Case _ whens els) =
>   text "case"
>   $+$ nest 2 (vcat (map convWhen whens)
>               $+$ maybeConv (\e -> text "else" <+> convExp nice e) els)
>   $+$ text "end"
>       where
>         convWhen (ex1, ex2) =
>             text "when" <+> sepCsvMap (convExp nice) ex1
>             <+> text "then" <+> convExp nice ex2
>
> convExp nice (CaseSimple _ val whens els) =
>   text "case" <+> convExp nice val
>   $+$ nest 2 (vcat (map convWhen whens)
>               $+$ maybeConv (\e -> text "else" <+> convExp nice e) els)
>   $+$ text "end"
>       where
>         convWhen (ex1, ex2) =
>             text "when" <+> sepCsvMap (convExp nice) ex1
>             <+> text "then" <+> convExp nice ex2
>
> convExp _ (PositionalArg _ a) = text "$" <> integer a
> convExp _ (Placeholder _) = text "?"
> convExp nice (Exists _ s) =
>   text "exists" <+> parens (convQueryExpr nice True True s)
> convExp nice (Cast _ ex t) = text "cast" <> parens (convExp nice ex
>                                              <+> text "as"
>                                              <+> convTypeName t)
> convExp nice (TypedStringLit a t s) =
>   convTypeName t <+> convExp nice (StringLit a s)
> convExp nice (Interval a v f p) =
>   text "interval" <+> convExp nice (StringLit a v)
>   <+> convIntervalField <+> convPrec
>   where
>     convIntervalField =
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
>     convPrec = case p of
>                  Nothing -> empty
>                  Just i -> parens (int i)
> convExp nice (Extract _ f e) =
>   text "extract"
>   <> parens (text convField <+> text "from" <+> convExp nice e)
>   where
>     convField =
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


> convExpSl :: Bool ->  ScalarExpr -> Doc
> convExpSl nice (FunCall _ f es) | Just "." <- getTName f
>                                 , [a@(Identifier _ _), b] <- es =
>   parens (convExpSl nice a) <> text "." <> convExpSl nice b
> convExpSl nice x = convExp nice x

>
> convSet :: Bool -> SetClause -> Doc
> convSet nice (SetClause _ a e) =
>    -- (FunCall _ "=" [Identifier _ a, e]) =
>   convNC a <+> text "=" <+> convExp nice e
> {-convSet nice (FunCall _ "=" [a, b]) | (FunCall _ "!rowctor" is1) <- a
>                                      ,(FunCall _ "!rowctor" is2) <- b =
>   rsNoRow is1 <+> text "=" <+> rsNoRow is2
>   where
>     rsNoRow is = parens (sepCsvMap (convExp nice) is)
> convSet _ a = error $ "bad expression in set in update: " ++ show a-}
> convSet nice (MultiSetClause _ is (FunCall _ f es)) | Just "!rowctor" <- getTName f =
>   parens (sepCsvMap convNC is) <+> text "="
>   <+> parens (sepCsvMap (convExp nice) es)
> convSet _ a = error $ "bad expression in set in update: " ++ show a
>
> --utils
>
> -- convert a list of expressions to horizontal csv
>
> csvExp :: Bool -> [ScalarExpr] -> Doc
> csvExp nice = hcatCsvMap (convExp nice)
>
> maybeConv :: (t -> Doc) -> Maybe t -> Doc
> maybeConv f c =
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

> convOrderBy :: Bool -> [(ScalarExpr,Direction)] -> Doc
> convOrderBy _ [] = empty
> convOrderBy nice os =
>   text "order by"
>   $+$ nest 2 (sepCsvMap (\(oe,od) -> convExp nice oe
>                                      <+> convDir od) os)

> --vcatCsvMap :: (a -> Doc) -> [a] -> Doc
> --vcatCsvMap ex = vcat . csv . map ex

>
> bool :: Bool -> Doc
> bool b = if b then text "true" else text "false"
>
> newline :: Doc
> newline = text "\n"
>
> convPa :: (Annotation -> String) -> Annotation -> Doc
> convPa ca a = let s = ca a
>               in if s == ""
>                    then empty
>                    else text "/*\n" <+> text s
>                         <+> text "*/\n"

> convLabel :: Maybe String -> Doc
> convLabel =
>   maybe empty (\l -> text "<<"
>                      <+> text l
>                      <+> text ">>" <> text "\n")

util: to be removed when outputting names is fixed

> getTName :: Name -> Maybe String
> getTName (Name _ [n]) = Just $ ncStr n
> getTName _ = Nothing

