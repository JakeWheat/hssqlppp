

> {- | Functions to convert sql asts to valid SQL source code.
>
>    Some effort is made produce human readable output.
> -}
> {-# LANGUAGE PatternGuards,OverloadedStrings,TypeSynonymInstances,FlexibleInstances #-}
> module Database.HsSqlPpp.Pretty
>     (--convert a sql ast to text
>      printStatements
>     --,printStatementsAnn
>     ,printQueryExpr
>      --convert a single expression parse node to text
>     ,printScalarExpr
>     ,PrettyPrintFlags(..)
>     ,defaultPPFlags
>     ,SQLSyntaxDialect(..)
>     )
>     where
>
> import Text.PrettyPrint
> import Data.Maybe
>
> import Database.HsSqlPpp.Ast hiding (ann)
> import Database.HsSqlPpp.Annotation
> --import Database.HsSqlPpp.Utils.Utils

> import Database.HsSqlPpp.SqlDialect

> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Internals.StringLike
> import qualified Data.Text as T
> import qualified Data.Text.Lazy as L
> import Database.HsSqlPpp.Utils.Utils

--------------------------------------------------------------------------------

Public functions

> data PrettyPrintFlags =
>   PrettyPrintFlags

todo: actually use the dialect. this will forced when the parser is
adjusted to reject postgres only syntax when in sql server dialect

>   {ppDialect :: SQLSyntaxDialect
>   }
>   deriving (Show,Eq)

> defaultPPFlags :: PrettyPrintFlags
> defaultPPFlags = PrettyPrintFlags {ppDialect = PostgreSQLDialect}

> -- | Convert an ast back to valid SQL source.
> printStatements :: PrettyPrintFlags -> StatementList -> L.Text
> printStatements f = printStatementsAnn f (const "")
>
> -- | Convert the ast back to valid source, and convert any annotations to
> -- text using the function provided and interpolate the output of
> -- this function (inside comments) with the SQL source.

this needs some work

> printStatementsAnn :: PrettyPrintFlags -> (Annotation -> String) -> StatementList -> L.Text
> printStatementsAnn flg f ast =
>   renderText $ vcat (map (statement flg True f) ast) <> text "\n"

> -- | pretty print a query expression
> printQueryExpr :: PrettyPrintFlags -> QueryExpr -> L.Text
> printQueryExpr f ast = renderText (queryExpr f True True Nothing ast <> statementEnd True)

> -- | pretty print a scalar expression
> printScalarExpr :: PrettyPrintFlags -> ScalarExpr -> L.Text
> printScalarExpr f = renderText . scalExpr f

direct = true means attempt to pretty print straight to text
direct = false means pretty print to string then pack to text
no idea which is better, since pretty printing to text directly uses a
 lot of Text.cons which might be pretty slow and bloated

> direct :: Bool
> direct = True

> renderText :: Doc -> L.Text
> renderText doc =
>   if direct
>   then fullRender (mode style) (lineLength style) (ribbonsPerLine style)
>                    dataTextPrinter "" doc
>   else L.pack $ render doc


> dataTextPrinter :: TextDetails -> L.Text -> L.Text
> dataTextPrinter (Chr c)   s  = L.cons c s
> dataTextPrinter (Str s1)  s2 = L.pack s1 `L.append` s2
> dataTextPrinter (PStr s1) s2 = L.pack s1 `L.append` s2

-------------------------------------------------------------------------------

Conversion routines - convert Sql asts into Docs

> -- Statements
>
> statement :: PrettyPrintFlags -> Bool -> (Annotation -> String) -> Statement -> Doc
> statement _flg _se _ca (AntiStatement s) = text $ "$(" ++ s ++ ")"
>
> -- selects
>
> statement flg se ca (QueryStatement ann s) =
>   annot ca ann <+>
>   queryExpr flg True True Nothing s <> statementEnd se
>
> --dml
>
> statement flg se pa (Insert ann tb atts idata rt) =
>   annot pa ann <+>
>   text "insert into" <+> name tb
>   <+> ifNotEmpty (parens . sepCsvMap nmc) atts
>   $+$ queryExpr flg True True Nothing idata
>   $+$ returning flg rt
>   <> statementEnd se
>
> statement flg se ca (Update ann tb scs fr wh rt) =
>    annot ca ann <+>
>    text "update" <+> name tb <+> text "set"
>    <+> sepCsvMap (set flg) scs
>    <+> ifNotEmpty (\_ -> text "from" <+> sepCsvMap (tref flg) fr) fr
>    <+> whr flg wh
>    $+$ returning flg rt <> statementEnd se
>
> statement flg se ca (Delete ann tbl us wh rt) =
>    annot ca ann <+>
>    text "delete from" <+> name tbl
>    <+> ifNotEmpty (\_ -> text "using" <+> sepCsvMap (tref flg) us) us
>    <+> whr flg wh
>    $+$ returning flg rt
>    <> statementEnd se
>
> statement flg se ca (Truncate ann names ri casc) =
>     annot ca ann <+>
>     text "truncate"
>     <+> sepCsvMap name names
>     <+> case (ppDialect flg) of
>        SQLServerDialect -> empty
>        _ -> text (case ri of
>                       RestartIdentity -> "restart identity"
>                       ContinueIdentity -> "continue identity")
>             <+> cascade casc
>     <> statementEnd se
>
> -- ddl
>
> statement flg se ca (CreateTable ann tbl atts cns partition) =
>     annot ca ann <+>
>     text "create table"
>     <+> name tbl <+> lparen
>     $+$ nest 2 (vcat (csv (map (attrDef flg) atts ++ map (constraint flg) cns)))
>     $+$ rparen 
>     $+$ case (ppDialect flg) of
>          SQLServerDialect -> empty
>          _ -> (tablePartition partition)
>     <> statementEnd se
>
> statement flg se ca (AlterTable ann nm op) =
>     annot ca ann <+>
>     text "alter table" <+> name nm
>     <+> alterOperation op <> statementEnd se
>     where
>       alterOperation (RenameTable _ nm) = 
>           text "rename to" <+> name nm
>       alterOperation (RenameColumn _ old new) = 
>           text "rename column" <+> nmc old <+> "to" <+> nmc new
>       alterOperation (AlterTableActions _ actions) = hcatCsvMap alterAction actions
>       alterAction (AddColumn _ att) = 
>           text "add column" <+> attrDef flg att
>       alterAction (DropColumn _ nm) = 
>           text "drop column" <+> nmc nm
>       alterAction (AlterColumn _ nm action) = 
>           text "alter column" <+> nmc nm <+> alterColumnAction action
>       alterAction (AddConstraint _ con) =
>           text "add" <+> constraint flg con
>       alterColumnAction (SetDataType _ t) =
>           text "set data type" <+> typeName t
>       alterColumnAction (SetNotNull _) =
>           text "set not null"
>       alterColumnAction (DropNotNull _) =
>           text "drop not null"
>       alterColumnAction (SetDefault _ def) =
>           text "set default" <+> scalExpr flg def
>       alterColumnAction (DropDefault _) =
>           text "drop default"
>
> statement _flg se ca (CreateSequence ann nm incr _ _ start cache) =
>     annot ca ann <+>
>     text "create sequence" <+> name nm <+>
>     text "increment" <+> text (show incr) <+>
>     text "no minvalue" <+>
>     text "no maxvalue" <+>
>     text "start" <+> text (show start) <+>
>     text "cache" <+> text (show cache) <> statementEnd se
>
> statement _flg se ca (AlterSequence ann nm o) =
>     annot ca ann <+>
>     text "alter sequence" <+> name nm
>     <+> text "owned by" <+> name o <> statementEnd se
>
> statement flg se ca (CreateTableAs ann t sel) =
>     annot ca ann <+>
>     text "create table"
>     <+> name t <+> text "as"
>     $+$ queryExpr flg True True Nothing sel
>     <> statementEnd se
>
> statement flg se ca (CreateFunction ann nm args retType rep lang body vol) =
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
>         nestedStatements flg ca sts
>       functionBody (PlpgsqlFnBody ann1 blk) =
>           annot ca ann1 <+>
>           statement flg True ca blk
>       paramDefn (ParamDef _ n t) = nmc n <+> typeName t
>       paramDefn  (ParamDefTp _ t) = typeName t
>
> statement flg se ca (Block ann lb decls sts) =
>   annot ca ann <+>
>   label lb <>
>   ifNotEmpty (\l -> text "declare"
>                   $+$ nest 2 (vcat $ map varDefn l)) decls
>   $+$ text "begin"
>   $+$ nestedStatements flg ca sts
>   $+$ text "end" <> statementEnd se
>   where
>       varDefn (VarDef _ n t v) =
>         nmc n <+> typeName t
>         <+> maybePrint (\x -> text ":=" <+> scalExpr flg x) v <> semi
>       varDefn (VarAlias _ n n1) =
>         nmc n <+> text "alias for" <+> name n1 <> semi
>       varDefn (ParamAlias _ n p) =
>         nmc n <+> text "alias for $" <> text (show p) <> semi
>
>
> statement flg se ca (CreateView ann nm cols sel) =
>     annot ca ann <+>
>     text "create view" <+> name nm
>     <> case cols of
>          Nothing -> empty
>          Just cs -> parens (sepCsvMap nmc cs)
>     <+> text "as"
>     $+$ nest 2 (queryExpr flg True True Nothing sel) <> statementEnd se
>
> statement flg se ca (CreateDomain ann nm tp n ex) =
>     annot ca ann <+>
>     text "create domain" <+> name nm <+> text "as"
>     <+> typeName tp <+> cname <+> checkExp ex <> statementEnd se
>     where
>       checkExp = maybePrint (\e -> text "check" <+> parens (scalExpr flg e))
>       cname = if n == ""
>                then empty
>                else text "constraint" <+> ttext n
>
> statement _flg se ca (CreateDatabase ann nm) =
>     annot ca ann <+>
>     text "create database" <+> name nm <> statementEnd se
>
> statement _flg se ca (DropFunction ann ifE fns casc) =
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
> statement flg se ca (DropSomething ann dropType ifE names casc) =
>     annot ca ann <+>
>     text "drop"
>     <+> text (case dropType of
>                 Table -> "table"
>                 View -> "view"
>                 Domain -> "domain"
>                 Type -> "type"
>                 Database -> "database")
>     <+> ifExists ifE
>     <+> sepCsvMap name names
>     <+> case (ppDialect flg) of
>            SQLServerDialect -> empty
>            _ -> cascade casc
>     <> statementEnd se
>
> statement _flg se ca (DropTrigger ann ifE nam tbn casc) =
>     annot ca ann <+>
>     text "drop"
>     <+> text "trigger"
>     <+> ifExists ifE
>     <+> nmc nam
>     <+> text "on"
>     <+> name tbn
>     <+> cascade casc
>     <> statementEnd se
>
> statement _flg se ca (CreateType ann nm atts) =
>     annot ca ann <+>
>     text "create type" <+> name nm <+> text "as" <+> lparen
>     $+$ nest 2 (vcat (csv
>           (map (\(TypeAttDef _ n t) -> nmc n <+> typeName t)  atts)))
>     $+$ rparen <> statementEnd se
>
> statement _flg se ca (CreateLanguage ann nm) =
>     annot ca ann <+>
>     text "create language" <+> ttext nm <> statementEnd se
>
> statement flg se ca (CreateTrigger ann nm wh events tbl firing fnName fnArgs) =
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
>     <> parens (sepCsvMap (scalExpr flg) fnArgs) <> statementEnd se
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
> statement _flg se ca (NullStatement ann) =
>   annot ca ann <+> text "null" <> statementEnd se
> statement _flg se ca (ExitStatement ann lb) =
>   annot ca ann <+> text "exit"
>     <+> maybe empty ttext lb <> statementEnd se
>

> statement flg se ca (Into ann str is (QueryStatement _ q)) =
>   annot ca ann <+>
>   queryExpr flg True True (Just (str,is)) q <> statementEnd se


> statement flg se ca (Into ann str into st) =
>   annot ca ann <+>
>   statement flg False ca st
>   <+> text "into"
>   <> (if str
>       then empty <+> text "strict"
>       else empty)
>   <+> sepCsvMap name into
>   <> statementEnd se
>   --fixme, should be insert,update,delete,execute

> statement flg se ca (Assignment ann nm val) =
>     annot ca ann <+>
>     name nm <+> text ":=" <+> scalExpr flg val <> statementEnd se
>
> statement flg se ca (Return ann ex) =
>     annot ca ann <+>
>     text "return" <+> maybePrint (scalExpr flg) ex <> statementEnd se
>
> statement flg se ca (ReturnNext ann ex) =
>     annot ca ann <+>
>     text "return" <+> text "next" <+> scalExpr flg ex <> statementEnd se
>
> statement flg se ca (ReturnQuery ann sel) =
>     annot ca ann <+>
>     text "return" <+> text "query"
>     <+> queryExpr flg True True Nothing sel <> statementEnd se
>
> statement flg se ca (Raise ann rt st exps) =
>     annot ca ann <+>
>     text "raise"
>     <+> case rt of
>                 RNotice -> text "notice"
>                 RException -> text "exception"
>                 RError -> text "error"
>     <+> scalExpr flg (StringLit emptyAnnotation st)
>     <> ifNotEmpty (\e -> comma <+> csvExp flg e) exps
>     <> statementEnd se
>
> statement flg se ca (ForQueryStatement ann lb i sel stmts) =
>     annot ca ann <+>
>     label lb <>
>     text "for" <+> nmc i <+> text "in"
>     <+> queryExpr flg True True Nothing sel <+> text "loop"
>     $+$ nestedStatements flg ca stmts
>     $+$ text "end loop" <> statementEnd se
>
> statement flg se ca (ForIntegerStatement ann lb var st en stmts) =
>     annot ca ann <+>
>     label lb <>
>     text "for" <+> nmc var <+> text "in"
>     <+> scalExpr flg st <+> text ".." <+> scalExpr flg en <+> text "loop"
>     $+$ nestedStatements flg ca stmts
>     $+$ text "end loop" <> statementEnd se
>
> statement flg se ca (WhileStatement ann lb ex stmts) =
>     annot ca ann <+>
>     label lb <>
>     text "while" <+> scalExpr flg ex <+> text "loop"
>     $+$ nestedStatements flg ca stmts
>     $+$ text "end loop" <> statementEnd se
> statement flg se ca (LoopStatement ann lb stmts) =
>     annot ca ann <+>
>     label lb <>
>     text "loop"
>     $+$ nestedStatements flg ca stmts
>     $+$ text "end loop" <> statementEnd se
>
> statement _flg se ca (ContinueStatement ann lb) =
>     annot ca ann <+> text "continue"
>       <+> maybe empty ttext lb <> statementEnd se
> statement flg se ca (Perform ann f@(App {})) =
>     annot ca ann <+>
>     text "perform" <+> scalExpr flg f <> statementEnd se
> statement _ _ _ (Perform _ x) =
>    error $ "internal error: statement not supported for " ++ show x
>
> statement _flg se ca (CopyFrom ann tb cols src opts) =
>     annot ca ann <+>
>     text "copy" <+> name tb
>     <+> ifNotEmpty (parens . sepCsvMap nmc) cols
>     <+> text "from"
>     <+> case src of
>                  CopyFilename s -> (quotes $ ttext s)
>                                    <+> copyOpts opts
>                                    <> statementEnd se

>                  Stdin -> text "stdin"
>                           <+> copyOpts opts
>                           -- put statement end without new line
>                           -- so that the copydata follows immediately after
>                           -- without an extra blank line inbetween
>                           <> statementEndNNL se

> statement flg se ca (CopyTo ann src fn opts) =
>     annot ca ann <+>
>     text "copy" <+> s src
>     <+> text "to"
>     <+> quotes (ttext fn)
>     <+> copyOpts opts
>     <> statementEnd se
>     where
>       s (CopyTable tb cols) = name tb
>                               <+> ifNotEmpty (parens . sepCsvMap nmc) cols
>       s (CopyQuery qry) = parens (queryExpr flg True True Nothing qry)
>
> statement _ _ ca (CopyData ann s) =
>     annot ca ann <+>
>     tltext s <> text "\\." <> newline
>
> statement flg se ca (If ann conds els) =
>    if tsql
>    then
>      annot ca ann <+>
>      text "if" <+> scalExpr flg (fst $ head conds)
>                <+> blck (snd $ head conds)
>      $+$ ifNotEmpty (\e -> text "else" <+> blck e) els
>      <> statementEnd se
>    else
>      annot ca ann <+>
>      text "if" <+> constraintd (head conds)
>      $+$ vcat (map (\c -> text "elseif" <+> constraintd c) $ tail conds)
>      $+$ ifNotEmpty (\e -> text "else" $+$ nestedStatements flg ca e) els
>      $+$ text "end if" <> statementEnd se
>    where
>      constraintd (ex, sts) = scalExpr flg ex <+> text "then"
>                              $+$ nestedStatements flg ca sts
>      tsql = ppDialect flg == SQLServerDialect
>      blck sts = sep [text "begin"
>                     ,nestedStatements flg ca sts
>                     ,text "end"]
> statement flg se ca (Execute ann s) =
>     annot ca ann <+>
>     text "execute" <+> scalExpr flg s <> statementEnd se
>
>
> statement flg se ca (CaseStatementSimple ann c conds els) =
>     annot ca ann <+>
>     text "case" <+> scalExpr flg c
>     $+$ nest 2 (
>                 vcat (map (uncurry whenStatement) conds)
>                 $+$ elseStatement els
>                 ) $+$ text "end case" <> statementEnd se
>     where
>       whenStatement ex sts = text "when" <+> sepCsvMap (scalExpr flg) ex
>                           <+> text "then" $+$ nestedStatements flg ca sts
>       elseStatement = ifNotEmpty (\s -> text "else"
>                                      $+$ nestedStatements flg ca s)
> statement flg se ca (CaseStatement ann conds els) =
>     annot ca ann <+>
>     text "case"
>     $+$ nest 2 (
>                 vcat (map (uncurry whenStatement) conds)
>                 $+$ elseStatement els
>                 ) $+$ text "end case" <> statementEnd se
>     where
>       whenStatement ex sts = text "when" <+> sepCsvMap (scalExpr flg) ex
>                           <+> text "then" $+$ nestedStatements flg ca sts
>       elseStatement = ifNotEmpty (\s -> text "else"
>                                      $+$ nestedStatements flg ca s)

> statement flg _se ca (DeclareStatement ann des) =
>   annot ca ann <+>
>   text "declare" <+> sepCsvMap de des
>   where
>     de (nm,ty,val) =
>       ttext nm <+> typeName ty
>       <+> maybe empty (\e -> text "=" <+> scalExpr flg e) val

>
> -- misc
>
> statement _flg se _ (Set _ n vs) =
>   text "set" <+> ttext n <+> text "="
>   <+> sepCsvMap (text . dv) vs <> statementEnd se
>   where
>     dv (SetStr _ s) = "'" ++ s ++ "'"
>     dv (SetId _ i) = i
>     dv (SetNum _ nm) = show nm
>
> statement _flg se _ (Notify _ n) =
>   text "notify" <+> ttext n  <> statementEnd se

> statement flg _se _ (ExecStatement _ nm args) =
>   text "exec" <+> name nm <+> sepCsvMap (scalExpr flg) args

> statement _flg _se _ (CreateIndexTSQL _ nm obj cols) =
>   text "create" <+> text "index"
>   <+> nmc nm <+> text "on"
>   <+> name obj <+> parens (sepCsvMap nmc cols)

> statementEnd :: Bool -> Doc
> statementEnd b = if b
>                  then semi <> newline
>                  else empty

> statementEndNNL :: Bool -> Doc
> statementEndNNL b = if b
>                     then semi
>                     else empty


-------------------------------------------------------------------------------

Statement components

> -- selects
>
> queryExpr :: PrettyPrintFlags -> Bool -> Bool -> Maybe (Bool,[Name]) -> QueryExpr -> Doc
> queryExpr flg writeSelect _ intoi (Select _ dis l tb wh grp hav
>                                     order lim off hs) =
>   (text (if writeSelect then "select" else "")
>          <+> (case dis of
>                  All -> empty
>                  Distinct -> text "distinct"))
>          <+> (case lim of
>                  Just lime | useTop -> text "top" <+> scalExpr flg lime
>                  _ -> empty)
>   $+$ nest 2 (vcat $ catMaybes
>   [fmap (\(str,is) -> text "into"
>                       <+> (if str
>                            then text "strict"
>                            else empty)
>                       <+> sepCsvMap name is) intoi
>   ,Just $ nest 2 $ selectList flg l
>   ,Just $ if null tb
>           then empty
>           else text "from" $+$ nest 2 (sepCsvMap (tref flg) tb)
>   ,Just $ whr flg wh
>   ,case grp of
>      [] -> Nothing
>      g -> Just $ text "group by" $+$ nest 2 (sepCsvMap (scalExpr flg) g)
>   ,flip fmap hav $ \h -> text "having" $+$ nest 2 (scalExpr flg h)
>   ,Just $ orderBy flg order
>   ,if useTop then Nothing else flip fmap lim $ \lm -> text "limit" <+> scalExpr flg lm
>   ,flip fmap off $ \offs -> text "offset" <+> scalExpr flg offs
>   ,if null hs then Nothing else Just $ text "option" $+$ parens (sepCsvMap (text . prettyQueryHint) hs)
>   ])
>   where
>     useTop = ppDialect flg == SQLServerDialect
>     prettyQueryHint QueryHintPartitionGroup = "partition group"
>     prettyQueryHint QueryHintColumnarCpuGroup = "columnar cpu group"
>
> queryExpr flg writeSelect topLev _ (CombineQueryExpr _ tp s1 s2) =
>   let p = queryExpr flg writeSelect False Nothing  s1
>           $+$ (case tp of
>                        Except -> text "except"
>                        Union -> text "union"
>                        UnionAll -> text "union" <+> text "all"
>                        Intersect -> text "intersect")
>           $+$ queryExpr flg True False Nothing s2
>   in if topLev then p else parens p
> queryExpr flg _ _ _ (Values _ expss) =
>   text "values" $$ nest 2 (vcat $ csv $ map (parens . csvExp flg) expss)
> queryExpr flg _ _ _ (WithQueryExpr _ wqs ex) =
>   text "with" $$ nest 2 (vcat $ csv $ map pwq wqs)
>        $+$ queryExpr flg True False Nothing ex
>   where
>     pwq (WithQuery _ nm cs ex1) =
>       nmc nm <> case cs of
>                    Nothing -> empty
>                    Just cs' -> parens $ sepCsvMap nmc cs'
>       <+> text "as"
>       <+> parens (queryExpr flg True False Nothing ex1)

> name :: Name -> Doc
> name (Name _ ns) = nmcs ns
> name (AntiName n) = text ("$n(" ++ n ++ ")")

> nmcs :: [NameComponent] -> Doc
> nmcs ns = hcat $ punctuate (text ".") $ map nmc ns

> nmc :: NameComponent -> Doc
> nmc (Nmc ns) = ttext ns
> nmc (QNmc ns) = doubleQuotes $ ttext ns
> nmc (AntiNameComponent n) = text ("$m(" ++ n ++ ")")

>
> tref :: PrettyPrintFlags -> TableRef -> Doc
> tref _ (Tref _ f) = name f
> tref flg (SubTref _ sub) =
>   parens (queryExpr flg True True Nothing sub)
> tref flg (FunTref _ f@(App {})) = scalExpr flg f
> tref _flg (FunTref _ x) =
>       error $ "internal error: node not supported in function tref: "
>             ++ show x
> tref flg (TableRefParens _ t) = parens (tref flg t)
> tref flg (TableAlias _ t tr) = maybeParen flg tr <+> text "as" <+> nmc t
> -- hack this out for now. When the type checking is fixed, can try
> -- to eliminate unneeded aliases?
> tref flg (FullAlias _ t s tr) =
>   maybeParen flg tr <+> text "as"
>   <+> nmc t <> parens (sepCsvMap nmc s)

> tref flg (JoinTref _ t1 nat jt ht t2 ex) =
>   sep [tref flg t1
>       ,hsep ([case nat of
>                 Natural -> text "natural"
>                 Unnatural -> empty
>             ,text $ case jt of
>                Inner -> "inner"
>                Cross -> "cross"
>                LeftOuter -> "left outer"
>                RightOuter -> "right outer"
>                FullOuter -> "full outer"]
>             ++ maybe [] (\h -> [text $ case h of
>                               Merge -> "merge"
>                               Loop -> "loop"
>                               Hash -> "hash"])
>                      ht
>             ++ [text "join"])
>       ,tref flg t2
>       ,maybePrint (nest 2 . joinScalarExpr) ex]
>   where
>     joinScalarExpr (JoinOn _ e) = text "on" <+> scalExpr flg e
>     joinScalarExpr (JoinUsing _ ids) =
>         text "using" <+> parens (sepCsvMap nmc ids)

todo: don't want to do this here since it changes pretty . parse = id
so if you don't add the parens explicitly you get incorrect/ invalid
syntax maybe should error instead of silently breaking

> maybeParen :: PrettyPrintFlags -> TableRef -> Doc
> --maybeParen flg t@(JoinTref {}) = parens $ tref flg t
> maybeParen = tref

> direction :: Direction -> Doc
> direction d = text $ case d of
>                           Asc -> "asc"
>                           Desc -> "desc"
>
> whr :: PrettyPrintFlags -> Maybe ScalarExpr -> Doc
> whr flg (Just ex) = text "where" $+$ nest 2 (scalExpr flg ex)
> whr _ Nothing = empty
>
> selectList :: PrettyPrintFlags -> SelectList -> Doc
> selectList flg (SelectList _ ex) =
>   sepCsvMap selectItem ex
>   where
>     selectItem (SelectItem _ ex1 nm) = scalExprSl flg ex1 <+> text "as" <+> nmc nm
>     selectItem (SelExp _ e) = scalExprSl flg e
>
> cascade :: Cascade -> Doc
> cascade casc = text $ case casc of
>                                  Cascade -> "cascade"
>                                  Restrict -> "restrict"

> copyOpts :: [CopyOption] -> Doc
> copyOpts opts =
>   ifNotEmpty (const $ "with" <+> sep (map po opts)) opts
>   where
>       po (CopyFormat s) = text "format" <+> text s
>       po (CopyDelimiter s) = text "delimiter" <+> quotes (text s)
>       po (CopyErrorLog s) = text "error_log" <+> quotes (text s)
>       po (CopyErrorVerbosity s) = text "error_verbosity" <+> int s

> -- ddl
>
> constraint :: PrettyPrintFlags -> Constraint -> Doc
> constraint _flg (UniqueConstraint _ n c) =
>         mname n <+> text "unique"
>         <+> parens (sepCsvMap nmc c)
> constraint _flg (PrimaryKeyConstraint _ n p) =
>         mname n <+>
>         text "primary key"
>         <+> parens (sepCsvMap nmc p)
> constraint flg (CheckConstraint _ n c) =
>         mname n <+> text "check" <+> parens (scalExpr flg c)
> constraint _flg (ReferenceConstraint _ n at tb rat ondel onupd) =
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
> returning :: PrettyPrintFlags -> Maybe SelectList -> Doc
> returning flg l = case l of
>                 Nothing -> empty
>                 Just ls -> nest 2 (text "returning" <+> selectList flg ls)
>
> ifExists :: IfExists -> Doc
> ifExists i = case i of
>                         Require -> empty
>                         IfExists -> text "if exists"
>

> tablePartition Nothing = text ""
> tablePartition (Just (TablePartitionDef _ cn tf interval)) = 
>      text "partition by range" <+> parens (nmc cn)
>      <> parens ((text "every") <+> (text $ show tf) <+> (intervalify interval))
>  where 
>   intervalify = \x-> text $ case x of
>            Year -> "years"
>            Month -> "months"
>            Day -> "days"
>            Hour -> "hours"
>            Minute -> "minutes"
>            Second -> "seconds"
>            Millisecond -> "milliseconds"
>            _ -> "unknown type " ++ (show x)

> attrDef flg (AttributeDef _ n t def cons) =
>   nmc n <+> typeName t
>   <+> maybePrint (\e -> text "default" <+> scalExpr flg e) def
>   <+> hsep (map cCons cons)
>   where
>     cCons (NullConstraint _ cn) =
>       mname cn <+> text "null"
>     cCons (NotNullConstraint _ cn) =
>       mname cn <+> text "not null"
>     cCons (RowCheckConstraint _ cn ew) =
>       mname cn <+> text "check" <+> parens (scalExpr flg ew)
>     cCons (RowUniqueConstraint _ cn) =
>       mname cn <+> text "unique"
>     cCons (RowPrimaryKeyConstraint _ cn) =
>       mname cn <+> text "primary key"
>     cCons (RowReferenceConstraint _ cn tb att ondel onupd) =
>       mname cn <+> text "references" <+> name tb
>       <+> maybePrint (parens . nmc) att
>       <+> text "on delete" <+> cascade ondel
>       <+> text "on update" <+> cascade onupd
>     cCons (IdentityConstraint _ cn si) = 
>       mname cn <+> text "identity" <> text (maybe "" show si)
> -- plpgsql
>
> nestedStatements :: PrettyPrintFlags -> (Annotation -> String) -> StatementList -> Doc
> nestedStatements flg pa = nest 2 . vcat . map (statement flg True pa)
>
> typeName :: TypeName -> Doc
> typeName (SimpleTypeName _ s) = name s
> typeName (PrecTypeName _ s i) = name s <> parens(integer i)
> typeName (Prec2TypeName _ s i i1) = name s <> parens (sepCsv [integer i, integer i1])
> typeName (ArrayTypeName _ t) = typeName t <> text "[]"
> typeName (SetOfTypeName _ t) = text "setof" <+> typeName t
>
> ppType:: Type -> Doc
> ppType (ScalarType t) = text "scalar type" <> parens (text $ T.unpack t)
> ppType (DomainType t) = text "domain type" <> parens (text $ T.unpack t)
> ppType (EnumType t) = text "enum type" <> parens (text $ T.unpack t)
> ppType (UnknownType) = text "unknown type"
> ppType (ArrayType t) = text "array type" <> parens (ppType t)
> ppType (NamedCompositeType t) = text "named composite type" <> parens (text $ T.unpack t)
> ppType (CompositeType ts)
>     = text "composite type"
>       <> brackets (sepCsv
>                     $ map (\(t,te)
>                           -> parens $ sepCsv [text (T.unpack t), typeExtra te])
>                         ts)
> ppType (TrefType ts)
>     = text "tref type"
>       <> brackets (sepCsv
>                     $ map (\((t1,t2),te)
>                           -> parens $ sepCsv
>                                 [parens $ sepCsv
>                                     [text (T.unpack t1)
>                                     ,text (T.unpack t2)]
>                                 , typeExtra te])
>                         ts)
> ppType (AnonymousCompositeType ts)
>     = text "anonymous composite type"
>       <> brackets (sepCsv $ map ppType ts)
> ppType (Pseudo _) = text "pseudo type"
>
> typeExtra:: TypeExtra -> Doc
> typeExtra te = parens $ sepCsv
>     [ppType (teType te)
>     ,ppPrec "precision" (tePrecision te)
>     ,ppPrec "scale" (teScale te)
>     ,ppNullability (teNullable te)]
> ppPrec precType prec = case prec of
>     Nothing -> text $ "no " ++ precType
>     Just p -> text $ precType ++ ' ':show p
> ppNullability n = text $ (if n then "" else "not ") ++ "nullable"
>
> -- expressions
>
> scalExpr :: PrettyPrintFlags -> ScalarExpr -> Doc
> scalExpr flg (Parens _ e) = parens (scalExpr flg e)
> scalExpr _ (AntiScalarExpr s) = text $ "$(" ++ s ++ ")"
> scalExpr _ (Star _) = text "*"
> scalExpr _ (QStar _ i) = nmc i <> text ".*"

> scalExpr _flg (Identifier _a (Name _ is)) =
>   hcat (punctuate (text ".") (map nmc is))


> scalExpr _ (NumberLit _ n) = ttext n
> scalExpr _ (StringLit _ s) = -- needs some thought about using $$?
>                           text "'" <> ttext replaceQuotes <> text "'"
>                           where
>                             replaceQuotes = replace "'" "''" s
>
> scalExpr flg (SpecialOp _ n es) =
>    case getTName n of
>      Just "arrayctor" -> text "array" <> brackets (csvExp flg es)
>      Just "between" -> scalExpr flg (head es) <+> text "between"
>                         <+> scalExpr flg (es !! 1)
>                         <+> text "and"
>                         <+> scalExpr flg (es !! 2)
>      Just "notbetween" -> scalExpr flg (head es) <+> text "not between"
>                         <+> scalExpr flg (es !! 1)
>                         <+> text "and"
>                         <+> scalExpr flg (es !! 2)
>      Just "substring" -> text "substring"
>                      <> parens (scalExpr flg (head es)
>                                 <+> text "from" <+> scalExpr flg (es !! 1)
>                                 <+> text "for" <+> scalExpr flg (es !! 2))
>      Just "arraysub" ->
>        case es of
>                (Identifier _ i : es1) -> name i
>                                          <> brackets (csvExp flg es1)
>                (e:es') -> scalExpr flg e
>                           <> brackets (csvExp flg es')
>                _ -> error $ "bad args to !arraysub: " ++ show es
>      Just "rowctor" -> text "row" <> parens (sepCsvMap (scalExpr flg) es)
>      x -> error $ "bad special operator name: " ++ show x

> scalExpr flg (BinaryOp _ n e0 e1) =
>    case getTName n of
>      Just "and" | otherwise -> sep [scalExpr flg e0
>                                    ,text "and"
>                                    ,scalExpr flg e1]
>      Just n' | Just n'' <- lookup n' [("or","or")
>                                      ,("like","like")
>                                      ,("rlike","rlike")
>                                      ,("notlike","not like")] ->
>        scalExpr flg e0
>        <+> text n''
>        <+> scalExpr flg e1
>      Just "."   -- special case to avoid ws around '.'. Don't know if this is important
>                 -- or just cosmetic
>          -> scalExpr flg e0 <> text "." <> scalExpr flg e1
>      Just n' -> scalExpr flg e0 <+> ttext n' <+> scalExpr flg e1
>      Nothing -> error $ "bad binary operator name:" ++ show n

> scalExpr flg (PrefixOp _ n e0)
>   | Just "not" <- getTName n =
>       text "not" <+> scalExpr flg e0
>   | Just n' <- getTName n =
>       ttext n'
>     <+> scalExpr flg e0
>   | otherwise = error $ "bad prefix operator name:" ++ show n


> scalExpr flg (PostfixOp _ n e0)
>   | Just n' <- getTName n >>= flip lookup [("isnull", "is null")
>                                           ,("isnotnull", "is not null")] =
>        scalExpr flg e0 <+> ttext n'
>   | Just n' <- getTName n =
>       scalExpr flg e0 <+> ttext n'
>   | otherwise = error $ "bad postfix operator name:" ++ show n

> scalExpr flg (App _ n es) =
>   name n <> parens (csvExp flg es)

>
> scalExpr _ (BooleanLit _ b) = bool b
> scalExpr flg (InPredicate _ att t lst) =
>   scalExpr flg att <+> (if not t then text "not" else empty) <+> text "in"
>   <+> parens (case lst of
>                        InList _ expr -> csvExp flg expr
>                        InQueryExpr _ sel -> queryExpr flg True True Nothing sel)
> scalExpr flg (LiftApp _ op flav args) =
>   scalExpr flg (head args) <+> name op
>   <+> text (case flav of
>               LiftAny -> "any"
>               LiftAll -> "all")
>   <+> parens (scalExpr flg $ head $ tail args)
> scalExpr flg (ScalarSubQuery _ s) = parens (queryExpr flg True True Nothing s)
> scalExpr _ (NullLit _) = text "null"
> scalExpr flg (WindowApp _ fn part order frm) =
>   scalExpr flg fn <+> text "over"
>   <+> parens (if hp || ho
>               then (if hp
>                     then text "partition by" <+> csvExp flg part
>                     else empty)
>                     <+> orderBy flg order
>                     <+> frameStuff
>               else empty)
>   where
>     hp = not (null part)
>     ho = not (null order)
>     frameStuff = case frm of
>         Nothing -> empty
>         Just FrameUnboundedPreceding -> text "range unbounded preceding"
>         Just FrameUnboundedFull -> text "range between unbounded preceding and unbounded following"
>         Just FrameRowsUnboundedPreceding -> text "rows unbounded preceding"
>
> scalExpr flg (AggregateApp _ d (App _ fn es) o) =
>   name fn <> parens ((case d of
>                         All -> text "all"
>                         Distinct -> text "distinct")
>                      <+> csvExp flg es
>                      <+> orderBy flg o)
> scalExpr _ a@(AggregateApp {}) = error $ "bad syntax for aggregate function" ++ show a
> scalExpr flg (Case _ whens els) =
>   text "case"
>   $+$ nest 2 (vcat (map whn whens)
>               $+$ maybePrint (\e -> text "else" <+> scalExpr flg e) els)
>   $+$ text "end"
>       where
>         whn (ex1, ex2) =
>             text "when" <+> sepCsvMap (scalExpr flg) ex1
>             <+> text "then" <+> scalExpr flg ex2
>
> scalExpr flg (CaseSimple _ val whens els) =
>   text "case" <+> scalExpr flg val
>   $+$ nest 2 (vcat (map whn whens)
>               $+$ maybePrint (\e -> text "else" <+> scalExpr flg e) els)
>   $+$ text "end"
>       where
>         whn (ex1, ex2) =
>             text "when" <+> sepCsvMap (scalExpr flg) ex1
>             <+> text "then" <+> scalExpr flg ex2
>
> scalExpr _ (PositionalArg _ a) = text "$" <> integer a
> scalExpr _ (Placeholder _) = text "?"
> scalExpr flg (Exists _ s) =
>   text "exists" <+> parens (queryExpr flg True True Nothing s)
> scalExpr flg (Cast _ ex t) = text "cast" <> parens (scalExpr flg ex
>                                              <+> text "as"
>                                              <+> typeName t)
> scalExpr flg (ImplicitCast _ ex te) = text "impl_cast" <> parens (scalExpr flg ex
>                                              <+> text "as"
>                                              <+> typeExtra te)
> scalExpr flg (TypedStringLit a t s) =
>   typeName t <+> scalExpr flg (StringLit a s)
> scalExpr flg (Interval a v f p) =
>   text "interval" <+> scalExpr flg (StringLit a v)
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
> scalExpr flg (Extract _ f e) =
>   text "extract"
>   <> parens (text field <+> text "from" <+> scalExpr flg e)
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


> scalExprSl :: PrettyPrintFlags ->  ScalarExpr -> Doc
> scalExprSl flg (App _ f es) | Just "." <- getTName f
>                                 , [a@(Identifier _ _), b] <- es =
>   scalExprSl flg a <> text "." <> scalExprSl flg b
> scalExprSl flg x = scalExpr flg x

>
> set :: PrettyPrintFlags -> SetClause -> Doc
> set flg (SetClause _ a e) =
>   nmc a <+> text "=" <+> scalExpr flg e
> set flg (MultiSetClause _ is (SpecialOp _ f es)) | Just "rowctor" <- getTName f =
>   parens (sepCsvMap nmc is) <+> text "="
>   <+> parens (sepCsvMap (scalExpr flg) es)
> set _ a = error $ "bad expression in set in update: " ++ show a
>
> --utils
>
> -- convert a list of expressions to horizontal csv
>
> csvExp :: PrettyPrintFlags -> [ScalarExpr] -> Doc
> csvExp flg = hcatCsvMap (scalExpr flg)
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

> orderBy :: PrettyPrintFlags -> [(ScalarExpr,Direction)] -> Doc
> orderBy _ [] = empty
> orderBy flg os =
>   text "order by"
>   $+$ nest 2 (sepCsvMap (\(oe,od) -> scalExpr flg oe
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

> ttext :: String -> Doc
> ttext = text

> tltext :: String -> Doc
> tltext = text
