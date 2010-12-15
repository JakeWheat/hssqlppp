
> {- | Functions to convert sql asts to valid SQL source code. Includes
>    a function - 'printSqlAnn' - to output the annotations from a tree
>    in comments in the outputted SQL source.
>
>    Produces sort of readable code, but mainly just written to produce
>    reparsable text. Could do with some work to make the outputted text
>    layout better.
> -}
> {-# LANGUAGE PatternGuards #-}
> module Database.HsSqlPpp.PrettyPrinter (
>                       --convert a sql ast to text
>                       printSql
>                      ,printSqlAnn
>                       --convert a single expression parse node to text
>                      ,printExpression
>                      )
>     where
>
> import Text.PrettyPrint
> import Data.Char
> import Data.List
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Utils.Utils

--------------------------------------------------------------------------------

Public functions

> -- | convert an ast back to valid SQL source, it's also almost human readable.
> printSql :: StatementList -> String
> printSql = printSqlAnn (const "")
>
> -- | convert the ast back to valid source, and convert any annotations to
> -- text using the function provided and interpolate the output of
> -- this function(inside comments) with the SQL source.
> printSqlAnn :: (Annotation -> String) -> StatementList -> String
> printSqlAnn f ast = render $ vcat (map (convStatement f) ast) <> text "\n"
>
> -- | Testing function, pretty print an expression
> printExpression :: Expression -> String
> printExpression = render . convExp

-------------------------------------------------------------------------------

Conversion routines - convert Sql asts into Docs

> -- Statements
>
> convStatement :: (Annotation -> String) -> Statement -> Doc
>
> -- selects
>
> convStatement ca (SelectStatement ann s) =
>   convPa ca ann <+>
>   convSelectExpression True True s <> statementEnd
>
> --dml
>
> convStatement pa (Insert ann tb atts idata rt) =
>   convPa pa ann <+>
>   text "insert into" <+> convExp tb
>   <+> ifNotEmpty (parens . hcatCsvMap text) atts
>   $+$ convSelectExpression True True idata
>   $+$ convReturning rt
>   <> statementEnd
>
> convStatement ca (Update ann tb scs fr wh rt) =
>    convPa ca ann <+>
>    text "update" <+> convExp tb <+> text "set"
>    <+> hcatCsvMap convSet scs
>    <+> ifNotEmpty (\_ -> text "from" <+> hcatCsvMap convTref fr) fr
>    <+> convWhere wh
>    $+$ convReturning rt <> statementEnd
>
> convStatement ca (Delete ann tbl us wh rt) =
>    convPa ca ann <+>
>    text "delete from" <+> convExp tbl
>    <+> ifNotEmpty (\_ -> text "using" <+> hcatCsvMap convTref us) us
>    <+> convWhere wh
>    $+$ convReturning rt
>    <> statementEnd
>
> convStatement ca (Truncate ann names ri casc) =
>     convPa ca ann <+>
>     text "truncate"
>     <+> hcatCsvMap text names
>     <+> text (case ri of
>                       RestartIdentity -> "restart identity"
>                       ContinueIdentity -> "continue identity")
>     <+> convCasc casc
>     <> statementEnd
>
> -- ddl
>
> convStatement ca (CreateTable ann tbl atts cns) =
>     convPa ca ann <+>
>     text "create table"
>     <+> text tbl <+> lparen
>     $+$ nest 2 (vcat (csv (map convAttDef atts ++ map convCon cns)))
>     $+$ rparen <> statementEnd
>     where
>       convAttDef (AttributeDef _ n t def cons) =
>         text n <+> convTypeName t
>         <+> maybeConv (\e -> text "default" <+> convExp e) def
>         <+> hsep (map cCons cons)
>       cCons (NullConstraint _ cn) =
>         mname cn <+> text "null"
>       cCons (NotNullConstraint _ cn) =
>         mname cn <+> text "not null"
>       cCons (RowCheckConstraint _ cn ew) =
>         mname cn <+> text "check" <+> parens (convExp ew)
>       cCons (RowUniqueConstraint _ cn) =
>         mname cn <+> text "unique"
>       cCons (RowPrimaryKeyConstraint _ cn) =
>         mname cn <+> text "primary key"
>       cCons (RowReferenceConstraint _ cn tb att ondel onupd) =
>         mname cn <+> text "references" <+> text tb
>         <+> maybeConv (parens . text) att
>         <+> text "on delete" <+> convCasc ondel
>         <+> text "on update" <+> convCasc onupd
>
> convStatement ca (AlterTable ann name act) =
>     convPa ca ann <+>
>     text "alter table" <+> text name
>     <+> hcatCsvMap convAct act <> statementEnd
>     where
>       convAct (AlterColumnDefault _ nm def) =
>           text "alter column" <+> text nm
>           <+> text "set default" <+> convExp def
>       convAct (AddConstraint _ con) =
>           text "add " <+> convCon con
>
> convStatement ca (CreateSequence ann nm incr _ _ start cache) =
>     convPa ca ann <+>
>     text "create sequence" <+> text nm <+>
>     text "increment" <+> text (show incr) <+>
>     text "no minvalue" <+>
>     text "no maxvalue" <+>
>     text "start" <+> text (show start) <+>
>     text "cache" <+> text (show cache) <> statementEnd
>
> convStatement ca (AlterSequence ann nm o) =
>     convPa ca ann <+>
>     text "alter sequence" <+> text nm
>     <+> text "owned by" <+> convExp o <> statementEnd
>
> convStatement ca (CreateTableAs ann t sel) =
>     convPa ca ann <+>
>     text "create table"
>     <+> text t <+> text "as"
>     $+$ convSelectExpression True True sel
>     <> statementEnd
>
> convStatement ca (CreateFunction ann name args retType rep lang body vol) =
>     convPa ca ann <+>
>     text ("create " ++ (case rep of
>                          Replace -> "or replace "
>                          _ -> "") ++ "function")
>     <+> text name
>     <+> parens (hcatCsvMap convParamDef args)
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
>     <> statementEnd
>     where
>       convFnBody (SqlFnBody ann1 sts) =
>         convPa ca ann1 <+>
>         convNestedStatements ca sts
>       convFnBody (PlpgsqlFnBody ann1 blk) =
>           convPa ca ann1 <+>
>           convStatement ca blk
>       convParamDef (ParamDef _ n t) = text n <+> convTypeName t
>       convParamDef  (ParamDefTp _ t) = convTypeName t
>
> convStatement ca (Block ann lb decls sts) =
>   convPa ca ann <+>
>   convLabel lb <>
>   ifNotEmpty (\l -> text "declare"
>                   $+$ nest 2 (vcat $ map convVarDef l)) decls
>   $+$ text "begin"
>   $+$ convNestedStatements ca sts
>   $+$ text "end;"
>   where
>       convVarDef (VarDef _ n t v) =
>         text n <+> convTypeName t
>         <+> maybeConv (\x -> text ":=" <+> convExp x) v <> semi
>
>
> convStatement ca (CreateView ann name sel) =
>     convPa ca ann <+>
>     text "create view" <+> text name <+> text "as"
>     $+$ nest 2 (convSelectExpression True True sel) <> statementEnd
>
> convStatement ca (CreateDomain ann name tp n ex) =
>     convPa ca ann <+>
>     text "create domain" <+> text name <+> text "as"
>     <+> convTypeName tp <+> cname <+> checkExp ex <> statementEnd
>     where
>       checkExp = maybeConv (\e -> text "check" <+> parens (convExp e))
>       cname = if n == ""
>                then empty
>                else text "constraint" <+> text n
>
> convStatement ca (DropFunction ann ifExists fns casc) =
>   convPa ca ann <+>
>   text "drop function"
>   <+> convIfExists ifExists
>   <+> hcatCsvMap doFunction fns
>   <+> convCasc casc
>   <> statementEnd
>   where
>     doFunction (name,types) =
>       text name <> parens (hcatCsvMap convTypeName types)
>
> convStatement ca (DropSomething ann dropType ifExists names casc) =
>     convPa ca ann <+>
>     text "drop"
>     <+> text (case dropType of
>                 Table -> "table"
>                 View -> "view"
>                 Domain -> "domain"
>                 Type -> "type")
>     <+> convIfExists ifExists
>     <+> hcatCsvMap text names
>     <+> convCasc casc
>     <> statementEnd
>
> convStatement ca (CreateType ann name atts) =
>     convPa ca ann <+>
>     text "create type" <+> text name <+> text "as" <+> lparen
>     $+$ nest 2 (vcat (csv
>           (map (\(TypeAttDef _ n t) -> text n <+> convTypeName t)  atts)))
>     $+$ rparen <> statementEnd
>
> convStatement ca (CreateLanguage ann name) =
>     convPa ca ann <+>
>     text "create language" <+> text name <> statementEnd
>
> convStatement ca (CreateTrigger ann name wh events tbl firing fnName fnArgs) =
>     convPa ca ann <+>
>     text "create trigger" <+> text name
>     <+> text (case wh of
>                       TriggerBefore -> "before"
>                       TriggerAfter -> "after")
>     <+> evs
>     <+> text "on" <+> text tbl
>     <+> text "for" <+> text (case firing of
>                                         EachRow -> "row"
>                                         EachStatement -> "statement")
>     <+> text "execute procedure" <+> text fnName
>     <> parens (hcatCsvMap convExp fnArgs) <> statementEnd
>     where
>       evs = hcat $ map text $ intersperse " or "
>             $ map (\e -> case e of
>                                 TInsert -> "insert"
>                                 TUpdate -> "update"
>                                 TDelete -> "delete") events
>
> -- plpgsql
>
> convStatement ca (NullStatement ann) =
>   convPa ca ann <+> text "null" <> statementEnd
> convStatement ca (ExitStatement ann lb) =
>   convPa ca ann <+> text "exit"
>     <+> maybe empty text lb <> statementEnd
>
> convStatement ca (Assignment ann name val) =
>     convPa ca ann <+>
>     convExp name <+> text ":=" <+> convExp val <> statementEnd
>
> convStatement ca (Return ann ex) =
>     convPa ca ann <+>
>     text "return" <+> maybeConv convExp ex <> statementEnd
>
> convStatement ca (ReturnNext ann ex) =
>     convPa ca ann <+>
>     text "return" <+> text "next" <+> convExp ex <> statementEnd
>
> convStatement ca (ReturnQuery ann sel) =
>     convPa ca ann <+>
>     text "return" <+> text "query"
>     <+> convSelectExpression True True sel <> statementEnd
>
> convStatement ca (Raise ann rt st exps) =
>     convPa ca ann <+>
>     text "raise"
>     <+> case rt of
>                 RNotice -> text "notice"
>                 RException -> text "exception"
>                 RError -> text "error"
>     <+> convExp (StringLit emptyAnnotation st)
>     <> ifNotEmpty (\e -> comma <+> csvExp e) exps
>     <> statementEnd
>
> convStatement ca (ForSelectStatement ann lb i sel stmts) =
>     convPa ca ann <+>
>     convLabel lb <>
>     text "for" <+> convExp i <+> text "in"
>     <+> convSelectExpression True True sel <+> text "loop"
>     $+$ convNestedStatements ca stmts
>     $+$ text "end loop" <> statementEnd
>
> convStatement ca (ForIntegerStatement ann lb var st en stmts) =
>     convPa ca ann <+>
>     convLabel lb <>
>     text "for" <+> convExp var <+> text "in"
>     <+> convExp st <+> text ".." <+> convExp en <+> text "loop"
>     $+$ convNestedStatements ca stmts
>     $+$ text "end loop" <> statementEnd
>
> convStatement ca (WhileStatement ann lb ex stmts) =
>     convPa ca ann <+>
>     convLabel lb <>
>     text "while" <+> convExp ex <+> text "loop"
>     $+$ convNestedStatements ca stmts
>     $+$ text "end loop" <> statementEnd
> convStatement ca (LoopStatement ann lb stmts) =
>     convPa ca ann <+>
>     convLabel lb <>
>     text "loop"
>     $+$ convNestedStatements ca stmts
>     $+$ text "end loop" <> statementEnd
>
> convStatement ca (ContinueStatement ann lb) =
>     convPa ca ann <+> text "continue"
>       <+> maybe empty text lb <> statementEnd
> convStatement ca (Perform ann f@(FunCall _ _ _)) =
>     convPa ca ann <+>
>     text "perform" <+> convExp f <> statementEnd
> convStatement _ (Perform _ x) =
>    error $ "internal error: convStatement not supported for " ++ show x
>
> convStatement ca (Copy ann tb cols src) =
>     convPa ca ann <+>
>     text "copy" <+> text tb
>     <+> ifNotEmpty (parens . hcatCsvMap text) cols
>     <+> text "from"
>     <+> case src of
>                  CopyFilename s -> quotes $ text s <> statementEnd
>                  Stdin -> text "stdin" <> text ";"
>
> convStatement ca (CopyData ann s) =
>     convPa ca ann <+>
>     text s <> text "\\." <> newline
>
> convStatement ca (If ann conds els) =
>    convPa ca ann <+>
>    text "if" <+> convCond (head conds)
>    $+$ vcat (map (\c -> text "elseif" <+> convCond c) $ tail conds)
>    $+$ ifNotEmpty (\e -> text "else" $+$ convNestedStatements ca e) els
>    $+$ text "end if" <> statementEnd
>     where
>       convCond (ex, sts) = convExp ex <+> text "then"
>                            $+$ convNestedStatements ca sts
> convStatement ca (Execute ann s) =
>     convPa ca ann <+>
>     text "execute" <+> convExp s <> statementEnd
>
> convStatement ca (ExecuteInto ann s is) =
>     convPa ca ann <+>
>     text "execute" <+> convExp s
>     <+> text "into" <+> hcatCsvMap text is
>     <> statementEnd
>
> convStatement ca (CaseStatementSimple ann c conds els) =
>     convPa ca ann <+>
>     text "case" <+> convExp c
>     $+$ nest 2 (
>                 vcat (map (uncurry convWhenSt) conds)
>                 $+$ convElseSt els
>                 ) $+$ text "end case" <> statementEnd
>     where
>       convWhenSt ex sts = text "when" <+> hcatCsvMap convExp ex
>                           <+> text "then" $+$ convNestedStatements ca sts
>       convElseSt = ifNotEmpty (\s -> text "else"
>                                      $+$ convNestedStatements ca s)
> convStatement ca (CaseStatement ann conds els) =
>     convPa ca ann <+>
>     text "case"
>     $+$ nest 2 (
>                 vcat (map (uncurry convWhenSt) conds)
>                 $+$ convElseSt els
>                 ) $+$ text "end case" <> statementEnd
>     where
>       convWhenSt ex sts = text "when" <+> hcatCsvMap convExp ex
>                           <+> text "then" $+$ convNestedStatements ca sts
>       convElseSt = ifNotEmpty (\s -> text "else"
>                                      $+$ convNestedStatements ca s)

>
> -- misc
>
> convStatement _ (Set _ n vs) =
>   text "set" <+> text n <+> text "="
>   <+> hcatCsvMap (text . dv) vs <> statementEnd
>   where
>     dv (SetStr _ s) = "'" ++ s ++ "'"
>     dv (SetId _ i) = i
>     dv (SetNum _ nm) = show nm
>
> convStatement _ (Notify _ n) =
>   text "notify" <+> text n  <> statementEnd
>
> statementEnd :: Doc
> statementEnd = semi <> newline

-------------------------------------------------------------------------------

Statement components

> -- selects
>
> convSelectExpression :: Bool -> Bool -> SelectExpression -> Doc
> convSelectExpression writeSelect _ (Select _ dis l tb wh grp hav
>                                 order lim off) =
>   text (if writeSelect then "select" else "")
>   <+> (case dis of
>          Dupes -> empty
>          Distinct -> text "distinct")
>   <+> convSelList l
>   $+$ nest 2 (
>               (if null tb
>                  then empty
>                  else text "from" <+> hcatCsvMap convTref tb)
>               $+$ convWhere wh)
>   <+> ifNotEmpty (\g -> text "group by" <+> hcatCsvMap convExp g) grp
>   <+> maybeConv (\h -> text "having" <+> convExp h) hav
>   <+> ifNotEmpty (\o -> text "order by" <+> hcatCsvMap (\(oe,od) -> convExp oe
>                   <+> convDir od) o) order
>   <+> maybeConv (\lm -> text "limit" <+> convExp lm) lim
>   <+> maybeConv (\offs -> text "offset" <+> convExp offs) off
>
> convSelectExpression writeSelect topLev (CombineSelect _ tp s1 s2) =
>   let p = convSelectExpression writeSelect False s1
>           $+$ (case tp of
>                        Except -> text "except"
>                        Union -> text "union"
>                        UnionAll -> text "union" <+> text "all"
>                        Intersect -> text "intersect")
>           $+$ convSelectExpression True False s2
>   in if topLev then p else parens p
> convSelectExpression _ _ (Values _ expss) =
>   text "values" $$ nest 2 (vcat $ csv $ map (parens . csvExp) expss)
> convSelectExpression _ _ (WithSelect _ wqs ex) =
>   text "with" $$ nest 2 (vcat $ csv $ map pwq wqs)
>        $+$ convSelectExpression True False ex
>   where
>     pwq (WithQuery _ nm ex1) =
>       text nm <+> text "as"
>       <+> parens (convSelectExpression True False ex1)
>
> convTref :: TableRef -> Doc
> convTref (Tref _ f a) = convExp f <+> convTrefAlias a
> convTref (JoinedTref _ t1 nat jt t2 ex a) =
>         parens (convTref t1
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
>         <+> convTref t2
>         <+> maybeConv (nest 2 . convJoinExpression) ex
>         <+> convTrefAlias a)
>         where
>           convJoinExpression (JoinOn _ e) = text "on" <+> convExp e
>           convJoinExpression (JoinUsing _ ids) =
>               text "using" <+> parens (hcatCsvMap text ids)
>
> convTref (SubTref _ sub alias) =
>         parens (convSelectExpression True True sub)
>         <+> text "as" <+> convTrefAlias alias
> convTref (TrefFun _ f@(FunCall _ _ _) a) = convExp f <+> convTrefAlias a
> convTref (TrefFun _ x _) =
>       error $ "internal error: node not supported in function tref: "
>             ++ show x
>
> convTrefAlias :: TableAlias -> Doc
> convTrefAlias NoAlias = empty
> convTrefAlias (TableAlias t) = text t
> convTrefAlias (FullAlias t s) = text t <+> parens (hcatCsvMap text s)

> convDir :: Direction -> Doc
> convDir d = text $ case d of
>                           Asc -> "asc"
>                           Desc -> "desc"
>
> convWhere :: Maybe Expression -> Doc
> convWhere (Just ex) = text "where" <+> convExp ex
> convWhere Nothing = empty
>
> convSelList :: SelectList -> Doc
> convSelList (SelectList _ ex into) =
>   hcatCsvMap convSelItem ex
>   <+> ifNotEmpty (\i -> text "into" <+> hcatCsvMap convExp i) into
>   where
>     convSelItem (SelectItem _ ex1 nm) = convExpSl ex1 <+> text "as" <+> text nm
>     convSelItem (SelExp _ e) = convExpSl e
>
> convCasc :: Cascade -> Doc
> convCasc casc = text $ case casc of
>                                  Cascade -> "cascade"
>                                  Restrict -> "restrict"
>
> -- ddl
>
> convCon :: Constraint -> Doc
> convCon (UniqueConstraint _ n c) =
>         mname n <+> text "unique"
>         <+> parens (hcatCsvMap text c)
> convCon (PrimaryKeyConstraint _ n p) =
>         mname n <+>
>         text "primary key"
>         <+> parens (hcatCsvMap text p)
> convCon (CheckConstraint _ n c) =
>         mname n <+> text "check" <+> parens (convExp c)
> convCon (ReferenceConstraint _ n at tb rat ondel onupd) =
>         mname n <+>
>         text "foreign key" <+> parens (hcatCsvMap text at)
>         <+> text "references" <+> text tb
>         <+> ifNotEmpty (parens . hcatCsvMap text) rat
>         <+> text "on update" <+> convCasc onupd
>         <+> text "on delete" <+> convCasc ondel
>
> mname :: String -> Doc
> mname n = if n == ""
>           then empty
>           else text "constraint" <+> text n
>
> convReturning :: Maybe SelectList -> Doc
> convReturning l = case l of
>                 Nothing -> empty
>                 Just ls -> nest 2 (text "returning" <+> convSelList ls)
>
> convIfExists :: IfExists -> Doc
> convIfExists i = case i of
>                         Require -> empty
>                         IfExists -> text "if exists"
>
> -- plpgsql
>
> convNestedStatements :: (Annotation -> String) -> StatementList -> Doc
> convNestedStatements pa = nest 2 . vcat . map (convStatement pa)
>
> convTypeName :: TypeName -> Doc
> convTypeName (SimpleTypeName _ s) = text s
> convTypeName (PrecTypeName _ s i) = text s <> parens(integer i)
> convTypeName (ArrayTypeName _ t) = convTypeName t <> text "[]"
> convTypeName (SetOfTypeName _ t) = text "setof" <+> convTypeName t
>
> -- expressions
>
> convExp :: Expression -> Doc
> convExp (Identifier _ i) =
>   if quotesNeeded
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
>                      okChar x =isAlphaNum x || x `elem` "*_."
> convExp (QIdentifier a i1@(Identifier _ _) i) = convExp i1 <> text "." <> convExp (Identifier a i)
> convExp (QIdentifier a e i) = parens (convExp e) <> text "." <> convExp (Identifier a i)

> --convExp (PIdentifier _ i) = parens $ convExp i
> convExp (IntegerLit _ n) = integer n
> convExp (FloatLit _ n) = double n
> convExp (StringLit _ s) = -- needs some thought about using $$?
>                           text "'" <> text replaceQuotes <> text "'"
>                           where
>                             replaceQuotes = replace "'" "''" s {-if tag == "'"
>                                               then replace "'" "''" s
>                                               else s-}
>
> convExp (FunCall _ n es) =
>     --check for special operators
>    case n of
>      "!arrayctor" -> text "array" <> brackets (csvExp es)
>      "!between" -> convExp (head es) <+> text "between"
>                    <+> parens (convExp (es !! 1))
>                   <+> text "and"
>                   <+> parens (convExp (es !! 2))
>      "!substring" -> text "substring"
>                      <> parens (convExp (head es)
>                                 <+> text "from" <+> convExp (es !! 1)
>                                 <+> text "for" <+> convExp (es !! 2))
>      "!arraysub" -> case es of
>                        (Identifier _ i : es1) -> text i
>                                                  <> brackets (csvExp es1)
>                        _ -> parens (convExp (head es))
>                             <> brackets (csvExp (tail es))
>      "!rowctor" -> text "row" <> parens (hcatCsvMap convExp es)
>      "."   -- special case to avoid ws around '.'. Don't know if this is important
>            -- or just cosmetic
>          | [a,b] <- es -> convExp a <> text "." <> convExp b
>      _ | isOperatorName n ->
>         case forceRight (getOperatorType defaultTemplate1Catalog n) of
>                           BinaryOp ->
>                               parens (convExp (head es)
>                                       <+> text (filterKeyword n)
>                                       <+> convExp (es !! 1))
>                           PrefixOp -> parens (text (if n == "u-"
>                                                        then "-"
>                                                        else filterKeyword n)
>                                                <+> parens (convExp (head es)))
>                           PostfixOp -> parens (convExp (head es)
>                                        <+> text (filterKeyword n))
>        | otherwise -> text n <> parens (csvExp es)
>    where
>      filterKeyword t = case t of
>                          "!and" -> "and"
>                          "!or" -> "or"
>                          "!not" -> "not"
>                          "!isnull" -> "is null"
>                          "!isnotnull" -> "is not null"
>                          "!like" -> "like"
>                          x -> x
>
> convExp (BooleanLit _ b) = bool b
> convExp (InPredicate _ att t lst) =
>   convExp att <+> (if not t then text "not" else empty) <+> text "in"
>   <+> parens (case lst of
>                        InList _ expr -> csvExp expr
>                        InSelect _ sel -> convSelectExpression True True sel)
> convExp (LiftOperator _ op flav args) =
>   convExp (head args) <+> text op
>   <+> text (case flav of
>               LiftAny -> "any"
>               LiftAll -> "all")
>   <+> parens (convExp $ head $ tail args)
> convExp (ScalarSubQuery _ s) = parens (convSelectExpression True True s)
> convExp (NullLit _) = text "null"
> convExp (WindowFn _ fn part order asc frm) =
>   convExp fn <+> text "over"
>   <+> parens (if hp || ho
>               then (if hp
>                     then text "partition by" <+> csvExp part
>                     else empty)
>                     <+> (if ho
>                          then text "order by" <+> csvExp order
>                               <+> convDir asc
>                          else empty)
>                     <+> convFrm
>               else empty)
>   where
>     hp = not (null part)
>     ho = not (null order)
>     convFrm = case frm of
>                 FrameUnboundedPreceding -> text "range unbounded preceding"
>                 FrameUnboundedFull -> text "range between unbounded \
>                                            \preceding and unbounded following"
>                 FrameRowsUnboundedPreceding -> text "rows unbounded preceding"
>
> convExp (Case _ whens els) =
>   text "case"
>   $+$ nest 2 (vcat (map convWhen whens)
>               $+$ maybeConv (\e -> text "else" <+> convExp e) els)
>   $+$ text "end"
>       where
>         convWhen (ex1, ex2) =
>             text "when" <+> hcatCsvMap convExp ex1
>             <+> text "then" <+> convExp ex2
>
> convExp (CaseSimple _ val whens els) =
>   text "case" <+> convExp val
>   $+$ nest 2 (vcat (map convWhen whens)
>               $+$ maybeConv (\e -> text "else" <+> convExp e) els)
>   $+$ text "end"
>       where
>         convWhen (ex1, ex2) =
>             text "when" <+> hcatCsvMap convExp ex1
>             <+> text "then" <+> convExp ex2
>
> convExp (PositionalArg _ a) = text "$" <> integer a
> convExp (Placeholder _) = text "?"
> convExp (Exists _ s) =
>   text "exists" <+> parens (convSelectExpression True True s)
> convExp (Cast _ ex t) = text "cast" <> parens (convExp ex
>                                              <+> text "as"
>                                              <+> convTypeName t)


> convExpSl :: Expression -> Doc
> convExpSl (FunCall _ "." es) | [a@(Identifier _ _), b] <- es =
>   parens (convExpSl a) <> text "." <> convExpSl b
> convExpSl x = convExp x

>
> convSet :: Expression -> Doc
> convSet (FunCall _ "=" [Identifier _ a, e]) =
>   text a <+> text "=" <+> convExp e
> convSet (FunCall _ "=" [a, b]) | (FunCall _ "!rowctor" is1) <- a
>                                 ,(FunCall _ "!rowctor" is2) <- b =
>   rsNoRow is1 <+> text "=" <+> rsNoRow is2
>   where
>     rsNoRow is = parens (hcatCsvMap convExp is)
> convSet a = error $ "bad expression in set in update: " ++ show a
>
> --utils
>
> -- convert a list of expressions to horizontal csv
>
> csvExp :: [Expression] -> Doc
> csvExp = hcatCsvMap convExp
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
>
> ifNotEmpty :: ([a] -> Doc) -> [a] -> Doc
> ifNotEmpty c l = if null l then empty else c l
>
> hcatCsvMap :: (a -> Doc) -> [a] -> Doc
> hcatCsvMap ex = hcatCsv . map ex
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
