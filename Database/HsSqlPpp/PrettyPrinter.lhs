Copyright 2009 Jake Wheat

The pretty printer which prints ast nodes from Ast.hs
It uses the hughes pj pretty printer

Produces sort of readable code, but mainly just written to produce
reparsable text. Could do with some work to make the outputted text
layout better.

Not much other comments, since it all should be pretty self evident.

> module Database.HsSqlPpp.PrettyPrinter (
>                       --convert a sql ast to text
>                       printSql
>                      ,printSqlAnn
>                       --convert a single expression parse node to text
>                      ,printExpression
>                      )
>     where

> import Text.PrettyPrint
> import Data.List (stripPrefix)
> import Data.Maybe

> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.TypeType

================================================================================

Public functions

> printSql :: StatementList -> String
> printSql ast = printSqlAnn (const "") ast

> printSqlAnn :: (Annotation -> String) -> StatementList -> String
> printSqlAnn f ast = render $ vcat (map (convStatement f) ast) <> text "\n"


> printExpression :: Expression -> String
> printExpression = render . convExp

================================================================================

Conversion routines - convert Sql asts into Docs
= Statements

> convStatement :: (Annotation -> String) -> Statement -> Doc

== selects

> convStatement ca (SelectStatement ann s) =
>   convPa ca ann <+>
>   convSelectExpression True s <> statementEnd

== dml

> convStatement pa (Insert ann tb atts idata rt) =
>   convPa pa ann <+>
>   text "insert into" <+> text tb
>   <+> ifNotEmpty (parens . hcatCsvMap text) atts
>   $+$ convSelectExpression True idata
>   $+$ convReturning rt
>   <> statementEnd

> convStatement ca (Update ann tb scs wh rt) =
>    convPa ca ann <+>
>    text "update" <+> text tb <+> text "set"
>    <+> hcatCsvMap convSetClause scs
>    <+> convWhere wh
>    $+$ convReturning rt <> statementEnd
>    where
>      convSetClause (SetClause att ex) = text att <+> text "=" <+> convExp ex
>      convSetClause (RowSetClause atts exs) =
>        parens (hcatCsvMap text atts)
>        <+> text "="
>        <+> parens (hcatCsvMap convExp exs)

> convStatement ca (Delete ann tbl wh rt) =
>    convPa ca ann <+>
>    text "delete from" <+> text tbl
>    <+> convWhere wh
>    $+$ convReturning rt
>    <> statementEnd

> convStatement ca (Truncate ann names ri casc) =
>     convPa ca ann <+>
>     text "truncate"
>     <+> hcatCsvMap text names
>     <+> text (case ri of
>                       RestartIdentity -> "restart identity"
>                       ContinueIdentity -> "continue identity")
>     <+> convCasc casc
>     <> statementEnd

== ddl

> convStatement ca (CreateTable ann tbl atts cns) =
>     convPa ca ann <+>
>     text "create table"
>     <+> text tbl <+> lparen

>     $+$ nest 2 (vcat (csv (map convAttDef atts ++ map convCon cns)))
>     $+$ rparen <> statementEnd
>     where
>       convAttDef (AttributeDef n t def cons) =
>         text n <+> convTypeName t
>         <+> maybeConv (\e -> text "default" <+> convExp e) def
>         <+> hsep (map (\e -> (case e of
>                                 NullConstraint -> text "null"
>                                 NotNullConstraint -> text "not null"
>                                 RowCheckConstraint ew ->
>                                     text "check" <+> parens (convExp ew)
>                                 RowUniqueConstraint -> text "unique"
>                                 RowPrimaryKeyConstraint -> text "primary key"
>                                 RowReferenceConstraint tb att ondel onupd ->
>                                     text "references" <+> text tb
>                                     <+> maybeConv (parens . text) att
>                                     <+> text "on delete" <+> convCasc ondel
>                                     <+> text "on update" <+> convCasc onupd
>                         )) cons)
>       convCon (UniqueConstraint c) = text "unique"
>                                      <+> parens (hcatCsvMap text c)
>       convCon (PrimaryKeyConstraint p) = text "primary key"
>                                          <+> parens (hcatCsvMap text p)
>       convCon (CheckConstraint c) = text "check" <+> parens (convExp c)
>       convCon (ReferenceConstraint at tb rat ondel onupd) =
>         text "foreign key" <+> parens (hcatCsvMap text at)
>         <+> text "references" <+> text tb
>         <+> ifNotEmpty (parens . hcatCsvMap text) rat
>         <+> text "on delete" <+> convCasc ondel
>         <+> text "on update" <+> convCasc onupd



> convStatement ca (CreateTableAs ann t sel) =
>     convPa ca ann <+>
>     text "create table"
>     <+> text t <+> text "as"
>     $+$ convSelectExpression True sel
>     <> statementEnd

> convStatement ca (CreateFunction ann lang name args retType qt body vol) =
>     convPa ca ann <+>
>     text "create function" <+> text name
>     <+> parens (hcatCsvMap convParamDef args)
>     <+> text "returns" <+> convTypeName retType <+> text "as" <+> text qt
>     $+$ convFnBody body
>     $+$ text qt <+> text "language"
>     <+> text (case lang of
>                         Sql -> "sql"
>                         Plpgsql -> "plpgsql")
>     <+> text (case vol of
>                        Volatile -> "volatile"
>                        Stable -> "stable"
>                        Immutable -> "immutable")
>     <> statementEnd
>     where
>       convFnBody (SqlFnBody sts) = convNestedStatements ca sts
>       convFnBody (PlpgsqlFnBody decls sts) =
>           ifNotEmpty (\l -> text "declare"
>                   $+$ nest 2 (vcat $ map convVarDef l)) decls
>           $+$ text "begin"
>           $+$ convNestedStatements ca sts
>           $+$ text "end;"
>       convParamDef (ParamDef n t) = text n <+> convTypeName t
>       convParamDef  (ParamDefTp t) = convTypeName t
>       convVarDef (VarDef n t v) =
>         text n <+> convTypeName t
>         <+> maybeConv (\x -> text ":=" <+> convExp x) v <> semi



> convStatement ca (CreateView ann name sel) =
>     convPa ca ann <+>
>     text "create view" <+> text name <+> text "as"
>     $+$ nest 2 (convSelectExpression True sel) <> statementEnd

> convStatement ca (CreateDomain ann name tp ex) =
>     convPa ca ann <+>
>     text "create domain" <+> text name <+> text "as"
>     <+> convTypeName tp <+> checkExp ex <> statementEnd
>     where
>       checkExp = maybeConv (\e -> text "check" <+> parens (convExp e))

> convStatement ca (DropFunction ann ifExists fns casc) =
>   convPa ca ann <+>
>   text "drop function"
>   <+> convIfExists ifExists
>   <+> hcatCsvMap doFunction fns
>   <+> convCasc casc
>   <> statementEnd
>   where
>     doFunction (name,types) = text name <> parens (hcatCsvMap text types)

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

> convStatement ca (CreateType ann name atts) =
>     convPa ca ann <+>
>     text "create type" <+> text name <+> text "as" <+> lparen
>     $+$ nest 2 (vcat (csv
>           (map (\(TypeAttDef n t) -> text n <+> convTypeName t)  atts)))
>     $+$ rparen <> statementEnd

== plpgsql

> convStatement ca (NullStatement ann) = convPa ca ann <+> text "null" <> statementEnd

> convStatement ca (Assignment ann name val) =
>     convPa ca ann <+>
>     text name <+> text ":=" <+> convExp val <> statementEnd

> convStatement ca (Return ann ex) =
>     convPa ca ann <+>
>     text "return" <+> maybeConv convExp ex <> statementEnd

> convStatement ca (ReturnNext ann ex) =
>     convPa ca ann <+>
>     text "return" <+> text "next" <+> convExp ex <> statementEnd

> convStatement ca (ReturnQuery ann sel) =
>     convPa ca ann <+>
>     text "return" <+> text "query"
>     <+> convSelectExpression True sel <> statementEnd

> convStatement ca (Raise ann rt st exps) =
>     convPa ca ann <+>
>     text "raise"
>     <+> case rt of
>                 RNotice -> text "notice"
>                 RException -> text "exception"
>                 RError -> text "error"
>     <+> convExp (StringLit "'" st)
>     <> ifNotEmpty (\e -> comma <+> csvExp e) exps
>     <> statementEnd

> convStatement ca (ForSelectStatement ann i sel stmts) =
>     convPa ca ann <+>
>     text "for" <+> text i <+> text "in"
>     <+> convSelectExpression True sel <+> text "loop"
>     $+$ convNestedStatements ca stmts
>     $+$ text "end loop" <> statementEnd

> convStatement ca (ForIntegerStatement ann var st en stmts) =
>     convPa ca ann <+>
>     text "for" <+> text var <+> text "in"
>     <+> convExp st <+> text ".." <+> convExp en <+> text "loop"
>     $+$ convNestedStatements ca stmts
>     $+$ text "end loop" <> statementEnd

> convStatement ca (WhileStatement ann ex stmts) =
>     convPa ca ann <+>
>     text "while" <+> convExp ex <+> text "loop"
>     $+$ convNestedStatements ca stmts
>     $+$ text "end loop" <> statementEnd

> convStatement ca (ContinueStatement ann) =
>     convPa ca ann <+> text "continue" <> statementEnd

> convStatement ca (Perform ann f@(FunCall _ _)) =
>     convPa ca ann <+>
>     text "perform" <+> convExp f <> statementEnd
> convStatement _ (Perform _ x) =
>    error $ "internal error: convStatement not supported for " ++ show x

> convStatement ca (Copy ann tb cols src) =
>     convPa ca ann <+>
>     text "copy" <+> text tb
>     <+> ifNotEmpty (parens . hcatCsvMap text) cols
>     <+> text "from" <+> case src of
>                                  CopyFilename s -> quotes $ text s
>                                  Stdin -> text "stdin"
>     <> statementEnd

> convStatement ca (CopyData ann s) =
>     convPa ca ann <+>
>     text s <> text "\\." <> newline

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

> convStatement ca (ExecuteInto ann s is) =
>     convPa ca ann <+>
>     text "execute" <+> convExp s
>     <+> text "into" <+> hcatCsvMap text is
>     <> statementEnd

> convStatement ca (CaseStatement ann c conds els) =
>     convPa ca ann <+>
>     text "case" <+> convExp c
>     $+$ nest 2 (
>                 vcat (map (uncurry convWhenSt) conds)
>                 $+$ convElseSt els
>                 ) $+$ text "end case" <> statementEnd
>     where
>       convWhenSt ex sts = text "when" <+> hcatCsvMap convExp ex
>                           <+> text "then" $+$ convNestedStatements ca sts
>       convElseSt = ifNotEmpty (\s -> text "else" $+$ convNestedStatements ca s)


> statementEnd :: Doc
> statementEnd = semi <> newline

================================================================================

= Statement components

== selects

> convSelectExpression :: Bool -> SelectExpression -> Doc
> convSelectExpression writeSelect (Select dis l tb wh grp hav
>                                 ord orddir lim off) =
>   text (if writeSelect then "select" else "")
>   <+> (case dis of
>          Dupes -> empty
>          Distinct -> text "distinct")
>   <+> convSelList l
>   $+$ nest 2 (
>               maybeConv (\tr -> text "from" <+> convTref tr) tb
>               $+$ convWhere wh)
>   <+> ifNotEmpty (\g -> text "group by" <+> hcatCsvMap convExp g) grp
>   <+> maybeConv (\h -> text "having" <+> convExp h) hav
>   <+> ifNotEmpty (\o -> text "order by" <+> hcatCsvMap convExp o
>                   <+> convDir orddir) ord
>   <+> maybeConv (\lm -> text "limit" <+> convExp lm) lim
>   <+> maybeConv (\offs -> text "offset" <+> convExp offs) off
>   where
>     convTref (Tref f) = text f
>     convTref (TrefAlias f a) = text f <+> text a
>     convTref (JoinedTref t1 nat jt t2 ex) =
>         convTref t1
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
>         where
>           convJoinExpression (JoinOn e) = text "on" <+> convExp e
>           convJoinExpression (JoinUsing ids) =
>               text "using" <+> parens (hcatCsvMap text ids)

>     convTref (SubTref sub alias) =
>         parens (convSelectExpression True sub)
>         <+> text "as" <+> text alias
>     convTref (TrefFun f@(FunCall _ _)) = convExp f
>     convTref (TrefFun x) =
>         error $ "internal error: node not supported in function tref: " ++ show x
>     convTref (TrefFunAlias f@(FunCall _ _) a) =
>         convExp f <+> text "as" <+> text a
>     convTref (TrefFunAlias x _) =
>         error $ "internal error: node not supported in function tref: " ++ show x

> convSelectExpression writeSelect (CombineSelect tp s1 s2) =
>   convSelectExpression writeSelect s1
>   $+$ (case tp of
>          Except -> text "except"
>          Union -> text "union"
>          UnionAll -> text "union" <+> text "all"
>          Intersect -> text "intersect")
>   $+$ convSelectExpression True s2
> convSelectExpression _ (Values expss) =
>   text "values" $$ nest 2 (vcat $ csv $ map (parens . csvExp) expss)

> convDir :: Direction -> Doc
> convDir d = text $ case d of
>                           Asc -> "asc"
>                           Desc -> "desc"


> convWhere :: (Maybe Expression) -> Doc
> convWhere (Just ex) = text "where" <+> convExp ex
> convWhere Nothing = empty

> convSelList :: SelectList -> Doc
> convSelList (SelectList ex into) =
>   hcatCsvMap convSelItem ex
>   <+> ifNotEmpty (\i -> text "into" <+> hcatCsvMap text i) into
>   where
>     convSelItem (SelectItem ex1 nm) = convExp ex1 <+> text "as" <+> text nm
>     convSelItem (SelExp e) = convExp e

> convCasc :: Cascade -> Doc
> convCasc casc = text $ case casc of
>                                  Cascade -> "cascade"
>                                  Restrict -> "restrict"

== ddl

> convReturning :: Maybe SelectList -> Doc
> convReturning l = case l of
>                 Nothing -> empty
>                 Just ls -> nest 2 (text "returning" <+> convSelList ls)

> convIfExists :: IfExists -> Doc
> convIfExists i = case i of
>                         Require -> empty
>                         IfExists -> text "if exists"

== plpgsql

> convNestedStatements :: (Annotation -> String) -> StatementList -> Doc
> convNestedStatements pa = nest 2 . vcat . map (convStatement pa)

> convTypeName :: TypeName -> Doc
> convTypeName (SimpleTypeName s) = text s
> convTypeName (PrecTypeName s i) = text s <> parens(integer i)
> convTypeName (ArrayTypeName t) = convTypeName t <> text "[]"
> convTypeName (SetOfTypeName t) = text "setof" <+> convTypeName t

= Expressions

> convExp :: Expression -> Doc
> convExp (Identifier i) = text i
> convExp (IntegerLit n) = integer n
> convExp (FloatLit n) = double n
> convExp (StringLit tag s) = text tag <> text replaceQuotes <> text tag
>                           where
>                             replaceQuotes = if tag == "'"
>                                               then replace "'" "''" s
>                                               else s

> convExp (FunCall n es) =
>     --check for special operators
>    case n of
>      "!arrayCtor" -> text "array" <> brackets (csvExp es)
>      "!between" -> convExp (head es) <+> text "between"
>                    <+> parens (convExp (es !! 1))
>                   <+> text "and"
>                   <+> parens (convExp (es !! 2))
>      "!substring" -> text "substring"
>                      <> parens (convExp (head es)
>                                 <+> text "from" <+> convExp (es !! 1)
>                                 <+> text "for" <+> convExp (es !! 2))
>      "!arraySub" -> case es of
>                        ((Identifier i):es1) -> text i <> brackets (csvExp es1)
>                        _ -> parens (convExp (head es)) <> brackets (csvExp (tail es))
>      "!rowCtor" -> text "row" <> parens (hcatCsvMap convExp es)
>      _ | isOperator n ->
>         case getOperatorType n of
>                           BinaryOp ->
>                               parens (convExp (head es)
>                                       <+> text (filterKeyword n)
>                                       <+> convExp (es !! 1))
>                           PrefixOp -> parens (text (if n == "u-"
>                                                        then "-"
>                                                        else filterKeyword n)
>                                                <+> convExp (head es))
>                           PostfixOp -> parens (convExp (head es) <+> text (filterKeyword n))
>        | otherwise -> text n <> parens (csvExp es)
>    where
>      filterKeyword t = case t of
>                          "!and" -> "and"
>                          "!or" -> "or"
>                          "!not" -> "not"
>                          "!isNull" -> "is null"
>                          "!isNotNull" -> "is not null"
>                          "!like" -> "like"
>                          x -> x

> convExp (BooleanLit b) = bool b
> convExp (InPredicate att t lst) =
>   convExp att <+> (if not t then text "not" else empty) <+> text "in"
>   <+> parens (case lst of
>                        InList expr -> csvExp expr
>                        InSelect sel -> convSelectExpression True sel)
> convExp (ScalarSubQuery s) = parens (convSelectExpression True s)
> convExp NullLit = text "null"
> convExp (WindowFn fn partition order asc) =
>   convExp fn <+> text "over"
>   <+> (if hp || ho
>        then
>           parens ((if hp
>                      then text "partition by" <+> csvExp partition
>                      else empty)
>                   <+> (if ho
>                          then text "order by" <+> csvExp order
>                               <+> convDir asc
>                          else empty))
>        else empty)
>   where
>     hp = not (null partition)
>     ho = not (null order)
> convExp (Case whens els) =
>   text "case"
>   $+$ nest 2 (vcat (map convWhen whens)
>               $+$ maybeConv (\e -> text "else" <+> convExp e) els)
>   $+$ text "end"
>       where
>         convWhen (ex1, ex2) =
>             text "when" <+> hcatCsvMap convExp ex1
>             <+> text "then" <+> convExp ex2

> convExp (CaseSimple val whens els) =
>   text "case" <+> convExp val
>   $+$ nest 2 (vcat (map convWhen whens)
>               $+$ maybeConv (\e -> text "else" <+> convExp e) els)
>   $+$ text "end"
>       where
>         convWhen (ex1, ex2) =
>             text "when" <+> hcatCsvMap convExp ex1
>             <+> text "then" <+> convExp ex2

> convExp (PositionalArg a) = text "$" <> integer a
> convExp (Exists s) = text "exists" <+> parens (convSelectExpression True s)
> convExp (Cast ex t) = text "cast" <> parens (convExp ex
>                                              <+> text "as"
>                                              <+> convTypeName t)

= Utils

convert a list of expressions to horizontal csv

> csvExp :: [Expression] -> Doc
> csvExp = hcatCsvMap convExp

run conversion function if Just, return empty if nothing

> maybeConv :: (t -> Doc) -> Maybe t -> Doc
> maybeConv f c =
>     case c of
>       Nothing -> empty
>       Just a -> f a

> csv :: [Doc] -> [Doc]
> csv = punctuate comma

> hcatCsv :: [Doc] -> Doc
> hcatCsv = hcat . csv

> ifNotEmpty :: ([a] -> Doc) -> [a] -> Doc
> ifNotEmpty c l = if null l then empty else c l

map the converter ex over a list
then hcatcsv the results

> hcatCsvMap :: (a -> Doc) -> [a] -> Doc
> hcatCsvMap ex = hcatCsv . map ex

> bool :: Bool -> Doc
> bool b = if b then text "true" else text "false"

> newline :: Doc
> newline = text "\n"

> replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
> replace _ _ [] = []
> replace old new xs@(y:ys) =
>   case stripPrefix old xs of
>     Nothing -> y : replace old new ys
>     Just ys' -> new ++ replace old new ys'

> convPa :: (Annotation -> String) -> Annotation -> Doc
> convPa ca a = let s = ca a
>               in if s == "asda"
>                    then empty
>                    else text "/*\n" <+> text s
>                         <+> text "*/\n"
