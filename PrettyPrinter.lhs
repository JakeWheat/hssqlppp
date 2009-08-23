Copyright 2009 Jake Wheat

The pretty printer which prints ast nodes from Tree.lhs
It uses the hughes pj pretty printer

Produces sort of readable code, but mainly just written to produce
reparsable text.

> module PrettyPrinter (
>                       --convert a sql ast to text
>                       printSql
>                       --convert a single expression parse node to text
>                      ,printExpression
>                      )
>     where

> import Text.PrettyPrint
> import Data.List (stripPrefix)
> import Data.Maybe
> import Ast

================================================================================

Public functions

> printSql :: [Statement] -> String
> printSql ast = render $ vcat (map convStatement ast) <> text "\n"

> printExpression :: Expression -> String
> printExpression = render . convExp


================================================================================

Conversion routines - convert Sql asts into Docs
= Statements

> convStatement :: Statement -> Doc

== selects

> convStatement s@(Select _ _ _ _ _ _ _ _ _ _) =
>   convSelectFragment True s <> statementEnd
> convStatement s@(CombineSelect _ _ _) =
>   convSelectFragment True s <> statementEnd

> convStatement v@(Values _) = convSelectFragment True v <> statementEnd

== dml

> convStatement (Insert tb atts idata rt) =
>   text "insert into" <+> text tb
>   <+> ifNotEmpty (parens . hcatCsvMap text) atts
>   $+$ convSelectFragment True idata
>   $+$ convReturning rt
>   <> statementEnd

> convStatement (Update tb scs wh rt) =
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

> convStatement (Delete tbl wh rt) = text "delete from" <+> text tbl
>                                 <+> convWhere wh
>                                 $+$ convReturning rt
>                                 <> statementEnd

> convStatement (Truncate names ri casc) =
>     text "truncate"
>     <+> hcatCsvMap text names
>     <+> text (case ri of
>                       RestartIdentity -> "restart identity"
>                       ContinueIdentity -> "continue identity")
>     <+> convCasc casc
>     <> statementEnd

== ddl

> convStatement (CreateTable tbl atts cns) =
>     text "create table"
>     <+> text tbl <+> lparen

>     $+$ nest 2 (vcat (csv (map convAttDef atts ++ map convCon cns)))
>     $+$ rparen <> statementEnd
>     where
>       convAttDef (AttributeDef n t def cons) =
>         text n <+> text t
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
>                                     <+> ifNotEmpty
>                                           (parens . hcatCsvMap text) att
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



> convStatement (CreateTableAs t sel) =
>     text "create table"
>     <+> text t <+> text "as"
>     $+$ convSelectFragment True sel
>     <> statementEnd

> convStatement (CreateFunction lang name args retType qt body vol) =
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
>       convFnBody (SqlFnBody sts) = convNestedStatements sts
>       convFnBody (PlpgsqlFnBody decls sts) =
>           ifNotEmpty (\l -> text "declare"
>                   $+$ nest 2 (vcat $ map convVarDef l)) decls
>           $+$ text "begin"
>           $+$ convNestedStatements sts
>           $+$ text "end;"
>       convParamDef (ParamDef n t) = text n <+> convTypeName t
>       convParamDef  (ParamDefTp t) = convTypeName t
>       convVarDef (VarDef n t v) =
>         text n <+> convTypeName t
>         <+> maybeConv (\x -> text ":=" <+> convExp x) v <> semi



> convStatement (CreateView name sel) =
>     text "create view" <+> text name <+> text "as"
>     $+$ nest 2 (convSelectFragment True sel) <> statementEnd

> convStatement (CreateDomain name tp ex) =
>     text "create domain" <+> text name <+> text "as"
>     <+> text tp <+> checkExp ex <> statementEnd
>     where
>       checkExp = maybeConv (\e -> text "check" <+> parens (convExp e))

> convStatement (DropFunction ifExists fns casc) =
>   text "drop function"
>   <+> convIfExists ifExists
>   <+> hcatCsvMap doFunction fns
>   <+> convCasc casc
>   <> statementEnd
>   where
>     doFunction (name,types) = text name <> parens (hcatCsvMap text types)

> convStatement (DropSomething dropType ifExists names casc) =
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

> convStatement (CreateType name atts) =
>     text "create type" <+> text name <+> text "as" <+> lparen
>     $+$ nest 2 (vcat (csv
>           (map (\(TypeAttDef n t) -> text n <+> text t)  atts)))
>     $+$ rparen <> statementEnd

== plpgsql

> convStatement NullStatement = text "null" <> statementEnd

> convStatement (Assignment name val) =
>     text name <+> text ":=" <+> convExp val <> statementEnd

> convStatement (Return ex) =
>     text "return" <+> maybeConv convExp ex <> statementEnd

> convStatement (ReturnNext ex) =
>     text "return" <+> text "next" <+> convExp ex <> statementEnd

> convStatement (ReturnQuery sel) =
>     text "return" <+> text "query"
>     <+> convSelectFragment True sel <> statementEnd

> convStatement (Raise rt st exps) =
>     text "raise"
>     <+> case rt of
>                 RNotice -> text "notice"
>                 RException -> text "exception"
>                 RError -> text "error"
>     <+> convExp (StringLit "'" st)
>     <> ifNotEmpty (\e -> comma <+> csvExp e) exps
>     <> statementEnd

> convStatement (ForSelectStatement i sel stmts) =
>     text "for" <+> text i <+> text "in"
>     <+> convSelectFragment True sel <+> text "loop"
>     $+$ convNestedStatements stmts
>     $+$ text "end loop" <> statementEnd

> convStatement (ForIntegerStatement var st en stmts) =
>     text "for" <+> text var <+> text "in"
>     <+> convExp st <+> text ".." <+> convExp en <+> text "loop"
>     $+$ convNestedStatements stmts
>     $+$ text "end loop" <> statementEnd

> convStatement (WhileStatement ex stmts) =
>     text "while" <+> convExp ex <+> text "loop"
>     $+$ convNestedStatements stmts
>     $+$ text "end loop" <> statementEnd

> convStatement (ContinueStatement) = text "continue" <> statementEnd

> convStatement (Perform f@(FunCall _ _)) =
>     text "perform" <+> convExp f <> statementEnd
> convStatement (Perform x) =
>    error $ "convStatement not supported for " ++ show x

> convStatement (Copy tb cols src) =
>     text "copy" <+> text tb
>     <+> ifNotEmpty (parens . hcatCsvMap text) cols
>     <+> text "from" <+> case src of
>                                  CopyFilename s -> quotes $ text s
>                                  Stdin -> text "stdin"
>     <> statementEnd

> convStatement (CopyData s) = text s <> text "\\." <> newline

> convStatement (If conds els) =
>    text "if" <+> convCond (head conds)
>    $+$ vcat (map (\c -> text "elseif" <+> convCond c) $ tail conds)
>    $+$ ifNotEmpty (\e -> text "else" $+$ convNestedStatements e) els
>    $+$ text "end if" <> statementEnd
>     where
>       convCond (ex, sts) = convExp ex <+> text "then"
>                            $+$ convNestedStatements sts
> convStatement (Execute s) = text "execute" <+> convExp s <> statementEnd
> convStatement (ExecuteInto s is) = text "execute" <+> convExp s
>                                    <+> text "into" <+> hcatCsvMap text is
>                                    <> statementEnd

> convStatement (CaseStatement c conds els) =
>     text "case" <+> convExp c
>     $+$ nest 2 (
>                 vcat (map (uncurry convWhenSt) conds)
>                 $+$ convElseSt els
>                 ) $+$ text "end case" <> statementEnd
>     where
>       convWhenSt ex sts = text "when" <+> hcatCsvMap convExp ex
>                           <+> text "then" $+$ convNestedStatements sts
>       convElseSt = ifNotEmpty (\s -> text "else" $+$ convNestedStatements s)


> statementEnd :: Doc
> statementEnd = semi <> newline

================================================================================

= Statement components

== selects

> convSelectFragment :: Bool -> Statement -> Doc
> convSelectFragment writeSelect (Select dis l tb wh grp hav
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
>         parens (convSelectFragment True sub)
>         <+> text "as" <+> text alias
>     convTref (TrefFun f@(FunCall _ _)) = convExp f
>     convTref (TrefFun x) =
>         error $ "node not supported in function tref: " ++ show x
>     convTref (TrefFunAlias f@(FunCall _ _) a) =
>         convExp f <+> text "as" <+> text a
>     convTref (TrefFunAlias x _) =
>         error $ "node not supported in function tref: " ++ show x

> convSelectFragment writeSelect (CombineSelect tp s1 s2) =
>   convSelectFragment writeSelect s1
>   $+$ (case tp of
>          Except -> text "except"
>          Union -> text "union"
>          UnionAll -> text "union" <+> text "all"
>          Intersect -> text "intersect")
>   $+$ convSelectFragment True s2
> convSelectFragment _ (Values expss) =
>   text "values" $$ nest 2 (vcat $ csv $ map (parens . csvExp) expss)

> convSelectFragment _ a = error $ "no convSelectFragment for " ++ show a

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

> convNestedStatements :: [Statement] -> Doc
> convNestedStatements = nest 2 . vcat . map convStatement

> convTypeName :: TypeName -> Doc
> convTypeName (SimpleType s) = text s
> convTypeName (PrecType s i) = text s <> parens(integer i)
> convTypeName (ArrayType t) = convTypeName t <> text "[]"
> convTypeName (SetOfType t) = text "setof" <+> convTypeName t

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

> convExp (FunCall i as) = text i <> parens (csvExp as)

> convExp (BinOpCall op a b) =
>   parens (convExp a <+> text (binOpToSymbol op) <+> convExp b)

> convExp (UnOpCall op a) =
>     case op of
>           Not -> parens (text (unOpToSymbol op) <+> convExp a)
>           SetOf -> text (unOpToSymbol op) <+> convExp a
>           IsNull -> parens (convExp a <+> text (unOpToSymbol op))
>           IsNotNull -> parens (convExp a <+> text (unOpToSymbol op))
>           Abs -> parens (text (unOpToSymbol op) <+> convExp a)
>           Neg -> parens (text (unOpToSymbol op) <+> convExp a)

> convExp (BooleanLit b) = bool b
> convExp (InPredicate att t lst) =
>   convExp att <+> (if not t then text "not" else empty) <+> text "in"
>   <+> parens (case lst of
>                        InList expr -> csvExp expr
>                        InSelect sel -> convSelectFragment True sel)
> convExp (ScalarSubQuery s) = parens (convSelectFragment True s)
> convExp NullLit = text "null"
> convExp (ArrayLit es) = text "array" <> brackets (csvExp es)
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

> convExp (PositionalArg a) = text "$" <> integer a
> convExp (Exists s) = text "exists" <+> parens (convSelectFragment True s)
> convExp (Row r) = text "row" <> parens (hcatCsvMap convExp r)
> convExp (ArraySub (Identifier i) s) = text i <> brackets (csvExp s)
> convExp (ArraySub e s) = parens (convExp e) <> brackets (csvExp s)
> convExp (Between i e f) = convExp i <+> text "between" <+> parens (convExp e)
>                           <+> text "and" <+> parens (convExp f)
> convExp (CastKeyword ex t) = text "cast" <> parens (convExp ex
>                                                     <+> text "as"
>                                                     <+> convTypeName t)
> convExp (Substring s b e) = text "substring"
>                             <> parens (convExp s
>                                        <+> text "from" <+> convExp b
>                                        <+> text "for" <+> convExp e)

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
