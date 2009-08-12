> module PrettyPrinter where

> import Text.PrettyPrint
> import Grammar
> import Data.List (stripPrefix)

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

> convStatement s@(Select _ _ _) = convSelectFragment s <> statementEnd
> convStatement s@(CombineSelect _ _ _) = convSelectFragment s <> statementEnd

> convStatement (CreateTable t atts) =
>     text "create table"
>     <+> text t <+> lparen
>     $+$ nest 2 (vcat (csv (map convAttDef atts)))
>     $+$ rparen <> statementEnd

> convStatement (Insert tb atts exps) = text "insert into" <+> text tb
>                                       <+> maybeConv (\x -> parens (hcatCsvMap text x)) atts
>                                       <+> text "values"
>                                       <+> parens (csvExp exps)
>                                       <> statementEnd

> convStatement (Update tb scs wh) = text "update" <+> text tb <+> text "set"
>                                    <+> hcatCsvMap convSetClause scs
>                                    <+> convWhere wh
>                                    <> statementEnd

> convStatement (Delete tbl wh) = text "delete from" <+> text tbl
>                                 <+> convWhere wh
>                                 <> statementEnd

> convStatement (CreateFunction lang name args retType qt body vol) =
>     text "create function" <+> text name
>     <+> parens (hcatCsvMap convParamDef args)
>     <+> text "returns" <+> text retType <+> text "as" <+> text qt
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

> convStatement (CreateView name sel) =
>     text "create view" <+> text name <+> text "as"
>     $+$ nest 2 (convSelectFragment sel) <> statementEnd

> convStatement (CreateDomain name tp ex) =
>     text "create domain" <+> text name <+> text "as"
>     <+> text tp <+> checkExp ex <> statementEnd

> convStatement (CreateType name atts) =
>     text "create type" <+> text name <+> text "as" <+> lparen
>     $+$ nest 2 (vcat (csv (map (\(TypeAttDef n t) -> text n <+> text t)  atts)))
>     $+$ rparen <> statementEnd

plpgsql

> convStatement NullStatement = text "null" <> statementEnd

> convStatement (Assignment name val) =
>     text name <+> text ":=" <+> convExp val <> statementEnd

> convStatement (Return ex) =
>     text "return" <+> convExp ex <> statementEnd

> convStatement (Raise rt st exps) =
>     text "raise"
>     <+> case rt of
>                 RNotice -> text "notice"
>                 RError -> text "error"
>     <+> quotes (text st)
>     <> (if not (null exps)
>          then
>            comma
>            <+> csvExp exps
>          else empty)
>     <> statementEnd

> convStatement (ForStatement i sel stmts) =
>     text "for" <+> text i <+> text "in" <+> convSelectFragment sel <+> text "loop"
>     $+$ nest 2 (vcat $ map convStatement stmts)
>     $+$ text "end loop" <> statementEnd

> convStatement (Perform f@(FunctionCall _ _)) =
>     text "perform" <+> convExp f <> statementEnd
> convStatement (Perform x) =
>    error $ "convStatement not supported for " ++ show x

> convStatement (Copy x) =
>     text "copy" <+> text x

> statementEnd :: Doc
> statementEnd = semi <> newline


= Statement components

> convSelectFragment :: Statement -> Doc
> convSelectFragment (Select l tb wh) =
>   text "select" <+> convSelList l
>   $+$ nest 2 (
>     maybeConv convFrom tb
>     $+$ convWhere wh)
> convSelectFragment (CombineSelect tp s1 s2) =
>   convSelectFragment s1
>   $+$ (case tp of
>          Except -> text "except"
>          Union -> text "union")
>   $+$ convSelectFragment s2
> convSelectFragment a = error $ "no convSelectFragment for " ++ show a

> convFrom :: From -> Doc
> convFrom (From tr) = text "from" <+> convTref tr

> convTref :: TableRef -> Doc
> convTref (Tref f) = text f
> convTref (TrefAlias f a) = text f <+> text a
> convTref (JoinedTref t1 nat jt t2 ex) =
>     convTref t1
>     $+$ case nat of
>           True -> text "natural"
>           False -> empty
>     <+> text (case jt of
>                       Inner -> "inner"
>                       Cross -> "cross"
>                       LeftOuter -> "left outer"
>                       RightOuter -> "right outer"
>                       FullOuter -> "full outer")
>     <+> text "join"
>     <+> convTref t2
>     <+> maybeConv (\e -> nest 2 (text "on" <+> convExp e)) ex
> convTref (SubTref sub alias) =
>     parens (convSelectFragment sub)
>     <+> text "as" <+> text alias

> convSetClause :: SetClause -> Doc
> convSetClause (SetClause att ex) = text att <+> text "=" <+> convExp ex

> convWhere :: Maybe Where -> Doc
> convWhere (Just (Where ex)) = text "where" <+> convExp ex
> convWhere Nothing = empty

> convSelList :: SelectList -> Doc
> convSelList (SelectList l) = hcatCsvMap convSelItem l

> convSelItem :: SelectItem -> Doc
> convSelItem (SelectItem ex nm) = (convExp ex) <+> text "as" <+> text nm
> convSelItem (SelExp e) = convExp e

> convAttDef :: AttributeDef -> Doc
> convAttDef (AttributeDef n t def ch) = text n <+> text t
>                                        <+> maybeConv (\e -> text "default" <+> convExp e) def
>                                        <+> checkExp ch

> checkExp :: Maybe Expression -> Doc
> checkExp c = maybeConv (\e -> text "check" <+> convExp e) c

> convParamDef :: ParamDef -> Doc
> convParamDef (ParamDef n t) = text n <+> text t
> convParamDef  (ParamDefTp t) = text t

> convVarDef :: VarDef -> Doc
> convVarDef (VarDef n t) = text n <+> text t <> semi

> convFnBody :: FnBody -> Doc
> convFnBody (SqlFnBody sts) = nest 2 (vcat $ map convStatement sts)
> convFnBody (PlpgsqlFnBody decls sts) =
>     (if not (null decls)
>           then
>             text "declare"
>             $+$ nest 2 (vcat $ map convVarDef decls)
>           else empty)
>     $+$ text "begin"
>     $+$ nest 2 (vcat $ map convStatement sts)
>     $+$ text "end;"

= Expressions

> convExp :: Expression -> Doc
> convExp (Identifier i) = text i
> --convExp (QualifiedIdentifier q i) = text q <> text "." <> text i
> convExp (IntegerL n) = integer n
> convExp (StringL s) = quotes $ text $ replace "'" "''" s
> convExp (StringLD t s) = tag <> text s <> tag
>     where tag = text "$" <> text t <> text "$"

> convExp (FunctionCall i as) = text i <> parens (csvExp as)
> convExp (BinaryOperatorCall op a b) = case op of
>                                       Not -> parens (text (opToSymbol op) <+> convExp b)
>                                       IsNull -> parens (convExp b <+> text (opToSymbol op))
>                                       IsNotNull -> parens (convExp b <+> text (opToSymbol op))
>                                       Qual -> parens (convExp a <> text (opToSymbol op) <> convExp b)
>                                       _ -> parens (convExp a <+> text (opToSymbol op) <+> convExp b)
> convExp (BooleanL b) = bool b
> convExp (InPredicate att expr) = text att <+> text "in" <+> parens (csvExp expr)
> convExp (ScalarSubQuery s) = parens (convSelectFragment s)
> convExp NullL = text "null"
> convExp (ArrayL es) = text "array" <> brackets (csvExp es)
> convExp (WindowFn fn order) = convExp fn <+> text "over"
>                                     <+> maybeConv (\x -> parens (text "order by" <+> csvExp x)) order
> convExp (Case whens (Else els)) = text "case"
>                            $+$ nest 2 (vcat (map convWhen whens)
>                                        $+$ text "else" <+> convExp els)
>                            $+$ text "end"
> convExp (PositionalArg a) = text "$" <> int a

> convWhen :: When -> Doc
> convWhen (When ex1 ex2) = text "when" <+> convExp ex1 <+> text "then" <+> convExp ex2

= Utils

> csvExp :: [Expression] -> Doc
> csvExp = hcatCsvMap convExp

> maybeConv :: (t -> Doc) -> Maybe t -> Doc
> maybeConv f c =
>     case c of
>       Nothing -> empty
>       Just a -> f a

> csv :: [Doc] -> [Doc]
> csv = punctuate comma

> hcatCsv :: [Doc] -> Doc
> hcatCsv = hcat . csv

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
