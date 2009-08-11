
> module PrettyPrinter where

> import Text.PrettyPrint
> import Grammar

================================================================================

Public functions

> printSql :: [Statement] -> String
> printSql ast = render $ (vcat $ (map convStatement ast)) <> text "\n"

> printExpression :: Expression -> String
> printExpression ast = render $ convExp ast


================================================================================

Conversion routines - convert Sql asts into Docs

= Statements

> convStatement :: Statement -> Doc

> convStatement (SelectE e) = text "select" <+> convExp e <> statementEnd

> convStatement s@(Select _ _ _) = convSelectFragment s <> statementEnd
> convStatement s@(ExceptSelect _ _) = convSelectFragment s <> statementEnd

> convStatement (CreateTable t atts) =
>     text "create table"
>     <+> text t <+> lparen
>     $+$ nest 2 (vcat (csv (map convAttDef atts)))
>     $+$ rparen <> statementEnd

> convStatement (Insert tb atts exps) = text "insert into" <+> text tb
>                                       <+> parens (hcatCsvMap text atts)
>                                       <+> text "values"
>                                       <+> parens (hcatCsvMap convExp exps)
>                                       <> statementEnd

> convStatement (Update tb scs wh) = text "update" <+> text tb <+> text "set"
>                                    <+> hcatCsvMap convSetClause scs
>                                    <+> convWhere wh
>                                    <> statementEnd

> convStatement (Delete tbl wh) = text "delete from" <+> text tbl
>                                 <+> convWhere wh
>                                 <> statementEnd

> convStatement (CreateFunction name args retType decls stmts) =
>     text "create function" <+> text name
>     <+> parens (hcatCsvMap convParamDef args)
>     <+> text "returns" <+> text retType <+> text "as" <+> text "$$"
>     $+$ (if not (null decls)
>           then
>             text "declare"
>             $+$ nest 2 (vcat $ map convVarDef decls)
>           else empty)
>     $+$ text "begin"
>     $+$ (nest 2 (vcat $ map convStatement stmts))
>     $+$ text "end;"
>     $+$ text "$$ language plpgsql volatile" <> statementEnd

> convStatement (CreateView name sel) =
>     text "create view" <+> text name <+> text "as"
>     $+$ (nest 2 (convSelectFragment sel)) <> statementEnd

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
>          then do
>            comma
>            <+> hcatCsvMap convExp exps
>          else empty)
>     <> statementEnd

> convStatement (ForStatement i sel stmts) =
>     text "for" <+> text i <+> text "in" <+> convSelectFragment sel <+> text "loop"
>     $+$ (nest 2 (vcat $ map convStatement stmts))
>     $+$ text "end loop" <> statementEnd

> statementEnd :: Doc
> statementEnd = semi <> newline


= Statement components

> convSelectFragment :: Statement -> Doc
> convSelectFragment (Select l tb wh) =
>   text "select" <+> convSelList l
>   $+$ nest 2 (
>     text "from" <+> text tb
>     $+$ convWhere wh)

> convSelectFragment (ExceptSelect s1 s2) =
>   convSelectFragment s1
>   $+$ text "except"
>   $+$ convSelectFragment s2

> convSelectFragment a = error $ "no convSelectFragment for " ++ show a

> convSetClause :: SetClause -> Doc
> convSetClause (SetClause att ex) = text att <+> text "=" <+> convExp ex

> convWhere :: Maybe Where -> Doc
> convWhere (Just (Where ex)) = text "where" <+> convExp ex
> convWhere Nothing = empty

> convSelList :: SelectList -> Doc
> convSelList (SelectList l) = hcatCsvMap text l
> convSelList (Star) = text "*"

> convAttDef :: AttributeDef -> Doc
> convAttDef (AttributeDef n t ch) = text n <+> text t
>                                    <+> case ch of
>                                          Nothing -> empty
>                                          Just e -> text "check" <+> convExp e

> convParamDef :: ParamDef -> Doc
> convParamDef (ParamDef n t) = text n <+> text t

> convVarDef :: VarDef -> Doc
> convVarDef (VarDef n t) = text n <+> text t <> semi

= Expressions

> convExp :: Expression -> Doc
> convExp (Identifier i) = text i
> convExp (QualifiedIdentifier q i) = text q <> text "." <> text i
> convExp (IntegerL n) = integer n
> convExp (StringL s) = quotes $ text s
> convExp (FunctionCall i as) = text i <> parens (hcatCsvMap convExp as)
> convExp (BinaryOperatorCall op a b) = parens (convExp a <+> text (opToSymbol op) <+> convExp b)
> convExp (BooleanL b) = bool b
> convExp (InPredicate att expr) = text att <+> text "in" <+> parens (hcatCsvMap convExp expr)

= Utils

> csv :: [Doc] -> [Doc]
> csv l = punctuate comma l

> hcatCsv :: [Doc] -> Doc
> hcatCsv l = hcat $ csv l

> hcatCsvMap :: (a -> Doc) -> [a] -> Doc
> hcatCsvMap ex l = hcatCsv (map ex l)

> bool :: Bool -> Doc
> bool b = case b of
>            True -> text "true"
>            False -> text "false"

> newline :: Doc
> newline = text "\n"