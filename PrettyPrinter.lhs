
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
> convStatement (SelectE e) = text "select" <+> convExp e <> semi
> convStatement (Select l tb) = text "select" <+> convSelList l
>                               <+> text "from" <+> text tb <> semi
> convStatement (CreateTable t atts) =
>     text "create table"
>     <+> text t <+> lparen
>     <+> hcat (csv (map convAttDef atts))
>     <+> rparen <> semi
> convStatement (Insert tb atts exps) = text "insert into" <+> text tb
>                                       <+> parens (hcatCsvMap text atts)
>                                       <+> text "values"
>                                       <+> parens (hcatCsvMap convExp exps)
>                                       <> semi
> convStatement (Update tb scs wh) = text "update" <+> text tb <+> text "set"
>                                    <+> hcatCsvMap convSetClause scs
>                                    <+> case wh of
>                                         Nothing -> empty
>                                         Just w -> convWhere w
>                                    <> semi
> convStatement (Delete tbl wh) = text "delete from" <+> text tbl
>                                 <+> case wh of
>                                            Nothing -> empty
>                                            Just w -> convWhere w
>                                 <> semi

= Statement components

> convSetClause :: SetClause -> Doc
> convSetClause (SetClause att ex) = text att <+> text "=" <+> convExp ex

> convWhere :: Where -> Doc
> convWhere (Where ex) = text "where" <+> convExp ex

> convSelList :: SelectList -> Doc
> convSelList (SelectList l) = hcatCsvMap text l
> convSelList (Star) = text "*"

> convAttDef :: AttributeDef -> Doc
> convAttDef (AttributeDef n t) = text n <+> text t

= Expressions

> convExp :: Expression -> Doc
> convExp (Identifier i) = text i
> convExp (IntegerL n) = integer n
> convExp (StringL s) = quotes $ text s
> convExp (FunctionCall i as) = text i <> parens (hcatCsvMap convExp as)
> convExp (BinaryOperatorCall op a b) = parens (convExp a <+> text (opToSymbol op) <+> convExp b)
> convExp (BooleanL b) = bool b

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