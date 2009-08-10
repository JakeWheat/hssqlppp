
> module PrettyPrinter where

> import Text.PrettyPrint
> import Grammar

> printSql :: [Statement] -> String
> printSql ast = render $ (vcat $ (map convStatement ast)) <> text "\n"

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
>                                       <+> lparen <> hcat (csv (map text atts)) <> rparen
>                                       <+> text "values"
>                                       <+> lparen <> hcat (csv $ map convExp exps) <> rparen
>                                       <> semi
> convStatement (Update tb scs wh) = text "update" <+> text tb <+> text "set"
>                                    <+> (hcat $ csv $ map convSetClause scs)
>                                    <> case wh of
>                                         Nothing -> empty
>                                         Just w -> convWhere w
>                                    <> semi

> convSetClause :: SetClause -> Doc
> convSetClause (SetClause att ex) = text att <+> text "=" <+> convExp ex

> convWhere :: Where -> Doc
> convWhere (Where ex) = text "where" <+> convExp ex

> convSelList :: SelectList -> Doc
> convSelList (SelectList l) = hcat $ csv (map text l)
> convSelList (Star) = text "*"

> convAttDef :: AttributeDef -> Doc
> convAttDef (AttributeDef n t) = text n <+> text t

> convExp :: Expression -> Doc
> convExp (Identifier i) = text i
> convExp (IntegerLiteral n) = integer n
> convExp (StringLiteral s) = quotes $ text s
> convExp (FunctionCall i as) = text i <> lparen <> hcat (csv (map convExp as)) <> rparen
> convExp (BinaryOperatorCall op a b) = convExp a <+> text op <+> convExp b
> convExp (BooleanLiteral b) = bool b

> csv :: [Doc] -> [Doc]
> csv l = punctuate comma l

> bool :: Bool -> Doc
> bool b = case b of
>            True -> text "true"
>            False -> text "false"