
> module PrettyPrinter where

> import Text.PrettyPrint
> import Grammar

> printSql :: [Statement] -> String
> printSql ast = render $ vcat $ (map convStatement ast)

> convStatement :: Statement -> Doc
> convStatement (Select e) = text "select" <+> convExp e <> semi
> convStatement (CreateTable t atts) =
>     text "create table"
>     <+> text t <+> lparen
>     <+> hcat (csv (map convAttDef atts))
>     <+> rparen <> semi

> convAttDef :: AttributeDef -> Doc
> convAttDef (AttributeDef n t) = text n <+> text t

> convExp :: Expression -> Doc
> convExp (Identifier i) = text i
> convExp (IntegerLiteral n) = integer n
> convExp (StringLiteral s) = quotes $ text s
> convExp (FunctionCall i as) = text i <> lparen <> hcat (csv (map convExp as)) <> rparen
> convExp (BinaryOperatorCall op a b) = convExp a <+> text op <+> convExp b

> csv :: [Doc] -> [Doc]
> csv l = punctuate comma l