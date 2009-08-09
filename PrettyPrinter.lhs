
> module PrettyPrinter where

> import Text.PrettyPrint
> import Grammar

> printSql :: Select -> String
> printSql ast = render $ convSelect ast

> convSelect :: Select -> Doc
> convSelect s = case s of Select e -> text "select" <+> convExp e <> semi

> convExp :: Expression -> Doc
> convExp e = case e of
>               Identifier i -> text i
>               IntegerLiteral n -> integer n
>               StringLiteral s -> quotes $ text s
>               FunctionCall i as -> text i <> lparen <> hcat (csv (map convExp as)) <> rparen
>               BinaryOperatorCall op a b -> convExp a <+> text op <+> convExp b

> csv :: [Doc] -> [Doc]
> csv l = punctuate comma l