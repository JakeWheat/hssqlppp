
> module Grammar where

> data Select = Select Expression
>               deriving (Eq,Show)

> data Expression = Identifier String
>                 | IntegerLiteral Integer
>                 | StringLiteral String
>                 | FunctionCall String [Expression]
>                 | BinaryOperatorCall String Expression Expression
>                   deriving (Eq,Show)
