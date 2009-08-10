
> module Grammar where

> data Statement = Select Expression
>                | CreateTable String [AttributeDef]
>                  deriving (Eq,Show)

> data Expression = Identifier String
>                 | IntegerLiteral Integer
>                 | StringLiteral String
>                 | FunctionCall String [Expression]
>                 | BinaryOperatorCall String Expression Expression
>                   deriving (Eq,Show)

> data AttributeDef = AttributeDef String String
>                     deriving (Eq,Show)