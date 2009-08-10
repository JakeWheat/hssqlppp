
> module Grammar where

> data Statement = SelectE Expression
>                | Select SelectList String
>                | CreateTable String [AttributeDef]
>                | Insert String [String] [Expression]
>                  deriving (Eq,Show)

> data SelectList = SelectList [String] | Star
>                   deriving (Eq,Show)

> data Expression = Identifier String
>                 | IntegerLiteral Integer
>                 | StringLiteral String
>                 | FunctionCall String [Expression]
>                 | BinaryOperatorCall String Expression Expression
>                   deriving (Eq,Show)

> data AttributeDef = AttributeDef String String
>                     deriving (Eq,Show)