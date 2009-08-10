
> module Grammar where

> data Statement = SelectE Expression
>                | Select SelectList String
>                | CreateTable String [AttributeDef]
>                | Insert String [String] [Expression]
>                | Update String [SetClause] (Maybe Where)
>                  deriving (Eq,Show)

> data SetClause = SetClause String Expression
>                  deriving (Eq,Show)

> data Where = Where Expression
>                    deriving (Eq,Show)

> data SelectList = SelectList [String] | Star
>                   deriving (Eq,Show)

 > data Expression = Identifier String
 >                 | IntegerLiteral Integer
 >                 | StringLiteral String
 >                 | BooleanLiteral Bool
 >                 | FunctionCall String [Expression]
 >                 | BinaryOperatorCall String Expression Expression
 >                   deriving (Eq,Show)

> data Op = Plus | Minus | Mult | Div | Pow | Mod | Neg | Eql
>   deriving (Show,Eq)

> opToSymbol :: Op -> [Char]
> opToSymbol op = case op of
>                         Plus -> "+"
>                         Minus -> "-"
>                         Mult -> "*"
>                         Div -> "/"
>                         Pow -> "^"
>                         Mod -> "%"
>                         Neg -> "-"
>                         Eql -> "="

> data Expression = BinaryOperatorCall Op Expression Expression
>            | IntegerL Integer
>            | StringL String
>            | BooleanL Bool
>            | Identifier String
>            | FunctionCall String [Expression]
>   deriving (Show,Eq)


> data AttributeDef = AttributeDef String String
>                     deriving (Eq,Show)