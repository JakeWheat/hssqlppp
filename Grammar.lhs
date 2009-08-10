
> module Grammar where

================================================================================

SQL top level statements

> data Statement = SelectE Expression
>                | Select SelectList String (Maybe Where)
>                | CreateTable String [AttributeDef]
>                | CreateView String Statement
>                | Insert String [String] [Expression]
>                | Update String [SetClause] (Maybe Where)
>                | Delete String (Maybe Where)
>                | CreateFunction String [ParamDef] String [Statement]
>                | NullStatement
>                  deriving (Eq,Show)

================================================================================

Statement components

> data SetClause = SetClause String Expression
>                  deriving (Eq,Show)

> data Where = Where Expression
>                    deriving (Eq,Show)

> data SelectList = SelectList [String] | Star
>                   deriving (Eq,Show)

> data AttributeDef = AttributeDef String String (Maybe Expression)
>                     deriving (Eq,Show)

> data ParamDef = ParamDef String String
>                     deriving (Eq,Show)

================================================================================

Expressions

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
>            | InPredicate String [Expression]
>            | FunctionCall String [Expression]
>   deriving (Show,Eq)


