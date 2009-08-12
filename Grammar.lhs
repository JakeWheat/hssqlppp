
> module Grammar where

================================================================================

SQL top level statements

> data Statement = Select SelectList (Maybe From) (Maybe Where)
>                | CombineSelect CombineType Statement Statement
>                | CreateTable String [AttributeDef]
>                | CreateView String Statement
>                | CreateType String [TypeAttributeDef]
>                | Insert String (Maybe [String]) [Expression]
>                | Update String [SetClause] (Maybe Where)
>                | Delete String (Maybe Where)
>                | CreateFunction String [ParamDef] String [VarDef] [Statement]
>                | CreateDomain String String (Maybe Expression)
>                | Assignment String Expression
>                | Return Expression
>                | Raise RaiseType String [Expression]
>                | NullStatement
>                | Perform Expression
>                | ForStatement String Statement [Statement]
>                | Copy String
>                  deriving (Eq,Show)

================================================================================

Statement components

> data SetClause = SetClause String Expression
>                  deriving (Eq,Show)

> data Where = Where Expression
>                    deriving (Eq,Show)

> data From = From TableRef
>             deriving (Eq,Show)

> data TableRef = Tref String
>               | TrefAlias String String
>               | JoinedTref TableRef JoinType TableRef (Maybe Expression)
>                 deriving (Eq,Show)

> data JoinType = Inner
>                 deriving (Eq,Show)

> data SelectList = SelectList [SelectItem]
>                   deriving (Eq,Show)

> data SelectItem = SelExp Expression
>                 | SelectItem Expression String
>                   deriving (Eq,Show)

> data AttributeDef = AttributeDef String String (Maybe Expression) (Maybe Expression)
>                     deriving (Eq,Show)

> data TypeAttributeDef = TypeAttDef String String
>                         deriving (Eq,Show)

> data ParamDef = ParamDef String String
>                     deriving (Eq,Show)

> data VarDef = VarDef String String
>                     deriving (Eq,Show)

> data RaiseType = RNotice | RError
>                  deriving (Eq, Show)

> data CombineType = Except | Union
>                    deriving (Eq, Show)

================================================================================

Expressions

> data Op = Plus | Minus | Mult | Div | Pow | Mod | Eql
>         | And | Conc | Like | Not | IsNull | IsNotNull
>         | Cast | Qual
>           deriving (Show,Eq)

> opToSymbol :: Op -> String
> opToSymbol op = case op of
>                         Plus -> "+"
>                         Minus -> "-"
>                         Mult -> "*"
>                         Div -> "/"
>                         Pow -> "^"
>                         Mod -> "%"
>                         Eql -> "="
>                         And -> "and"
>                         Conc -> "||"
>                         Like -> "like"
>                         Not -> "not"
>                         IsNull -> "is null"
>                         IsNotNull -> "is not null"
>                         Cast -> "::"
>                         Qual -> "."

> data Expression = BinaryOperatorCall Op Expression Expression
>                 | IntegerL Integer
>                 | StringL String
>                 | StringLD String String
>                 | NullL
>                 | BooleanL Bool
>                 | Identifier String
>                 -- | QualifiedIdentifier String String
>                 | InPredicate String [Expression]
>                 | FunctionCall String [Expression]
>                 | ScalarSubQuery Statement
>                 | ArrayL [Expression]
>                 | WindowFn Expression (Maybe [Expression])
>                 | Case [When] Else
>                   deriving (Show,Eq)

> data When = When Expression Expression
>                   deriving (Show,Eq)
> data Else = Else Expression
>                   deriving (Show,Eq)
