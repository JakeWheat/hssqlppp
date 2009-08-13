
> module Grammar where

================================================================================

SQL top level statements

> data Statement = Select SelectList (Maybe From) (Maybe Where) (Maybe [Expression]) (Maybe Expression)
>                -- selectlist from where orderby limit
>                | CombineSelect CombineType Statement Statement
>                | SelectInto [String] Statement
>                | CreateTable String [AttributeDef]
>                | CreateView String Statement
>                | CreateType String [TypeAttributeDef]
>                | Insert String (Maybe [String]) InsertData
>                | Update String [SetClause] (Maybe Where)
>                | Delete String (Maybe Where)
>                | CreateFunction Language String [ParamDef] String String FnBody Volatility

 >                    language :: Language
 >                   ,name :: String
 >                   ,args :: [ParamDef]
 >                   ,retType :: String
 >                   ,body :: FnBody
                     quotetpye
 >                   ,volatility :: Volatility}

>                | CreateDomain String String (Maybe Expression)
>                | Assignment String Expression
>                | Return Expression
>                | Raise RaiseType String [Expression]
>                | NullStatement
>                | Perform Expression
>                | ForStatement String Statement [Statement]
>                | Copy String
>                | If Expression [Statement] (Maybe [Statement])
>                  deriving (Eq,Show)

================================================================================

Statement components

> data InsertData = InsertData [[Expression]] | InsertQuery Statement
>               deriving (Eq,Show)

> data FnBody = SqlFnBody [Statement] | PlpgsqlFnBody [VarDef] [Statement]
>               deriving (Eq,Show)

> data SetClause = SetClause String Expression
>                  deriving (Eq,Show)

> data Where = Where Expression
>                    deriving (Eq,Show)

> data From = From TableRef
>             deriving (Eq,Show)

> data TableRef = Tref String
>               | TrefAlias String String
>               | JoinedTref TableRef Bool JoinType TableRef (Maybe Expression)
>               | SubTref Statement String
>                 deriving (Eq,Show)

> data JoinType = Inner | LeftOuter| RightOuter | FullOuter | Cross
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
>               | ParamDefTp String
>                     deriving (Eq,Show)

> data VarDef = VarDef String String
>                     deriving (Eq,Show)

> data RaiseType = RNotice | RException | RError
>                  deriving (Eq, Show)

> data CombineType = Except | Union
>                    deriving (Eq, Show)

> data Volatility = Volatile | Stable | Immutable
>                   deriving (Eq, Show)

> data Language = Sql | Plpgsql
>                 deriving (Eq, Show)

================================================================================

Expressions

> data Op = Plus | Minus | Mult | Div | Pow | Mod | Eql
>         | And | Conc | Like | Not | IsNull | IsNotNull
>         | Cast | Qual | NotEql | Lt | Gt | Lte |Gte
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
>                         NotEql -> "<>"
>                         Lt -> "<"
>                         Gt -> ">"
>                         Lte -> "<="
>                         Gte -> ">="

> data Expression = BinaryOperatorCall Op Expression Expression
>                 | IntegerL Integer
>                 | StringL String
>                 | StringLD String String
>                 | NullL
>                 | PositionalArg Int
>                 | BooleanL Bool
>                 | Identifier String
>                 -- | QualifiedIdentifier String String
>                 | Row [Expression]
>                 | InPredicate String InList
>                 | FunctionCall String [Expression]
>                 | ScalarSubQuery Statement
>                 | ArrayL [Expression]
>                 | WindowFn Expression (Maybe [Expression]) (Maybe [Expression])
>                   -- windowfn selectitem partitionby orderby
>                 | Case [When] (Maybe Else)
>                 | Exists Statement
>                   deriving (Show,Eq)

> data InList = InList [Expression] | InSelect Statement
>               deriving (Show,Eq)

> data When = When Expression Expression
>                   deriving (Show,Eq)
> data Else = Else Expression
>                   deriving (Show,Eq)
