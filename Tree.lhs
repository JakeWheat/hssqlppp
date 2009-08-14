Copyright 2009 Jake Wheat

This file contains the data types for the sql parse tree nodes
No real thought or taste has gone into the decisions on how to structure
these.
They contain no source line/column references.
Lots of invalid sql is allowed.
The code currently uses this both to represent the parse tree for sql
and as the tree nodes which are used to generate sql using the pretty printer
don't know if the parse tree and the input to the pretty printer should use
the same data types, particularly for a language as funky as sql.

The parser will not parse as many parse trees that correspond to invalid
sql as the tree types can represent.

> module Tree where

================================================================================

SQL top level statements

everything is chucked in here, in particular this means that many places where
a select can appear inside another statement (e.g. a subselect), you can
instead put any statement - this type checks but is totally invalid.

> data Statement =
>                  -- selectlist from where orderby limit
>                  Select SelectList (Maybe From) (Maybe Where) (Maybe [Expression]) (Maybe Expression)
>                | CombineSelect CombineType Statement Statement
>                | SelectInto [String] Statement

>                | Insert String (Maybe [String]) InsertData
>                | Update String [SetClause] (Maybe Where)
>                | Delete String (Maybe Where)
>                | Copy String

>                | CreateTable String [AttributeDef]
>                | CreateView String Statement
>                | CreateType String [TypeAttributeDef]
>                  -- language name args rettype bodyquoteused body vol
>                | CreateFunction Language String [ParamDef] Expression String FnBody Volatility
>                  -- name type checkexpression
>                | CreateDomain String String (Maybe Expression)
>                | DropFunction String [String]
>                | Assignment String Expression
>                | Return (Maybe Expression)
>                | Raise RaiseType String [Expression]
>                | NullStatement
>                | Perform Expression
>                | Execute Expression
>                | ForStatement String Statement [Statement]
>                | If [(Expression, [Statement])] (Maybe [Statement])

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
>               | JoinedTref TableRef Bool JoinType TableRef (Maybe JoinExpression)
>               | SubTref Statement String
>               | TrefFun Expression
>               | TrefFunAlias Expression String
>                 deriving (Eq,Show)

> data JoinExpression = JoinOn Expression | JoinUsing [String]
>                       deriving (Eq,Show)

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

> data VarDef = VarDef String String (Maybe Expression)
>                     deriving (Eq,Show)

> data RaiseType = RNotice | RException | RError
>                  deriving (Eq, Show)

> data CombineType = Except | Union | Intersect
>                    deriving (Eq, Show)

> data Volatility = Volatile | Stable | Immutable
>                   deriving (Eq, Show)

> data Language = Sql | Plpgsql
>                 deriving (Eq, Show)

================================================================================

Expressions

> data BinOp = Plus | Minus | Mult | Div | Pow | Mod | Eql
>         | And | Or | Conc | Like 
>         | Cast | Qual | NotEql | Lt | Gt | Lte | Gte
>         | DistBetween
>           deriving (Show,Eq)

> data UnOp = Not | IsNull | IsNotNull
>             deriving (Show,Eq)

> binOpToSymbol :: BinOp -> String
> binOpToSymbol op = case op of
>                         Plus -> "+"
>                         Minus -> "-"
>                         Mult -> "*"
>                         Div -> "/"
>                         Pow -> "^"
>                         Mod -> "%"
>                         Eql -> "="
>                         And -> "and"
>                         Or -> "or"
>                         Conc -> "||"
>                         Like -> "like"
>                         Cast -> "::"
>                         Qual -> "."
>                         NotEql -> "<>"
>                         Lt -> "<"
>                         Gt -> ">"
>                         Lte -> "<="
>                         Gte -> ">="
>                         DistBetween -> "<->"

> unOpToSymbol :: UnOp -> String
> unOpToSymbol op = case op of
>                         Not -> "not"
>                         IsNull -> "is null"
>                         IsNotNull -> "is not null"


Similarly to the statement type, all expressions
are chucked into one even though there are many restrictions
on which expressions can appear in different places.

> data Expression =
>                   IntegerL Integer
>                 | StringL String
>                 | StringLD String String
>                 | NullL
>                 | BooleanL Bool
>                 | PositionalArg Int
>                 | Identifier String
>                 | Row [Expression]
>                 | ArrayL [Expression]
>                 | Case [When] (Maybe Else)
>                 | Exists Statement
>                 | BinOpCall BinOp Expression Expression
>                 | UnOpCall UnOp Expression
>                 | FunCall String [Expression]
>                 | InPredicate Expression InList
>                 | WindowFn Expression (Maybe [Expression]) (Maybe [Expression])
>                   -- windowfn selectitem partitionby orderby
>                 | ScalarSubQuery Statement
>                   deriving (Show,Eq)

> data InList = InList [Expression] | InSelect Statement
>               deriving (Show,Eq)

> data When = When Expression Expression
>                   deriving (Show,Eq)
> data Else = Else Expression
>                   deriving (Show,Eq)
