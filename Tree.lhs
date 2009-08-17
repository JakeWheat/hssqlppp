Copyright 2009 Jake Wheat

This file contains the data types for the sql parse tree nodes
No real thought or taste has gone into the decisions on how to structure
these.
They contain no source line/column references.
Lots of invalid sql is allowed.
The code currently uses this both to represent the parse tree for sql
and as the tree nodes which are used to generate sql using the pretty printer.
Don't know if the parse tree and the input to the pretty printer should use
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

queries

>                  -- distinct? selectlist from where
>                  Select Distinct SelectList (Maybe TableRef) (Maybe Expression)
>                             --groupby orderby
>                             [Expression] [Expression]
>                             --orderby direction limit offset
>                             Direction (Maybe Expression) (Maybe Expression)
>                | CombineSelect CombineType Statement Statement
>                | Values [[Expression]]

dml

>                  --table targetcolumns insertdata(values or select statement) returning
>                | Insert String [String] Statement (Maybe SelectList)
>                  --tablename setitems where returning
>                | Update String [SetClause] (Maybe Expression) (Maybe SelectList)
>                  --tablename, where, returning
>                | Delete String (Maybe Expression) (Maybe SelectList)
>                | Copy String
>                | Truncate [String] RestartIdentity Cascade

ddl

>                | CreateTable String [AttributeDef] [Constraint]
>                | CreateTableAs String Statement
>                | CreateView String Statement
>                | CreateType String [TypeAttributeDef]
>                  -- language name args rettype bodyquoteused body vol
>                | CreateFunction Language String [ParamDef] TypeName String FnBody Volatility
>                  -- name type checkexpression
>                | CreateDomain String String (Maybe Expression)

>                  -- ifexists (name,argtypes)* cascadeorrestrict
>                | DropFunction IfExists [(String,[String])] Cascade
>                  -- ifexists names cascadeorrestrict
>                | DropSomething DropType IfExists [String] Cascade

>                | Assignment String Expression
>                | Return (Maybe Expression)
>                | ReturnNext Expression
>                | ReturnQuery Statement
>                | Raise RaiseType String [Expression]
>                | NullStatement
>                | Perform Expression
>                | Execute Expression
>                | ExecuteInto Expression [String]
>                | ForSelectStatement String Statement [Statement]
>                | ForIntegerStatement String Expression Expression [Statement]
>                | WhileStatement Expression [Statement]
>                | ContinueStatement
>                  --variable, list of when parts, else part
>                | CaseStatement Expression [(Expression,[Statement])] [Statement]
>                  --list is
>                  --first if (condition, statements):elseifs(condition, statements)
>                  --last bit is else statements
>                | If [(Expression, [Statement])] [Statement]

>                  deriving (Eq,Show)

================================================================================

Statement components

> data FnBody = SqlFnBody [Statement] | PlpgsqlFnBody [VarDef] [Statement]
>               deriving (Eq,Show)

> data SetClause = SetClause String Expression | RowSetClause [String] [Expression]
>                  deriving (Eq,Show)

> data TableRef = Tref String
>               | TrefAlias String String
>               | JoinedTref TableRef Natural JoinType TableRef (Maybe JoinExpression)
>               | SubTref Statement String
>               | TrefFun Expression
>               | TrefFunAlias Expression String
>                 deriving (Eq,Show)

> data JoinExpression = JoinOn Expression | JoinUsing [String]
>                       deriving (Eq,Show)

> data JoinType = Inner | LeftOuter| RightOuter | FullOuter | Cross
>                 deriving (Eq,Show)

select columns, into columns

> data SelectList = SelectList [SelectItem] [String]
>                   deriving (Eq,Show)

> data SelectItem = SelExp Expression
>                 | SelectItem Expression String
>                   deriving (Eq,Show)

name type default null constraint

> data AttributeDef = AttributeDef String String (Maybe Expression) [RowConstraint]
>                     deriving (Eq,Show)

Constraints which appear attached to an individual field

> data RowConstraint = NullConstraint
>                    | NotNullConstraint
>                    | RowCheckConstraint Expression
>                    | RowUniqueConstraint
>                    | RowPrimaryKeyConstraint
>                    | RowReferenceConstraint String [String] Cascade Cascade
>                       deriving (Eq,Show)

constraints which appear on a separate row in the create table

> data Constraint = UniqueConstraint [String]
>                 | PrimaryKeyConstraint [String]
>                 | CheckConstraint Expression
>                   -- sourcecols targettable targetcols ondelete onupdate
>                 | ReferenceConstraint [String] String [String] Cascade Cascade
>                   deriving (Eq,Show)

> data TypeAttributeDef = TypeAttDef String String
>                         deriving (Eq,Show)

> data ParamDef = ParamDef String TypeName
>               | ParamDefTp TypeName
>                     deriving (Eq,Show)

> data VarDef = VarDef String TypeName (Maybe Expression)
>                     deriving (Eq,Show)

> data RaiseType = RNotice | RException | RError
>                  deriving (Eq, Show)

> data CombineType = Except | Union | Intersect | UnionAll
>                    deriving (Eq, Show)

> data Volatility = Volatile | Stable | Immutable
>                   deriving (Eq, Show)

> data Language = Sql | Plpgsql
>                 deriving (Eq, Show)

> data TypeName = SimpleType String
>               | PrecType String Integer
>               | ArrayType TypeName
>               | SetOfType TypeName
>                 deriving (Eq, Show)

> data DropType = Table
>          | Domain
>          | View
>          | Type
>            deriving (Eq, Show)

> data Cascade = Cascade | Restrict
>                 deriving (Eq, Show)

> data Direction = Asc | Desc
>                 deriving (Eq, Show)

> data Distinct = Distinct | Dupes
>                 deriving (Eq, Show)

> data Natural = Natural | Unnatural
>                 deriving (Eq, Show)

> data IfExists = Require | IfExists
>                 deriving (Eq, Show)

> data RestartIdentity = RestartIdentity | ContinueIdentity
>                 deriving (Eq, Show)


================================================================================

Expressions

> data BinOp = Plus | Minus | Mult | Div | Pow | Mod | Eql
>         | And | Or | Conc | Like
>         | Cast | NotEql | Lt | Gt | Lte | Gte
>         | DistBetween
>           deriving (Show,Eq)

> data UnOp = Not | IsNull | IsNotNull | SetOf | Abs | Neg
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
>                         SetOf -> "setof"
>                         Abs -> "@"
>                         Neg -> "-"


Similarly to the statement type, all expressions
are chucked into one even though there are many restrictions
on which expressions can appear in different places.

> data Expression =
>                   IntegerL Integer
>                 | FloatL Double
>                 | StringL String
>                 | StringLD String String
>                 | NullL
>                 | BooleanL Bool
>                 | PositionalArg Int
>                 | CastKeyword Expression TypeName
>                   -- sourcestring start length
>                 | Substring Expression Expression Expression
>                 | Identifier String
>                 | Row [Expression]
>                 | ArrayL [Expression]
>                 | Case [(Expression,Expression)] (Maybe Expression)
>                 | Exists Statement
>                 | BinOpCall BinOp Expression Expression
>                 | UnOpCall UnOp Expression
>                 | FunCall String [Expression]
>                 | InPredicate Expression Bool InList
>                   -- windowfn selectitem partitionby orderby orderbyasc?
>                 | WindowFn Expression [Expression] [Expression] Direction
>                 | ScalarSubQuery Statement
>                 | ArraySub Expression [Expression]
>                 | Between Expression Expression Expression
>                   deriving (Show,Eq)

> data InList = InList [Expression] | InSelect Statement
>               deriving (Show,Eq)
