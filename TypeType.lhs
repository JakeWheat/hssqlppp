Copyright 2009 Jake Wheat

This file contains the data type Type. It is kept separate so we can
compile the types and function information from the database
separately to Ast.ag.

Types overview:
Regular types: scalarType, arrayType, composite type, domaintype
we can used these anywhere

semi first class types: row, unknownstringlit (called unknown in pg) -
these can be used in some places, but not others, in particular an
expression can have this type or select can have a row with these
type, but a view can't have a column with this type (so a select can
be valid on it's own but not as a view. Not sure if Row types can be
variables, unknownstringlit definitely can't

pseudo types - mirror pg pseudo types
Internal, LanguageHandler, Opaque probably won't ever be properly supported
Not sure exactly what Any is, can't use it in functions like the other any types.
Cstring - treated as variant of text?
record -dynamically typed, depends where it is used, i think is used
for type inference in function return types, and as a dynamically
typed variable in functions, where it can take any composite typed
value.
void is used for functions which return nothing
trigger is a tag to say a function is used in triggers, used as a
return type only

typelist is an internal type used during type checking
typeerror represents something which has failed to typecheck
unknowntype is used to represent the type of anything which the code
is currently unable to type check, this should dissappear at some
point, or at least become unused

The Type type identifies the type of a node, but doesn't neccessarily
describe it.
Arraytype, setoftype and row are treated as type generators, and are
unnamed so have structural equality.

Typing relational valued expressions:
use SetOfType combined with composite type for now, see if it works out.
Will also need to support unnamed composite types
TYPE MySourcePos = (String, Int, Int)

> module TypeType where

> type MySourcePos = (String, Int, Int)

> data Type = ScalarType String
>           | ArrayType Type
>           | SetOfType Type
>           | CompositeType String
>           | UnnamedCompositeType [(String,Type)]
>           | DomainType String
>           | EnumType String
>           | Row [Type]
>           -- type list is used internally in the type checking.
>           | TypeList [Type]
>           | Pseudo PseudoType
>           | TypeError MySourcePos TypeErrorInfo
>           | UnknownType -- represents something which the type checker
>                         -- doesn't know how to type check
>           | UnknownStringLit -- represents a string literal
>                              -- token whose type isn't yet
>                              -- determined
>             deriving (Eq,Show)

> data PseudoType = Any
>                 | AnyArray
>                 | AnyElement
>                 | AnyEnum
>                 | AnyNonArray
>                 | Cstring
>                 | Record
>                 | Trigger
>                 | Void
>                 | Internal
>                 | LanguageHandler
>                 | Opaque
>                   deriving (Eq,Show)

this list will need reviewing, probably refactor to a completely
different set of infos, also will want to add more information to
these, and to provide a way of converting into a user friendly
string. It is intended for this code to produce highly useful errors
later on down the line.

>                    -- mostly expected,got
> data TypeErrorInfo = WrongTypes Type [Type]
>                    | WrongTypeList [Type] [Type]
>                    | WrongNumArgs Int Int
>                    | WrongType Type Type
>                    | NotArrayType Type
>                    | NeedOneOrMoreArgs
>                    | OtherTypeError String
>                    | UnknownTypeError Type
>                    | OperatorNeeds1Or2Args Int
>                    | NoMatchingOperator String [Type]
>                    | MultipleMatchingOperators String [Type]
>                    | NoMatchingKOperator KeywordOperator [Type]
>                    | MultipleMatchingKOperators KeywordOperator [Type]
>                    | TypelessEmptyArray
>                    | IncompatibleTypes [Type]
>                    | ValuesListsMustBeSameLength
>                    | NoRowsGivenForValues
>                    | UnrecognisedIdentifier String
>                    | UnrecognisedRelation String
>                    | ContextError String
>                    | MissingJoinAttribute
>                      deriving (Eq,Show)

> typesFromTypeList :: Type -> [Type]
> typesFromTypeList (TypeList ts) = ts
> typesFromTypeList x = error $ "can't get types from list " ++ show x

> typeFromArray :: Type -> Type
> typeFromArray (ArrayType t) = t
> typeFromArray x = error $ "can't get types from non array " ++ show x

> isArrayType :: Type -> Bool
> isArrayType (ArrayType _) = True
> isArrayType (Pseudo AnyArray) = True
> isArrayType _ = False

> data KeywordOperator = And | Or | Not
>                      | IsNull | IsNotNull
>                      | Like
>                        deriving (Eq,Show)
>                       -- add is distinct from, ilike, etc

> data CastContext = ImplicitCastContext
>                  | AssignmentCastContext
>                  | ExplicitCastContext
>                    deriving (Eq,Show)

> data CompositeFlavour = Composite | TableComposite | ViewComposite
>                         deriving (Eq,Show)

> type CompositeDef = (String, CompositeFlavour, Type)
