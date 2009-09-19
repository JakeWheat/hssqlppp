Copyright 2009 Jake Wheat

This file contains the data type Type. It is kept separate so we can
compile the types and function information from the database
separately to AstInternal.ag.

Types overview:

Regular types: scalarType, arrayType, composite type, domaintype
we can use these anywhere.

semi first class types: row, unknownstringlit (called unknown in pg) -
these can be used in some places, but not others, in particular an
expression can have this type or select can have a row with these
type, but a view can't have a column with this type (so a select can
be valid on it's own but not as a view. Not sure if Row types can be
variables, unknownstringlit definitely can't. (Update, seems a view
can have a column with type unknown.)

pseudo types - mirror pg pseudo types
Internal, LanguageHandler, Opaque probably won't ever be properly supported
Not sure exactly what Any is, can't use it in functions like the other any types?
 - update: seems that Any is like AnyElement, but multiple Anys don't
   have to have the same type.
AnyElement, AnyArray, AnyEnum, AnyNonArray - used to implement polymorphic functions,
  can only be used in these functions as params, return type (and local vars?).
Cstring - treated as variant of text?
record -dynamically typed, depends where it is used, i think is used
for type inference in function return types (so you don't have to
write the correct type, the compiler fills it in), and as a dynamically
typed variable in functions, where it can take any composite typed
value.
void is used for functions which return nothing
trigger is a tag to say a function is used in triggers, used as a
return type only
typecheckfailed is used to represent the type of anything which the code
is currently unable to type check, this should disappear at some
point

The Type type identifies the type of a node, but doesn't necessarily
describe the type.
Arraytype, setoftype and row are treated as type generators, and are
unnamed so have structural equality. Other types sometimes effectively
have structural equality depending on the context...

Typing relational valued expressions:
use SetOfType combined with composite type for now, see if it works
out. If not, will have to add another type.

> {-# OPTIONS_HADDOCK hide #-}

 > {-# LANGUAGE DeriveDataTypeable #-}

> module Database.HsSqlPpp.TypeChecking.TypeType where

 > import Data.Generics
 > import Data.Binary

> data Type = ScalarType String
>           | ArrayType Type
>           | SetOfType Type
>           | CompositeType String
>           | UnnamedCompositeType [(String,Type)]
>           | DomainType String
>           | EnumType String
>           | RowCtor [Type]
>           | Pseudo PseudoType
>           | TypeCheckFailed -- represents something which the type checker
>                             -- doesn't know how to type check
>                             -- or it cannot work the type out because of errors
>           | UnknownStringLit -- represents a string literal
>                              -- token whose type isn't yet
>                              -- determined
>             deriving (Eq,Show {-,Typeable,Data-})

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
>                   deriving (Eq,Show {-,Typeable,Data-})

this list will need reviewing, probably refactor to a completely
different set of infos, also will want to add more information to
these, and to provide a way of converting into a user friendly
string. It is intended for this code to produce highly useful errors
later on down the line.

>                    -- mostly expected,got
> data TypeError = WrongTypes Type [Type]
>                | UnknownTypeError Type
>                | UnknownTypeName String
>                | NoMatchingOperator String [Type]
>                | TypelessEmptyArray
>                | IncompatibleTypeSet [Type]
>                | IncompatibleTypes Type Type
>                | ValuesListsMustBeSameLength
>                | NoRowsGivenForValues
>                | UnrecognisedIdentifier String
>                | UnrecognisedRelation String
>                | UnrecognisedCorrelationName String
>                | AmbiguousIdentifier String
>                | ContextError String
>                | MissingJoinAttribute
>                | ExpressionMustBeBool
>                | WrongNumberOfColumns
>                 --shoved in to humour the Either Monad
>                | MiscError String
>                  deriving (Eq,Show)

some random stuff needed here because of compilation orders

cast context - used in the casts catalog

> data CastContext = ImplicitCastContext
>                  | AssignmentCastContext
>                  | ExplicitCastContext
>                    deriving (Eq,Show {-,Typeable,Data-})

used in the attribute catalog which holds the attribute names and
types of tables, views and other composite types.

> data CompositeFlavour = Composite | TableComposite | ViewComposite
>                         deriving (Eq,Show {-,Typeable,Data-})

type is always an UnnamedCompositeType:

> type CompositeDef = (String, CompositeFlavour, Type)

> isOperator :: String -> Bool
> isOperator = any (`elem` "+-*/<>=~!@#%^&|`?")

> type FunctionPrototype = (String, [Type], Type)
> type DomainDefinition = (Type,Type)

> {-
> instance Binary Database.HsSqlPpp.TypeChecking.TypeType.CompositeFlavour where
>   put Composite = putWord8 0
>   put TableComposite = putWord8 1
>   put ViewComposite = putWord8 2
>   get = do
>     tag_ <- getWord8
>     case tag_ of
>       0 -> return Composite
>       1 -> return TableComposite
>       2 -> return ViewComposite
>       _ -> fail "no parse"

> instance Binary Database.HsSqlPpp.TypeChecking.TypeType.CastContext where
>   put ImplicitCastContext = putWord8 0
>   put AssignmentCastContext = putWord8 1
>   put ExplicitCastContext = putWord8 2
>   get = do
>     tag_ <- getWord8
>     case tag_ of
>       0 -> return ImplicitCastContext
>       1 -> return AssignmentCastContext
>       2 -> return ExplicitCastContext
>       _ -> fail "no parse"

> instance Binary Database.HsSqlPpp.TypeChecking.TypeType.Type where
>   put (ScalarType a) = putWord8 0 >> put a
>   put (ArrayType a) = putWord8 1 >> put a
>   put (SetOfType a) = putWord8 2 >> put a
>   put (CompositeType a) = putWord8 3 >> put a
>   put (UnnamedCompositeType a) = putWord8 4 >> put a
>   put (DomainType a) = putWord8 5 >> put a
>   put (EnumType a) = putWord8 6 >> put a
>   put (RowCtor a) = putWord8 7 >> put a
>   put (Pseudo a) = putWord8 8 >> put a
>   put TypeCheckFailed = putWord8 9
>   put UnknownStringLit = putWord8 10
>   get = do
>     tag_ <- getWord8
>     case tag_ of
>       0 -> get >>= \a -> return (ScalarType a)
>       1 -> get >>= \a -> return (ArrayType a)
>       2 -> get >>= \a -> return (SetOfType a)
>       3 -> get >>= \a -> return (CompositeType a)
>       4 -> get >>= \a -> return (UnnamedCompositeType a)
>       5 -> get >>= \a -> return (DomainType a)
>       6 -> get >>= \a -> return (EnumType a)
>       7 -> get >>= \a -> return (RowCtor a)
>       8 -> get >>= \a -> return (Pseudo a)
>       9 -> return TypeCheckFailed
>       10 -> return UnknownStringLit
>       _ -> fail "no parse"

> instance Binary Database.HsSqlPpp.TypeChecking.TypeType.PseudoType where
>   put Any = putWord8 0
>   put AnyArray = putWord8 1
>   put AnyElement = putWord8 2
>   put AnyEnum = putWord8 3
>   put AnyNonArray = putWord8 4
>   put Cstring = putWord8 5
>   put Record = putWord8 6
>   put Trigger = putWord8 7
>   put Void = putWord8 8
>   put Internal = putWord8 9
>   put LanguageHandler = putWord8 10
>   put Opaque = putWord8 11
>   get = do
>     tag_ <- getWord8
>     case tag_ of
>       0 -> return Any
>       1 -> return AnyArray
>       2 -> return AnyElement
>       3 -> return AnyEnum
>       4 -> return AnyNonArray
>       5 -> return Cstring
>       6 -> return Record
>       7 -> return Trigger
>       8 -> return Void
>       9 -> return Internal
>       10 -> return LanguageHandler
>       11 -> return Opaque
>       _ -> fail "no parse"
>-}