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

> {-# LANGUAGE FlexibleInstances,DeriveDataTypeable #-}


> module Database.HsSqlPpp.AstInternals.TypeType where

> import Control.Monad.Error

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
>             deriving (Eq,Show,Typeable,Data)

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
>                   deriving (Eq,Show,Typeable,Data)

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
>                | ExpectedDomainType Type
>                | DomainDefNotFound Type
>                | BadEnvironmentUpdate String
>                | TypeAlreadyExists Type
>                | InternalError String
>                 --shoved in to humour the Either Monad
>                | MiscError String
>                  deriving (Eq,Show,Typeable,Data)

> instance Error ([TypeError]) where
>   noMsg = [MiscError "Unknown error"]
>   strMsg str = [MiscError str]

=== canonical type name support

Introduce some aliases to protect client code if/when the ast
canonical names are changed:

> typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4,
>   typeFloat8,typeVarChar,typeChar,typeBool :: Type
> typeSmallInt = ScalarType "int2"
> typeBigInt = ScalarType "int8"
> typeInt = ScalarType "int4"
> typeNumeric = ScalarType "numeric"
> typeFloat4 = ScalarType "float4"
> typeFloat8 = ScalarType "float8"
> typeVarChar = ScalarType "varchar"
> typeChar = ScalarType "char"
> typeBool = ScalarType "bool"

this converts the name of a type to its canonical name

> canonicalizeTypeName :: String -> String
> canonicalizeTypeName s =
>   case () of
>                   _ | s `elem` smallIntNames -> "int2"
>                     | s `elem` intNames -> "int4"
>                     | s `elem` bigIntNames -> "int8"
>                     | s `elem` numericNames -> "numeric"
>                     | s `elem` float4Names -> "float4"
>                     | s `elem` float8Names -> "float8"
>                     | s `elem` varcharNames -> "varchar"
>                     | s `elem` charNames -> "char"
>                     | s `elem` boolNames -> "bool"
>                     | otherwise -> s
>   where
>       smallIntNames = ["int2", "smallint"]
>       intNames = ["int4", "integer", "int"]
>       bigIntNames = ["int8", "bigint"]
>       numericNames = ["numeric", "decimal"]
>       float4Names = ["real", "float4"]
>       float8Names = ["double precision", "float"]
>       varcharNames = ["character varying", "varchar"]
>       charNames = ["character", "char"]
>       boolNames = ["boolean", "bool"]

===============================================================================

= basic types

random notes on pg types:

== domains:
the point of domains is you can't put constraints on types, but you
can wrap a type up in a domain and put a constraint on it there.

== literals/selectors

source strings are parsed as unknown type: they can be implicitly cast
to almost any type in the right context.

rows ctors can also be implicitly cast to any composite type matching
the elements (now sure how exactly are they matched? - number of
elements, type compatibility of elements, just by context?).

string literals are not checked for valid syntax currently, but this
will probably change so we can type check string literals statically.
Postgres defers all checking to runtime, because it has to cope with
custom data types. This code will allow adding a grammar checker for
each type so you can optionally check the string lits statically.

== notes on type checking types

=== basic type checking
Currently can lookup type names against a default template1 list of
types, or against the current list in a database (which is read before
processing and sql code).

todo: collect type names from current source file to check against
A lot of the infrastructure to do this is already in place. We also
need to do this for all other definitions, etc.

=== Type aliases

Some types in postgresql have multiple names. I think this is
hardcoded in the pg parser.

For the canonical name in this code, we use the name given in the
postgresql pg_type catalog relvar.

TODO: Change the ast canonical names: where there is a choice, prefer
the sql standard name, where there are multiple sql standard names,
choose the most concise or common one, so the ast will use different
canonical names to postgresql.

planned ast canonical names:
numbers:
int2, int4/integer, int8 -> smallint, int, bigint
numeric, decimal -> numeric
float(1) to float(24), real -> float(24)
float, float(25) to float(53), double precision -> float
serial, serial4 -> int
bigserial, serial8 -> bigint
character varying(n), varchar(n)-> varchar(n)
character(n), char(n) -> char(n)

TODO:

what about PrecTypeName? - need to fix the ast and parser (found out
these are called type modifiers in pg)

also, what can setof be applied to - don't know if it can apply to an
array or setof type

array types have to match an exact array type in the catalog, so we
can't create an arbitrary array of any type. Not sure if this is
handled quite correctly in this code.

================================================================================

utilities for working with Types

> isArrayType :: Type -> Bool
> isArrayType (ArrayType _) = True
> isArrayType _ = False

> unwrapArray :: Type -> Either [TypeError] Type
> unwrapArray (ArrayType t) = Right t
> unwrapArray x = Left $ [InternalError $ "can't get types from non array " ++ show x]

> unwrapSetOfWhenComposite :: Type -> Either [TypeError] Type
> unwrapSetOfWhenComposite (SetOfType a@(UnnamedCompositeType _)) = Right a
> unwrapSetOfWhenComposite x = Left $ [InternalError $ "tried to unwrapSetOfWhenComposite on " ++ show x]

> unwrapSetOfComposite :: Type -> Either [TypeError]  [(String,Type)]
> unwrapSetOfComposite (SetOfType (UnnamedCompositeType a)) = Right a
> unwrapSetOfComposite x = Left $ [InternalError $ "tried to unwrapSetOfComposite on " ++ show x]


> unwrapSetOf :: Type -> Either [TypeError] Type
> unwrapSetOf (SetOfType a) = Right a
> unwrapSetOf x = Left $ [InternalError $ "tried to unwrapSetOf on " ++ show x]

> unwrapComposite :: Type -> Either [TypeError] [(String,Type)]
> unwrapComposite (UnnamedCompositeType a) = Right a
> unwrapComposite x = Left $ [InternalError $ "cannot unwrapComposite on " ++ show x]

> consComposite :: (String,Type) -> Type -> Either [TypeError] Type
> consComposite l (UnnamedCompositeType a) = Right $ UnnamedCompositeType (l:a)
> consComposite a b = Left $ [InternalError $ "called consComposite on " ++ show (a,b)]

> unwrapRowCtor :: Type -> Either [TypeError] [Type]
> unwrapRowCtor (RowCtor a) = Right a
> unwrapRowCtor x = Left $ [InternalError $ "cannot unwrapRowCtor on " ++ show x]



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
