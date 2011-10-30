
Datatypes to represent postgres types and type errors. Below are some
notes on what the types are for and how they are used in postgres.

> {-# LANGUAGE FlexibleInstances,DeriveDataTypeable #-}
>
> module Database.HsSqlPpp.Internals.TypesInternal where
>
> import Control.Monad.Trans.Error
>
> import Data.Data
> import Data.Generics.Uniplate.Data
> import Data.Char

> -- | Standard types of things. This covers all the usual postgres types
> -- plus some extra ones added for use by the hssqlppp typechecker
> data Type = -- | basic type of a scalar value. These are either built in types
>             -- in postgres, or implemented in C or similar
>             ScalarType String
>           -- | postgres automatically creates an array type for every scalar type
>           -- (plus some other types?) If there is no array type for a type in the
>           -- catalog, then you can't work with arrays of that type
>           | ArrayType Type
>           -- | refer to composite type in catalog by name
>           | NamedCompositeType String
>           -- | refer to composite type generated on fly (?)
>           | CompositeType [(String,Type)]
>           -- | refer to anonymous composite type: the fields have no names
>           | AnonymousRecordType [Type]
>           -- | a domain type is used for a constraint on a table column which
>           -- would used on multiple columns on a table or in multiple tables,
>           -- using a domain type is a way of just writing the constraint once
>           | DomainType String
>           -- | enum type, not well support in hssqlppp yet?
>           | EnumType String
>           -- | postgres pseudo type is used for types which only appear
>           -- as argument or return types in function definitions and/or
>           -- are used only in plpgsql and not regular sql
>           | Pseudo PseudoType
>           -- | String literals in postgres have an unknown type. The effective
>           -- type is determined using what seems to amount to some simple ad hoc rules
>           -- based on the context of the string literal. Hssqlppp also uses the same process
>           -- for determining the effective types of ? placeholders in parameterized
>           -- statements
>           | UnknownType
>             deriving (Eq,Show,Ord,Typeable,Data)
>

> -- | Pseudo types: mainly used for the argument and return types of
> -- functions. The weird undocumented types are just used to represent
> -- functions with those types which are in the postgres default catalog
> data PseudoType =
>                   -- | used to represent polymorphic functions, all the
>                   -- AnyElement parameters and the return type if
>                   -- AnyElement must be the same type for a given function
>                   -- call invocation.
>                   AnyElement
>                   -- | like AnyElement, but the type must be an array type
>                 | AnyArray
>                   -- | like AnyElement, but the type must be an enum type
>                 | AnyEnum
>                   -- | like AnyElement, but the type must be a non array type
>                 | AnyNonArray
>                   -- | Any drops the restriction that all the Any* types must
>                   -- be the same type
>                 | Any
>                 -- | setof is used for set returning functions
>                 | SetOfType Type
>                 -- | pg record types (used only in plpgsql) can be assigned to from all
>                 -- three record types - the type in the maybe should only ever
>                 -- be namedcomposite, composite or anonymousrecord
>                 | PgRecord (Maybe Type)
>                 -- | cstring - a C string
>                 | Cstring
>                 -- | record - used for postgres record types, used in hssqlppp
>                 -- in the function catalog only
>                 | Record
>                 -- | presumably used for the types of OLD and NEW in a trigger
>                 -- function. Hssqlppp will probably use the PgRecord type above
>                 -- for these.
>                 | TriggerRecord
>                 | Trigger

>                 -- | represents the return type of a function which doesn't return
>                 -- anything. Not sure if it is used anywhere else
>                 | Void
>                 | Internal
>                 | LanguageHandler
>                 | Opaque
>                 | FdwHandler
>                   deriving (Eq,Show,Ord,Typeable,Data)


The possible type errors. This is a bit unorganised, at some point if
better error messages are wanted, then a lot more information could be
added.

> data TypeError = {-WrongTypes Type [Type]
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
>                | BadStarExpand
>                | AmbiguousIdentifier String
>                | ContextError String
>                | MissingJoinAttribute
>                | ExpressionMustBeBool
>                | WrongNumberOfColumns
>                | ExpectedDomainType Type
>                | BadCatalogUpdate String
>                | TypeAlreadyExists Type
>                | AnyAllError String
>                | InternalError String
>                | FromToTypesNotSame Type Type
>                | WrongNumberOfAliasCols Int Int

type conversion errors

>                | -} NoMatchingOperator String [Type]
>                | TypelessEmptyArray
>                | IncompatibleTypeSet [Type]
>                | IncompatibleTypes Type Type
>                | WrongNumberOfColumns


old catalog type errors: to be replaced when the catalog code is
gutted and rewritten

>                | TypeAlreadyExists Type
>                | BadCatalogUpdate String
>                | UnrecognisedRelation String
>                | DomainDefNotFound Type
>                | TypeNotKnown Type
>                | UnknownTypeName String


>                | InternalError String
>                 --shoved in to humour the Either Monad
>                | MiscError String
>                  deriving (Eq,Show,Ord,Typeable,Data)
>

> instance ErrorList TypeError where
>    listMsg s = [MiscError s]


> -- | Using these gives the hssqlppp canonical names of these
> -- types, which have multiple names in postgres and SQL. The actual names follow
> -- what postgres uses in a dump.
> typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4,
>   typeFloat8,typeVarChar,typeChar,typeBool,typeDate,
>   typeInterval :: Type
> typeSmallInt = ScalarType "int2"
> typeBigInt = ScalarType "int8"
> typeInt = ScalarType "int4"
> typeNumeric = ScalarType "numeric"
> typeFloat4 = ScalarType "float4"
> typeFloat8 = ScalarType "float8"
> typeVarChar = ScalarType "varchar"
> typeChar = ScalarType "char"
> typeBool = ScalarType "bool"
> typeDate = ScalarType "date"
> typeInterval = ScalarType "interval"

> -- | convert the name of a type to its canonical name. For types
> -- without multiple names, it returns the name unchanged
> canonicalizeTypeName :: String -> String
> canonicalizeTypeName s' =
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
>       intNames = ["int4", "integer", "int", "serial"]
>       bigIntNames = ["int8", "bigint"]
>       numericNames = ["numeric", "decimal"]
>       float4Names = ["real", "float4"]
>       float8Names = ["double precision", "float"]
>       varcharNames = ["character varying", "varchar"]
>       charNames = ["character", "char"]
>       boolNames = ["boolean", "bool"]
>       s = map toLower s'

> -- | run canonicalizeTypeName on all the typenames in an ast
> canonicalizeTypeNames :: Data a => a -> a
> canonicalizeTypeNames =
>   transformBi $ \x ->
>       case x of
>         ScalarType s -> ScalarType $ canonicalizeTypeName s
>         x' -> x'


TODO:

what about PrecTypeName - called modifiers in pg. No idea how these
should work in the typechecker. I think the modifier values might be
completely dynamic in postgres.

array types have to match an exact array type in the catalog, so we
can't create an arbitrary array of any type. Not sure if this is
handled quite correctly in this code. Not sure if you would ever
create or drop an array type manually.
