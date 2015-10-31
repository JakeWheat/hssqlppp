
Datatypes to represent postgres types and type errors. Below are some
notes on what the types are for and how they are used in postgres.

> {-# LANGUAGE FlexibleInstances,DeriveDataTypeable,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Internals.TypesInternal where
>
> import Data.Data
> --import Data.Generics.Uniplate.Data
> import Data.Char
> import Data.Text (Text)
> import qualified Data.Text as T
> import Database.HsSqlPpp.Internals.Dialect
> import Data.List
> --import Debug.Trace

> --import Control.Monad.Error
> --import Control.Monad.Except
> --import Control.Monad.Trans.Except

where should precision and nullability go?


TODO:
rename UnknownType to ScalarType "!unknown" ? or something else?
do we need separate entries for domain and enums?
think of a better way to implement array types - get rid of the
  special casing which is based on a postgresql implementation detail
get rid of pseudo types, maybe use ScalarType or SpecialType String
  which is dialect specific?

Maybe there should be a concept of a typeid, and a typedescription?
So the typeid is used everywhere and is just a string or something,
and the typedescription is got from the catalog/environment when
needed in the typechecking?


> -- | Standard types of things. This covers all the usual postgres types
> -- plus some extra ones added for use by the hssqlppp typechecker
> data Type = -- | basic type of a scalar value. These are either built in types
>             -- in postgres, or implemented in C or similar
>             ScalarType Text
>           -- | a domain type is used for a constraint on a table column which
>           -- would used on multiple columns on a table or in multiple tables,
>           -- using a domain type is a way of just writing the constraint once
>           | DomainType Text
>           -- | enum type, not really supported in hssqlppp yet
>           | EnumType Text
>           -- | String literals in postgres have an unknown type. The effective
>           -- type is determined using what seems to amount to some simple ad hoc rules
>           -- based on the context of the string literal. Hssqlppp also treats
>           -- ? placeholders and nulls the same way, so they have UnknownType,
>           -- not sure how closely this matches postgres
>           | UnknownType
>           -- | postgres automatically creates an array type for every scalar type
>           -- (plus some other types?) If there is no array type for a type in the
>           -- catalog, then you can't work with arrays of that type
>           | ArrayType Type
>           -- | refer to composite type in catalog by name. not sure if this needs
>           -- to exist along with CompositeType
>           | NamedCompositeType Text
>           -- | refer to composite type by structure
>           | CompositeType [(Text,TypeExtra)]
>           -- | CompositeTypeExtra [(Text,TypeExtra)]
>           -- | hack to support the environment for a tref
>           | TrefType [((Text,Text),TypeExtra)]
>           -- | the fields are anonymous as well as the type itself
>           | AnonymousCompositeType [Type]
>           -- | The pseudo type is used for types which only appear
>           -- as argument or return types in function definitions and/or
>           -- are used only in plpgsql and not regular sql. hssqlppp also
>           -- follows this usage for the types used in hssqlppp which don't
>           -- have an exact counterpart in postgres
>           | Pseudo PseudoType
>             deriving (Eq,Show,Ord,Typeable,Data)
>

> -- | Quick fix to add precision and nullable information to the
> -- annotation types. This approach should be revisited, maybe this
> -- information should be in the Type type?
> data TypeExtra = TypeExtra {teType :: Type
>                            ,tePrecision :: Maybe Int
>                            ,teScale :: Maybe Int
>                            ,teNullable :: Bool}
>                deriving (Eq,Ord,Show,Typeable,Data)
> mkTypeExtra :: Type -> TypeExtra
> mkTypeExtra t = TypeExtra t Nothing Nothing True
> mkTypeExtraNN :: Type -> TypeExtra
> mkTypeExtraNN t = TypeExtra t Nothing Nothing False
> mkNullable:: TypeExtra -> TypeExtra
> mkNullable te = te{teNullable=True}

> -- | Pseudo types: mainly used for the argument and return types of
> -- functions. The weird undocumented types are just used to represent
> -- functions with those types which are in the postgres default catalog
> data PseudoType =
>                 -- | setof is used for set returning functions
>                   SetOfType Type
>                   -- | used to represent polymorphic functions, all the
>                   -- AnyElement parameters and the return type if
>                   -- AnyElement must be the same type for a given function
>                   -- call invocation.
>                 | AnyElement
>                   -- | like AnyElement, but the type must be an array type
>                 | AnyArray
>                   -- | like AnyElement, but the type must be an enum type
>                 | AnyEnum
>                   -- | like AnyElement, but the type must be a non array type
>                 | AnyNonArray
>                 | AnyRange
>                   -- | Any drops the restriction that all the Any types must
>                   -- be the same type
>                 | Any
>                 -- | record types are used in plpgsql for a sort of dynamic
>                 -- typing or rough polymorphism substitute. They can refer to
>                 -- values of named composite type, composite type or
>                 -- anonymous composite type, not sure if non composite types as well.
>                 | Record (Maybe Type)
>                 -- | presumably used for the types of OLD and NEW in a trigger
>                 -- function. Hssqlppp will probably use the Record type above
>                 -- for these.
>                 | TriggerRecord
>                 | Trigger
>                 -- | cstring - a C string
>                 | Cstring
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

> data TypeError = {-
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

>                | -} NoMatchingOperator Text [Type]
>                | AmbiguousOperator Text [Type]
>                | TypelessEmptyArray
>                | IncompatibleTypeSet [Type]
>                | IncompatibleTypes Type Type
>                | WrongNumberOfColumns
>                | WrongTypes Type [Type]
>                | IncompatibleUnionTypes Type Type


old catalog type errors: to be replaced when the catalog code is
gutted and rewritten

>                | TypeAlreadyExists Type
>                | SchemaAlreadyExists Text
>                | BadCatalogUpdate Text
>                | UnrecognisedRelation (Text,Text)
>                | DomainDefNotFound Type
>                | TypeNotKnown Type
>                | UnknownTypeName Text
>                | UnrecognisedIdentifier Text
>                | UnrecognisedCorrelationName Text
>                | BadStarExpand
>                | InternalError String
>                | AmbiguousIdentifier Text
>                | OdbcFuncBadContent
>                | DuplicateColumnName Text
>                | TooManyColumnsInInsert
>                  deriving (Eq,Show,Ord,Typeable,Data)
>
> --instance ErrorList TypeError where
> --  listMsg s = [InternalError s]

TODO: remove these aliases

> -- | Using these gives the hssqlppp canonical names of these
> -- types, which have multiple names in postgres and SQL. The names which
> -- hssqlppp uses as canonical are the names that postgres uses in a pg_dump.
> typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4,
>   typeFloat8,typeVarChar,typeChar,typeBool,typeDate,
>   typeTime,typeTimestamp, typeInterval :: Type
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
> typeTime = ScalarType "time"
> typeTimestamp = ScalarType "timestamp"
> typeInterval = ScalarType "interval"

> {-traceit :: Show a => String -> a -> a
> traceit m a = trace (m ++ ": " ++ show a) a-}


Canonicalize type names

When you typecheck a tree or use the catalog internal functions or
many other things, you must use the canonical type names and you have
to convert them yourself. It would be nice if all the functions did
this automatically, but I am concerned about the performance of doing
many redundant passes over a tree when the library is being used, and
I think the overhead of verifying this in the type system is really
big (maybe it would not be too bad?).

only canonicalizes the built in types (type synonyms or whatever for
user types will be handled in the catalog not here).

maybe canonicalize should do case folding for unquoted identifiers
also (which is dependent on the dialect).

> -- | convert the name of a type to its canonical name. For types
> -- without multiple names, it returns the name unchanged
> canonicalizeTypeName :: Dialect -> Text -> Text
> canonicalizeTypeName d = ct (tm d)
>   where
>     tm ANSI = ansiTypeNames
>     tm SQLServer = postgresqlTypeNames
>     tm Oracle = postgresqlTypeNames
>     tm PostgreSQL = postgresqlTypeNames
>     hasType t p = let t' = T.map toLower t
>                   in t' `elem` snd p
>     ct m tn = maybe tn fst
>               $ find (hasType tn) m

ansi:

no concept of canonical names in ansi? so just choose the
shortest version of each

> ansiTypeNames :: [(Text,[Text])]
> ansiTypeNames =
>     [("char",["character"])
>     ,("varchar",["char varying","character varying"])
>     ,("clob",["character large object","char large object"])
>     ,("nchar",["national character","national char"])
>     ,("nvarchar",["national character varying"
>                  ,"national char varying"
>                  ,"nchar varying"])
>     ,("nclob",["national character large object"
>               ,"nchar large object"])
>     ,("varbinary",["binary varying"])
>     ,("blob",["binary large object"])
>     ,("int",["integer"])
>     ,("float",["double precision"])]

postgresql:

the canonical names are what pg_dump uses and appear in the
catalog. Some of the canonical names for ansi types don't use the ansi
names.

> postgresqlTypeNames :: [(Text,[Text])]
> postgresqlTypeNames =
>     [("timestamp", ["datetime"])
>      -- todo: temp before sqlserver dialect is done properly
>      -- this hack should probably move to the ansi dialect first
>     ,("int1", ["tinyint"])
>     ,("int2", ["smallint"])
>     ,("int4", ["integer","int"])
>     ,("int8", ["bigint"])
>     ,("numeric", ["decimal"])
>     ,("float4", ["real"])
>     ,("float8", ["double precision","float","double"])
>      -- probably some missing here
>     ,("varchar", ["character varying"])
>     ,("char", ["character"])
>     ,("bool", ["boolean"])]

TODO:

what about PrecTypeName - called modifiers in pg. No idea how these
should work in the typechecker. I think the modifier values might be
completely dynamic in postgres.

array types have to match an exact array type in the catalog, so we
can't create an arbitrary array of any type. Not sure if this is
handled quite correctly in this code. Not sure if you would ever
create or drop an array type manually.


