

Type identifiers and anonymous types

Basic type identified by name
These can be quoted or unquoted, and with or without schema

unknown type: this is used for the types of literals in a kind of poor
mans type inference, modelled on postgresql's behaviour. Dialects can
choose not to use this approach and give a literal a type based only
on its syntax (and thus the dialect would never use the unknown
type. (The direct unknown typing isn't quite implemented yet).

any types

these are used to implement a kind of poor man's polymorphism. based
on the any types in postgresql. These are used for polymorphic
udfs. This is separate to overloading functions.

anonymous composite type

this represents a tuple of types, all, some or none of which can have
field names. This is used for composite types which are created on the
fly and don't have a name in the catalog or environment. Maybe this
should be changed so they are given a temporary name to reduce the
special cases

tref type

this is an internal type used by the typechecker (todo: can this be
hidden?) Maybe could add optional correlation names to the field names
in anonymous composite type and reuse that?

setof?
record?
void?

typeextra

ugly way to add nullability and precision to a type. This should get a
big rethink

step 1 todo:
combine composite and anonymous composite types
combine scalartype, domaintype, enum, named composite type

get rid of explicit array types and figure out a better way to handle
them

Split the errors into another module, and in the public api
The errors should contain not contain 'Type's.

figure out a better way of handling nullability, precision, scale than
the typeextra

> {-# LANGUAGE FlexibleInstances,DeriveDataTypeable,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Internals.TypesInternal where
>
> import Data.Data
> --import Data.Generics.Uniplate.Data
> --import Data.Char
> import Data.Text (Text)
> --import qualified Data.Text as T
> --import Database.HsSqlPpp.Internals.Dialect
> --import Data.List
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
>                 --  | TriggerRecord
>                 --  | Trigger
>                 -- | cstring - a C string
>                 --  | Cstring
>                 -- | represents the return type of a function which doesn't return
>                 -- anything. Not sure if it is used anywhere else
>                 | Void
>                 --  | Internal
>                 --  | LanguageHandler
>                 --  | Opaque
>                 --  | FdwHandler
>                   deriving (Eq,Show,Ord,Typeable,Data)

TODO idea

> {-data Type = -- | A normal type identified by name. Includes scalar
>             -- types, domain types, enums, (non-anonymous)
>             -- structured types
>             Type (Maybe Text,Text)
>              -- | represents a unknown type like in postgres. Used
>              -- for literals in some dialects and a few other places
>           | UnknownType
>           | ArrayType Type
>           | CompositeType [(Text,TypeExtra)]
>           -- | hack to support the environment for a tref
>           | TrefType [((Text,Text),TypeExtra)]
>           -- | the fields are anonymous as well as the type itself
>           | AnonymousCompositeType [Type]
>           | SetOfType Type
>             -- | record types are used in plpgsql for a sort of dynamic
>             -- typing or rough polymorphism substitute. They can refer to
>             -- values of named composite type, composite type or
>             -- anonymous composite type, not sure if non composite types as well.
>           | Record (Maybe Type)
>             deriving (Eq,Show,Ord,Typeable,Data)-}


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
>                | SchemadColumnName Text
>                | DbSchemadColumnName Text
>                | BadStarExpand
>                | InternalError String
>                | AmbiguousIdentifier Text
>                | OdbcFuncBadContent
>                | DuplicateColumnName Text
>                | TooManyColumnsInInsert
>                  deriving (Eq,Show,Ord,Typeable,Data)
>
