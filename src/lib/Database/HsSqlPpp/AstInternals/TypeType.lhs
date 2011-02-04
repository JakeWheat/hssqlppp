
This file contains the data type Type. It is kept separate so we can
compile the types and function information from the database
separately to AstInternal.ag.

Types overview:

Regular types: scalarType, arrayType, composite type, domaintype
we can use these anywhere.

~~~~
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
is currently unable to type check, this usage should disappear at some
point, and then it will only represent code which doesn't type check
because of a mistake.

The Type type identifies the type of a node, but doesn't necessarily
describe the type.

Typing relational valued expressions:
use SetOfType combined with composite type for now, see if it works
out. If not, will have to add another type.
~~~~

> {-# LANGUAGE FlexibleInstances,DeriveDataTypeable #-}
>
> module Database.HsSqlPpp.AstInternals.TypeType where
>
> import Control.Monad.Trans.Error
> --import Control.Monad.Error
>
> import Data.Data
> import Data.Generics.PlateData
>
> data Type = ScalarType String
>           | ArrayType Type
>           | SetOfType Type
>             --three variations, choose better name for rowctor
>             --refer to composite type in catalog
>           | NamedCompositeType String
>             --refer to composite type either in catalog or one generated on fly
>           | CompositeType [(String,Type)]
>             --refer to anonymous composite type: the fields have no names
>             --these three types are equality and assign compatible if the fields
>             --are ignoring the names, so the fields have to be in the same order.
>           | AnonymousRecordType [Type] -- was rowctor
>           | PgRecord (Maybe Type) -- can only hold namedcomposite, composite or anonymousrecord
>           | DomainType String
>           | EnumType String
>           | Pseudo PseudoType
>           | UnknownType -- represents a string literal
>                              -- token whose type isn't yet
>                              -- determined
>             deriving (Eq,Show,Ord,Typeable,Data)
>
> data PseudoType = Any
>                 | AnyArray
>                 | AnyElement
>                 | AnyEnum
>                 | AnyNonArray
>                 | Cstring
>                 | Record
>                 | TriggerRecord
>                 | Trigger
>                 | Void
>                 | Internal
>                 | LanguageHandler
>                 | Opaque
>                   deriving (Eq,Show,Ord,Typeable,Data)

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
>                | BadStarExpand
>                | AmbiguousIdentifier String
>                | ContextError String
>                | MissingJoinAttribute
>                | ExpressionMustBeBool
>                | WrongNumberOfColumns
>                | ExpectedDomainType Type
>                | DomainDefNotFound Type
>                | BadCatalogUpdate String
>                | TypeAlreadyExists Type
>                | AnyAllError String
>                | InternalError String
>                | FromToTypesNotSame Type Type
>                 --shoved in to humour the Either Monad
>                | MiscError String
>                  deriving (Eq,Show,Ord,Typeable,Data)
>

 > instance Error ([TypeError]) where
 >   noMsg = [MiscError "Unknown error"]
 >   strMsg str = [MiscError str]

> instance ErrorList TypeError where
>    listMsg s = [MiscError s]


=== canonical type name support

Introduce some aliases to protect client code if/when the ast
canonical names are changed:

> typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4,
>   typeFloat8,typeVarChar,typeChar,typeBool,typeDate :: Type
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

this converts the name of a type to its canonical name:
try to follow the names that pg uses in a dump
this converts the name of a type to its canonical name:
try to follow the names that pg uses in a dump

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
>       intNames = ["int4", "integer", "int", "serial"]
>       bigIntNames = ["int8", "bigint"]
>       numericNames = ["numeric", "decimal"]
>       float4Names = ["real", "float4"]
>       float8Names = ["double precision", "float"]
>       varcharNames = ["character varying", "varchar"]
>       charNames = ["character", "char"]
>       boolNames = ["boolean", "bool"]
>
> canonicalizeTypes :: Data a => a -> a
> canonicalizeTypes =
>   transformBi $ \x ->
>       case x of
>         ScalarType s -> ScalarType $ canonicalizeTypeName s
>         x1 -> x1

random notes on pg types:

~~~~

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

~~~~

--------------------------------------------------------------------------------

utilities for working with Types

These may indicate that the haskell type system isn't being used very well.

> isArrayType :: Type -> Bool
> isArrayType (ArrayType _) = True
> isArrayType _ = False
>
> isDomainType :: Type -> Bool
> isDomainType (DomainType _) = True
> isDomainType _ = False
>
> isCompositeType :: Type -> Bool
> isCompositeType (CompositeType _) = True
> isCompositeType (NamedCompositeType _) = True
> isCompositeType (AnonymousRecordType _) = True
> isCompositeType (PgRecord _) = True
> isCompositeType _ = False
>
> isCompositeOrSetOfCompositeType :: Type -> Bool
> isCompositeOrSetOfCompositeType (SetOfType a) = isCompositeType a
> isCompositeOrSetOfCompositeType a = isCompositeType a
>
> unwrapArray :: Type -> Either [TypeError] Type
> unwrapArray (ArrayType t) = Right t
> unwrapArray x = Left [InternalError $ "can't get types from non array " ++ show x]
>
> unwrapSetOfWhenComposite :: Type -> Either [TypeError] Type
> unwrapSetOfWhenComposite (SetOfType a@(CompositeType _)) = Right a
> unwrapSetOfWhenComposite x = Left [InternalError $ "tried to unwrapSetOfWhenComposite on " ++ show x]
>
> unwrapSetOfComposite :: Type -> Either [TypeError]  [(String,Type)]
> unwrapSetOfComposite (SetOfType (CompositeType a)) = Right a
> unwrapSetOfComposite x = Left [InternalError $ "tried to unwrapSetOfComposite on " ++ show x]
>
> unwrapSetOf :: Type -> Either [TypeError] Type
> unwrapSetOf (SetOfType a) = Right a
> unwrapSetOf x = Left [InternalError $ "tried to unwrapSetOf on " ++ show x]
>
> unwrapComposite :: Type -> Either [TypeError] [(String,Type)]
> unwrapComposite (CompositeType a) = Right a
> unwrapComposite x = Left [InternalError $ "cannot unwrapComposite on " ++ show x]
>
> consComposite :: (String,Type) -> Type -> Either [TypeError] Type
> consComposite l (CompositeType a) = Right $ CompositeType (l:a)
> consComposite a b = Left [InternalError $ "called consComposite on " ++ show (a,b)]
>
> unwrapRowCtor :: Type -> Either [TypeError] [Type]
> unwrapRowCtor (AnonymousRecordType a) = Right a
> unwrapRowCtor x = Left [InternalError $ "cannot unwrapRowCtor on " ++ show x]

--------------------------------------------------------------------------------

~~~~
new plan for types:

The type annotations attached to nodes will be either typecheckfailed 'type'
this will allow chaining type check failed more robustly. Type errors don't change.

the sql type type will be changed so we can track the different
contexts where different sql types can be used, split to use different
sets of types so we can use the haskell type system to enforce these
contexts.

rename type to sqltype, too confusing otherwise
new set of sqltypes: (these aren't in one haskell type anymore)
scalar type - corresponds to sqltypes in pg which aren't one of the
  other types (no easy way to define this)
array type
setof type : can be applied to scalar, domain, ... but not all types
composite types are changed:
namedcomposite string
composite [(string,type)] -> what types can appear in the attribute list?
anonymousrecord [type] -> what types can appear here? -> this
represents a row expression type, which can be returned in select
expressions (but not as a column in a view), and is used in other places
domaintype : what types are allowable as the base type?
pseudo types: these are used for arguments and return values for function prototypes only
any*, record - just means pg infers the return type which will be a composite type (namedcomposite, composite or anonymousrecord), trigger: just a tag, void: just a tag, any*: polymorphic functions, ignore cstring, internal, language handler and opaque.
issue: a variable declaration can be a polymorphic type in a polymorphic function, but we can't use the other pseudo types so need to split these
a variable declared as type record IS NOT the same thing as the pseudo record type, I've finally worked out.
so our plpgsqlrecordtype is plpgsqlrecordtype (Nothing|one of the three composite types)

typechecking polymorphic functions: will have to relax the type
checking to some degree. want to check against each usage of the
polymorphic function in the source, so substitute in the actual types
and check the function.

contexts:
function prototypes: args and return type
function calls: args and return
variable declaration in plpgsql
attribute in a select expression
attribute in a view (less permissive than a select expression), also create table as?
attribute in a table, type (is this the same list as for composite and anonymous records? - can nest anonymous records)
base type for domain
cast expression
insert: targetatts are same set of types as attributes in a table
update: targets can be attr types or anonymous records of attr types for rowset style assign (x,y) = selectexp
assignment statement: type on left same set as type in a for, types in select into, etc?
return statement
return query statement
expressions in a raise statement
expressions in execute, anything else?
types used inside select expression checking:
  selectlist, trefs, groupby, orderby, limit, offset, where, having
expressions:
what can an identifiers type be?
what can the type of an Expression node be?
  scalar: integerlit,floatlit
  stringlit: what can an unknown resolve to (not quite everything)
  booleanlit: scalar
  nulllit: same as stringlit?
  positionalarg: any type that a function arg can have
  cast: any type you can cast to
  identifier: ?
  case, casesimple: any type in the rhs expressions, what are these?
  exists: scalar
  funcall: any type a function return type can have
  inpredicate: scalar
  windowfn: ?
  liftoperator: booleans only?

problem: can't get types using gettypeannotation function and support
different valid combinations of types


new notes
type contexts - these all have different possible types
function return type
function parameter type
OLD & NEW
declared variable
attribute in create table
attribute create type
column in select expression
column in view
column in insert
item in set in update: includes row type with contained types restricted
placeholder
id lookups in local bindings
typenames??

create list of each type for each context
create wrapper types which convert from other types to either
  typeerror type for bad type for context

~~~~

