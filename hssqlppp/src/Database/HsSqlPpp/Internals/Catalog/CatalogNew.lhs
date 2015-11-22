
New catalog:

updates to the catalog are high level not low level
error checking
supports schemas and search paths properly
object dependencies with cascade and restrict
this catalog will be clear if an object is based on another object in
  the catalog (possibly with a bit of tweaking)
  (for instance, an alias to a type)
  or if the object is 'externally implemented' and its relationship
    with any possible base type is opaque

How should the catalog support syntax? This is needed in:
domain constraints
table constraints
view definitions
also: function/routine bodies

option 1: save them as strings
option 2: parameterize on the scalarexpr and queryexpr?
option 3: hardcode the scalarexp and queryexpr types

the possible benefits to parameterized versions are:
1. don't have to reparse
2. this module doesn't depend on the syntax module
3. this module cannot do anything with the syntax internals

objects to support:
schemas
scalar types
domain types
composite types
enums
type categories (use in implicit cast/matching resolution)
prefix, postfix, binary operators
special syntax ops (between, extract, position)
casts
functions
aggregates
windows
tables
views
sequences
character sets
collations
roles (just for ownership tracking)

later: permissions

later: add fuzzy matching to all relevant error messages

what kind of function matching options are supported:
basic matching
matching with implicit casts and overloads
variadic functions (like postgres does it?)
keyword arguments (ansi)
polymorphic functions (like postgres does it?)

todo: figure out how to do extras for aggregates and windows

at the same time, refactor the types and type errors to be something
different to what there is now:


These catalog functions accept only the dialect canonical names of
built in types such as 'char' which have more than one name, and they
only accept names which have had the escaping applied, "somethin""g"
-> in haskell -> "something\"g", and the case folded properly if the
identifier is unquoted (different dialects fold the case
differently). Utilities to help with these are provided somewhere. All
operations on objects that have a schema must have the schema
explicit. There are helper functions to lookup what the schema for an
unqualified name should be.

This code does no validation on the identifier syntax or characters


TODO: for each object:
describe some example syntax
describe the alter and drop considerations
describe all the anomalies to check for when doing an update
  put cascade/restrict behaviour tests in the dependent object tests,
  not the depended-upon object tests
describe the type check/ public api query functions related to that
  object
write the tests against the updates and query apis


TODO docs:
how to use this catalog code:
if you have no schemas
if you don't have roles or permissions

todo:
figure out how to do object dependencies:
mainly should the dependency discovery be as automatic as possible
or should it be all manual


> module Database.HsSqlPpp.Internals.Catalog.CatalogNew
>    (CatalogUpdate(..)
>    ,Catalog(..)
>    ,updateCatalog
>    ,emptyCatalog
>    ) where

> import qualified Data.Map as M
> import qualified Data.Set as S


todo object order:
roles (will be used to implement logins and groups like in postgresql)
  a role is a container for other roles, and for permissions
  and is used to own other objects (including roles)
collation and character sets
schemas
sequences
typeids
type categories
external types
casts
functions, operators
windows, aggregates?
tables
  a table with a implicitly declared sequence must create a separate
  sequence in a desugaring process
views
domain types
composite type
enums

what is missing from ansi:
  routines/procedures (function is not exactly corresponding - fix this)
  transliteration (conversion between character sets?)
  assertion
  trigger
  user defined ordering (is this to order composite types?)
  transform (conversion between sql representations and host language
    representations of values)
  grant (this is on the todo)
missing from postgres:
  conversion
  database
  event trigger
  extensions
  foreign data wrapper
  foreign table
  index
  language
  materialized view
  operator stuff (for indexes?)
  policy (row level permissions)
  rule
  server
  tablespace
  text search stff
  transform
  trigger
  user mapping


> --data TypeId = ...

a TypeId is one of:
astraction of the syntax for type names
one of these type names with extra annotation added
  (this can include nullability information)
a special type (some are internal):
  polymorphic function parameters
  unknown type used to type check literals (and a few other things) in
    some dialects
  void, used to represent the return type of functions which don't
    return a value
  setof, used to represent the return type of table returning functions
  tref type used in typechecking joins and a few other places (kind of
    an anonymous composite type)

what about tuple types
what about arrays and multisets

question: should the catalog manage the following:
  if a scale/precision and what kind is allowable/mandatory for a
    type?
  if a type can be used in various operations
    (e.g. does == exist for it, ordering?, can it be used in an array
    or multiset?)

> data CatError = RoleAlreadyExists String

ast for ddl-like operations

> data SchemadName = SN String String

> data Catalog = Catalog
>     {cRoles :: S.Set String
>     }

> emptyCatalog :: Catalog
> emptyCatalog = Catalog {cRoles = S.empty}

> updateCatalog :: CatalogUpdate -> Catalog -> Either CatError Catalog
> updateCatalog (CreateRole r) c@(Catalog {cRoles = rs})
>     | r `S.member` rs = Left $ RoleAlreadyExists r
>     | otherwise = Right $ c {cRoles = S.insert r rs}

used for dependencies:

> --data ObjectType = ...

now the dependencies can be tracked in one table

> data CatalogUpdate =

create schema _name_ [authorization _role_]
  [DEFAULT CHARACTER SET 'character set' [collate c]]

character sets are strings and not identifiers atm
roles are identifiers but don't exist in a schema or database (since
this catalog only covers a single database at a time, they look like
they exist in a database)

not going to support path for schema right now (I think this is
supposed to be for the bodies of routines in that schema, maybe it
could also be for all lookups for schema elements (such as views)

question: should there be a database default search path? one that can
contain variables (e.g. a schema with the same name as the current
user?) This would be the default for sessions. You cannot enter a
schema, so the schema default search path works differently.

question: I think in some dialects, character set names are
identifiers or schema-d identifiers (and collations also), and in
others they are strings. How should this be handled?

todo: add object dependencies to the updates (from dependent to
depended-upon in the dependent). Try to make these automatic as much
as possible.

list schemas
create schema: schema already exists
  character set invalid
  collation not valid
  auth not valid
later: more schema-based queries

>     {-  CreateSchema String String -- name, default char set
>     | AlterSchemaName String String  [role|name|default char set]
>     | AlterSchemaAuth String -}

what happens when you change this default: existing columns are not
changed (there could be some utility to change them all for
consistency. This would mean storing a uses default flag on each
relevent place also).

>     {- | AlterSchemaDefaultCharSet String
>     | DropSchema String Cascade -}

= Roles

roles are used for ownership tracking only at the moment
dependencies: todo

anomalies: role already exist
           role doesn't exist

queries: list roles

>       CreateRole String
>     | DropRole String Cascade
>     | RenameRole String String Cascade -- from, to

= basic types

-- external type represents a type whose implementation is outside the
-- system completely, either it is built in or implemented in C for
-- instance

-- external types can also be modelled using the other types such as
-- domain, compose, enum if this matches their behaviour

>     {-  | CreateExternalType SchemadName

>     | CreateDomainType SchemadName SchemadName -- name, base name

>     | CreateCompositeType

>     | CreateEnum

+ a type category for each of the above
+ a bool to say whether it is the preferred type for that category
todo: how can these be changed? is there a reason to change them
what about the ability to wipe the prefered type for a category only,
then other operations can be sugar on this
what about altering a type to set it as prefered or not preferred or
to change its type category

>     | CreateTypeCategory
>     | AlterTypeCategory?
>     | DropTypeCategory



>     | DropType
>     | AlterType - rename
>     | AlterCompositeType - add, reorder or remove columns
>     | AlterEnum - add, reorder or remove enum values

= scalar operations

by convention, it supports keyword operations as prefix,postfix or
binary if they match these, and if the operator is more than one
keyword separated by space then the name in the catalog is the
keywords separated by exactly one space each in lower case. Operators
which don't fit this set of options (such as mixfix or ternary
operators go in the SpecialOperators and are handled as special cases)

prefix, postfix, binary operators

>     | CreatePrefixOp String String String String -- op schema, op name,
>         --  input type schema, input type name, output type schema,name

>     | CreatePostfixOp String String String String -- op schema, op name,
>         --  input type schema, input type name, output type schema,name

>     | CreateBinaryOp
>     | DropPrefixOp, PostfixOp, BinaryOp

>     | CreateSpecialOp -- should the list of names be a enum?
>                       -- if it is freeform, then the catalog doesn't need to know as much
>                       --  about the syntax, which is a good thing

>     | CreateCast
>     | DropCast
>     | AlterCast? - doesn't seem like there is a real point

>     | CreateFunction
>     | AlterFunction - rename, change some metadata?

>     -}

aggregates
windows
tables
views
sequences
character sets
collations
roles


> data Cascade = Cascade | Restrict


this e.g. is the return value when you want to get all the information
on a type. some of this information is contained in the typeid syntax,
and some is stored in the catalog or environment

> --data TypeInfo = ...

is scalar
precision, scale, nullability
char set, collation
isdomain, domain base, domain constraint
is composite, fields in the composite
