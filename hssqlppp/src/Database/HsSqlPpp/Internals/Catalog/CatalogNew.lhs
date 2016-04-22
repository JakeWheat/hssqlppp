
This catalog code:

1. is used by the typechecker internally to hssqlppp
2. does most of the validation on ddl updates and provides nice
error message when an update is not valid. It also tracks ownership,
dependencies and models cascade and restrict on drop

The CatalogUpdate's model alter and drop, as well as create. The
CatalogUpdate's are supposed to represent updates after they have been
desugared, so they don't correspond 1-1 with dml statements.

The catalog information is only to model enough for these features,
and e.g. doesn't have enough information for a complete catalog in a
sql dbms, for instance it doesn't track the current value for a
sequence.

The catalog code api follows these principles:
unquoted identifiers must be folded to the correct case for the dialect
  select * from t -> use 't' in postgres, and 'T' in sql server
  for instance
names of built in types must use the canonical names in the current
  dialect for those types
  integer -> int4 in postgres, int in ansi
escapes should already be applied for identifiers and strings
e.g. select * from "my "" table"
  this code should get: some_table_function "my \" table" (using
  haskell escaping)
names must always be schema qualified

This code does no validation on the identifier syntax.

There are separate helper functions to do the case transform,
canonicalize type names and to find the correct schema for a name
given the schema search path.

= supported objects

schemas
scalar types/ externally implemented types/built in types
domain types
composite types
enums
type catagories (used in implicit cast/matching resolution)
prefix,postfix,binary operators
special syntax operations (e.g. between, extract, position)
casts
functions
aggregates
windows
tables
constraints (used for row, table, domain constraints and assertions)
defaults (column defaults)
  constraints and defaults are considered separate objects to tables
  so that they can be dropped as part of a cascade without dropping
  the table with a more straightforward implementation
views
sequences
character sets
collations
roles (for ownership tracking only, permissions todo)

= TODO

understand routines, etc. in ansi
  + variadic, keyword args, function metadata/annotation-like stuff
  polymorphic functions
and types
and these in the main dialects
understand aggregate and windows options
summarize missing objects from sql server, oracle and db2
do the example syntax for all supported operations
detail all the dependency stuff
understand some of the questions like this:
  identifier, schema qualified identifier or string for e.g. character
    set
  do/can roles be in a schema
  
describe the helper apis (apply escapes, fold case, find schema, etc.)
make a list of all the query apis wanted, do the typeid
understand composite types, anonymous, tuples, arrays, multisets
understand what schema various built in objects should be in in
  different dialects
make a detailed spec and start working on it
write a catalog value consistency check function
multiple updates? - can avoid hardcoding the public schema, etc. with
  this

= not currently supported objects

== ansi

routines/procedures (where they differ from functions, I think the
  main thing missing is the function bodies and analysis based on
  these)
transliteration (conversion between character sets?)
trigger
  could use the dependency and some basic checking quite easily
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
  text search stuff
  transform
  trigger
  user mapping

sql server full list:
aggregate
application role
assembly
asymmetric key
availabiltiy group
broker priority
certificate
column encryption key
column master key definition
columnstore index
contract
credential
cryptographic provider
database (azure sql database)
database (sql server)
database audit specification
database encryption key
default
endpoint
event notification
event session
external data source
external file format
external table
federation
fulltext catalog
fulltext index
fulltext stoplist
function
index
login
master key
message type
partition function
partition scheme
procedure
queue
remote service binding
resource pool
role
route
rule
schema
search property list
security policy
selective xml index
sequence
server audit
server audit specification
server role
service
spatial index
statistics
symmetric key
synonym
table
table (azure sql database)
trigger
type
user
view
workload group
xml index
xml schema collection

db2 full list:
alias
audit policy
bufferpool
database partition group
event monitor
event monitor (activities)
event monitor (change history)
event monitor (locking)
event monitor (package cache) statement
event monitor (statistics)
event monitor (threshold violations)
event monitor (unit of work)
event monitor dbactivities
function
function (external scalar)
function (external table)
function (ole db external table)
function (sourced or template)
function (sql scalar, table, or row)
function mapping
global temporary table
histogram template
index
index extension
mask
method
module
nickname
permission
permission csr_row_access on customer
permission teller_row_access on customer
procedure
procedure (external)
procedure (sourced)
procedure (sql)
role
schema
security label
security label component
security policy
sequence
sequence org_seq
server
service class
stogroup
synonym
table
tablespace
threshold
transform
trigger
trusted context
type
type (array)
type (cursor)
type (distinct)
type (row)
type (structurxxxxxxxxxxxxxxxxxxxxxxed)
type mapping
usage list
user mapping
variable
view
work action set
work class set
workload
wrapper

oracle full list:
audit policy (unified auditing)
cluster
context
controlfile
database
database link
dimension
directory
diskgroup
edition
flashback archive
function
index
indextype
java
library
materialized view
materialized view log
materialized zonemap
operator
outline
package
package body
pfile
pluggable database
procedure
profile
restore point
role
rollback segment
schema
sequence
spfile
synonym
table
tablespace
trigger
type
type body
user
view


what would be nice to add:
triggers are pretty common
indexes are pretty common
synonyms: do a review of these in different dbmss
  what can we support synonyms for:
    not special syntax ops?
    not casts
    what about operator synonyms for functions and vice versa?
    defaults, constraints: doesn't make sense (even though constraints
      do have a name)
    everything else seems like fair game, including synonyms themselves
partition stuff??
global temporary tables?
distinct type: this is like a domain, but cannot be implicitly cast to
  the base type. Does this catalog need to know the difference between
  this and an external type?

= implementation progress notes


New catalog lower level goals:

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

later: add fuzzy matching to all relevant error messages

TODO docs:
how to use this catalog code:
if you have no schemas
if you don't have roles or permissions
if you don't want to use the bootstrap items (public schema, ascii
  charset, binary collation)

> {-# LANGUAGE DeriveDataTypeable #-}
> module Database.HsSqlPpp.Internals.Catalog.CatalogNew
>    (CatalogUpdate(..)
>    ,Catalog(..)
>    ,Cascade(..)
>    ,CatError(..)
>    ,updateCatalog
>    ,emptyCatalog
>    ) where

> import qualified Data.Map as M
> import qualified Data.Set as S
> import Data.Data


> --data TypeId = ...

a TypeId is one of:
astraction of the syntax for type names (probably just cut and paste
  the syntax)
one of these type names with nullablility information
a special type (some are internal):
  polymorphic function parameters
  unknown type used to type check literals (and a few other things) in
    some dialects
  void, used to represent the return type of functions which don't
    return a value
  setof, used to represent the return type of table returning functions
    ... not sure about this
  tref type used in typechecking joins and a few other places (kind of
    an anonymous composite type)

composite types:
  with names and order
  without names
  either single row or 0 or 1 rows, or 0-many rows
what about tuple types?
what about arrays and multisets?

question: should the catalog manage the following:
  if a scale/precision and what kind is allowable/mandatory for a
    type?
  if a type can be used in various operations
    (e.g. does == exist for it, ordering?, can it be used in an array
    or multiset?)

> -- | Represents an error from an invalid catalog update or query
> data CatError = RoleAlreadyExists String
>               | RoleNotRecognised String
>                 deriving (Eq,Show)


> -- | A schema qualified name. These should also have the
> -- canonicalization, case folding and escaping fixed
> data SchemadName = SN String String
>                    deriving (Eq,Show)

> -- | The model of a catalog use in hsqlppp for validating ddl
> -- updates and for typechecking sql.
> data Catalog = Catalog
>     {cRoles :: S.Set String
>     ,cSchemas :: M.Map String CatSchema
>     ,cCharacterSets :: M.Map SchemadName CatCharacterSet
>     ,cCollations :: M.Map SchemadName CatCollation
>     ,cSequences :: M.Map SchemadName CatSequence
>     ,cTypeCategories :: S.Set String -- this is internal, used in the
>     -- implicit cast/function resolution
>     ,cExternalTypes :: S.Set SchemadName -- externally implemented/built in types
>     ,cCasts :: S.Set (SchemadName,SchemadName,CastContext)
>     ,cFunctions :: M.Map SchemadName [CatFunSig]
>     ,cPrefixOps :: M.Map SchemadName [CatFunSig]
>     ,cPostfixOps :: M.Map SchemadName [CatFunSig]
>     ,cBinaryOps :: M.Map SchemadName [CatFunSig]
>     ,cSpecialOps :: M.Map SchemadName [CatFunSig]
>     ,cWindows :: Int
>     ,cAggregates :: Int
>     ,cTables :: M.Map SchemadName [(String,SchemadName)]
>     ,cConstraints :: Int
>     ,cDefaults :: Int
>     ,cViews :: M.Map SchemadName [(String,SchemadName)]
>     ,cDomainTypes :: M.Map SchemadName SchemadName
>     ,cCompositeTypes :: M.Map SchemadName [(String,SchemadName)]
>     ,cEnums :: Int
>     -- dependencies?
>     }
>     deriving (Eq,Show)

> data CastContext = ImplicitCastContext
>                  | AssignmentCastContext
>                  | ExplicitCastContext
>                    deriving (Eq,Show,Ord,Typeable,Data)

> -- | owner, default char set, default collation (if it
> -- is nothing, it is the global collation for the charset
> type CatSchema = (RoleName, String, Maybe String)

> -- | Character set: name, owner, default collation
> type CatCharacterSet = (RoleName, String)

> -- | Collation: owner, character set
> type CatCollation = (RoleName,String)

> -- | sequence info: data type
> -- this is used e.g. if we need to use an implicit
> -- cast around a call to 'next value for'.
> type CatSequence = SchemadName

> type CatFunSig = (SchemadName,[SchemadName],SchemadName{-,Options-})
> --type FunOptions = 

> -- | A catalog value containing nothing
> emptyCatalog :: Catalog
> emptyCatalog = Catalog {cRoles = S.empty
>                        ,cSchemas = M.empty
>                        ,cCharacterSets = M.empty
>                        ,cCollations = M.empty
>                        ,cSequences = M.empty
>                        ,cTypeCategories = S.empty
>                        ,cExternalTypes = S.empty
>                        ,cCasts = S.empty
>                        ,cFunctions = M.empty
>                        ,cPrefixOps = M.empty
>                        ,cPostfixOps = M.empty
>                        ,cBinaryOps = M.empty
>                        ,cSpecialOps = M.empty
>                        ,cWindows = 0
>                        ,cAggregates = 0
>                        ,cTables = M.empty
>                        ,cConstraints = 0
>                        ,cDefaults = 0
>                        ,cViews = M.empty
>                        ,cDomainTypes = M.empty
>                        ,cCompositeTypes = M.empty
>                        ,cEnums = 0
>                        }

> updateCatalog :: CatalogUpdate -> Catalog -> Either CatError Catalog
> updateCatalog (CreateRole r) c@(Catalog {cRoles = rs})
>     | r `S.member` rs = Left $ RoleAlreadyExists r
>     | otherwise = Right $ c {cRoles = S.insert r rs}

> updateCatalog (DropRole r _) c@(Catalog {cRoles = rs})
>     | not (r `S.member` rs) = Left $ RoleNotRecognised r
>     | otherwise = Right $ c {cRoles = S.delete r rs}

todo: cascade the rename

> updateCatalog (RenameRole r rn) c@(Catalog {cRoles = rs})
>     | not (r `S.member` rs) = Left $ RoleNotRecognised r
>     | rn `S.member` rs = Left $ RoleAlreadyExists rn
>     | otherwise = Right $ c {cRoles = S.insert rn $ S.delete r rs}

> updateCatalog _ _ = error "update catalog not completed yet"


> type RoleName = String


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

question: how does a minimal catalog start out?
1. has a schema called public
2. has a global default character set called ascii
3. has a collation called binary for that character set
4. has a role called public which owns the above

In the tests, show an example of creating a catalog from this default
   without the public schema or role still existing
   and without the character set and collation

>       CreateSchema String (Maybe (SchemadName,SchemadName)) RoleName

schema name, (default char set, default collation for that character set in
that schema), owner

>     | RenameSchema String String
>     | SetSchemaOwner String RoleName
>     | SetSchemaDefaultCharacterSet (Maybe (SchemadName,SchemadName))

what happens when you change this default: existing columns are not
changed (there could be some utility to change them all for
consistency. This would mean storing a uses default flag on each
relevent place also).

>     | DropSchema String Cascade

= Roles

roles are used for ownership tracking only at the moment

create role my_role;
drop role my_role;
alter role my_role rename to my_new_role;

dependencies: todo

anomalies: role already exist
           role doesn't exist

queries: list roles

>     | CreateRole RoleName
>     | DropRole RoleName Cascade
>     | RenameRole RoleName RoleName -- from, to

= Collations and character sets

This code doesn't track character set and collation aliases

collations appear in separate namespaces according to their character
set, so you can have different collations for different character sets
with the same name

this catalog code will create a collation called 'binary' for each new
character set. If you don't want this collation, you can delete it
after creating another collation for the new character set, or rename
the collation. This is one way to avoid the catch-22 which can also
work for sql users.

create character set my_character_set
create character set my_character_set as get another_character_set
create character set my_character_set as 'some external source'
create character set my_character_set as 'some external source' collate 'some other external source'
create collation my_collation for my_character_set from some external source [set as default?]
alter character set set default collation 'collation_name'

drop character set
drop collation

alter character set my_set set schema my_schema
alter collation my_coll set schema my_schema
rename for both
set owner for both

question: does a character set have to have a collation?

dependencies: todo

queries:
list character sets (returns owner also)
list collations (returns the owner and character sets also)
lookup collation/ is collation valid for character set
get character set default collation

anomalies:
role no exist
already exist
doesn't exist
maybe nice message when schema is ambiguous, when collation is in another schema?
      as well as fuzzy name matching

>     | CreateCharacterSet SchemadName RoleName
>     | CreateCollation SchemadName SchemadName RoleName --second is the character set this collation is for
>
>     | DropCollation SchemadName SchemadName Cascade -- the first is the character set this collation is for
>                                                     -- the front end can make this optional in the concrete
>                                                     -- syntax iff
>                                                     -- there is only
>                                                     -- one collation
>                                                     -- with this
>                                                     -- name for
>                                                     -- instance
>     | DropCharacterSet SchemadName Cascade
>     | RenameCharacterSet SchemadName SchemadName -- covers schema changes and name changes
>     | RenameCollation SchemadName SchemadName SchemadName -- char set name, collation name, new collation name
>     | SetCharacterSetOwner SchemadName RoleName
>     | SetCollationOwner SchemadName SchemadName RoleName -- char set name, collation name
>     | SetCharacterSetDefaultCollation SchemadName SchemadName

= basic types

TODO: understand basic syntax and semantics of types in ansi sql

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
>     deriving (Eq,Show)

aggregates
windows
tables
views
sequences
character sets
collations
roles


> data Cascade = Cascade | Restrict
>                deriving (Eq,Show)


this e.g. is the return value when you want to get all the information
on a type. some of this information is contained in the typeid syntax,
and some is stored in the catalog or environment

> --data TypeInfo = ...

is scalar
precision, scale, nullability
char set, collation
isdomain, domain base, domain constraint
is composite, fields in the composite



= dependencies, cascade, restrict

roles
schema
character set
collation
sequence
type category
external types
cast
functions, operators, windows, aggregates
tables
  constraints
  defaults
views
domain types
composite types
enums

question: what about ownership of dependencies, and does changing
owner ever cascade? (Maybe these are separate helper fns?)

rough set of cascade flavours:
1. remove connection, leave depender existing
2. update connection to point to new value (for alter)
3. drop the depender object (recursively to its dependents)

for alter, can use 1 or 2
for drop can use 2 or 3
policy it: make each version options on the updates, then a dialect or
some other code can decide which behavior to use

in order to track dependencies:

create separate concept of constraints so we can handle table and
domain constraints the same (plus assertions?)

maybe have a concept of a sql body or something to model constraints,
as well as view definitions and function bodies referencing things via
sql statements syntax + column defaults

things to add to this catalog:
constraints
defaults
character set conversions? - if we want to type check string
  operations a bit better

== schemas

alter and drop schema can restrict or cascade to schema objects:
all other catalog supported objects appear in a schema apart from
  roles
rename schema just cascades automatically, there is no restrict option
  there can be a generic has dependencies or list dependencies
  function to implement a restrict like this
set default character set will affect the character set of new:
    table columns, domains, views (in some obscure cases), composite
    types
    no existing objects will be changed to match the new characterset/collation
      they must be altered separately
set schema owner will not cascade to objects in the schema, or will??
  not sure, maybe just objects which are owned by a role with the same
  name as the schema? add this as an option?
drop schema will not work if any objects owned by the schema on restrict
  it will drop them also if on cascade

== role

rename role cascades automatically, no options
drop role will restrict if there are any objects owned
cascade drop role will drop all owned items
there can be a util to show all owned items
and to reown them to another role easily

later will have role membership in other roles to do shared ownership
and group permissions, this will work slightly differently for drop:

a dropped role will simply be removed from any roles it is in without
affecting them: only option

a dropped role will other roles in it: those roles will just be
removed from this role. Restrict can be used to not allow dropping the
role

permissions for a being dropped role will always be dropped also

== character set

dependencies:
table column
domain
schema - default char set
database - default char set
collations - belong to a single character set
types: each character type can have a whitelist or blacklist of
  allowed character sets
views will depend only indirectly
composite types

rename:
always cascades

alter default collation:
doesn't affect the collation on any existing dependencies

drop restrict:
if character set is used anywhere above, restrict will stop the drop
  (apart from types blacklist or whitelist, this char set will be
  removed from these lists and the drop will succeed (if it has no
  other dependencies)
drop cascade:
will delete:
tables(!)
domains
schema will always block??
database will definitely always block: you must set another default
  char set to drop the current default one
collations
types: same as restrict, types are not dropped
composite types are dropped (could be very destructive!)

== collation

dependencies:
character set default collation
table and domain collation on char type, composite type also
collation on default char set for database and schemas

drop restrict:
any use of collation will stop the drop

drop cascade:
will drop the character set (!)
will drop tables, domains, and composites (!)
will drop schema (!) - maybe these behaviours should be same as character sets?
cannot drop database, so must restrict this

== sequence

dependencies:
tables
functions
views
domains? - could reference the sequence in the constraint?

drop restrict:
if the sequence is reference in any of the above, the drop is stopped

drop cascade:
views, functions dropped
option table can be dropped (!) or the defaults/constraints dropped
same with domain

== type category

dependencies:
types

a type can have the default category which means no category

drop restrict:
if used in any types, cannot drop

drop cascade:
option 1: types categories changed to default
option 2: types are dropped

== external types

dependencies:
domains
functions
tables
views
composite types

restrict: if used anywhere, drop prevented
cascade: all dependents dropped

== cast

dependencies:
can be used in constraints or views

restrict: drop stopped
cascade, drops constraint and views also

== functions, operators, windows, aggregates

each function can have a set of dependencies supplied by the calling
code this represents e.g. the tables, functions, etc. referenced in
the body of the function. This is to help with cascade/restrict
behaviour.

what can a function depend on? what can't a function directly depend
on:
itself
type category

that's it. Potentially anything else. Unusual (direct) dependencies
would be: schema, roles, constraints, defaults.


== tables

== constraints

== defaults
== views
== domain types
== composite types
== enums

