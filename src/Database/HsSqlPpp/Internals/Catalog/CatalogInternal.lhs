
This module contains the implementation of the Catalog data types
and functions, and provides the api for the other type checking
modules.

NEW CATALOG!

create new types from scratch
1st version only supports checking type names
fix the read catalog code
continue to ignore schemas
start handling case sensitivity properly
ignore modifiers

= Catalog overview

The main purpose of the catalog is to support typechecking. A
secondary purpose is to be able to support documentation generation.

== What information does the catalog contain?

types:

base types
names of all types
domains
casts
type categories (what is this?)

tables, views, composite types

operators:
  prefix ops
  postfix ops
  binary ops
  regular fns
  aggregate fns
  window fns


triggers

indexes

sequences


> {-# LANGUAGE DeriveDataTypeable #-}
>
> module Database.HsSqlPpp.Internals.Catalog.CatalogInternal
>     (
>      -- catalog type plus values
>      Catalog
>     ,emptyCatalog
>     ,defaultCatalog
>     ,NameComponent(..)
>     ,ncStr
>     ,CompositeFlavour(..)
>     ,CatName
>      -- catalog updates
>     ,CatalogUpdate(..)
>     ,updateCatalog
>      -- catalog queries
>     ,catLookupType
>     ) where
>
> import Control.Monad
> import Data.List
> import Data.Data
> import Data.Char

> import qualified Data.Map as M
> import qualified Data.Set as S
> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Utils.Utils

-------------------------------------------------------------

types:

What information does the catalog store and use on types?

The basic set of types recorded here are:

scalar types - the base types implemented outside of plpgsql

domain types - this is a base type with a constraint (can composite
types be used in a domain?)

enum types - the usual. The values are strings and are case
sensitive. I think you can overload the values so they can be members
of more than one enum type

named composite types - these are structs, created using 'create type
x as', and also a composite type is implicitly created for each table
and view definition with the same name as the table or view

array types - not sure exactly how to handle this, generally, postgres
automatically creates an array type for each scalar, domain, composite
and enum type (any others?), and you can't use arrays of something if
the type of the array isn't in the catalog. So lots of other types
can't be made into arrays (what about arrays of arrays? don't
know). Hssqlppp simply assumes that the array type of any type is
available


None of the types apart from these exist in the same way in postgres -
so unnamed compositetypes, anonymous record type, unknown and pseudo
type are used in less contexts. Maybe one way of looking at them is to
consider them more like type generators which exist simply when they
are used, and don't have to be declared up front. Only the above
privileged types can be used for the types of columns in tables and
views (I think?).

for scalar types, no information is used apart from the fact that a
type with the given name exists and can be referred to.

for domain types, there is also the base type name, and the check
constraint

for enum types, name and the list of labels

for named composite types, you need the name of the type, and the
names and types of the fields

for array types, you only need the base type name, array types don't
have their own separate name

---------------------------------------------------------------

operators

operators is used as the generic term for all sorts of function things:
prefix, postfix, binary operators, functions, aggregate functions and
window functions

The information currently stored is:
name, parameter types, return type and variadic flag

---------------------------------------

catalog values


> -- | represents the name of something in the catalog, maybe in the future
> -- this will include the schema.
> type CatName = String

> data CompositeFlavour = Composite | TableComposite | ViewComposite
>                         deriving (Eq,Ord,Show)

> type OperatorPrototype = (CatName, [Type], Type, Bool)

> -- | The main datatype, this holds the catalog and context
> -- information to type check against.
> data Catalog = Catalog
>     {catScalarTypeNames :: S.Set CatName -- one name component per type
>     ,catDomainTypes :: M.Map CatName CatName -- stores the base type name
>                                              -- constraint is stored separately
>      --,catEnumTypes :: {[(String,[String])]}
>     ,catCompositeTypes :: M.Map CatName
>                                 (CompositeFlavour
>                                 ,[(String,CatName)] -- public attrs
>                                 ,[(String,CatName)])-- system columns
>     ,catArrayTypes :: M.Map CatName CatName --pg array type name, base type name
>     ,catPrefixOps :: M.Map CatName OperatorPrototype
>     ,catPostfixOps :: M.Map CatName OperatorPrototype
>     ,catBinaryOps :: M.Map CatName OperatorPrototype
>     ,catFunctions :: M.Map CatName OperatorPrototype
>     ,catAggregateFunctions :: M.Map CatName OperatorPrototype
>     ,catWindowFunctions :: M.Map CatName OperatorPrototype
>     ,catUpdates :: [CatalogUpdate]
>     }
>                deriving Show




> -- | Represents an empty catalog. This doesn't contain things
> -- like the \'and\' operator, 'defaultCatalog' contains these.
> emptyCatalog :: Catalog
> emptyCatalog = Catalog S.empty M.empty M.empty M.empty M.empty M.empty
>                        M.empty M.empty M.empty M.empty []
>
> -- | Represents what you probably want to use as a starting point if
> -- you are building an catalog from scratch. It contains
> -- information on built in function like things that aren't in the
> -- Postgres catalog, such as greatest, coalesce, keyword operators
> -- like \'and\', etc..
> defaultCatalog :: Catalog
> defaultCatalog =
>     -- todo: specify in terms of catalog updates
>   emptyCatalog {catBinaryOps = M.fromList systemBinaryOps
>                ,catPrefixOps = M.fromList systemPrefixOps
>                ,catPostfixOps = M.fromList systemPostfixOps
>                ,catFunctions = M.fromList systemFunctions}

-------------------------------------------------------------

'system' stuff

bunch of operators which you can use but don't appear in the
postgresql catalog

> systemBinaryOps :: [(CatName,OperatorPrototype)]
> systemBinaryOps =
>    [("=", ("=",[Pseudo AnyElement, Pseudo AnyElement], typeBool, False))
>    ,("!and",("!and", [typeBool, typeBool], typeBool, False))
>    ,("!or",("!or", [typeBool, typeBool], typeBool, False))
>    ,("!like",("!like", [ScalarType "text", ScalarType "text"], typeBool, False))
>    ,("!like",("!like", [ScalarType "char", ScalarType "char"], typeBool, False))
>    ,("!like",("!like", [ScalarType "varchar", ScalarType "varchar"], typeBool, False))
>    ,("!notlike",("!notlike", [ScalarType "text", ScalarType "text"], typeBool, False))
>    ,("!notlike",("!notlike", [ScalarType "char", ScalarType "char"], typeBool, False))
>    ,("!notlike",("!notlike", [ScalarType "varchar", ScalarType "varchar"], typeBool, False))
>    ,("!arrayctor",("!arrayctor", [ArrayType $ Pseudo AnyElement], Pseudo AnyArray, True))
>    ,("!between",("!between", [Pseudo AnyElement
>                              ,Pseudo AnyElement
>                              ,Pseudo AnyElement], typeBool, False))
>    ,("!substring",("!substring",[ScalarType "text",typeInt,typeInt],ScalarType "text",False))
>    ,("!substring",("!substring",[ScalarType "varchar",typeInt,typeInt],ScalarType "varchar",False))
>    ,("!substring",("!substring",[ScalarType "char",typeInt,typeInt],ScalarType "char",False))
>    ,("!arraysub",("!arraysub", [Pseudo AnyArray,typeInt], Pseudo AnyElement, False))
>    ]

> systemPrefixOps :: [(CatName,OperatorPrototype)]
> systemPrefixOps =
>    [("!not",("!not", [typeBool], typeBool, False))]

> systemPostfixOps :: [(CatName,OperatorPrototype)]
> systemPostfixOps =
>    [("!isnull",("!isnull", [Pseudo AnyElement], typeBool, False))
>    ,("!isnotnull",("!isnotnull", [Pseudo AnyElement], typeBool, False))]

> systemFunctions :: [(CatName, OperatorPrototype)]
> systemFunctions =
>  [("coalesce",("coalesce", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement, True))
>  ,("nullif", ("nullif",[Pseudo AnyElement, Pseudo AnyElement], Pseudo AnyElement,False))
>  ,("greatest",("greatest", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement,True))
>  ,("least",("least", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement,True))
>  ]

names to refer to the pseudo types

> pseudoTypes :: M.Map CatName Type
> pseudoTypes = M.fromList
>     [("any",Pseudo Any)
>     ,("anyarray",Pseudo AnyArray)
>     ,("anyelement",Pseudo AnyElement)
>     ,("anyenum",Pseudo AnyEnum)
>     ,("anynonarray",Pseudo AnyNonArray)
>     ,("cstring",Pseudo Cstring)
>     ,("record",Pseudo Record)
>     ,("trigger",Pseudo Trigger)
>     ,("void",Pseudo Void)
>     ,("_cstring",ArrayType $ Pseudo Cstring)
>     ,("_record",ArrayType $ Pseudo Record)
>     ]



---------------------------------------------------------

name component - this represents quoted and unquoted
possibly-qualified names (so names of things are lists of
namecomponents). Perhaps should be a syntactic namecomponent which is
in AstInternal, and a semantic namecomponent which is used here, but I
am lazy so the same type is shared.

The name components are only used here so that the logic for ignoring
or respecting case is in one place, these are only used in the query
functions and not in catalog values themselves.

> data NameComponent = Nmc String
>                    | QNmc String -- quoted
>                      deriving (Data,Eq,Show,Typeable)
> -- this is a transition function
> -- it should be removed when ready, since all the code
> -- should be working with NameComponents directly
> ncStr :: NameComponent -> String
> ncStr (Nmc n) = n
> ncStr (QNmc n) = n

------------------------------------------------------

 updates

> data CatalogUpdate =
>     -- | register a base scalar type with the given name
>     CatCreateScalarType CatName
>     -- | register a domain type with name and base type
>   | CatCreateDomainType CatName CatName
>     -- | register an array type with name and base type
>   | CatCreateArrayType CatName CatName
>     -- | register a prefix op, opname, param type, return type
>   | CatCreatePrefixOp CatName CatName CatName --op name, argtypename, rettypename
>     -- | register a postfix op, opname, param type, return type
>   | CatCreatePostfixOp CatName CatName CatName
>     -- | register a binary op, opname, the two param types, return type
>   | CatCreateBinaryOp CatName CatName CatName CatName
>     deriving (Eq,Ord,Typeable,Data,Show)

> -- | Applies a list of 'CatalogUpdate's to an 'Catalog' value
> -- to produce a new Catalog value.
> updateCatalog :: Catalog
>               -> [CatalogUpdate]
>               -> Either [TypeError] Catalog
> updateCatalog cat' eus =
>   foldM updateCat' (cat' {catUpdates = catUpdates cat' ++ eus}) eus
>   where
>     updateCat' cat u = case u of
>       CatCreateScalarType n ->
>         if S.member n (catScalarTypeNames cat)
>         -- todo: need to check all the type lists
>         -- and maybe need to check the name doesn't conflict with pseudo names or something?
>         -- this should happen with other cases as well
>         then Left [InternalError $ "type already exists: " ++ show n]
>         else Right $ cat {catScalarTypeNames = S.insert n (catScalarTypeNames cat)}
>       CatCreateDomainType n b ->
>         Right $ cat {catDomainTypes = M.insert n b (catDomainTypes cat)}
>       CatCreateArrayType n b ->
>         Right $ cat {catArrayTypes = M.insert n b (catArrayTypes cat)}
>       -- todo: check the uniqueness of operator names (can overload by type)
>       -- also check the name of the operator is a valid operator name
>       -- and that the op has the correct number of args (1 or 2 resp.)
>       CatCreatePrefixOp n lt ret -> do
>         ltt <- catLookupType cat [QNmc lt]
>         rett <- catLookupType cat [QNmc ret]
>         Right $ cat {catPrefixOps = M.insert n (n,[ltt],rett,False) (catPrefixOps cat)}
>       CatCreatePostfixOp n rt ret -> do
>         rtt <- catLookupType cat [QNmc rt]
>         rett <- catLookupType cat [QNmc ret]
>         Right $ cat {catPostfixOps = M.insert n (n,[rtt],rett,False) (catPostfixOps cat)}
>       CatCreateBinaryOp n lt rt ret -> do
>         ltt <- catLookupType cat [QNmc lt]
>         rtt <- catLookupType cat [QNmc rt]
>         rett <- catLookupType cat [QNmc ret]
>         Right $ cat {catBinaryOps = M.insert n (n,[ltt,rtt],rett,False) (catBinaryOps cat)}

-----------------------------------------------------------

queries

> getCatName :: [NameComponent] -> CatName
> getCatName [] = error "empty name component in catalog code"
> getCatName ncs = case last ncs of
>                                Nmc n -> map toLower n
>                                QNmc n -> n


> -- | takes a [NameComponent] and returns the type for that name
> -- will return a type not recognised if the type isn't in the catalog
> catLookupType :: Catalog -> [NameComponent] -> Either [TypeError] Type
> catLookupType cat ncs =
>   case canonicalizeTypeName $ getCatName ncs of
>     -- check if is a pseudo type
>     cn | Just p <- M.lookup cn pseudoTypes -> Right p
>     -- check for base, domain, enum or composite, and array
>        | S.member cn (catScalarTypeNames cat) -> Right $ ScalarType cn
>        | M.member cn (catDomainTypes cat) -> Right $ DomainType cn
>        | M.member cn (catCompositeTypes cat) -> Right $ NamedCompositeType cn
>        | Just t <- M.lookup cn (catArrayTypes cat) -> Right $ ArrayType $ ScalarType t
>        | otherwise -> Left [UnknownTypeName cn]





the TypeConversion module handles checking assignment compatibility,
'resolving result set types', and finding function call matches since
this relies on some heavy algorithms to match postgress really complex
overloading and implicit cast system.

