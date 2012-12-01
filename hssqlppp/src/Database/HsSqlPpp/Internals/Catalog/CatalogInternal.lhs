
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


> {-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Internals.Catalog.CatalogInternal
>     (
>      -- catalog type plus values
>      Catalog
>     ,emptyCatalog
>     ,defaultCatalog
>     ,NameComponent(..)
>     ,ncStr
>     ,ncStrT
>     ,CompositeFlavour(..)
>     ,CatName
>      -- catalog updates
>     ,CatalogUpdate(..)
>     ,updateCatalog
>     ,deconstructCatalog
>      -- catalog queries
>     ,catLookupType
>     ,catLookupTableAndAttrs
>     ,catGetOpsMatchingName
>      -- temp stuff for old typeconversion
>     ,OperatorPrototype

>     ,CastContext(..)
>     ,catLookupFns
>     ,catPreferredType
>     ,isOperatorName
>     ,catTypeCategory
>     ,catCast
>     ,catCompositePublicAttrs
>     ,catDomainBaseType
>     ) where

>
> import Control.Monad
> --import Data.List
> import Data.Data
> import Data.Char
> import Data.Maybe

> import qualified Data.Map as M
> import qualified Data.Set as S
> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Utils.Utils
> import Data.Text (Text)
> import qualified Data.Text as T
> --import qualified Data.Text.Lazy as LT

-----------------------------------

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


> -- | represents the name of something in the catalog, when schema
> -- support is added then this will change to (String,String)
> type CatName = Text

> data CompositeFlavour = Composite | TableComposite | ViewComposite
>                         deriving (Eq,Ord,Show)

> -- | name, inparams, outtype, is variadic?
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
>                                 ,[(Text,CatName)] -- public attrs
>                                 ,[(Text,CatName)])-- system columns
>     ,catArrayTypes :: M.Map CatName CatName --pg array type name, base type name
>     ,catPrefixOps :: M.Map CatName [OperatorPrototype]
>     ,catPostfixOps :: M.Map CatName [OperatorPrototype]
>     ,catBinaryOps :: M.Map CatName [OperatorPrototype]
>     ,catFunctions :: M.Map CatName [OperatorPrototype]
>     ,catAggregateFunctions :: M.Map CatName [OperatorPrototype]
>     ,catWindowFunctions :: M.Map CatName [OperatorPrototype]
>     ,catTables :: M.Map CatName ([(Text,Type)] -- public attrs
>                                 ,[(Text,Type)]) -- system columns
>     -- needs more work:
>     ,catCasts :: S.Set (Type,Type,CastContext)
>     ,catTypeCategories :: M.Map Type (Text,Bool)
>      -- save the updates
>     ,catUpdates :: [CatalogUpdate]
>     }
>                deriving Show




> -- | Represents an empty catalog. This doesn't contain things
> -- like the \'and\' operator, 'defaultCatalog' contains these.
> emptyCatalog :: Catalog
> emptyCatalog = Catalog S.empty M.empty M.empty M.empty M.empty M.empty
>                        M.empty M.empty M.empty M.empty M.empty
>                        S.empty M.empty
>                        []
>
> -- | Represents what you probably want to use as a starting point if
> -- you are building an catalog from scratch. It contains
> -- information on built in function like things that aren't in the
> -- Postgres catalog, such as greatest, coalesce, keyword operators
> -- like \'and\', etc..
> defaultCatalog :: Catalog
> defaultCatalog =
>     -- todo: specify in terms of catalog updates
>   emptyCatalog {catBinaryOps = insertOperators systemBinaryOps M.empty
>                ,catPrefixOps = insertOperators systemPrefixOps M.empty
>                ,catPostfixOps = insertOperators systemPostfixOps M.empty
>                ,catFunctions = insertOperators systemFunctions M.empty}

> insertOperators :: [(CatName,OperatorPrototype)]
>                 -> M.Map CatName [OperatorPrototype]
>                 -> M.Map CatName [OperatorPrototype]
> insertOperators vs m =
>   foldr i m vs
>   where
>     i (k,v) = M.insertWith (++) k [v]

-------------------------------------------------------------

'system' stuff

bunch of operators which you can use but don't appear in the
postgresql catalog

> systemBinaryOps :: [(CatName,OperatorPrototype)]
> systemBinaryOps =
>    [("=", ("=",[Pseudo AnyElement, Pseudo AnyElement], typeBool, False))
>    ,("and",("and", [typeBool, typeBool], typeBool, False))
>    ,("or",("or", [typeBool, typeBool], typeBool, False))
>    ,("like",("like", [ScalarType "text", ScalarType "text"], typeBool, False))
>    ,("like",("like", [ScalarType "char", ScalarType "char"], typeBool, False))
>    ,("like",("like", [ScalarType "varchar", ScalarType "varchar"], typeBool, False))
>    ,("notlike",("notlike", [ScalarType "text", ScalarType "text"], typeBool, False))
>    ,("notlike",("notlike", [ScalarType "char", ScalarType "char"], typeBool, False))
>    ,("notlike",("notlike", [ScalarType "varchar", ScalarType "varchar"], typeBool, False))
>    ,("arrayctor",("arrayctor", [ArrayType $ Pseudo AnyElement], Pseudo AnyArray, True))
>    ,("between",("between", [Pseudo AnyElement
>                              ,Pseudo AnyElement
>                              ,Pseudo AnyElement], typeBool, False))
>    ,("substring",("substring",[ScalarType "text",typeInt,typeInt],ScalarType "text",False))
>    ,("substring",("substring",[ScalarType "varchar",typeInt,typeInt],ScalarType "varchar",False))
>    ,("substring",("substring",[ScalarType "char",typeInt,typeInt],ScalarType "char",False))
>    ,("arraysub",("arraysub", [Pseudo AnyArray,typeInt], Pseudo AnyElement, False))
>    ]

> systemPrefixOps :: [(CatName,OperatorPrototype)]
> systemPrefixOps =
>    [("not",("not", [typeBool], typeBool, False))]

> systemPostfixOps :: [(CatName,OperatorPrototype)]
> systemPostfixOps =
>    [("isnull",("isnull", [Pseudo AnyElement], typeBool, False))
>    ,("isnotnull",("isnotnull", [Pseudo AnyElement], typeBool, False))]

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
>     ,("record",Pseudo (Record Nothing))
>     ,("trigger",Pseudo Trigger)
>     ,("void",Pseudo Void)
>     ,("_cstring",ArrayType $ Pseudo Cstring)
>     ,("_record",ArrayType $ Pseudo (Record Nothing))
>     ,("internal",Pseudo Internal)
>     ,("language_handler", Pseudo LanguageHandler)
>     ,("opaque", Pseudo Opaque)
>     ,("fdw_handler", Pseudo FdwHandler)
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
>                    | AntiNameComponent String
>                      deriving (Data,Eq,Show,Typeable,Ord)
> -- this is a transition function
> -- it should be removed when ready, since all the code
> -- should be working with NameComponents directly
> ncStr :: NameComponent -> String
> ncStr (Nmc n) = map toLower n
> ncStr (QNmc n) = n
> ncStr (AntiNameComponent _n) =
>   error "tried to get the name component string of an anti name component"

> ncStrT :: NameComponent -> Text
> ncStrT (Nmc n) = T.pack $ map toLower n
> ncStrT (QNmc n) = T.pack n
> ncStrT (AntiNameComponent _n) =
>   error "tried to get the name component string of an anti name component"


todo: use left or something instead of error

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
>   | CatCreatePrefixOp CatName CatName CatName
>     -- | register a postfix op, opname, param type, return type
>   | CatCreatePostfixOp CatName CatName CatName
>     -- | register a binary op, opname, the two param types, return type
>   | CatCreateBinaryOp CatName CatName CatName CatName
>     -- | register a function: name, param types, retsetof, return type
>   | CatCreateFunction CatName [CatName] Bool CatName
>     -- | register a aggregate: name, param types, return type
>   | CatCreateAggregate CatName [CatName] CatName
>     -- | register a table only: name, (colname,typename) pairs
>   | CatCreateTable CatName [(CatName,CatName)]
>     -- | register a cast in the catalog
>   | CatCreateCast CatName CatName CastContext
>     -- | register a type category for a type (used in the implicit cast resolution)
>   | CatCreateTypeCategoryEntry CatName (Text,Bool)
>     deriving (Eq,Ord,Typeable,Data,Show)

> -- | Applies a list of 'CatalogUpdate's to an 'Catalog' value
> -- to produce a new Catalog value. TODO: there will be a split
> -- between the individual low level updates which just update
> -- one 'row' in the catalog type, and the high level updates
> -- which correspond to ddl (e.g. create type will also add the
> -- array type, create table will add a table, supply the
> -- private columns automatically, and add the composite type)
> -- highlevel not implemented yet
> updateCatalog :: [CatalogUpdate]
>               -> Catalog
>               -> Either [TypeError] Catalog
> updateCatalog eus cat' =
>   foldM updateCat' (cat' {catUpdates = catUpdates cat' ++ eus}) eus
>   where
>     updateCat' cat u = case u of
>       CatCreateScalarType n ->
>         if S.member n (catScalarTypeNames cat)
>         -- todo: need to check all the type lists
>         -- and maybe need to check the name doesn't conflict with pseudo names or something?
>         -- this should happen with other cases as well
>         -- also: needs to take into account alias, so int and int4 are
>         -- both disallowed for new types, and lookup of either finds int4
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
>         ltt <- catLookupType cat [QNmc $ T.unpack lt]
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catPrefixOps = insertOperators
>                                     [(n,(n,[ltt],rett,False))]
>                                     (catPrefixOps cat)}
>       CatCreatePostfixOp n rt ret -> do
>         rtt <- catLookupType cat [QNmc $ T.unpack rt]
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catPostfixOps = insertOperators
>                                      [(n,(n,[rtt],rett,False))]
>                                      (catPostfixOps cat)}
>       CatCreateBinaryOp n lt rt ret -> do
>         ltt <- catLookupType cat [QNmc $ T.unpack lt]
>         rtt <- catLookupType cat [QNmc $ T.unpack rt]
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catBinaryOps = insertOperators
>                                     [(n,(n,[ltt,rtt],rett,False))]
>                                     (catBinaryOps cat)}
>       CatCreateFunction n ps rs ret -> do
>         pst <- mapM (\nc -> catLookupType cat [QNmc $ T.unpack nc]) ps
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         let rett' = if rs
>                     then Pseudo $ SetOfType rett
>                     else rett
>         Right $ cat {catFunctions = insertOperators
>                                     [(n,(n,pst,rett',False))]
>                                     (catFunctions cat)}
>       CatCreateAggregate n ps ret -> do
>         pst <- mapM (\nc -> catLookupType cat [QNmc $ T.unpack nc]) ps
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catAggregateFunctions = insertOperators
>                                     [(n,(n,pst,rett,False))]
>                                     (catAggregateFunctions cat)}
>       CatCreateTable n cs -> do
>         cts <- mapM (\(cn,t) -> do
>                        t' <- catLookupType cat [QNmc $ T.unpack t]
>                        return (cn,t')) cs
>         Right $ cat {catTables = M.insert n (cts,[]) (catTables cat)}
>       CatCreateCast n0 n1 ctx -> do
>         t0 <- catLookupType cat [QNmc $ T.unpack n0]
>         t1 <- catLookupType cat [QNmc $ T.unpack n1]
>         Right $ cat {catCasts = S.insert (t0,t1,ctx) (catCasts cat)}
>       CatCreateTypeCategoryEntry n (c,p) -> do
>         t <- catLookupType cat [QNmc $ T.unpack n]
>         Right $ cat {catTypeCategories = M.insert t (c,p) $ catTypeCategories cat}

> deconstructCatalog :: Catalog -> [CatalogUpdate]
> deconstructCatalog = catUpdates

-----------------------------------------------------------

queries

> getCatName :: [NameComponent] -> CatName
> getCatName [] = error "empty name component in catalog code"
> getCatName ncs = case last ncs of
>                                Nmc n -> T.pack $ map toLower n
>                                QNmc n -> T.pack n


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


> -- | takes a table name, and returns the exact table name (to deal
> -- with quoting), and the public and private attr names
> catLookupTableAndAttrs :: Catalog
>                        -> [NameComponent]
>                        -> Either [TypeError] (Text,[(Text,Type)], [(Text,Type)])
> catLookupTableAndAttrs cat nmcs = do
>   let n = getCatName nmcs
>   (pu,pv) <- maybe (Left [UnrecognisedRelation n]) Right
>              $ M.lookup n (catTables cat)
>   return (n,pu,pv)



> catGetOpsMatchingName :: Catalog -> [NameComponent] -> [OperatorPrototype]
> catGetOpsMatchingName cat nmcs =
>   let nm = getCatName nmcs
>   in concatMap (\f -> fromMaybe [] $ M.lookup nm $ f cat)
>        [catPrefixOps
>        ,catPostfixOps
>        ,catBinaryOps
>        ,catFunctions
>        ,catAggregateFunctions
>        ,catWindowFunctions]

the TypeConversion module handles checking assignment compatibility,
'resolving result set types', and finding function call matches since
this relies on some heavy algorithms to match postgress really complex
overloading and implicit cast system.


--------------------------------------------------------

old stuff chucked in to support the old typeconversion, to be promoted
to new code or deleted as typeconversion is rewritten


> -- | Use to note what the flavour of a cast is, i.e. if/when it can
> -- be used implicitly.
> data CastContext = ImplicitCastContext
>                  | AssignmentCastContext
>                  | ExplicitCastContext
>                    deriving (Eq,Show,Ord,Typeable,Data)

> catCompositePublicAttrs :: Catalog -> [CompositeFlavour] -> Text
>                   -> Either [TypeError] [(Text,Type)]
> catCompositePublicAttrs cat _flvs ty = do
>    (_,a,_) <- catLookupTableAndAttrs cat [Nmc $ T.unpack ty]
>    return a

> catPreferredType :: Catalog -> Type -> Either [TypeError] Bool
> catPreferredType cat ty =
>   fmap snd $ catGetCategoryInfo cat ty
>
> catCast :: Catalog -> CastContext -> Type -> Type -> Either [TypeError] Bool
> catCast cat ctx from to =
>     case from of
>       t@(DomainType _) -> do
>                 baseType <- catDomainBaseType cat t
>                 cc <- catCast cat ctx baseType to
>                 return $ (baseType == to) ||
>                                (cc || S.member (from, to, ctx) (catCasts cat))
>       _ -> Right $ S.member (from, to, ctx) (catCasts cat)
>
> catDomainBaseType :: Catalog -> Type -> Either [TypeError] Type
> catDomainBaseType cat (ScalarType ty) =
>   case M.lookup ty $ catDomainTypes cat of
>     Just n -> Right $ ScalarType n
>     Nothing -> Left [DomainDefNotFound $ ScalarType ty]
> catDomainBaseType _cat ty = Left [DomainDefNotFound ty]
>
> catLookupFns :: Catalog -> Text -> [OperatorPrototype]
> catLookupFns cat name =
>    catGetOpsMatchingName cat [Nmc $ T.unpack name]

> catTypeCategory :: Catalog -> Type -> Either [TypeError] Text
> catTypeCategory cat ty =
>   fmap fst $ catGetCategoryInfo cat ty

> isOperatorName :: Text -> Bool
> isOperatorName = T.any (`elem` "+-*/<>=~!@#%^&|`?.")

> catGetCategoryInfo :: Catalog -> Type -> Either [TypeError] (Text, Bool)
> catGetCategoryInfo cat ty =
>   case ty of
>     Pseudo (SetOfType _) -> Right ("", False)
>     AnonymousCompositeType _ -> Right ("", False)
>     ArrayType (Pseudo _) -> Right ("A",False)
>     Pseudo _ -> Right ("P",False)
>     _ -> case M.lookup ty $ catTypeCategories cat of
>            Nothing -> Left [InternalError $ "no type category for " ++ show ty]
>            Just x -> Right x
