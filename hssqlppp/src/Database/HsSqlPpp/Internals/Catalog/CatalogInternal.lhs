
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
>     (catLookupType
>     ,catLookupTableAndAttrs
>     ,catGetOpsMatchingName
>      -- temp stuff for old typeconversion
>     ,catLookupFns
>     ,catPreferredType
>     ,isOperatorName
>     ,catTypeCategory
>     ,catCast
>     ,catCompositePublicAttrs
>     ,catDomainBaseType
>     ,typeToCatName
>     ) where

>
> --import Control.Monad
> --import Data.List
> --import Data.Data
> --import Data.Char
> import Data.Maybe

> import qualified Data.Map as M
> import qualified Data.Set as S
> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Utils.Utils
> import Data.Text (Text)
> import qualified Data.Text as T
> --import qualified Data.Text.Lazy as LT

> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> --import Database.HsSqlPpp.Internals.Catalog.BaseCatalog
> import Database.HsSqlPpp.Internals.Catalog.CatalogUtils

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

todo: where should this utility live? Probably should be connected 
to the dialects

> typeToCatName :: TypeExtra -> Either [TypeError] CatNameExtra
> typeToCatName te = case teType te of
>   ScalarType t -> return
>       $ CatNameExtra t (tePrecision te) (teScale te) (teNullable te)
>   _ -> Left [InternalError "typeToCatName on a non scalar type"]

-----------------------------------------------------------

queries


gets a schema qualified catname, puts in the default 'public' if there
is only one name component. This will be altered when schema search
paths are implemented.

> getCatName2 :: [NameComponent] -> (CatName,CatName)
> -- todo: don't use error here
> getCatName2 [] = error "empty name component in catalog code"
> getCatName2 [a] = ("public",ncStrT a)
> getCatName2 [a,b] = (ncStrT a, ncStrT b)
> getCatName2 (_:xs) = getCatName2 xs


TODO: add inverse of this operation, give a type, returns a typename


> -- | takes a table name, and returns the exact table name (to deal
> -- with quoting), and the public and private attr names
> catLookupTableAndAttrs :: Catalog
>                        -> [NameComponent]
>                        -> Either [TypeError] ((Text,Text),[(Text,TypeExtra)], [(Text,Type)])
> catLookupTableAndAttrs cat nmcs = do
>   let n = getCatName2 nmcs
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



> catCompositePublicAttrs :: Catalog -> [CompositeFlavour] -> Text
>                   -> Either [TypeError] [(Text,TypeExtra)]
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
> isOperatorName = T.any (`elem` ("+-*/<>=~!@#%^&|`?."::String))

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
