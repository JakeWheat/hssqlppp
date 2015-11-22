
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.Catalog
>     (catalog) where

> import Database.HsSqlPpp.Internals.Catalog.CatalogNew
> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Internals.TypeChecking.Environment
> --import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import qualified Data.Text.Lazy as T
> import qualified Test.Tasty.HUnit as H
> import qualified Data.Map as M
> import qualified Data.Set as S

> catalog :: Item
> catalog =
>     Group "catalog"
>     [

create role
query to see it there

>      Custom "create role" $ do
>          let Right c = updateCatalog (CreateRole "me") emptyCatalog
>          let rs = cRoles c
>          H.assertEqual "" (S.fromList ["me"]) rs
>          let Right c1 = updateCatalog (CreateRole "me2") c
>          let rs = cRoles c1
>          H.assertEqual "" (S.fromList ["me","me2"]) rs

drop role
query to see it gone

rename role
query to see old role gone and new role there
todo: check for stuff linked to role

create role already exists

drop role doesn't exist

rename role new already exists

rename role old doesn't exist

rename role old doesn't exist and new exists


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

>   ]
