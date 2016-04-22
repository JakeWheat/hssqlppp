
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.Catalog
>     (catalog) where

> import Database.HsSqlPpp.Internals.Catalog.CatalogNew
> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Internals.TypeChecking.Environment
> --import Database.HsSqlPpp.Tests.TypeChecking.Utils
> --import qualified Data.Text.Lazy as T
> import qualified Test.Tasty.HUnit as H
> --import qualified Data.Map as M
> import qualified Data.Set as S
> import Control.Monad

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
>          let rs1 = cRoles c1
>          H.assertEqual "" (S.fromList ["me","me2"]) rs1

drop role
query to see it gone

>     ,Custom "drop role" $ do
>          let Right c = foldM (flip updateCatalog)
>                              emptyCatalog
>                              [CreateRole "me"
>                              ,CreateRole "me2"]
>              Right c1 = updateCatalog (DropRole "me" Restrict) c
>          let rs = cRoles c1
>          H.assertEqual "" (S.fromList ["me2"]) rs
>          let Right c2 = updateCatalog (DropRole "me2" Restrict) c
>          let rs1 = cRoles c2
>          H.assertEqual "" (S.fromList ["me"]) rs1


rename role
query to see old role gone and new role there

>     ,Custom "rename role" $ do
>          let Right c = foldM (flip updateCatalog)
>                              emptyCatalog
>                              [CreateRole "me"
>                              ,CreateRole "me2"]
>              Right c1 = updateCatalog (RenameRole "me" "anotherme") c
>          let rs = cRoles c1
>          H.assertEqual "" (S.fromList ["anotherme","me2"]) rs



todo: check for stuff linked to role



create role already exists

>     ,Custom "create role already exists" $ do
>          let Right c = updateCatalog (CreateRole "me") emptyCatalog
>          H.assertEqual "" (Left $ RoleAlreadyExists "me") $ updateCatalog (CreateRole "me") c

drop role doesn't exist

>     ,Custom "drop role doesn't exist" $ do
>          let Right c = updateCatalog (CreateRole "me") emptyCatalog
>          H.assertEqual "" (Left $ RoleNotRecognised "me1")
>               $ updateCatalog (DropRole "me1" Restrict) c

rename role new already exists

rename role old doesn't exist

rename role old doesn't exist and new exists


collation and character sets

create character set
check it is there
check the default collation exists and is binary

create character set
create a collation
check collation is there

create character set
create a collation
set it as default
delete the binary collation
check collation is there and is default and binary isn't there

create two collations and check

create two collations and drop one

drop a character set with cascade
     with restrict

rename character set

rename collation - default
rename collation - non-default

renames with schema change only

set character set owner
set collation owner

anomalies for above...

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
