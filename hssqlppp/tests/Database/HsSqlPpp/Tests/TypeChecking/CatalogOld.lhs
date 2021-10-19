
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.CatalogOld
>     (catalogOld) where

> import Database.HsSqlPpp.Internals.Catalog.CatalogBuilder
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> import Database.HsSqlPpp.Tests.TestTypes
> import qualified Test.Tasty.HUnit as H
> import qualified Data.Map as M
> import qualified Data.Set as S
> import Control.Monad

> catalogOld :: Item
> catalogOld =
>     Group "catalogOld"
>     [

create table
query to see it there

>      Custom "create table" $ do
>          let tableId = ("public", "t1")
>          let Right c = updateCatalog [CatCreateTable tableId []] emptyCatalog
>          let tm = catTables c
>          H.assertEqual "" (M.fromList [(tableId,([],[]))]) tm
>          let tableId1 = ("public", "t2")
>          let Right c1 = updateCatalog [CatCreateTable tableId1 []] c
>          let tm1 = catTables c1
>          H.assertEqual "" (M.fromList [(tableId,([],[])), (tableId1,([],[]))]) tm1

drop table
query to see it gone

>     ,Custom "drop table" $ do
>          let Right c = updateCatalog
>                              [CatCreateTable ("public", "t") []
>                              ,CatCreateTable ("public", "t1") []]
>                              emptyCatalog
>              Right c1 = updateCatalog [CatDropTable ("public","t")] c
>          let tm = catTables c1
>          H.assertEqual "" (M.fromList [(("public","t1"),([],[]))]) tm
>          let Right c2 = updateCatalog [CatDropTable ("public","t1")] c
>          let tm1 = catTables c2
>          H.assertEqual "" (M.fromList [(("public","t"),([],[]))]) tm1

>   ]
