
This file tests the basic typechecking for non query dml (aka updates).

insert
update
delete
copy from
copy to
truncate


> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.Updates
>     (updates) where

> --import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Types

> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> updates :: Item
> updates =
>   Group "updates" []
