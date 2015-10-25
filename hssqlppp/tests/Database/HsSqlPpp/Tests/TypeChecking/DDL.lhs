
This file does basic typechecking for DDL. It only covers some ddl at
the moment.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.DDL
>     (ddl) where

> --import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Types

> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> ddl :: Item
> ddl =
>   Group "ddl" []


create schema
alter schema
drop schema



create table
alter table
drop table

create sequence
alter sequence
drop sequence

create view
drop view
alter view

