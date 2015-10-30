
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
>   Group "updates"
>   [Group "tcinsert"
>   [

simplest insert

>    tcStatements simpleTEnv
>    "insert into t values (1,'2');"
>    $ Nothing

too many values

>   ,tcStatements simpleTEnv
>    "insert into t values (1,'2',3);"
>    $ Just [TooManyColumnsInInsert]

too few values: this is ok, not currently statically checked

>   ,tcStatements simpleTEnv
>    "insert into t values (1);"
>    $ Nothing


bad types in one row of multi values
todo: I'm not sure this is correct.

For an insert, I think you should see if there is an assignment cast
available on a row by row basis, not use the resolve result set on
each value row. For values in every other context, I think you should
use the resolve result set. This will change the error message when it
fails, and probably give different results in some unusual scenarios.

>   ,tcStatements simpleTEnv
>    "insert into t values (1,'2'), ('1'::text,2);"
>    $ Just [IncompatibleUnionTypes
>           -- todo: how should the column names work?
>           -- todo: check nullability
>           -- why does the unknown type string literal already have a precision of 1?
>           (CompositeType [("", (mkTypeExtra typeInt) {teNullable=False})
>                          ,("", (mkTypeExtra UnknownType) {teNullable=False,tePrecision=Just 1})])
>           (CompositeType [("values%0", (mkTypeExtra $ ScalarType "text") {teNullable=False})
>                          ,("values%1", (mkTypeExtra typeInt) {teNullable=False})])]

>   ,tcStatements simpleTEnv
>    "insert into t values ('1'::text,2), (1,'2');"
>    $ Just [IncompatibleUnionTypes
>           (CompositeType [("", (mkTypeExtra $ ScalarType "text") {teNullable=False})
>                          ,("", (mkTypeExtra typeInt) {teNullable=False})])
>           (CompositeType [("values%0", (mkTypeExtra typeInt) {teNullable=False})
>                          ,("values%1", (mkTypeExtra UnknownType) {teNullable=False,tePrecision=Just 1})])]

non existent table

>   ,tcStatements simpleTEnv
>    "insert into zt values (1,'2');"
>    $ Just [UnrecognisedRelation ("public","zt")]

table with explicit schema

>   ,tcStatements simpleTEnv
>    "insert into public.t values (1,'2');"
>    $ Nothing

table with wrong explicit schema

>   ,tcStatements simpleTEnv
>    "insert into something.t values (1,'2');"
>    $ Just [UnrecognisedRelation ("something","t")]

name all columns

>   ,tcStatements simpleTEnv
>    "insert into t(a,b) values (1,'2');"
>    $ Nothing

name columns in different order

>   ,tcStatements simpleTEnv
>    "insert into t(b,a) values ('2'::text,1);"
>    $ Nothing

too many values for the named columns

>   ,tcStatements simpleTEnv
>    "insert into t(a) values (1,'2');"
>    $ Just [TooManyColumnsInInsert]

>   ,tcStatements simpleTEnv
>    "insert into t(a,b) values (1,'2',3);"
>    $ Just [TooManyColumnsInInsert]

The typechecker will never catch this issue for now, even if it is
statically determinable that the insert will fail because of defaults/
not null.

>   ,tcStatements simpleTEnv
>    "insert into t(a) values (1);"
>    $ Nothing

name wrong column

>   ,tcStatements simpleTEnv
>    "insert into t(a,c) values (1,'2');"
>    $ Just [UnrecognisedIdentifier "c"]

duplicate columns

>   ,tcStatements simpleTEnv
>    "insert into t(a,b,a) values (1,'2',1);"
>    $ Just [DuplicateColumnName "a"]

***todo: implicit casts
make sure to check casts which are assignment and not implicit
also works with no column names given

1. implicit casts from literals: already tested

2. implicit casts from typed expressions

>   --,tcStatements simpleTEnv
>   -- "insert into t(a,b) values (1::int8,'2');"
>   -- $ Nothing

3. casts which are only available explicitly: for now treated no
different to casts which aren't possible at all, in future could give
a nicer error message

>   --,tcStatements simpleTEnv
>   -- "insert into t(a,b) values ('1'::text,'2');"
>   -- $ Just []

4. casts which aren't possible at all

>   --,tcStatements simpleTEnv
>   -- "insert into t(a,b) values ('2005-01-01'::interval,'2');"
>   -- $ Just []


*** todo: repeat tests above with all the other queryexpr ctors:
select, with, setops

*** todo: check multiple values rows use resolve result set algo
outside of insert, and use row by row assignment casts in an insert

todo: more checking with presence of defaults/nulls?

todo: returning

see also InsertQueryExprs.lhs for some typechecking tests for insert

= update

regular update
row style update
wrong table name
explicit schema
qualified assignment targets, good and bad
wrong column name
simple where
where with wrong col
other type check fail in where

TODO: from list, returning

= delete

simple delete
with schema
bad table name
bad schema
simple where
where with wrong col
where with non bool

todo: using and returning

= copy from

can't check the copy from source, but can check the columns

simple copy from
with column names
different order column names
missing columns
non existent columns
repeated column
schema table name
bad table name
bad schema

= copy to

check table name and column names

check simple good and bad queries

= truncate

check table names + schema options

>   ]
>   ]
>   where
>     simpleTEnv = [CatCreateTable ("public","t")
>                   [("a", mkCatNameExtra "int4")
>                   ,("b", mkCatNameExtra "text")]]
>     _anotherUEnv = [CatCreateTable ("something","u")
>                   [("a", mkCatNameExtra "int4")
>                   ,("b", mkCatNameExtra "text")]]
>     tcStatements cus =
>         let cat = makeCatalog PostgreSQL cus defaultTemplate1Catalog
>         in TCStatements cat defaultTypeCheckFlags
