
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> module Database.HsSqlPpp.Tests.TypeChecking.Insert
>     (tcInsertTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> --import Database.HsSqlPpp.Catalog

> tcInsertTestData :: Item
> tcInsertTestData =
>   Group "insert" [
>       s "insert into nope (a,b) values (c,d);"
>         $ Left [UnrecognisedRelation "nope"
>                ,UnrecognisedIdentifier "c"
>                ,UnrecognisedIdentifier "d"]
>      ,s "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         $ Right [Just $ ([], [] {-InsertInfo "pg_attrdef"
>                           [("adrelid",ScalarType "oid")
>                           ,("adnum",ScalarType "int2")
>                           ,("adbin",ScalarType "text")
>                           ,("adsrc",ScalarType "text")]-})]
>      ,s "insert into pg_attrdef\n\
>         \values (1,2, 'a', 'c');"
>         $ Right [Just $ ([], [] {-InsertInfo "pg_attrdef"
>                           [("adrelid",ScalarType "oid")
>                           ,("adnum",ScalarType "int2")
>                           ,("adbin",ScalarType "text")
>                           ,("adsrc",ScalarType "text")]-})]
>      ,s "insert into pg_attrdef (hello,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         $ Left [UnrecognisedIdentifier "hello"]
>      ,s "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,true, 'a', 'b');"
>         $ Left [IncompatibleTypes (ScalarType "int2") (ScalarType "bool")]
>      ,s "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,true, 'a', 'b','c');"
>         $ Left [WrongNumberOfColumns]
>      ]

>  where
>    --e = Expr
>    s = StmtType
>    --c = CatStmtType
>    --d = Ddl

