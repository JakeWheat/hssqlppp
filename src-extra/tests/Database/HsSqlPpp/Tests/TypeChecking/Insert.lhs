
> module Database.HsSqlPpp.Tests.TypeChecking.Insert
>     (tcInsertTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types

> tcInsertTestData :: Item
> tcInsertTestData =
>   Group "insert" [
>       s "insert into nope (a,b) values (c,d);"
>         $ Left [UnrecognisedRelation "nope"
>                ,UnrecognisedIdentifier "c"
>                ,UnrecognisedIdentifier "d"]
>      ,s "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         $ Right [Just ([], [])]
>      ,s "insert into pg_attrdef\n\
>         \values (1,2, 'a', 'c');"
>         $ Right [Just ([], [])]
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
>    s = StmtType
