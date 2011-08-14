
> module Database.HsSqlPpp.Tests.TypeChecking.Delete
>     (tcDeleteTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types

> tcDeleteTestData :: Item
> tcDeleteTestData =
>   Group "delete" [
>       s "delete from nope;"
>         $ Left [UnrecognisedRelation "nope"]
>      ,s "delete from pg_attrdef where 1=2;"
>         $ Right [Just ([], [])]
>      ,s "delete from pg_attrdef where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,s "delete from pg_attrdef where adsrc='';"
>         $ Right [Just $ ([], [])]
>      ]

>  where
>    s = StmtType
