
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> module Database.HsSqlPpp.Tests.TypeChecking.Delete
>     (tcDeleteTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> --import Database.HsSqlPpp.Catalog

> tcDeleteTestData :: Item
> tcDeleteTestData =
>   Group "delete" [
>       s "delete from nope;"
>         $ Left [UnrecognisedRelation "nope"]
>      ,s "delete from pg_attrdef where 1=2;"
>         $ Right [Just $ ([], [])]
>      ,s "delete from pg_attrdef where 1;"
>         $ Left [ExpressionMustBeBool]
>         -- FIXME
>      --,s "delete from pg_attrdef where adsrc='';"
>      --   $ Right [Just $ ([], [])]
>      ]

>  where
>    --e = Expr
>    s = StmtType
>    --c = CatStmtType
>    --d = Ddl

