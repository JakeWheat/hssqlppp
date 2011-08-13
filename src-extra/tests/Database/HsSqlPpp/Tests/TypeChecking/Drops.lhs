
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> module Database.HsSqlPpp.Tests.TypeChecking.Drops
>     (tcDropsTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> import Database.HsSqlPpp.Catalog

--------------------------------------------------------------------------------


> tcDropsTestData :: Item
> tcDropsTestData = --fixme
>   Group "drop stuff" [
>       d "create function test(a int) returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql;"
>         [CatCreateFunction FunName "test" [typeInt] (Pseudo Void) False]
>      ,d "create function test(a int) returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql;\n\
>         \drop function test(int);"
>         []
>      ,d "drop function test(int);" -- this should fail but doesn't
>         []
>      ,d "drop function if exists test(int);"
>         []
>      ]

>  where
>    --e = Expr
>    --s = StmtType
>    --c = CatStmtType
>    d = Ddl

