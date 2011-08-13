
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> module Database.HsSqlPpp.Tests.TypeChecking.CatalogChaining
>     (tcCatalogChainingTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> --import Database.HsSqlPpp.Catalog

> tcCatalogChainingTestData :: Item
> tcCatalogChainingTestData =
>   Group "check catalog chaining" [
>
>     -- create function then select
>     -- select then create function
>     -- then in two separate chained asts
>
>       s "create function t1() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;\n\
>         \select t1();"
>         (Right [Nothing,Just (([], []))])
>      ,s "select t1();\n\
>         \create function t1() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [NoMatchingOperator "t1" []])
>      ]
>
>   {-,Group "check catalog chaining2" [ StatementTypes [
>       p ["create function t1() returns void as $$\n\
>          \begin\n\
>          \  null;\n\
>          \end;\n\
>          \$$ language plpgsql stable;"
>         ,"select t1();"]
>         (Right [Just (SelectInfo (Pseudo Void))])
>      ,p ["select t1();"
>         ,"create function t1() returns void as $$\n\
>          \begin\n\
>          \  null;\n\
>          \end;\n\
>          \$$ language plpgsql stable;"]
>         (Left [NoMatchingOperator "t1" []])
>      ]]-}
>  where
>    --e = Expr
>    s = StmtType
>    --c = CatStmtType
>    --d = Ddl

