

> module Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests
>     (typeCheckTestData
>     ,Item(..)) where

> --import Test.Framework


> import Database.HsSqlPpp.Tests.TestTypes
> import Database.HsSqlPpp.Tests.TypeChecking.ScalarExprs
> import Database.HsSqlPpp.Tests.TypeChecking.SimpleQueryExprs
> import Database.HsSqlPpp.Tests.TypeChecking.Rewrites
> import Database.HsSqlPpp.Tests.TypeChecking.Joins
> import Database.HsSqlPpp.Tests.TypeChecking.Tpch
> import Database.HsSqlPpp.Tests.TypeChecking.TrefIdentifiers
> import Database.HsSqlPpp.Tests.TypeChecking.CaseExpressions
> import Database.HsSqlPpp.Tests.TypeChecking.ImplicitCasts
> import Database.HsSqlPpp.Tests.TypeChecking.TSQL
> import Database.HsSqlPpp.Tests.TypeChecking.Issues
> import Database.HsSqlPpp.Tests.TypeChecking.Aggregates
> import Database.HsSqlPpp.Tests.TypeChecking.PrecisionAndNullable
> import Database.HsSqlPpp.Tests.TypeChecking.InsertQueryExprs

> typeCheckTestData :: Item
> typeCheckTestData =
>   Group "typeCheckTests"
>     [scalarExprs
>     ,simpleQueryExprs
>     ,joins
>     ,trefIdentifiers
>     ,rewrites
>     ,caseExpressions
>     ,tpch
>     ,impCasts
>     ,tsqlQueryExprs
>     ,issues
>     ,aggregates
>     ,precisionAndNullable
>     ,insertQueryExprs
>     ]
