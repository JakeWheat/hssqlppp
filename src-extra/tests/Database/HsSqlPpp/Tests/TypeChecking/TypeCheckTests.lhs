

> module Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests
>     (typeCheckTests
>     ,typeCheckTestData
>     ,Item(..)) where

> import Test.Framework


> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Tests.TypeChecking.ScalarExprs
> import Database.HsSqlPpp.Tests.TypeChecking.SimpleQueryExprs
> import Database.HsSqlPpp.Tests.TypeChecking.Rewrites
> import Database.HsSqlPpp.Tests.TypeChecking.Joins
> import Database.HsSqlPpp.Tests.TypeChecking.Tpch
> import Database.HsSqlPpp.Tests.TypeChecking.TrefIdentifiers

> typeCheckTestData :: Item
> typeCheckTestData =
>   Group "typeCheckTests"
>     [scalarExprs
>     ,simpleQueryExprs
>     ,joins
>     ,trefIdentifiers
>     --,rewrites
>     --,tpch
>     ]

> typeCheckTests :: Test.Framework.Test
> typeCheckTests =
>   itemToTft typeCheckTestData
