

> module Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests
>     (typeCheckTests
>     ,typeCheckTestData
>     ,Item(..)) where

> import Test.Framework


> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Tests.TypeChecking.ScalarExprs
> import Database.HsSqlPpp.Tests.TypeChecking.SimpleQueryExprs
> import Database.HsSqlPpp.Tests.TypeChecking.Joins

> typeCheckTestData :: Item
> typeCheckTestData =
>   Group "typeCheckTests"
>     [scalarExprs
>     ,simpleQueryExprs
>     ,joins]

> typeCheckTests :: Test.Framework.Test
> typeCheckTests =
>   itemToTft typeCheckTestData
