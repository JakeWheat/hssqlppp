
> module Database.HsSqlPpp.Tests.Tests (allTests) where
>
> import Test.Tasty
>
> import Database.HsSqlPpp.Tests.Parsing.ParserTests

> import Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests

> import Database.HsSqlPpp.Tests.TestUtils

> allTests :: [TestTree]
> allTests = map itemToTft
>            [parserTestData
>            ,typeCheckTestData]
