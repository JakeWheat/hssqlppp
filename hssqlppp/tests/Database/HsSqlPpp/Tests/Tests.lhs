
> module Database.HsSqlPpp.Tests.Tests (allTests) where
>
> import Test.Framework
>
> import Database.HsSqlPpp.Tests.Parsing.ParserTests

> import Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests


> allTests :: [Test]
> allTests =
>     parserTests
>     : typeCheckTests
>     : []
