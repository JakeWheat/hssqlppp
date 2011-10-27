
> module Database.HsSqlPpp.Tests.Tests (allTests) where
>
> import Test.Framework
>
> import Database.HsSqlPpp.Tests.Parsing.ParserTests

> import Database.HsSqlPpp.Tests.FixTree.FixUpIdentifiersTests
> import Database.HsSqlPpp.Tests.FixTree.ExplicitCasts

> import Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests

> -- import Database.HsSqlPpp.Tests.QuasiQuoteTests

> --import Database.HsSqlPpp.Tests.ExtensionTests
> import Database.HsSqlPpp.Tests.ParameterizedStatementTests
>
> allTests :: [Test]
> allTests =
>     parserTests
>     : fixUpIdentifiersTests
>     : explicitCastTests
>     -- : quasiQuoteTests
>     : typeCheckTests
>     : parameterizedStatementTests
>     -- : extensionTests
>     : []

