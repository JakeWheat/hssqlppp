
> module Database.HsSqlPpp.Tests.Tests (allTests) where
>
> import Test.Framework
>
> import Database.HsSqlPpp.Tests.Parsing.ParserTests

> import Database.HsSqlPpp.Tests.FixTree.FixUpIdentifiersTests
> import Database.HsSqlPpp.Tests.FixTree.ExplicitCasts

> import Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests

> import Database.HsSqlPpp.Tests.QuasiQuoteTests

> --import Database.HsSqlPpp.Tests.TypeInferenceTests
> --import Database.HsSqlPpp.Tests.TypeCheckTests
> --import Database.HsSqlPpp.Tests.TypeChecking.TpchTests
> --import Database.HsSqlPpp.Extensions.ExtensionTests
> --import Database.HsSqlPpp.Tests.ParameterizedStatementTests
> --import Database.HsSqlPpp.Tests.RoundtripTests

>
> allTests :: [Test]
> allTests =
>     parserTests
>     : fixUpIdentifiersTests
>     : explicitCastTests
>     -- : tableRefTests
>     : typeCheckTests
>     -- : tpchTests
>     : quasiQuoteTests
>     -- : typeInferenceTests
>     -- : parameterizedStatementTests
>     -- : extensionTests
>     -- : roundtripTests
>     : []

