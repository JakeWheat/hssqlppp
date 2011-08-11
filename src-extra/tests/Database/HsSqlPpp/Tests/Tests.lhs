
> module Database.HsSqlPpp.Tests.Tests (allTests) where
>
> import Test.Framework
>
> import Database.HsSqlPpp.Tests.ParserTests
> import Database.HsSqlPpp.Tests.FixUpIdentifiersTests
> import Database.HsSqlPpp.Tests.TypeCheckTests
> import Database.HsSqlPpp.Tests.TypeChecking.TpchTests
> --import Database.HsSqlPpp.Extensions.ExtensionTests
> import Database.HsSqlPpp.Tests.ParameterizedStatementTests
> --import Database.HsSqlPpp.Tests.RoundtripTests
> import Database.HsSqlPpp.Tests.QuasiQuoteTests
> import Database.HsSqlPpp.Tests.TypeInferenceTests
> import Database.HsSqlPpp.Tests.TypeChecking.TableRefTests
> import Database.HsSqlPpp.Tests.ExplicitCasts
>
> allTests :: [Test]
> allTests =
>     parserTests
>     : fixUpIdentifiersTests
>     : tableRefTests
>     : typeCheckTests
>     : tpchTests
>     : explicitCastTests
>     : quasiQuoteTests
>     : typeInferenceTests
>     : parameterizedStatementTests
>     -- : extensionTests
>     -- : roundtripTests
>     : []

