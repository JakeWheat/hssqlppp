Copyright 2010 Jake Wheat

> module Database.HsSqlPpp.Tests.Tests (allTests) where
>
> import Test.Framework
>
> import Database.HsSqlPpp.Tests.ParserTests
> import Database.HsSqlPpp.Tests.TypeCheckTests
> import Database.HsSqlPpp.Examples.Extensions.ExtensionTests
> import Database.HsSqlPpp.Tests.ParameterizedStatementTests
> import Database.HsSqlPpp.Tests.RoundtripTests
> import Database.HsSqlPpp.Tests.LocalBindingsTests
> import Database.HsSqlPpp.Tests.QuasiQuoteTests
> import Database.HsSqlPpp.Tests.TypeInferenceTests
>
> allTests :: [Test]
> allTests =
>     parserTests :
>     typeCheckTests :
>     extensionTests :
>     quasiQuoteTests :
>     typeInferenceTests :
>     parameterizedStatementTests ++
>     --roundtripTests ++
>     localBindingsTests
