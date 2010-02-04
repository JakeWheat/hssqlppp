Copyright 2010 Jake Wheat

Gather together the examples from the extension modules and convert to regular test code

> module Database.HsSqlPpp.Examples.Extensions.ExtensionTests
>     (extensionTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> --import Debug.Trace
>
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Annotation
>
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Examples.Extensions.CreateVar

> testData :: [ExtensionTest]
> testData = [
>    createVarExample
>    ]

> extensionTests :: Test.Framework.Test
> extensionTests = testGroup "extensionTests" $ map testExtension testData

> testExtension :: ExtensionTest -> Test.Framework.Test
> testExtension (ExtensionTest nm tr sql trSql) =
>   testCase nm $
>       case (do
>             sast <- parseSql "" sql
>             let esast = tr sast
>             tast <- parseSql "" trSql
>             return (tast,esast)) of
>         Left e -> assertFailure $ show e
>         Right (ts,es) -> assertEqual "" (stripAnnotations ts) (stripAnnotations es)
