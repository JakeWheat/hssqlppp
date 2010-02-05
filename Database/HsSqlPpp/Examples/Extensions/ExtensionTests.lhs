Copyright 2010 Jake Wheat

Gather together the examples from the extension modules and convert to regular test code

> module Database.HsSqlPpp.Examples.Extensions.ExtensionTests
>     (extensionTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Monad
> --import Debug.Trace
>
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Utils.Utils
>
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Examples.Extensions.CreateVarSimple
> import Database.HsSqlPpp.Examples.Extensions.CreateVar
> import Database.HsSqlPpp.Examples.Extensions.TransitionConstraints

> testData :: [ExtensionTest]
> testData = transitionConstraintExamples ++
>            [createVarSimpleExample
>            ,createVarExample
>            ]

ddl 'triggers' -> reject, transform, or supplement a ddl statement
addreadonlytriggers
addnotifytriggers
constraints
zeroonetuple
transitionconstraints
default not null
modules
'_mr' table definitions and initialization data / relation constants
multiple updates
out of order definitions (after missing catalog elements are done)
simplified catalog

chaos: leftovers already written,
       turn sequence progression
       action valid tables
       ai
       what else?
       revlar types
       check only updates are in action_ functions

idea for attributes:
select attribute('type','readonly');
create table readonly_table ...

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
>         Right (ts,es) -> do
>               let ts' = stripAnnotations ts
>                   es' = stripAnnotations es
>               when (ts' /= es')
>                    $ putStrLn $ ppExpr ts' ++ "\n\n" ++ ppExpr es'
>               assertEqual "" ts' es'
