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
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.PrettyPrinter
>
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Examples.Extensions.CreateVarSimple
> import Database.HsSqlPpp.Examples.Extensions.CreateVar
> import Database.HsSqlPpp.Examples.Extensions.TransitionConstraints
> import Database.HsSqlPpp.Examples.Extensions.CreateAssertionTests
> import Database.HsSqlPpp.Examples.Extensions.CardinalityRestrict
> import Database.HsSqlPpp.Examples.Extensions.ChaosExtensions
> import Database.HsSqlPpp.Examples.Extensions.Modules
> import Database.HsSqlPpp.Examples.Extensions.GeneralInclusion
> import Database.HsSqlPpp.Examples.Extensions.Denormalized6nfExamples
> import Database.HsSqlPpp.Examples.Extensions.DenormSyntax
>
> testData :: [ExtensionTest]
> testData = transitionConstraintExamples ++
>            createAssertionExamples ++
>            chaosExtensionsExamples ++
>            generalInclusionsExamples ++
>            denormalized6nfExamples ++
>            [createVarSimpleExample
>            ,createVarExample
>            ,cardinalityRestrictExample
>            ,modulesExample
>            ]
>
> otherTests :: [Test.Framework.Test]
> otherTests = [denormParseTests]

~~~~
rough plan for extensions

immediate goal is to rewrite chaos2010 much more nicely using new
extensions

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
~~~~

> extensionTests :: Test.Framework.Test
> extensionTests = testGroup "extensionTests" $ map testExtension testData
>                  ++ otherTests

>
> testExtension :: ExtensionTest -> Test.Framework.Test
> testExtension (ExtensionTest nm tr sast ts) =
>   testCase nm $ do
>     let ts' = stripAnnotations ts
>         es' = stripAnnotations $ tr sast
>     when (ts' /= es') $ putStrLn $ printSql ts' ++ "\n----\n" ++ printSql es' ++ "\n====\n"
>                         -- ++ ppExpr ts' ++ "\n----\n" ++ ppExpr es'
>     assertEqual "" ts' es'
