
Gather together the examples from the extension modules and convert to regular test code

> module Database.HsSqlPpp.Extensions.ExtensionTests
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
> import Database.HsSqlPpp.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Extensions.CreateVarSimple
> import Database.HsSqlPpp.Extensions.CreateVar
> import Database.HsSqlPpp.Extensions.TransitionConstraints
> import Database.HsSqlPpp.Extensions.CreateAssertionTests
> import Database.HsSqlPpp.Extensions.CardinalityRestrict
> --import Database.HsSqlPpp.Extensions.ChaosExtensions
> import Database.HsSqlPpp.Extensions.Modules
> import Database.HsSqlPpp.Extensions.GeneralInclusion
> import Database.HsSqlPpp.Extensions.Denormalized6nfExamples
> import Database.HsSqlPpp.Extensions.DenormSyntax
>
> testData :: [ExtensionTest]
> testData = transitionConstraintExamples ++
>            createAssertionExamples ++
>            --chaosExtensionsExamples ++
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
>     let ts' = resetAnnotations ts
>         es' = resetAnnotations $ tr sast
>     when (ts' /= es') $ putStrLn $ printStatements ts'
>                         ++ "\n----\n" ++ printStatements es'
>                         ++ "\n====\n"
>                         -- ++ ppExpr ts' ++ "\n----\n" ++ ppExpr es'
>     assertEqual "" ts' es'
