
> module Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests
>     (typeCheckTests
>     ,typeCheckTestData
>     ,Item(..)) where

> import Test.Framework


> import Test.HUnit
> import Test.Framework.Providers.HUnit
> import Data.List
> import Data.Generics.Uniplate.Data
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Text.Groom
> import Database.HsSqlPpp.Tests.TestUtils

> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Tests.TypeChecking.TableRefTests
> import Database.HsSqlPpp.Tests.TypeChecking.TpchTests
> import Database.HsSqlPpp.Tests.TypeChecking.TypeInferenceTests
> import Database.HsSqlPpp.Tests.TypeChecking.Literals
> import Database.HsSqlPpp.Tests.TypeChecking.SimpleExpressions
> import Database.HsSqlPpp.Tests.TypeChecking.SpecialFunctions
> import Database.HsSqlPpp.Tests.TypeChecking.RowCtors
> import Database.HsSqlPpp.Tests.TypeChecking.CaseExpressions
> import Database.HsSqlPpp.Tests.TypeChecking.MiscExpressions
> import Database.HsSqlPpp.Tests.TypeChecking.SimpleSelects
> import Database.HsSqlPpp.Tests.TypeChecking.CombineSelects
> import Database.HsSqlPpp.Tests.TypeChecking.SelectFrom
> import Database.HsSqlPpp.Tests.TypeChecking.Joins
> import Database.HsSqlPpp.Tests.TypeChecking.Qualification
> import Database.HsSqlPpp.Tests.TypeChecking.MiscSelects
> import Database.HsSqlPpp.Tests.TypeChecking.Insert
> import Database.HsSqlPpp.Tests.TypeChecking.Update
> import Database.HsSqlPpp.Tests.TypeChecking.Delete
> import Database.HsSqlPpp.Tests.TypeChecking.Creates
> import Database.HsSqlPpp.Tests.TypeChecking.Plpgsql
> import Database.HsSqlPpp.Tests.TypeChecking.CatalogChaining
> import Database.HsSqlPpp.Tests.TypeChecking.Into
> import Database.HsSqlPpp.Tests.TypeChecking.Drops
> import Database.HsSqlPpp.Tests.TypeChecking.Triggers
> import Database.HsSqlPpp.Tests.TypeChecking.Misc

> typeCheckTestData :: Item
> typeCheckTestData = Group "typeCheckTests" typeCheckTestList

> typeCheckTestList :: [Item]
> typeCheckTestList = [tcLiteralTestData
>                     ,tcSimpleExpressionTestData
>                     ,tcSpecialFunctionsTestData
>                     ,tcRowCtorsTestData
>                     ,caseExpressionsTestData
>                     ,miscExpressionsTestData
>                     ,tcSimpleSelectsTestData
>                     ,tcCombineSelectsTestData
>                     ,tcSelectFromTestData
>                     ,tcJoinsTestData
>                     ,tcQualificationTestData
>                     ,tcMiscSelectTestData
>                     ,tcInsertTestData
>                     ,tcUpdateTestData
>                     ,tcDeleteTestData
>                     ,tcCreateTestData
>                     ,tcPlpgsqlTestData
>                     ,tcCatalogChainingTestData
>                     ,tcIntoTestData
>                     ,tcDropsTestData
>                     ,tcTriggersTestData
>                     ,tcMiscTestData]

todo:

--------------------------------------------------------------------------------

~~~~
test some casts
assign composite to record
  then assign record to composite
assign row to composite
 : check wrong cols, wrong types
check read and write fields in composite->record
check read and write fields in composite
check domain <-> base assigns
check call function with compatible composite, compatible row ctor
assign comp to comp

todo for chaos sql
for loop var = scalar, select = setof composite with one scalar

select into
composite assignment and equality
autocast from rowctor to composite when calling function
assignment to composite fields
read fields of composite

array_contains -> match anyelement
createtable as cat update
window functions
assign domain <-> base
sql function not working
~~~~


~~~~
check insert returning, update returning, delete returning, one check each
check select into: multiple vars, record (then access fields to check),
  composite var
check errors: select into wrong number of vars, wrong types, and into
  composite wrong number and wrong type
~~~~




> typeCheckTests :: Test.Framework.Test
> typeCheckTests =
>   testGroup "typeChecking" $
>                 [tableRefTests
>                 ,tpchTests
>                 ,typeInferenceTests]
>                 ++ map itemToTft typeCheckTestList



--------------------------------------------------------------------------------

> testExpressionType :: String -> Either [TypeError] Type -> Test.Framework.Test
> testExpressionType src et = testCase ("typecheck " ++ src) $
>   let ast = case parseScalarExpr "" src of
>                                      Left e -> error $ show e
>                                      Right l -> l
>       aast = typeCheckScalarExpr defaultTemplate1Catalog ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = [x | x <- universeBi aast]
>   in if null er
>      then assertEqual ("typecheck " ++ src) (Just et) $ fmap Right ty
>      else assertEqual ("typecheck " ++ src) et $ Left er
>
> testStatementType :: String -> Either [TypeError] [Maybe StatementType] -> Test.Framework.Test
> testStatementType src sis = testCase ("typecheck " ++ src) $
>   let ast = case parseStatements "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = snd $ typeCheckStatements defaultTemplate1Catalog ast
>       is = map (stType . getAnnotation) aast
>       er :: [TypeError]
>       er = [x | x <- universeBi aast]
>   in case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertTrace (groom aast) ("typecheck " ++ src) sis $ Right is
>        _ -> assertTrace (groom aast) ("typecheck " ++ src) sis $ Left er

> testCatUpStatementType :: String
>                        -> [CatalogUpdate]
>                        -> Either [TypeError] [Maybe StatementType]
>                        -> Test.Framework.Test
> testCatUpStatementType src eu sis = testCase ("typecheck " ++ src) $
>   let ast = case parseStatements "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = snd $ typeCheckStatements makeCat ast
>       is = map (stType . getAnnotation) aast
>       er :: [TypeError]
>       er = [x | x <- universeBi aast]
>   in {-trace (show aast) $-} case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertEqual ("typecheck " ++ src) sis $ Right is
>        _ -> assertEqual ("typecheck " ++ src) sis $ Left er
>   where
>     makeCat = case updateCatalog defaultTemplate1Catalog eu of
>                         Left x -> error $ show x
>                         Right e -> e
>
> testCat :: String -> [CatalogUpdate] -> Test.Framework.Test
> testCat src eu = testCase ("check catalog: " ++ src) $
>   let ast = case parseStatements "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       (ncat,aast) = typeCheckStatements defaultTemplate1Catalog ast
>       er :: [TypeError]
>       er = [x | x <- universeBi aast]
>       neu = deconstructCatalog ncat \\ deconstructCatalog defaultTemplate1Catalog
>   in if not (null er)
>        then assertFailure $ show er
>        else assertEqual "check eus" eu neu
>
> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Expr s r) = testExpressionType s r
> itemToTft (StmtType s r) = testStatementType s r
> itemToTft (CatStmtType s c r) = testCatUpStatementType s c r
> itemToTft (Ddl s c) = testCat s c
> itemToTft (Group s is) = testGroup s $ map itemToTft is
