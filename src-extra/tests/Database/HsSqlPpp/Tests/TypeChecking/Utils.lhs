
> module Database.HsSqlPpp.Tests.TypeChecking.Utils where

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


> data Item = Group String [Item]
>           | Expr String (Either [TypeError] Type)
>           | StmtType String (Either [TypeError] [Maybe StatementType])
>           | CatStmtType String [CatalogUpdate] (Either [TypeError] [Maybe StatementType])
>           | Ddl String [CatalogUpdate]


> testExpressionType :: String -> Either [TypeError] Type -> Test.Framework.Test
> testExpressionType src et = testCase ("typecheck " ++ src) $
>   let ast = case parseScalarExpr "" src of
>                                      Left e -> error $ show e
>                                      Right l -> l
>       aast = typeCheckScalarExpr defaultTemplate1Catalog ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = universeBi aast
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
>       er = universeBi aast
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
>       er = universeBi aast
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
>       er = universeBi aast
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
