

> module Database.HsSqlPpp.Tests.TypeChecking.Utils where

> import Test.HUnit
> import Test.Framework.Providers.HUnit
> import Test.Framework
> import Data.List
> import Data.Generics.Uniplate.Data
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> --import Database.HsSqlPpp.Pretty
> import Text.Groom
> import Debug.Trace
> --import Database.HsSqlPpp.Tests.TestUtils


> data Item = Group String [Item]
>           | ScalExpr String (Either [TypeError] Type)
>           | QueryExpr [CatalogUpdate] String (Either [TypeError] Type)

> testScalarExprType :: String -> Either [TypeError] Type -> Test.Framework.Test
> testScalarExprType src et = testCase ("typecheck " ++ src) $
>   let ast = case parseScalarExpr "" src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExpr defaultTemplate1Catalog ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = universeBi aast
>       got = if null er then Right ty else Left er
>   in assertEqual "" (either Left (Right . Just) et) got

> testQueryExprType :: [CatalogUpdate] -> String -> Either [TypeError] Type -> Test.Framework.Test
> testQueryExprType cus src et = testCase ("typecheck " ++ src) $
>   let ast = case parseQueryExpr "" src of
>               Left e -> error $ show e
>               Right l -> l
>       Right cat = updateCatalog cus defaultTemplate1Catalog
>       aast = typeCheckQueryExpr cat ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = universeBi aast
>       got :: Either [TypeError] Type
>       got = case () of
>               _ | null er -> maybe (Left []) Right ty
>                 | otherwise -> Left er
>   in (if et /= got
>       then trace (groom aast)
>       else id) $ assertEqual "" et got


> itemToTft :: Item -> Test.Framework.Test
> itemToTft (ScalExpr s r) = testScalarExprType s r
> itemToTft (QueryExpr cus s r) = testQueryExprType cus s r
> itemToTft (Group s is) = testGroup s $ map itemToTft is
