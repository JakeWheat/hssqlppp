

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
> --import Text.Groom
> --import Database.HsSqlPpp.Tests.TestUtils


> data Item = Group String [Item]
>           | ScalExpr String (Either [TypeError] Type)

> testExpressionType :: String -> Either [TypeError] Type -> Test.Framework.Test
> testExpressionType src et = testCase ("typecheck " ++ src) $
>   let ast = case parseScalarExpr "" src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExpr defaultTemplate1Catalog ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = universeBi aast
>       got = if null er then Right ty else Left er
>   in assertEqual "" (either Left (Right . Just) et) got

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (ScalExpr s r) = testExpressionType s r
> itemToTft (Group s is) = testGroup s $ map itemToTft is