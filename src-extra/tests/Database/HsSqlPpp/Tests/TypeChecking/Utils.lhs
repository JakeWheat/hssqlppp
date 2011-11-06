

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
> import Database.HsSqlPpp.Pretty
> import Text.Groom
> import Debug.Trace
> import Database.HsSqlPpp.Tests.TestUtils

> import Database.HsSqlPpp.Utils.GroomNoAnns
> import Language.Haskell.Exts hiding (Type)

> data Item = Group String [Item]
>           | ScalExpr String (Either [TypeError] Type)
>           | QueryExpr [CatalogUpdate] String (Either [TypeError] Type)
>           | RewriteQueryExpr TypeCheckingFlags [CatalogUpdate] String String

> testScalarExprType :: String -> Either [TypeError] Type -> Test.Framework.Test
> testScalarExprType src et = testCase ("typecheck " ++ src) $
>   let ast = case parseScalarExpr defaultParseFlags "" src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExpr defaultTypeCheckingFlags defaultTemplate1Catalog ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = universeBi aast
>       got = if null er then Right ty else Left er
>   in assertEqual "" (either Left (Right . Just) et) got

> testQueryExprType :: [CatalogUpdate] -> String -> Either [TypeError] Type -> Test.Framework.Test
> testQueryExprType cus src et = testCase ("typecheck " ++ src) $
>   let ast = case parseQueryExpr defaultParseFlags "" src of
>               Left e -> error $ show e
>               Right l -> l
>       Right cat = updateCatalog cus defaultTemplate1Catalog
>       aast = typeCheckQueryExpr defaultTypeCheckingFlags cat ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = universeBi aast
>       got :: Either [TypeError] Type
>       got = case () of
>               _ | null er -> maybe (Left []) Right ty
>                 | otherwise -> Left er
>   in (if et /= got
>       then trace (groom{-AnnTypeOnly-} aast)
>       else id) $ assertEqual "" et got

> testRewrite :: TypeCheckingFlags -> [CatalogUpdate] -> String -> String
>             -> Test.Framework.Test
> testRewrite f cus src src' = testCase ("rewrite " ++ src) $
>   let ast = case parseQueryExpr defaultParseFlags "" src of
>               Left e -> error $ show e
>               Right l -> l
>       Right cat = updateCatalog cus defaultTemplate1Catalog
>       aast = typeCheckQueryExpr f cat ast
>       astrw = resetAnnotations aast
>       ast' = case parseQueryExpr defaultParseFlags "" src' of
>               Left e -> error $ show e
>               Right l -> resetAnnotations l
>   in (if astrw /= ast'
>       then trace ("\n*****************\n" ++
>                   printQueryExpr defaultPPFlags ast'
>                   ++ "\n" ++ printQueryExpr defaultPPFlags astrw
>                   ++ "\n*****************\n")
>       else id) $ assertEqual "" ast' astrw


> itemToTft :: Item -> Test.Framework.Test
> itemToTft (ScalExpr s r) = testScalarExprType s r
> itemToTft (QueryExpr cus s r) = testQueryExprType cus s r
> itemToTft (RewriteQueryExpr f cus s s') = testRewrite f cus s s'
> itemToTft (Group s is) = testGroup s $ map itemToTft is

> groomAnnTypeOnly :: Show a => a -> String
> groomAnnTypeOnly d =
>   groomF tte d
>   where
>     tte :: Exp -> Exp
>     tte (Paren
>                   (App
>                    (App
>                     (App
>                      (App
>                       (App
>                        (App (Con (UnQual (Ident "Annotation")))
>                         _) t) te) _) _) _))
>          = case (t,te) of
>               (Con (UnQual (Ident "Nothing")) ,List []) ->
>                  Con (UnQual (Ident "A"))
>               (y,List []) -> y
>               (_,x) -> x
>            {-trace ("\n*************\n"
>                   ++ groom t
>                   ++ "\n*************\n"
>                   ++ groom te
>                   ++ "\n*************\n") $ Tuple [t,te]-}
>     tte x = x