

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
> import Database.HsSqlPpp.Ast hiding (App)
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Pretty
> import Text.Groom
> import Debug.Trace
> import Database.HsSqlPpp.Tests.TestUtils
> import Control.Monad

> import Database.HsSqlPpp.Utils.GroomNoAnns
> import Language.Haskell.Exts hiding (Type)

> data Item = Group String [Item]
>           | ScalExpr String (Either [TypeError] Type)
>           | QueryExpr [CatalogUpdate] String (Either [TypeError] Type)
>           | RewriteQueryExpr TypeCheckingFlags [CatalogUpdate] String String
>           | ImpCastsScalar String String

> testScalarExprType :: String -> Either [TypeError] Type -> Test.Framework.Test
> testScalarExprType src et = testCase ("typecheck " ++ src) $ do
>   let ast = case parseScalarExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExpr defaultTypeCheckingFlags defaultTemplate1Catalog ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = universeBi aast
>       got = case () of
>               _ | null er -> maybe (Left []) Right ty
>                 | otherwise -> Left er

>       noTypeSEs :: [ScalarExpr]
>       noTypeSEs = [x | x <- universeBi got
>                      , atype (getAnnotation x) == Nothing]
>       noTypeQEs :: [QueryExpr]
>       noTypeQEs = [x | x <- universeBi got
>                      , atype (getAnnotation x) == Nothing]
>       allTyped = case et of
>                    Left _ -> True -- don't check if everything is typed
>                                   -- if expecting a type error
>                    Right _ -> null noTypeSEs && null noTypeQEs
>   unless allTyped $
>        trace ("MISSING TYPES: " ++ groomAnnTypeOnly aast)
>        $ assertBool "" allTyped
>   unless (et == got) $ trace (groomAnnTypeOnly aast) $ return ()
>   assertEqual "" et got

> testImpCastsScalar :: String -> String -> Test.Framework.Test
> testImpCastsScalar src wsrc = testCase ("typecheck " ++ src) $
>   let ast = case parseScalarExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExpr defaultTypeCheckingFlags defaultTemplate1Catalog ast
>       aast' = addExplicitCasts aast
>       wast = case parseScalarExpr defaultParseFlags "" Nothing wsrc of
>                Left e -> error $ show e
>                Right l -> l
>   in (if (resetAnnotations aast') /= (resetAnnotations wast)
>       then trace ("\n*****************\n" ++
>                   printScalarExpr defaultPPFlags aast'
>                   ++ "\n" ++ printScalarExpr defaultPPFlags wast
>                   ++ "\n*****************\n")
>       else id) $ assertEqual "" (resetAnnotations aast') (resetAnnotations wast)


> testQueryExprType :: [CatalogUpdate] -> String -> Either [TypeError] Type -> Test.Framework.Test
> testQueryExprType cus src et = testCase ("typecheck " ++ src) $ do
>   let ast = case parseQueryExpr defaultParseFlags "" Nothing src of
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
>       noTypeSEs :: [ScalarExpr]
>       noTypeSEs = [x | x <- universeBi aast
>                      , atype (getAnnotation x) == Nothing]
>       noTypeQEs :: [QueryExpr]
>       noTypeQEs = [x | x <- universeBi aast
>                      , atype (getAnnotation x) == Nothing]
>       allTyped = case et of
>                    Left _ -> True -- don't check if everything is typed
>                                   -- if expecting a type error
>                    Right _ -> null noTypeSEs && null noTypeQEs
>   unless allTyped $
>        trace ("MISSING TYPES: " ++ groomAnnTypeOnly aast)
>        $ assertBool "" allTyped
>   unless (et == got) $ trace (groomAnnTypeOnly aast) $ return ()
>   assertEqual "" et got

> testRewrite :: TypeCheckingFlags -> [CatalogUpdate] -> String -> String
>             -> Test.Framework.Test
> testRewrite f cus src src' = testCase ("rewrite " ++ src) $
>   let ast = case parseQueryExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       Right cat = updateCatalog cus defaultTemplate1Catalog
>       aast = typeCheckQueryExpr f cat ast
>       astrw = resetAnnotations aast
>       ast' = case parseQueryExpr defaultParseFlags "" Nothing src' of
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
> itemToTft (ImpCastsScalar s s') = testImpCastsScalar s s'
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