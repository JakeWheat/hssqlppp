

> module Database.HsSqlPpp.Tests.TypeChecking.Utils where

> import Test.HUnit
> import Test.Framework.Providers.HUnit
> import Test.Framework
> import Data.List
> --import Data.Generics.Uniplate.Data
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.Ast hiding (App)
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.Utility
> --import Text.Groom
> import Debug.Trace
> --import Database.HsSqlPpp.Tests.TestUtils
> import Control.Monad

> import Database.HsSqlPpp.Utils.GroomUtils
> --import Language.Haskell.Exts hiding (Type)

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
>       (ty,errs,noTypeQEs,noTypeSEs) = tcTreeInfo aast
>       er = concatMap fst errs
>       got = case () of
>               _ | null er -> maybe (Left []) Right ty
>                 | otherwise -> Left er
>       allTyped = case et of
>                    Left _ -> True -- don't check if everything is typed
>                                   -- if expecting a type error
>                    Right _ -> null noTypeSEs && null noTypeQEs
>   unless allTyped $
>        trace ("MISSING TYPES: " ++ groomTypes aast)
>        $ assertBool "" allTyped
>   unless (et == got) $ trace (groomTypes aast) $ return ()
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
>       (ty,errs,noTypeQEs,noTypeSEs) = tcTreeInfo aast
>       er = concatMap fst errs
>       got :: Either [TypeError] Type
>       got = case () of
>               _ | null er -> maybe (Left []) Right ty
>                 | otherwise -> Left er
>       allTyped = case et of
>                    Left _ -> True -- don't check if everything is typed
>                                   -- if expecting a type error
>                    Right _ -> null noTypeSEs && null noTypeQEs
>   unless allTyped $
>        trace ("MISSING TYPES: " ++ groomTypes aast)
>        $ assertBool "" allTyped
>   unless (et == got) $ trace (groomTypes aast) $ return ()
>   assertEqual "" et got
>   --testQueryExprRewrites cus src et

rewrite the queryexpr with all the options true

pretty print, then check that the resultant sql parses the same, and
type checks properly and produces the same type

> queryExprRewrites :: [CatalogUpdate] -> String -> Either [TypeError] Type -> IO () --Test.Framework.Test
> queryExprRewrites cus src et = {-testCase ("rewrite expanded " ++ src) $-} do
>   let ast = case parseQueryExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ "parse: " ++ src ++ "\n" ++ show e
>               Right l -> l
>   let Right cat = updateCatalog cus defaultTemplate1Catalog
>       aast = typeCheckQueryExpr
>                defaultTypeCheckingFlags {tcfAddQualifiers = True
>                                         ,tcfAddSelectItemAliases = True
>                                         ,tcfExpandStars = True
>                                         ,tcfAddFullTablerefAliases = True}
>                cat ast
>       ty = anType $ getAnnotation aast
>       -- print with rewritten tree
>       pp = printQueryExpr defaultPPFlags aast
>       astrw = case parseQueryExpr defaultParseFlags "" Nothing pp of
>                 Left e -> error $ "parse: " ++ pp ++ "\n" ++ show e
>                 Right l -> l
>       aastrw = typeCheckQueryExpr
>                  defaultTypeCheckingFlags
>                  cat astrw
>       tyrw = anType $ getAnnotation aast
>   assertEqual "rewrite pp . parse" (resetAnnotations aast) (resetAnnotations aastrw)
>   assertEqual "rewrite ty" ty tyrw
>   let (_,errs,noTypeQEs,noTypeSEs) = tcTreeInfo aast
>       er = concatMap fst errs
>       allTyped = case et of
>                    Left _ -> True -- don't check if everything is typed
>                                   -- if expecting a type error
>                    Right _ -> null noTypeSEs && null noTypeQEs
>   unless (null er) $
>        trace ("errors in tree: " ++ groomTypes aastrw)
>        $ assertBool "" (null er)
>   unless allTyped $
>        trace ("MISSING TYPES: " ++ groomTypes aastrw)
>        $ assertBool "" allTyped




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
