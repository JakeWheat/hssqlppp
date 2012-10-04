

> module Database.HsSqlPpp.Tests.TypeChecking.Utils where

> import Test.HUnit
> import Test.Framework.Providers.HUnit
> import Test.Framework
> --import Data.List
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
> import qualified Data.Text.Lazy as L
> --import Language.Haskell.Exts hiding (Type)

> data Item = Group String [Item]
>           | ScalExpr L.Text (Either [TypeError] Type)
>           | QueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | TSQLQueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | RewriteQueryExpr TypeCheckingFlags [CatalogUpdate] L.Text L.Text
>           | ImpCastsScalar TypeCheckingFlags L.Text L.Text


> testScalarExprType :: L.Text -> Either [TypeError] Type -> Test.Framework.Test
> testScalarExprType src et = testCase ("typecheck " ++ L.unpack src) $ do
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

> testImpCastsScalar :: TypeCheckingFlags -> L.Text -> L.Text -> Test.Framework.Test
> testImpCastsScalar f src wsrc = testCase ("typecheck " ++ L.unpack src) $
>   let ast = case parseScalarExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExpr f defaultTemplate1Catalog ast
>       aast' = addExplicitCasts aast
>       wast = case parseScalarExpr defaultParseFlags "" Nothing wsrc of
>                Left e -> error $ show e
>                Right l -> l
>   in (if (resetAnnotations aast') /= (resetAnnotations wast)
>       then trace ("\n***************** got: \n"
>                   ++ L.unpack (printScalarExpr defaultPPFlags aast')
>                   ++ "\nwanted:\n" ++ L.unpack (printScalarExpr defaultPPFlags wast)
>                   ++ "\n*****************\n"
>                   ++ "\n***************** got: \n"
>                   ++ groomNoAnns aast'
>                   ++ "\nwanted:\n" ++ groomNoAnns wast
>                   ++ "\n*****************\n"
>                   )
>       else id) $ assertEqual "" (resetAnnotations aast') (resetAnnotations wast)


> testQueryExprType :: Bool -> [CatalogUpdate] -> L.Text -> Either [TypeError] Type -> Test.Framework.Test
> testQueryExprType pg cus src et = testCase ("typecheck " ++ L.unpack src) $ do
>   let ast = case parseQueryExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       Right cat = updateCatalog cus $ if pg
>                                       then defaultTemplate1Catalog
>                                       else defaultTSQLCatalog
>       flg = if pg
>             then defaultTypeCheckingFlags
>             else defaultTypeCheckingFlags {tcfDialect = SQLServerDialect}
>       aast = typeCheckQueryExpr flg cat ast
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
>   --queryExprRewrites cus src et

rewrite the queryexpr with all the options true

pretty print, then check that the resultant sql parses the same, and
type checks properly and produces the same type

> queryExprRewrites :: [CatalogUpdate] -> L.Text -> Either [TypeError] Type -> IO () --Test.Framework.Test
> queryExprRewrites cus src et = {-testCase ("rewrite expanded " ++ src) $-} do
>   let ast = case parseQueryExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ "parse: " ++ L.unpack src ++ "\n" ++ show e
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
>                 Left e -> error $ "parse: " ++ L.unpack pp ++ "\n" ++ show e
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




> testRewrite :: TypeCheckingFlags -> [CatalogUpdate] -> L.Text -> L.Text
>             -> Test.Framework.Test
> testRewrite f cus src src' = testCase ("rewrite " ++ L.unpack src) $ do
>   let ast = case parseQueryExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       Right cat = updateCatalog cus defaultTemplate1Catalog
>       aast = typeCheckQueryExpr f cat ast
>       astrw = resetAnnotations aast
>       ast' = case parseQueryExpr defaultParseFlags "" Nothing src' of
>               Left e -> error $ show e
>               Right l -> resetAnnotations l
>   (if astrw /= ast'
>       then trace ("\n***************** expected\n" ++
>                   L.unpack (printQueryExpr defaultPPFlags ast')
>                   ++ "\n" ++ L.unpack (printQueryExpr defaultPPFlags astrw)
>                   ++ "\n\n" ++ groomTypes ast'
>                   ++ "\n\n" ++ groomTypes astrw
>                   ++ "\n***************** got\n")
>       else id) $ assertEqual "" ast' astrw
>   -- check second rewrite is idempotent
>   {-let astrw2 = resetAnnotations $ typeCheckQueryExpr f cat astrw
>   (if astrw /= astrw2
>       then trace ("\nSECOND REWRITE\n***************** expected\n" ++
>                   printQueryExpr defaultPPFlags astrw
>                   ++ "\n" ++ printQueryExpr defaultPPFlags astrw2
>                   ++ "\n\n" ++ groomTypes astrw
>                   ++ "\n\n" ++ groomTypes astrw2
>                   ++ "\n***************** got\n")
>       else id) $ assertEqual "second rewrite" astrw astrw2-}


> itemToTft :: Item -> Test.Framework.Test
> itemToTft (ScalExpr s r) = testScalarExprType s r
> itemToTft (QueryExpr cus s r) = testQueryExprType True cus s r
> itemToTft (TSQLQueryExpr cus s r) = testQueryExprType False cus s r
> itemToTft (RewriteQueryExpr f cus s s') = testRewrite f cus s s'
> itemToTft (ImpCastsScalar f s s') = testImpCastsScalar f s s'
> itemToTft (Group s is) = testGroup s $ map itemToTft is
