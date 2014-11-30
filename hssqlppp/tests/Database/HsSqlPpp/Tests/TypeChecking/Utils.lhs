

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
> import qualified Database.HsSqlPpp.Ast as Ast
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.Utility
> import Database.HsSqlPpp.Internals.TypeChecking.Environment
> import Text.Groom
> import Debug.Trace
> --import Database.HsSqlPpp.Tests.TestUtils
> import Control.Monad

> import Database.HsSqlPpp.Utils.GroomUtils
> import qualified Data.Text.Lazy as L
> --import Language.Haskell.Exts hiding (Type)

> data Item = Group String [Item]
>           | ScalExpr L.Text (Either [TypeError] Type)
>           | QueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | InsertQueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | TSQLQueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | OracleQueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | RewriteQueryExpr TypeCheckingFlags [CatalogUpdate] L.Text L.Text
>           | ImpCastsScalar TypeCheckingFlags L.Text L.Text
>           | ScalarExprExtra Catalog Environment L.Text (Either [TypeError] TypeExtra)


> testScalarExprType :: L.Text -> Either [TypeError] Type -> Test.Framework.Test
> testScalarExprType src et = testCase ("typecheck " ++ L.unpack src) $ do
>   let ast = case parseScalarExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExpr defaultTypeCheckingFlags defaultTemplate1Catalog ast
>       (ty,errs,noTypeQEs,noTypeSEs) = tcTreeInfo aast
>       er = concatMap fst errs
>       got = case () of
>               _ | null er -> maybe (Left []) (Right . teType) ty
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

> testScalarExprTypeExtra:: Catalog -> Environment -> L.Text
>                           -> Either [TypeError] TypeExtra
>                           -> Test.Framework.Test
> testScalarExprTypeExtra cat env src ete = testCase ("typecheck " ++ L.unpack src) $ do
>   let ast = case parseScalarExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExprEnv defaultTypeCheckingFlags cat env ast
>       (ty,errs,noTypeQEs,noTypeSEs) = tcTreeInfo aast
>       er = concatMap fst errs
>       got = case () of
>               _ | null er -> maybe (Left []) Right ty
>                 | otherwise -> Left er
>       allTyped = case ete of
>           Left _ -> True  -- don't check if everything is typed
>                           -- if expecting a type error
>           Right _ -> null noTypeSEs && null noTypeQEs
>   unless allTyped $
>        trace ("MISSING TYPES: " ++ groomTypes aast)
>        $ assertBool "" allTyped
>   unless (ete == got) $ trace (groomTypes aast) $ return ()
>   assertEqual "" ete got

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


> testQueryExprType :: SQLSyntaxDialect -> [CatalogUpdate] -> L.Text -> Either [TypeError] Type -> Test.Framework.Test
> testQueryExprType dl cus src et = testCase ("typecheck " ++ L.unpack src) $ do
>   let ast = case parseQueryExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       Right cat = updateCatalog cus $ case dl of
>           PostgreSQLDialect -> defaultTemplate1Catalog
>           SQLServerDialect -> defaultTSQLCatalog
>           OracleDialect -> defaultTSQLCatalog
>       flg = case dl of
>           PostgreSQLDialect -> defaultTypeCheckingFlags
>           SQLServerDialect -> defaultTypeCheckingFlags {tcfDialect = SQLServerDialect}
>           OracleDialect -> defaultTypeCheckingFlags {tcfDialect = OracleDialect}
>       aast = typeCheckQueryExpr flg cat ast
>       (ty,errs,noTypeQEs,noTypeSEs) = tcTreeInfo aast
>       er = concatMap fst errs
>       got :: Either [TypeError] Type
>       got = case () of
>               _ | null er -> maybe (Left []) (Right . teType) ty
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

> testInsertQueryExprType :: SQLSyntaxDialect -> [CatalogUpdate] -> L.Text -> Either [TypeError] Type -> Test.Framework.Test
> testInsertQueryExprType dl cus src et = testCase ("typecheck " ++ L.unpack src) $ do
>   let Right cat = updateCatalog cus $ case dl of
>           PostgreSQLDialect -> defaultTemplate1Catalog
>           SQLServerDialect -> defaultTSQLCatalog
>           OracleDialect -> defaultTSQLCatalog
>       flg = case dl of
>           PostgreSQLDialect -> defaultTypeCheckingFlags
>           SQLServerDialect -> defaultTypeCheckingFlags {tcfDialect = SQLServerDialect}
>           OracleDialect -> defaultTypeCheckingFlags {tcfDialect = OracleDialect}
>       asts = either (error . show) id $ parseStatements defaultParseFlags "" Nothing src
>       Ast.Insert _ _ _ q _ = extractInsert $ snd $ typeCheckStatements flg cat asts
>       q' = addImplicitCasts cat q
>       q'' = typeCheckQueryExpr flg cat q'
>       (ty,errs,noTypeQEs,noTypeSEs) = tcTreeInfo q''
>       er = concatMap fst errs
>       got :: Either [TypeError] Type
>       got = case () of
>               _ | null er -> maybe (Left []) (Right . teType) ty
>                 | otherwise -> Left er
>       allTyped = case et of
>                    Left _ -> True -- don't check if everything is typed
>                                   -- if expecting a type error
>                    Right _ -> null noTypeSEs && null noTypeQEs
>   unless allTyped $
>        trace ("MISSING TYPES: " ++ groomTypes q'')
>        $ assertBool "" allTyped
>   unless (et == got) $ trace (groomTypes q'') $ return ()
>   assertEqual "" et got
>   where
>     extractInsert [i@Ast.Insert{}] = i
>     extractInsert x = error $ "expected a single insert statement, got " ++ groom x


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
> itemToTft (QueryExpr cus s r) = testQueryExprType PostgreSQLDialect cus s r
> itemToTft (InsertQueryExpr cus s r) = testInsertQueryExprType PostgreSQLDialect cus s r
> itemToTft (TSQLQueryExpr cus s r) = testQueryExprType SQLServerDialect cus s r
> itemToTft (OracleQueryExpr cus s r) = testQueryExprType OracleDialect cus s r
> itemToTft (RewriteQueryExpr f cus s s') = testRewrite f cus s s'
> itemToTft (ImpCastsScalar f s s') = testImpCastsScalar f s s'
> itemToTft (Group s is) = testGroup s $ map itemToTft is
> itemToTft (ScalarExprExtra cat env s r) = testScalarExprTypeExtra cat env s r
