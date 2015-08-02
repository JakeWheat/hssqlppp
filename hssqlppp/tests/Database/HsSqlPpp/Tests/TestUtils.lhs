
> module Database.HsSqlPpp.Tests.TestUtils
>     (assertTrace,itemToTft) where
>
> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as H
> --import Data.List
> --import Data.Generics.Uniplate.Data
> import Data.Data

> import Debug.Trace
> import Control.Monad

> --import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Utils.GroomUtils
> import Database.HsSqlPpp.Utility
> --import Database.HsSqlPpp.SqlTypes
> --import Database.HsSqlPpp.Utils.PPExpr
> import Data.Text.Lazy (Text)
> import qualified Data.Text.Lazy as L
> import qualified Data.Text as T
> import Database.HsSqlPpp.LexicalSyntax (sqlTokens,prettyToken,Token)
> --import Text.Parsec.Text (runParser)
> import Control.Applicative

> import Database.HsSqlPpp.Tests.TestTypes

> --import Test.HUnit
> --import Test.Framework.Providers.HUnit
> --import Test.Framework
> --import Data.List
> --import Data.Generics.Uniplate.Data
> --import Database.HsSqlPpp.Parser
> --import Database.HsSqlPpp.TypeChecker
> --import Database.HsSqlPpp.Annotation
> --import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.Ast hiding (App)
> import Database.HsSqlPpp.Types
> --import Database.HsSqlPpp.Pretty
> --import Database.HsSqlPpp.Utility
> --import Database.HsSqlPpp.Internals.TypeChecking.Environment
> --import Text.Groom
> --import Debug.Trace
> --import Database.HsSqlPpp.Tests.TestUtils
> --import Control.Monad

> --import Database.HsSqlPpp.Utils.GroomUtils
> --import qualified Data.Text.Lazy as L

> assertTrace :: (Show a,Eq a) => String -> String -> a -> a -> IO ()
> assertTrace nem s a1 a2 = do
>     when (a1 /= a2) $ trace nem $ return ()
>     H.assertEqual s a1 a2

> itemToTft :: Item -> T.TestTree
> itemToTft (Expr a b) = testParseScalarExpr a b
> itemToTft (QueryExpr a b) = testParseQueryExpr a b
> itemToTft (PgSqlStmt a b) = testParsePlpgsqlStatements PostgreSQLDialect a b
> itemToTft (Stmt a b) = testParseStatements PostgreSQLDialect a b
> itemToTft (TSQL a b) =
>   testParsePlpgsqlStatements (if True
>                        then SQLServerDialect
>                        else PostgreSQLDialect) a b
> itemToTft (Oracle a b) =
>   testParsePlpgsqlStatements OracleDialect a b
> --itemToTft (MSStmt a b) = testParseStatements a b
> itemToTft (Group s is) = T.testGroup s $ map itemToTft is
> itemToTft (Lex d a b) = testLex d a b

> itemToTft (ScalExpr s r) = testScalarExprType s r
> itemToTft (TCQueryExpr cus s r) = testQueryExprType PostgreSQLDialect cus s r
> itemToTft (InsertQueryExpr cus s r) = testInsertQueryExprType SQLServerDialect {-PostgreSQLDialect-} cus s r
> itemToTft (TSQLQueryExpr cus s r) = testQueryExprType SQLServerDialect cus s r
> itemToTft (OracleQueryExpr cus s r) = testQueryExprType OracleDialect cus s r
> itemToTft (RewriteQueryExpr f cus s s') = testRewrite f cus s s'
> itemToTft (ImpCastsScalar f s s') = testImpCastsScalar f s s'
> itemToTft (ScalarExprExtra cat env s r) = testScalarExprTypeExtra cat env s r

> testParseScalarExpr :: Text -> ScalarExpr -> T.TestTree
> testParseScalarExpr src ast =
>   parseUtil src ast (parseScalarExpr defaultParseFlags "" Nothing)
>                     (parseScalarExpr defaultParseFlags "" Nothing)
>                     (printScalarExpr defaultPPFlags)
> testParseQueryExpr :: Text -> QueryExpr -> T.TestTree
> testParseQueryExpr src ast =
>   parseUtil src ast (parseQueryExpr defaultParseFlags "" Nothing)
>                     (parseQueryExpr defaultParseFlags "" Nothing)
>                     (printQueryExpr defaultPPFlags)

>
> testParseStatements :: SQLSyntaxDialect -> Text -> [Statement] -> T.TestTree
> testParseStatements flg src ast =
>   let parse = parseStatements defaultParseFlags {pfDialect=flg} "" Nothing
>       pp = printStatements defaultPPFlags {ppDialect=flg}
>   in parseUtil src ast parse parse pp
>
> testParsePlpgsqlStatements :: SQLSyntaxDialect -> Text -> [Statement] -> T.TestTree
> testParsePlpgsqlStatements flg src ast =
>   parseUtil src ast (parsePlpgsql defaultParseFlags {pfDialect=flg} "" Nothing)
>                     (parsePlpgsql defaultParseFlags {pfDialect=flg} "" Nothing)
>                     (printStatements defaultPPFlags {ppDialect=flg})
>
> parseUtil :: (Show t, Eq b, Show b, Data b) =>
>              Text
>           -> b
>           -> (Text -> Either t b)
>           -> (Text -> Either t b)
>           -> (b -> Text)
>           -> T.TestTree
> parseUtil src ast parser reparser printer = H.testCase ("parse " ++ L.unpack src) $
>   case parser src of
>     Left er -> H.assertFailure $ show er
>     Right ast' -> do
>       when (ast /= resetAnnotations ast') $ do
>         putStrLn $ groomNoAnns ast
>         putStrLn $ groomNoAnns $ resetAnnotations ast'
>       H.assertEqual ("parse " ++ L.unpack src) ast $ resetAnnotations ast'
>       case reparser (printer ast) of
>         Left er -> H.assertFailure $ "reparse\n" ++ (L.unpack $ printer ast) ++ "\n" ++ show er ++ "\n" -- ++ pp ++ "\n"
>         Right ast'' -> H.assertEqual ("reparse: " ++ L.unpack (printer ast)) ast $ resetAnnotations ast''

> testLex :: SQLSyntaxDialect -> T.Text -> [Token] -> T.TestTree
> testLex d t r = H.testCase ("lex "++ T.unpack t) $ do
>     let x = sqlTokens d "" Nothing t
>         y = either (error . show) id x
>     H.assertEqual "lex" r (map snd y)
>     let t' = L.concat $ map (prettyToken d) r
>     H.assertEqual "lex . pretty" (L.fromChunks [t]) t'



> testScalarExprType :: L.Text -> Either [TypeError] Type -> T.TestTree
> testScalarExprType src et = H.testCase ("typecheck " ++ L.unpack src) $ do
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
>        $ H.assertBool "" allTyped
>   unless (et == got) $ trace (groomTypes aast) $ return ()
>   H.assertEqual "" et got

> testScalarExprTypeExtra:: Catalog -> Environment -> L.Text
>                           -> Either [TypeError] TypeExtra
>                           -> T.TestTree
> testScalarExprTypeExtra cat env src ete = H.testCase ("typecheck " ++ L.unpack src) $ do
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
>        $ H.assertBool "" allTyped
>   unless (ete == got) $ trace (groomTypes aast) $ return ()
>   H.assertEqual "" ete got

> testImpCastsScalar :: TypeCheckingFlags -> L.Text -> L.Text -> T.TestTree
> testImpCastsScalar f src wsrc = H.testCase ("typecheck " ++ L.unpack src) $
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
>       else id) $ H.assertEqual "" (resetAnnotations aast') (resetAnnotations wast)


> testQueryExprType :: SQLSyntaxDialect -> [CatalogUpdate] -> L.Text -> Either [TypeError] Type -> T.TestTree
> testQueryExprType dl cus src et = H.testCase ("typecheck " ++ L.unpack src) $ do
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
>        $ H.assertBool "" allTyped
>   unless (et == got) $ trace (groomTypes aast) $ return ()
>   H.assertEqual "" et got
>   --queryExprRewrites cus src et

> testInsertQueryExprType :: SQLSyntaxDialect -> [CatalogUpdate] -> L.Text -> Either [TypeError] Type -> T.TestTree
> testInsertQueryExprType dl cus src et = H.testCase ("typecheck " ++ L.unpack src) $ do
>   let Right cat = updateCatalog cus $ case dl of
>           PostgreSQLDialect -> defaultTemplate1Catalog
>           SQLServerDialect -> defaultTSQLCatalog
>           OracleDialect -> defaultTSQLCatalog
>       flg = case dl of
>           PostgreSQLDialect -> defaultTypeCheckingFlags
>           SQLServerDialect -> defaultTypeCheckingFlags {tcfDialect = SQLServerDialect}
>           OracleDialect -> defaultTypeCheckingFlags {tcfDialect = OracleDialect}
>       asts = either (error . show) id $ parseStatements defaultParseFlags "" Nothing src
>       Insert _ _ _ q _ = extractInsert $ snd $ typeCheckStatements flg cat asts
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
>        $ H.assertBool "" allTyped
>   unless (et == got) $ trace (groomTypes q'') $ return ()
>   H.assertEqual "" et got
>   where
>     extractInsert [i@Insert{}] = i
>     extractInsert x = error $ "expected a single insert statement, got " ++ groomTypes x

rewrite the queryexpr with all the options true

pretty print, then check that the resultant sql parses the same, and
type checks properly and produces the same type

> _queryExprRewrites :: [CatalogUpdate] -> L.Text -> Either [TypeError] Type -> IO () --Test.Framework.Test
> _queryExprRewrites cus src et = {-testCase ("rewrite expanded " ++ src) $-} do
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
>   H.assertEqual "rewrite pp . parse" (resetAnnotations aast) (resetAnnotations aastrw)
>   H.assertEqual "rewrite ty" ty tyrw
>   let (_,errs,noTypeQEs,noTypeSEs) = tcTreeInfo aast
>       er = concatMap fst errs
>       allTyped = case et of
>                    Left _ -> True -- don't check if everything is typed
>                                   -- if expecting a type error
>                    Right _ -> null noTypeSEs && null noTypeQEs
>   unless (null er) $
>        trace ("errors in tree: " ++ groomTypes aastrw)
>        $ H.assertBool "" (null er)
>   unless allTyped $
>        trace ("MISSING TYPES: " ++ groomTypes aastrw)
>        $ H.assertBool "" allTyped




> testRewrite :: TypeCheckingFlags -> [CatalogUpdate] -> L.Text -> L.Text
>             -> T.TestTree
> testRewrite f cus src src' = H.testCase ("rewrite " ++ L.unpack src) $ do
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
>       else id) $ H.assertEqual "" ast' astrw
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





~~~~
TODO
new idea for testing:
parsesql -> ast1
parse, pretty print, parse -> ast2
load into pg, pg_dump, parse -> ast3
parse, pretty print, load into pg, pg_dump, parse -> ast4
check all these asts are the same
~~~~
