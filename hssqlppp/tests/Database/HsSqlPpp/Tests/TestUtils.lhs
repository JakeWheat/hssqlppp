
> module Database.HsSqlPpp.Tests.TestUtils
>     (assertTrace,itemToTft) where
>
> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as H
> --import Data.List
> import Data.Generics.Uniplate.Data
> import Data.Data

> import Debug.Trace
> import Control.Monad

> --import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Parse
> import Database.HsSqlPpp.Syntax
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.TypeCheck
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Utils.GroomUtils
> import Database.HsSqlPpp.Utility
> --import Database.HsSqlPpp.SqlTypes
> --import Database.HsSqlPpp.Utils.PPExpr
> import Data.Text.Lazy (Text)
> import qualified Data.Text.Lazy as L
> import qualified Data.Text as T
> import Database.HsSqlPpp.Lex (lexTokens,prettyToken,Token)
> --import Text.Parsec.Text (runParser)
> --import Control.Applicative
> --import Data.Generics.Uniplate.Data
> import Database.HsSqlPpp.Tests.TestTypes
> import Database.HsSqlPpp.Internals.TypeChecking.TypeConversion.TypeConversion2

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
> --import Text.Show.Pretty
> --import Debug.Trace
> --import Database.HsSqlPpp.Tests.TestUtils
> --import Control.Monad

> --import Database.HsSqlPpp.Utils.GroomUtils

> --import qualified Data.Text.Lazy as L
> import Database.HsSqlPpp.Dialect

> assertTrace :: (Show a,Eq a) => String -> String -> a -> a -> IO ()
> assertTrace nem s a1 a2 = do
>     when (a1 /= a2) $ trace nem $ return ()
>     H.assertEqual s a1 a2

> itemToTft :: Item -> T.TestTree
> itemToTft (ParseScalarExpr f a b) = testParseScalarExpr f a b
> itemToTft (ParseQueryExpr f a b) = testParseQueryExpr f a b
> --itemToTft (ParseProcSql f a b) = testParseProcSql f a b
> itemToTft (ParseStmts f a b) = testParseStatements f a b
> itemToTft (ParseProcSql f a b) = testParseProcSql f a b
> {-itemToTft (TSQL a b) =
>   testParsePlpgsqlStatements (if True
>                        then SQLServer
>                        else PostgreSQL) a b-}
> {-itemToTft (OracleX a b) =
>   testParsePlpgsqlStatements Oracle a b -}
> --itemToTft (MSStmt a b) = testParseStatements a b
> itemToTft (Group s is) = T.testGroup s $ map itemToTft is
> itemToTft (Lex d a b) = testLex d a b

> itemToTft (TCScalExpr c e f s r) = testTCScalarExpr c e f s r
> itemToTft (TCQueryExpr cat f s r) = testQueryExprType cat f s r
> itemToTft (TCStatements cat f s r) = testStatementsTypecheck cat f s r
> itemToTft (InsertQueryExpr cus s r) = testInsertQueryExprType sqlServerDialect {-PostgreSQL-} cus s r
> --itemToTft (TSQLQueryExpr cus s r) = undefined --testQueryExprType SQLServer cus s r
> --itemToTft (OracleQueryExpr cus s r) = undefined --testQueryExprType Oracle cus s r
> itemToTft (RewriteQueryExpr f cus s s') = testRewrite f cus s s'

> itemToTft (ImpCastsScalar f s s') = testImpCastsScalar f s s'
> itemToTft (ScalarExprExtra d cat env s r) = testScalarExprTypeExtra d cat env s r
> itemToTft (MatchApp d cat f as r) = testMatchApp d cat f as r
> itemToTft (Custom nm f) = H.testCase nm f

> testParseScalarExpr :: ParseFlags -> Text -> ScalarExpr -> T.TestTree
> testParseScalarExpr f src ast =
>   parseUtil src ast (parseScalarExpr f "" Nothing)
>                     (parseScalarExpr f "" Nothing)
>                     (prettyScalarExpr
>                      defaultPrettyFlags {ppDialect = pfDialect f} )
> testParseQueryExpr :: ParseFlags -> Text -> QueryExpr -> T.TestTree
> testParseQueryExpr f src ast =
>   parseUtil src ast (parseQueryExpr f "" Nothing)
>                     (parseQueryExpr f "" Nothing)
>                     (prettyQueryExpr defaultPrettyFlags {ppDialect = pfDialect f})

>
> testParseStatements :: ParseFlags -> Text -> [Statement] -> T.TestTree
> testParseStatements flg src ast =
>   let parse = parseStatements flg "" Nothing
>       pp = prettyStatements defaultPrettyFlags {ppDialect=pfDialect flg}
>   in parseUtil src ast parse parse pp

> testParseProcSql :: ParseFlags -> Text -> [Statement] -> T.TestTree
> testParseProcSql flg src ast =
>   let parse = parseProcSQL flg "" Nothing
>       pp = prettyStatements defaultPrettyFlags {ppDialect=pfDialect flg}
>   in parseUtil src ast parse parse pp

>
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

> testLex :: Dialect -> T.Text -> [Token] -> T.TestTree
> testLex d t r = H.testCase ("lex "++ T.unpack t) $ do
>     let x = lexTokens d "" Nothing t
>         y = either (error . show) id x
>     H.assertEqual "lex" r (map snd y)
>     let t' = L.concat $ map (prettyToken d) r
>     H.assertEqual "lex . pretty" (L.fromChunks [t]) t'

> testTCScalarExpr :: Catalog -> Environment -> TypeCheckFlags
>                    -> L.Text -> Either [TypeError] Type -> T.TestTree
> testTCScalarExpr cat env f src et =
>   H.testCase ("typecheck " ++ L.unpack src) $ do
>   let ast = case parseScalarExpr defaultParseFlags {pfDialect = tcfDialect f} "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExpr f cat
>              (canonicalizeEnvTypes (tcfDialect f) env)
>              $ canonicalizeTypes (tcfDialect f) ast
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

> testScalarExprTypeExtra:: Dialect -> Catalog -> Environment -> L.Text
>                           -> Either [TypeError] TypeExtra
>                           -> T.TestTree
> testScalarExprTypeExtra d cat env src ete = H.testCase ("typecheck " ++ L.unpack src) $ do
>   let ast = case parseScalarExpr defaultParseFlags {pfDialect = d} "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExpr defaultTypeCheckFlags {tcfDialect = d} cat
>              (canonicalizeEnvTypes d env)
>              $ canonicalizeTypes d ast
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

> testImpCastsScalar :: TypeCheckFlags -> L.Text -> L.Text -> T.TestTree
> testImpCastsScalar f src wsrc = H.testCase ("typecheck " ++ L.unpack src) $
>   let ast = case parseScalarExpr defaultParseFlags "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckScalarExpr f (diDefaultCatalog postgresDialect) emptyEnvironment
>              $ canonicalizeTypes (tcfDialect f) ast
>       aast' = addExplicitCasts aast
>       wast = case parseScalarExpr defaultParseFlags "" Nothing wsrc of
>                Left e -> error $ show e
>                Right l -> l
>   in (if (resetAnnotations aast') /= (resetAnnotations wast)
>       then trace ("\n***************** got: \n"
>                   ++ L.unpack (prettyScalarExpr defaultPrettyFlags aast')
>                   ++ "\nwanted:\n" ++ L.unpack (prettyScalarExpr defaultPrettyFlags wast)
>                   ++ "\n*****************\n"
>                   ++ "\n***************** got: \n"
>                   ++ groomNoAnns aast'
>                   ++ "\nwanted:\n" ++ groomNoAnns wast
>                   ++ "\n*****************\n"
>                   )
>       else id) $ H.assertEqual "" (resetAnnotations aast') (resetAnnotations wast)


> testQueryExprType :: Catalog -> TypeCheckFlags
>                   -> L.Text -> Either [TypeError] Type -> T.TestTree
> testQueryExprType cat f src et =
>   H.testCase ("typecheck " ++ L.unpack src) $ do
>   let ast = case parseQueryExpr
>                  defaultParseFlags {pfDialect = tcfDialect f}
>                  "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       {-Right cat = updateCatalog cus $ case dl of
>           PostgreSQL -> defaultTemplate1Catalog
>           SQLServer -> defaultTSQLCatalog
>           Oracle -> defaultTSQLCatalog-}
>       {-flg = case dl of
>           PostgreSQL -> defaultTypeCheckFlags
>           SQLServer -> defaultTypeCheckFlags {tcfDialect = SQLServer}
>           Oracle -> defaultTypeCheckFlags {tcfDialect = Oracle}-}
>       aast = typeCheckQueryExpr f cat
>              $ canonicalizeTypes (tcfDialect f) ast
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

> testStatementsTypecheck :: Catalog -> TypeCheckFlags -> L.Text -> Maybe [TypeError] -> T.TestTree
> testStatementsTypecheck cat f src et =
>   H.testCase ("typecheck " ++ L.unpack src) $ do
>   let ast = case parseStatements defaultParseFlags {pfDialect = tcfDialect f} "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       {-Right cat = updateCatalog cus $ case dl of
>           PostgreSQL -> defaultTemplate1Catalog
>           SQLServer -> defaultTSQLCatalog
>           Oracle -> defaultTSQLCatalog
>       flg = case dl of
>           PostgreSQL -> defaultTypeCheckFlags
>           SQLServer -> defaultTypeCheckFlags {tcfDialect = SQLServer}
>           Oracle -> defaultTypeCheckFlags {tcfDialect = Oracle}-}
>       (_,aast) = typeCheckStatements f cat
>                  $ canonicalizeTypes (tcfDialect f) ast
>       (_,errs,noTypeQEs,noTypeSEs) = tcTreeInfo aast
>       er = concatMap fst errs
>       got :: Maybe [TypeError]
>       got = case () of
>               _ | null er -> Nothing
>                 | otherwise -> Just er
>       allTyped = case et of
>                    Just _ -> True -- don't check if everything is typed
>                                   -- if expecting a type error
>                    Nothing -> null noTypeSEs && null noTypeQEs
>   unless allTyped $
>        trace ("MISSING TYPES: " ++ groomTypes aast)
>        $ H.assertBool "" allTyped
>   unless (et == got) $ trace (groomTypes aast) $ return ()
>   H.assertEqual "" et got
>   --queryExprRewrites cus src et


> testInsertQueryExprType :: Dialect -> [CatalogUpdate] -> L.Text -> Either [TypeError] Type -> T.TestTree
> testInsertQueryExprType dl cus src et = H.testCase ("typecheck " ++ L.unpack src) $ do
>   let cat = makeCatalog dl cus
>       flg = defaultTypeCheckFlags {tcfDialect = dl}
>       asts = either (error . show) id $ parseStatements defaultParseFlags "" Nothing src
>       Insert _ _ _ q _ = extractInsert $ snd $ typeCheckStatements flg cat
>                          $ canonicalizeTypes (tcfDialect flg) asts
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
>   let cat = makeCatalog postgresDialect cus
>       aast = typeCheckQueryExpr
>                defaultTypeCheckFlags {tcfAddQualifiers = True
>                                         ,tcfAddSelectItemAliases = True
>                                         ,tcfExpandStars = True
>                                         ,tcfAddFullTablerefAliases = True}
>                cat $ canonicalizeTypes
>                      (tcfDialect defaultTypeCheckFlags) ast
>       ty = anType $ getAnnotation aast
>       -- print with rewritten tree
>       pp = prettyQueryExpr defaultPrettyFlags aast
>       astrw = case parseQueryExpr defaultParseFlags "" Nothing pp of
>                 Left e -> error $ "parse: " ++ L.unpack pp ++ "\n" ++ show e
>                 Right l -> l
>       aastrw = typeCheckQueryExpr
>                  defaultTypeCheckFlags
>                  cat $ canonicalizeTypes
>                        (tcfDialect defaultTypeCheckFlags) astrw
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




> testRewrite :: TypeCheckFlags -> [CatalogUpdate] -> L.Text -> L.Text
>             -> T.TestTree
> testRewrite f cus src src' = H.testCase ("rewrite " ++ L.unpack src) $ do
>   let ast = case parseQueryExpr defaultParseFlags {pfDialect = tcfDialect f}
>                  "" Nothing src of
>               Left e -> error $ show e
>               Right l -> l
>       cat = makeCatalog (tcfDialect f) cus
>       aast = typeCheckQueryExpr f cat ast
>       astrw = resetAnnotations aast
>       ast' = case parseQueryExpr defaultParseFlags "" Nothing src' of
>               Left e -> error $ show e
>               Right l -> resetAnnotations l
>   (if astrw /= ast'
>       then trace ("\n***************** expected\n" ++
>                   L.unpack (prettyQueryExpr defaultPrettyFlags ast')
>                   ++ "\n" ++ L.unpack (prettyQueryExpr defaultPrettyFlags astrw)
>                   ++ "\n\n" ++ groomTypes ast'
>                   ++ "\n\n" ++ groomTypes astrw
>                   ++ "\n***************** got\n")
>       else id) $ H.assertEqual "" ast' astrw
>   -- check second rewrite is idempotent
>   {-let astrw2 = resetAnnotations $ typeCheckQueryExpr f cat astrw
>   (if astrw /= astrw2
>       then trace ("\nSECOND REWRITE\n***************** expected\n" ++
>                   prettyQueryExpr defaultPrettyFlags astrw
>                   ++ "\n" ++ prettyQueryExpr defaultPrettyFlags astrw2
>                   ++ "\n\n" ++ groomTypes astrw
>                   ++ "\n\n" ++ groomTypes astrw2
>                   ++ "\n***************** got\n")
>       else id) $ assertEqual "second rewrite" astrw astrw2-}


> testMatchApp :: Dialect -> Catalog -> [NameComponent]
>              -> [(TypeExtra, Maybe LitArg)]
>              -> (Either [TypeError] ([TypeExtra],TypeExtra))
>              -> T.TestTree
> testMatchApp d cat f as r = H.testCase (show f ++ show as) $
>     H.assertEqual "" r $ matchApp d cat f as


> canonicalizeTypes :: Data a => Dialect -> a -> a
> canonicalizeTypes d = transformBi $ \x -> case x of
>     ScalarType t -> ScalarType $ canonicalizeTypeName d t
>     _ -> x

> canonicalizeEnvTypes :: Data a => Dialect -> a -> a
> canonicalizeEnvTypes d = transformBi $ \x -> case x of
>     TypeExtra (ScalarType t) p s n ->
>       TypeExtra (ScalarType $ canonicalizeTypeName d t) p s n
>     _ -> x

~~~~
TODO
new idea for testing:
parsesql -> ast1
parse, pretty print, parse -> ast2
load into pg, pg_dump, parse -> ast3
parse, pretty print, load into pg, pg_dump, parse -> ast4
check all these asts are the same
~~~~
