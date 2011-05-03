
Tests using the tpch queries. Just tests the result type at the
moment.

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.TypeChecking.TpchTests
>     (tpchTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> --import Data.List
> import Data.Generics.Uniplate.Data
> import Control.Monad
>
> --import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.SqlTypes
> import Database.HsSqlPpp.Utils.PPExpr
> --import Database.HsSqlPpp.Tests.TestUtils
> import Database.HsSqlPpp.PrettyPrinter
> import Database.HsSqlPpp.Tests.TpchData
>
> data Item = Group String [Item]
>           | Query String String Type
>
> tpchTests :: Test.Framework.Test
> tpchTests = itemToTft tpchTestData
>
> tpchTestData :: Item
> tpchTestData =
>   Group "tpch" $ zipWith (\(n,s) t ->
>                           Query n s (SetOfType $ CompositeType t))
>   tpchQueries
>   [-- q1
>    [("l_returnflag", typeChar), ("l_linestatus", typeChar)
>    ,("sum_qty", typeNumeric), ("sum_base_price", typeNumeric)
>    ,("sum_disc_price", typeNumeric), ("sum_charge", typeNumeric)
>    ,("avg_qty", typeNumeric),("avg_price", typeNumeric)
>    ,("avg_disc", typeNumeric),("count_order", typeBigInt)]
>   ,--q2
>    [("s_acctbal", typeNumeric)
>    ,("s_name", typeChar)
>    ,("n_name", typeChar), ("p_partkey", typeInt), ("p_mfgr", typeChar)
>    ,("s_address", typeVarChar), ("s_phone", typeChar)
>    ,("s_comment", typeVarChar)]

>   ]

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Group s is) = testGroup s $ map itemToTft is
> itemToTft (Query n sql t) = testCase ("typecheck tpch " ++ n) $ do
>   let ast = case parseQueryExpr "" sql of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckQueryExpr cat ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = [x | x <- universeBi aast]
>       res = if null er
>             then case ty of
>                    Nothing -> Left []
>                    Just ty' -> Right ty'
>             else Left er
>   when (Right t /= res) $ putStrLn $ "bad sql: " ++ printQueryExpr aast
>        ++ "\n" ++ ppExpr aast
>   assertEqual "" (Right t) res
>   where
>     cat = case updateCatalog defaultTemplate1Catalog tpchCatalog of
>                         Left x -> error $ show x
>                         Right e -> e



