
cd /home/jake/wd/hssqlppp/trunk/src/lib/Database/HsSqlPpp/AstInternals && uuagc --genlinepragmas -dcfwsp AstInternal.ag && cd /home/jake/wd/hssqlppp/trunk/ && cabal build

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.FixUpIdentifiersTests
>     (fixUpIdentifiersTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Generics.Uniplate.Data
> import Control.Monad

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.SqlTypes
> import Database.HsSqlPpp.Utils.PPExpr
> import Database.HsSqlPpp.Tests.TestUtils
> import Database.HsSqlPpp.PrettyPrinter
> import Database.HsSqlPpp.Tests.TpchData
> import Database.HsSqlPpp.Utils.Here

> data Item = Group String [Item]
>           | Item [CatalogUpdate] String String
>
> fixUpIdentifiersTests :: Test.Framework.Test
> fixUpIdentifiersTests = itemToTft fixUpIdentifiersTestsData
>
> fixUpIdentifiersTestsData :: Item
> fixUpIdentifiersTestsData = Group "cantests"
>   [Group "select lists"
>     [Item db1 "select a,b from t;" "select t.a, t.b from t;"
>     ,Item db1 "select * from t;" "select t.a, t.b from t;"
>     ,Item db1 "select a,c from t,u;" "select t.a, u.c from t,u;"
>     ,Item db1 "select * from t,u;" "select t.a,t.b,u.c,u.d from t,u;"
>     ,Item db1 "select t.* from t,u;" "select t.a, t.b from t,u;"
>     ,Item db1 "select u.* from t,u;" "select u.c,u.d from t,u;"
>     ,Item db1 "select v from v;" "select v.v from v;"
>     ,Item db1 "select count(*) from t;" "select count(*) from t;"
>     ]
>   ,Group "where"
>     [Item db1 "select a,b from t where a > b;" "select t.a, t.b from t where t.a > t.b;"
>     ,Item db1 "select a,c from t,u where b = d;" "select t.a,u.c from t,u where t.b = u.d;"
>     ]
>   ,Group "groupby"
>     [Item db1 "select a,sum(b) from t group by a;" "select t.a,sum(t.b) from t group by t.a;"
>     ]
>   ,Group "having"
>     [Item db1 "select a,sum(b) from t group by a having a > b;" "select t.a,sum(t.b) from t group by t.a having t.a > t.b;"
>     ]
>   ,Group "orderby"
>     [Item db1 "select a from t order by b;" "select t.a from t order by t.b;"
>     ]
>   ,Group "correlated subquery"
>     [Item db1 "select a,b from t where (select min(c) from u where b=d);"
>        "select t.a,t.b from t where (select min(u.c) from u where t.b=u.d);"
>     ]
>   ,Group "tpch"
>     $ zipWith (\q q1 -> Item tpchCatalog q q1) (map snd tpchQueries)
>         [[$here|
\begin{code}
select
        lineitem.l_returnflag,
        lineitem.l_linestatus,
        sum(lineitem.l_quantity) as sum_qty,
        sum(lineitem.l_extendedprice) as sum_base_price,
        sum(lineitem.l_extendedprice * (1 - lineitem.l_discount)) as sum_disc_price,
        sum(lineitem.l_extendedprice * (1 - lineitem.l_discount) * (1 + lineitem.l_tax)) as sum_charge,
        avg(lineitem.l_quantity) as avg_qty,
        avg(lineitem.l_extendedprice) as avg_price,
        avg(lineitem.l_discount) as avg_disc,
        count(*) as count_order
from
        lineitem
where
        lineitem.l_shipdate <= date '1998-12-01' - interval '63' day (3)
group by
        lineitem.l_returnflag,
        lineitem.l_linestatus
order by
        lineitem.l_returnflag,
        lineitem.l_linestatus;
\end{code}
>                                     |]
>         ,[$here|
\begin{code}

select
        supplier.s_acctbal,
        supplier.s_name,
        nation.n_name,
        part.p_partkey,
        part.p_mfgr,
        supplier.s_address,
        supplier.s_phone,
        supplier.s_comment
from
        part,
        supplier,
        partsupp,
        nation,
        region
where
        part.p_partkey = partsupp.ps_partkey
        and supplier.s_suppkey = partsupp.ps_suppkey
        and part.p_size = 15
        and part.p_type like '%BRASS'
        and supplier.s_nationkey = nation.n_nationkey
        and nation.n_regionkey = region.r_regionkey
        and region.r_name = 'EUROPE'
        and partsupp.ps_supplycost = (
                select
                        min(partsupp.ps_supplycost)
                from
                        partsupp,
                        supplier,
                        nation,
                        region
                where
                        part.p_partkey = partsupp.ps_partkey
                        and supplier.s_suppkey = partsupp.ps_suppkey
                        and supplier.s_nationkey = nation.n_nationkey
                        and nation.n_regionkey = region.r_regionkey
                        and region.r_name = 'EUROPE'
        )
order by
        supplier.s_acctbal desc,
        nation.n_name,
        supplier.s_name,
        part.p_partkey;
--set rowcount 100
--go
\end{code}
>                                     |]
>     ]
>   ]

qualifier and column name the same


> db1 :: [CatalogUpdate]
> db1 = [CatCreateTable "t" [("a",typeInt)
>                           ,("b", typeInt)] []
>       ,CatCreateTable "u" [("c",typeInt)
>                           ,("d", typeInt)] []
>       ,CatCreateTable "v" [("v",typeInt)] []]


------------------------

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Group s is) = testGroup s $ map itemToTft is
> itemToTft (Item eu sql esql) = testCase sql $ do
>   let eAst = case parseStatements "" esql of
>                               Left e -> error $ show e
>                               Right l -> resetAnnotations l
>       ast = case parseStatements "" sql of
>                               Left e -> error $ show e
>                               Right l -> resetAnnotations l
>       cAst = fixUpIdentifiers makeCat ast
>       c2Ast = fixUpIdentifiers makeCat cAst
>   --putStrLn $ printStatements cAst ++ "\n" ++ printStatements c2Ast
>   when (eAst /= cAst) $ do
>     putStrLn $ "\nExpected:\n\n" ++ printStatements eAst
>     putStrLn $ "\nGot:\n\n" ++ printStatements cAst ++ "\n\n"
>   when (cAst /= c2Ast) $ do
>     putStrLn $ "\nREDO, Expected:\n\n" ++ printStatements cAst
>     putStrLn $ "\nGot:\n\n" ++ printStatements c2Ast ++ "\n\n"
>   assertEqual "" eAst cAst
>   assertEqual "redo" cAst c2Ast
>   where
>     makeCat = case updateCatalog defaultTemplate1Catalog eu of
>                         Left x -> error $ show x
>                         Right e -> e
