
cd /home/jake/wd/hssqlppp/trunk/src/lib/Database/HsSqlPpp/AstInternals && uuagc --genlinepragmas -dcfwsp AstInternal.ag && cd /home/jake/wd/hssqlppp/trunk/ && cabal build

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.FixUpIdentifiersTests
>     (fixUpIdentifiersTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> --import Data.Generics.Uniplate.Data
> import Control.Monad

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.SqlTypes
> --import Database.HsSqlPpp.Utils.PPExpr
> --import Database.HsSqlPpp.Tests.TestUtils
> import Database.HsSqlPpp.PrettyPrinter
> --import Database.HsSqlPpp.Tests.TpchData
> --import Database.HsSqlPpp.Utils.Here

> data Item = Group String [Item]
>           | Item [CatalogUpdate] String String
>           | MSItem [CatalogUpdate] String String
>
> fixUpIdentifiersTests :: Test.Framework.Test
> fixUpIdentifiersTests = itemToTft fixUpIdentifiersTestsData
>
> fixUpIdentifiersTestsData :: Item
> fixUpIdentifiersTestsData = Group "cantests"
>   [Group "select lists"
>     [Item db1 "select a,b from t;" "select t.a as a, t.b as b from t as t(a,b);"
>     ,Item db1 "select * from t;" "select t.a as a, t.b as b from t as t(a,b);"
>     ,Item db1 "select * from (select a from t) t;"
>               "select t.a as a from (select t.a as a from t as t(a,b)) as t(a);"
>     ,Item db1 "select * from generate_series(1,5);"
>               "select generate_series.generate_series as generate_series\n\
>               \from generate_series(1,5) as generate_series(generate_series);"
>     ,Item db1 "select a,c from t,u;"
>               "select t.a as a, u.c as c from t as t(a,b),u as u(c,d);"
>     ,Item db1 "select * from t,u;"
>               "select t.a as a,t.b as b,u.c as c,u.d as d \n\
>               \from t as t(a,b),u as u(c,d);"
>     ,Item db1 "select t.* from t,u;"
>               "select t.a as a, t.b as b from t as t(a,b),u as u(c,d);"
>     ,Item db1 "select u.* from t,u;"
>               "select u.c as c,u.d as d from t as t(a,b),u as u(c,d);"
>     ,Item db1 "select t.*,u.*,* from t,u;"
>               "select t.a as a, t.b as b,\n\
>               \       u.c as c, u.d as d,\n\
>               \       t.a as a, t.b as b, u.c as c, u.d as d\n\
>               \  from t as t(a,b),u as u(c,d);"
>     ,Item db1 "select v from v;" "select v.v as v from v as v(v);"
>     ,Item db1 "select count(*) from t;"
>               "select count(true) as count from t as t(a,b);"
>     ]
>   ,Group "trefs"
>     [Item db1 "select * from generate_series(1,5) g;"
>               "select g.generate_series as generate_series\n\
>               \from generate_series(1,5) as g(generate_series);"
>     ,Item db1 "select a from t as x;"
>               "select x.a as a from t as x(a,b);"
>     ,Item db1 "select * from t as x(f,g);"
>               "select x.f as f ,x.g as g from t as x(f,g);"
>     ]
>   ,Group "where"
>     [Item db1 "select a,b from t where a > b;"
>               "select t.a as a, t.b as b from t as t(a,b) where t.a > t.b;"
>     ,Item db1 "select a,c from t,u where b = d;"
>               "select t.a as a,u.c as c\n\
>               \  from t as t(a,b),u as u(c,d)\n\
>               \  where t.b = u.d;"
>     ]
>   ,Group "groupby"
>     [Item db1 "select a,sum(b) from t group by a;"
>               "select t.a as a,sum(t.b) as sum\n\
>               \from t as t(a,b) group by t.a;"
>     ]
>   ,Group "having"
>     [Item db1 "select a,sum(b) from t group by a having a > b;"
>               "select t.a as a,sum(t.b) as sum from t as t(a,b)\n\
>               \group by t.a having t.a > t.b;"
>     ]
>   ,Group "orderby"
>     [Item db1 "select a from t order by b;"
>               "select t.a as a from t as t(a,b) order by t.b;"
>     ]
>    -- needs some work before this passes
>   {-,Group "ctes"
>     [Item db1   "with a as (select 1 as a, 2 as b),\n\
>                 \     b as (select * from t)\n\
>                 \select * from a\n\
>                 \union select * from b;"
>
>                 "with a(a,b) as (select 1 as a, 2 as b),\n\
>                 \     b(a,b) as (select t.a as a, t.b as b from t as t(a,b))\n\
>                 \select a.a as a, a.b as b from a as a(a,b)\n\
>                 \union select b.a as a, b.b as b from b as b(a,b);"

>     ]-}
>   ,Group "correlated subqueries"
>     [Item db1 "select a,b from t where (select min(c) from u where b=d);"
>        "select t.a as a,t.b as b from t as t(a,b)\n\
>        \where (select min(u.c) as min from u as u(c,d) where t.b=u.d);"
>     ]
>   ,Group "select lists"
>     [Item db1 "select a,b,f(a),a::int,a+b,row_number() over (order by a), a as c from t;"
>               "select t.a as a,t.b as b,f(t.a) as f,t.a::int as \"int\",t.a+t.b as \"?column?\",row_number() over (order by t.a) as row_number, t.a as c from t as t(a,b);"
>     ]
>   {-,Group "tpch"
>     $ zipWith (\q q1 -> MSItem tpchCatalog q q1) (map snd tpchQueries)
>         [[$here|
\begin{code}
select
        lineitem.l_returnflag as l_returnflag,
        lineitem.l_linestatus as l_linestatus,
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
set rowcount -1
go
\end{code}
>                                     |]
>         ,[$here|
\begin{code}

select
        supplier.s_acctbal as s_acctbal,
        supplier.s_name as s_name,
        nation.n_name as n_name,
        part.p_partkey as p_partkey,
        part.p_mfgr as p_mfgr,
        supplier.s_address as s_address,
        supplier.s_phone as s_phone,
        supplier.s_comment as s_comment
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
                        min(partsupp.ps_supplycost) as min
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
set rowcount 100
go
\end{code}
>                                     |]
>     ] -}
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
> itemToTft (Item eu sql esql) = itemToTft' parseQueryExpr eu sql esql
> itemToTft (MSItem eu sql esql) = itemToTft' parseSqlServerQueryExpr eu sql esql

> type P =  String -> String -> Either ParseErrorExtra QueryExpr

> itemToTft' :: P -> [CatalogUpdate] -> String -> String -> Test.Framework.Test
> itemToTft' p eu sql esql = testCase sql $ do
>   let eAst = [QueryStatement emptyAnnotation
>               $ case p "" esql of
>                              Left e -> error $ show e
>                              Right l -> resetAnnotations l]
>       ast = [QueryStatement emptyAnnotation
>              $ case p "" sql of
>                            Left e -> error $ show e
>                            Right l -> resetAnnotations l]
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
