
cd /home/jake/wd/hssqlppp/trunk/src/lib/Database/HsSqlPpp/Internals && uuagc --genlinepragmas -dcfwsp AstInternal.ag && cd /home/jake/wd/hssqlppp/trunk/ && cabal build

> module Database.HsSqlPpp.Tests.FixTree.FixUpIdentifiersTests
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
> import Database.HsSqlPpp.Types
> --import Database.HsSqlPpp.Utils.PPExpr
> --import Database.HsSqlPpp.Tests.TestUtils
> import Database.HsSqlPpp.Pretty
> --import Database.HsSqlPpp.Tests.TpchData
> --import Database.HsSqlPpp.Utils.Here

> data Item = Group String [Item]
>           | Item [CatalogUpdate] String String
>           | SItem [CatalogUpdate] String String
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
>               "select g.g as g\n\
>               \from generate_series(1,5) as g(g);"
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
>    -- fixme: needs some work before these pass
>   {-,Group "ctes"
>     [Item []   "with ta as (select 1 as a, 2 as b)\n\
>                 \select * from ta;"
>
>                 "with ta(a,b) as (select 1 as a, 2 as b)\n\
>                 \select ta.a as a, ta.b as b from ta as ta(a,b);"
>     ,Item db1   "/*66642*/with ta as (select 1 as a, 2 as b),\n\
>                 \     tb as (select * from t)\n\
>                 \select * from ta\n\
>                 \union select * from tb;"
>
>                 "with ta(a,b) as (select 1 as a, 2 as b),\n\
>                 \     tb(a,b) as (select t.a as a, t.b as b from t as t(a,b))\n\
>                 \select ta.a as a, ta.b as b from ta as ta(a,b)\n\
>                 \union select tb.a as a, tb.b as tb from tb as tb(a,b);"

>     ]-}
>   ,Group "correlated subqueries"
>     [Item db1 "select a,b from t where (select min(c) from u where b=d);"
>        "select t.a as a,t.b as b from t as t(a,b)\n\
>        \where (select min(u.c) as min from u as u(c,d) where t.b=u.d);"
>     ]
>   ,Group "funtrefs"
>     [Item [] "select * from generate_series(1,7) g\n\
>              \where g not in (select * from generate_series(3,5));"
>              "select g.g as g from generate_series(1,7) g(g)\n\
>              \where g.g not in (select generate_series.generate_series as generate_series\n\
>              \                  from generate_series(3,5) as generate_series(generate_series));"
>     ,Item [] "select g from generate_series(1,7) g\n\
>              \where g not in (select * from generate_series(3,5));"
>              "select g.g as g from generate_series(1,7) g(g)\n\
>              \where g.g not in (select generate_series.generate_series as generate_series\n\
>              \                  from generate_series(3,5) as generate_series(generate_series));"
>     ,Item [] "select g.* from generate_series(1,7) g\n\
>              \where g not in (select * from generate_series(3,5));"
>              "select g.g as g from generate_series(1,7) g(g)\n\
>              \where g.g not in (select generate_series.generate_series as generate_series\n\
>              \                  from generate_series(3,5) as generate_series(generate_series));"

>     ]
>   ,Group "select lists"
>     [Item db1 "select a,b,f(a),a::int,a+b,row_number() over (order by a), a as c from t;"
>               "select t.a as a,t.b as b,f(t.a) as f,t.a::int as \"int\",t.a+t.b as \"?column?\",row_number() over (order by t.a) as row_number, t.a as c from t as t(a,b);"
>     ]
>   ,Group "joins"
>     [Item db1 "select * from t natural inner join t1;"
>               "select t.a as a,t.b as b,t1.c as c \
>               \from t as t(a,b) natural inner join t1 as t1(a,c);"
>     ,Item db1 "select * from t inner join t1 using (a);"
>               "select t.a as a,t.b as b,t1.c as c \
>               \from t as t(a,b) inner join t1 as t1(a,c) using (a);"
>     ,Item db1 "select * from t cross join t1;"
>               "select t.a as a,t.b as b,t1.a as a,t1.c as c \
>               \from t as t(a,b) cross join t1 as t1(a,c);"
>     ,Item db1 "select t.* from t natural inner join t1;"
>               "select t.a as a,t.b as b \
>               \from t as t(a,b) natural inner join t1 as t1(a,c);"
>     ,Item db1 "select t1.* from t natural inner join t1;"
>               "select t1.a as a,t1.c as c \
>               \from t as t(a,b) natural inner join t1 as t1(a,c);"
>     ]
>   ,Group "dml"
>     [SItem db1 "update t set a = b where a = 5;"
>               "update t set a = t.b where t.a = 5;"
>     ,SItem db1 "delete from t where a = 0;"
>               "delete from t where t.a = 0;"]
>   ,Group "returning"
>     [SItem db1 "update t set a = 1 returning a;"
>                "update t set a = 1 returning t.a as a;"
>     ,SItem db1 "insert into t (a,b) values (1,2) returning a,b;"
>                "insert into t (a,b) values (1,2) returning t.a as a,t.b as b;"
>     ,SItem db1 "delete from t returning a;"
>                "delete from t returning t.a as a;"]
>   ]

qualifier and column name the same


> db1 :: [CatalogUpdate]
> db1 = [CatCreateTable "t" [("a",typeInt)
>                           ,("b", typeInt)] []
>       ,CatCreateTable "u" [("c",typeInt)
>                           ,("d", typeInt)] []
>       ,CatCreateTable "v" [("v",typeInt)] []
>       ,CatCreateTable "t1" [("a",typeInt)
>                            ,("c", typeInt)] []
>       ]


------------------------

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Group s is) = testGroup s $ map itemToTft is
> itemToTft (Item eu sql esql) =
>   itemToTft' parseStatement eu sql esql
> --itemToTft (MSItem eu sql esql) = itemToTft' parseSqlServerQueryExpr eu sql esql
> itemToTft (SItem eu sql esql) = itemToTft' parseStatement eu sql esql

> parseStatement :: String -> String -> Either ParseErrorExtra Statement
> parseStatement f s =
>   let p = parseStatements f s
>   in case p of
>        Left e -> Left e
>        Right [a] -> Right a
>        Right _ -> error "wrong number of statements parsed"

> type P = String -> String -> Either ParseErrorExtra Statement

> itemToTft' :: P -> [CatalogUpdate] -> String -> String -> Test.Framework.Test
> itemToTft' p eu sql esql = testCase sql $ do
>   let eAst = [case p "" esql of
>                              Left e -> error $ show e
>                              Right l -> resetAnnotations l]
>       ast = [case p "" sql of
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
