
> module Database.HsSqlPpp.Tests.TypeChecking.SelectFrom
>     (tcSelectFromTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> import Database.HsSqlPpp.Catalog

> tcSelectFromTestData :: Item
> tcSelectFromTestData = -- FIXME: some of these fail
>   Group "selects from" [
>   Group "simple selects from" [
>       s "select a from (select 1 as a, 2 as b) x;"
>         $ Right [Just ([], [("a", typeInt)])]
>      ,s "select b from (select 1 as a, 2 as b) x;"
>         $ Right [Just ([], [("b", typeInt)])]
>      ,s "select c from (select 1 as a, 2 as b) x;"
>         $ Left [UnrecognisedIdentifier "c"]
>      ,s "select typlen from pg_type;"
>         $ Right [Just ([], [("typlen", typeSmallInt)])]
>      -- fixme: oid?
>      ,s "select oid from pg_type;"
>         $ Right [Just ([], [("oid", ScalarType "oid")])]
>      ,s "select p.oid from pg_type p;"
>         $ Right [Just ([], [("oid", ScalarType "oid")])]
>      ,s "select typlen from nope;"
>         $ Left [UnrecognisedRelation "nope"]
>      -- fixme funtref alias?
>      {-,s "select generate_series from generate_series(1,7);"
>         $ Right [Just ([], [("generate_series", typeInt)])]-}
>
>      -- check aliasing
>      -- fixme funtref alias?
>      {-,s "select generate_series.generate_series from generate_series(1,7);"
>         $ Right [Just ([], [("generate_series", typeInt)])]-}
>      -- fixme funtref alias?
>      {-,s "select g from generate_series(1,7) g;"
>         $ Right [Just ([], [("g", typeInt)])]-}
>      -- fixme funtref alias?
>      {-,s "select g.g from generate_series(1,7) g;"
>         $ Right [Just ([], [("g", typeInt)])]-}
>      --fixme:funtref?
>      ,s "select generate_series.g from generate_series(1,7) g;"
>         $ Left [UnrecognisedCorrelationName "generate_series"]
>      -- fixme funtref?
>      {-,s "select g.generate_series from generate_series(1,7) g;"
>         $ Left [UnrecognisedIdentifier "g.generate_series"]-}
>
>      ,s "select * from pg_attrdef;"
>         $ Right [Just ([],
>          [("adrelid",ScalarType "oid")
>          ,("adnum",ScalarType "int2")
>          ,("adbin",ScalarType "text")
>          ,("adsrc",ScalarType "text")])]
>         -- fixme: needs funtref
>      {-,s "select abs from abs(3);"
>         $ Right [Just ([], [("abs", typeInt)])]-}
>         --todo: these are both valid,
>         --the second one means select 3+generate_series from generate_series(1,7)
>         --  select generate_series(1,7);
>         -- select 3 + generate_series(1,7);
>      ]

>   ,Group "simple selects from 2" [
>       -- fixme: needs funtref?
>       {-c "select a,b from testfunc();"
>         [CatCreateComposite "testType" [("a", ScalarType "text")
>                                        ,("b", typeInt)
>                                        ,("c", typeInt)]
>         ,CatCreateFunction FunName "testfunc" []
>          (SetOfType $ NamedCompositeType "testType") False]
>         $ Right [Just ([],
>                  [("a",ScalarType "text"),("b",ScalarType "int4")])]
>
>      ,-}c "select testfunc();"
>         [CatCreateFunction FunName "testfunc" [] (Pseudo Void) False]
>         $ Right [Just ([], [])]
>      ]
>   ]

>  where
>    s = StmtType
>    c = CatStmtType
