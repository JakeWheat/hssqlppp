
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> module Database.HsSqlPpp.Tests.TypeChecking.Joins
>     (tcJoinsTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> --import Database.HsSqlPpp.Catalog

> tcJoinsTestData :: Item
> tcJoinsTestData = -- FIXME: some of these fail
>     Group "simple join selects" $ drop 10 [
>       s "select * from (select 1 as a, 2 as b) a\n\
>         \  cross join (select true as c, 4.5 as d) b;"
>         $ Right [Just $ ([], [("a", typeInt)
>                              ,("b", typeInt)
>                              ,("c", typeBool)
>                              ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select true as c, 4.5 as d) b on true;"
>         $ Right [Just $ ([], [("a", typeInt)
>                              ,("b", typeInt)
>                              ,("c", typeBool)
>                              ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select 1 as a, 4.5 as d) b using(a);"
>         $ Right [Just $ ([], [("a", typeInt)
>                              ,("b", typeInt)
>                              ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a, 2 as b) a\n\
>         \ natural inner join (select 1 as a, 4.5 as d) b;"
>         $ Right [Just $ ([], [("a", typeInt)
>                              ,("b", typeInt)
>                              ,("d", typeNumeric)])]
>         --check the attribute order
>      ,s "select * from (select 2 as b, 1 as a) a\n\
>         \ natural inner join (select 4.5 as d, 1 as a) b;"
>         $ Right [Just $ ([], [("a", typeInt)
>                              ,("b", typeInt)
>                              ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a1, 2 as b) a\n\
>         \ natural inner join (select true as a1, 4.5 as d) b;"
>         $ Left [UnrecognisedCorrelationName ""
>                ,IncompatibleTypeSet [ScalarType "int4"
>                                     ,ScalarType "bool"]]
>        ,s "select * from (select 1 as a1, 2 as b) a\n\
>         \ natural inner join (select true as a1, 4.5 as d) b;"
>         $ Left [UnrecognisedCorrelationName ""
>                ,IncompatibleTypeSet [ScalarType "int4"
>                                     ,ScalarType "bool"]]
>        ,s "select * from (select 1 as a1) a, (select 2 as a2) b;"
>         $ Right [Just $ ([], [("a1", typeInt)
>                                            ,("a2", typeInt)])]
>
>        ,s "select * from (select 1 as a1) a, (select 2 as a1) b;"
>         $ Right [Just $ ([], [("a1", typeInt)
>                                            ,("a1", typeInt)])]
>
>        ,s "select a1 from (select 1 as a1) a,  (select 2 as a1) b;"
>         $ Left [AmbiguousIdentifier "a1"]
>      ]

>  where
>    --e = Expr
>    s = StmtType
>    --c = CatStmtType
>    --d = Ddl

