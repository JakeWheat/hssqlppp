
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.TypeChecking.CombineSelects
>     (tcCombineSelectsTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
> --import Database.HsSqlPpp.Utils.Here
>
> tcCombineSelectsTestData :: Item
> tcCombineSelectsTestData =
>   Group "combine selects" [
>      s "select 1,2  union select '3', '4';" $ Right [Just $ ([],
>                                      [("?column?", typeInt)
>                                      ,("?column?", typeInt)])]
>      ,s "select 1,2 intersect select 'a', true;" $ Left [IncompatibleTypeSet [typeInt
>                                                         ,typeBool]]
>      ,s "select '3', '4' except select 1,2;" $ Right [Just $ ([],
>                                      [("?column?", typeInt)
>                                      ,("?column?", typeInt)])]
>      ,s "select 'a', true union select 1,2;"
>                                      $ Left [IncompatibleTypeSet [typeBool
>                                                         ,typeInt]]
>      ,s "select 'a'::text, '2'::int2 intersect select '1','2';" $ Right [Just $ ([],
>                                      [("text", ScalarType "text")
>                                      ,("int2", typeSmallInt)])]
>      ,s "select 1,2,3 except select 1,2;" $ Left [ValuesListsMustBeSameLength]
>      ,s "select '3' as a, '4' as b except select 1,2;" $ Right [Just $ ([],
>                                      [("a", typeInt)
>                                      ,("b", typeInt)])]
>      --FIXME: withs not working
>      {-,s [here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from a
>               union select * from b; |]
>          $ Right [Just $ ([],
>                            [("a1", typeInt)])]-}
>      ]

>  where
>    --e = Expr
>    s = StmtType
>    --c = CatStmtType
>    --d = Ddl

