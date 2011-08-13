
> module Database.HsSqlPpp.Tests.TypeChecking.SimpleSelects
>     (tcSimpleSelectsTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
>
> tcSimpleSelectsTestData :: Item
> tcSimpleSelectsTestData =
>   Group "simple selects" [
>       s "select 1;" $ Right [Just ([], [("?column?", typeInt)])]
>      ,s "select 1 as a;" $
>         Right [Just ([], [("a", typeInt)])]
>      ,s "select 1,2;" $
>         Right [Just ([], [("?column?", typeInt)
>                                                 ,("?column?", typeInt)])]
>      ,s "select 1 as a, 2 as b;" $
>         Right [Just ([], [("a", typeInt)
>                                                 ,("b", typeInt)])]
>      ,s "select 1+2 as a, 'a' || 'b';" $
>         Right [Just ([], [("a", typeInt)
>                                        ,("?column?", ScalarType "text")])]
>      ,s "values (1,2);" $ Right [Just ([],
>                                           [("column1", typeInt)
>                                           ,("column2", typeInt)])]
>      ,s "values (1,2),('3', '4');" $ Right [Just ([],
>                                                      [("column1", typeInt)
>                                                      ,("column2", typeInt)])]
>      ,s "values (1,2),('a', true);" $ Left [IncompatibleTypeSet [typeInt
>                                                                 ,typeBool]]
>      ,s "values ('3', '4'),(1,2);" $ Right [Just ([],
>                                                      [("column1", typeInt)
>                                                      ,("column2", typeInt)])]
>      ,s "values ('a', true),(1,2);" $ Left [IncompatibleTypeSet [typeBool
>                                                                 ,typeInt]]
>      ,s "values ('a'::text, '2'::int2),('1','2');" $ Right [Just ([],
>                                      [("column1", ScalarType "text")
>                                      ,("column2", typeSmallInt)])]
>      ,s "values (1,2,3),(1,2);" $ Left [ValuesListsMustBeSameLength]
>      ]
>  where
>    s = StmtType
