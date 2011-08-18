
> module Database.HsSqlPpp.Tests.TypeChecking.MiscSelects
>     (tcMiscSelectTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog

> tcMiscSelectTestData :: Item
> tcMiscSelectTestData =
>   Group "misc selects" [
>   Group "aggregates" [
>        s "select max(prorettype::int) from pg_proc;"
>         $ Right [Just ([], [("max", typeInt)])]
>      ]

>   ,Group "simple wheres" [
>       s "select 1 from pg_type where true;"
>         $ Right [Just ([], [("?column?", typeInt)])]
>      ,s "select 1 from pg_type where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,s "select typname from pg_type where typbyval;"
>         $ Right [Just ([], [("typname", ScalarType "name")])]
>      ,s "select typname from pg_type where typtype = 'b';"
>         $ Right [Just ([], [("typname", ScalarType "name")])]
>      ,s "select typname from pg_type where what = 'b';"
>         $ Left [UnrecognisedIdentifier "what"]
>      ]

>   ,Group "unnest" [
>       s "select conname,unnest(conkey) as attnum from pg_constraint;"
>         $ Right [Just ([], [("conname",ScalarType "name")
>                                          ,("attnum",typeSmallInt)])]
>      ]

TODO: check identifier stacking working, then remove the pg_namespace
qualifier before oid and this should still work

>   ,Group "subqueries" [
>       s "select relname as relvar_name\n\
>         \    from pg_class\n\
>         \    where ((relnamespace =\n\
>         \           (select oid\n\
>         \              from pg_namespace\n\
>         \              where (nspname = 'public'))) and (relkind = 'r'));"
>         $ Right [Just ([], [("relvar_name",ScalarType "name")])]
>      ,s "select relname from pg_class where relkind in ('r', 'v');"
>         $ Right [Just ([], [("relname",ScalarType "name")])]
>      -- fixme: needs funtrefs?
>      {-,s "select * from generate_series(1,7) g\n\
>         \where g not in (select * from generate_series(3,5));"
>         $ Right [Just ([], [("g",typeInt)])]-}
>      ,s "select 3 = any(array[1,2,3]);"
>         $ Right [Just ([], [("?column?",typeBool)])]
>      ,c "select b.t,b.u from a,b where a.t = b.t\n\
>         \ and b.u = (select min(b.u) from b where a.t = b.t);"
>         [CatCreateTable "a" [("t", typeInt)] []
>         ,CatCreateTable "b" [("t", typeInt)
>                             ,("u", typeNumeric)] []]
>         $ Right [Just ([], [("t", typeInt)
>                            ,("u", typeNumeric)])]
>      ]
>
>  -- identifiers in select parts
>     ,Group "select part identifiers" [
>       s "select relname as relvar_name,attname as name from pg_class\n\
>         \inner join pg_attribute\n\
>         \on pg_attribute.attrelid = pg_class.oid;"
>         $ Right [Just ([], [("relvar_name",ScalarType "name")
>                            ,("name",ScalarType "name")])]
>      ]
>   ]


>  where
>    s = StmtType
>    c = CatStmtType
