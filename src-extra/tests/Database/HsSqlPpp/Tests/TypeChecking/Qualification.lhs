
> module Database.HsSqlPpp.Tests.TypeChecking.Qualification
>     (tcQualificationTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types

> tcQualificationTestData :: Item
> tcQualificationTestData = -- FIXME: some of these fail
>   Group "simple scalar identifier qualification" [
>       s "select a.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         $ Right [Just ([], [("a", typeInt)
>                                           ,("b", typeInt)])]
>      ,s "select nothere.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         $ Left [UnrecognisedCorrelationName "nothere"]
>      {- currently broken, needs fixing
>        ,s "select a.b,b.c from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         $ Right [Just ([], [("b", typeInt)
>                                           ,("c", typeInt)])-}
>      {- currently broken, needs fixing
>        ,s "select a.a,b.a from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         $ Right [Just ([], [("a", typeInt)
>                                           ,("a", typeInt)])]-}
>
>      ,s "select pg_attrdef.adsrc from pg_attrdef;"
>         $ Right [Just ([], [("adsrc", ScalarType "text")])]
>
>      ,s "select a.adsrc from pg_attrdef a;"
>         $ Right [Just ([], [("adsrc", ScalarType "text")])]
>
>      ,s "select pg_attrdef.adsrc from pg_attrdef a;"
>         $ Left [UnrecognisedCorrelationName "pg_attrdef"]
>
>      ,s "select a from (select 2 as b, 1 as a) a\n\
>         \natural inner join (select 4.5 as d, 1 as a) b;"
>         $ Right [Just ([], [("a", typeInt)])]
>
> -- select g.fn from fn() g
>
>      ]

>  where
>    s = StmtType
