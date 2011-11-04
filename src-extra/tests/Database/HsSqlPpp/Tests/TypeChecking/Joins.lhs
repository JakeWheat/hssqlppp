
> module Database.HsSqlPpp.Tests.TypeChecking.Joins
>     (joins) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog


> joins :: Item
> joins =
>   Group "joins"
>   [qe "select * from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b", ScalarType "text")
>                               ,("c",typeInt)
>                               ,("d", ScalarType "text")]
>   ,qe "select a from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)]
>   ,qe "select b from t0 cross join t1"
>       $ Right $ CompositeType [("b",ScalarType "text")]
>   ,qe "select c from t0 cross join t1"
>       $ Right $ CompositeType [("c",typeInt)]
>   ,qe "select d from t0 cross join t1"
>       $ Right $ CompositeType [("d",ScalarType "text")]
>   ,qe "select a,b,c,d from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b", ScalarType "text")
>                               ,("c",typeInt)
>                               ,("d", ScalarType "text")]
>   ]
>   where
>     qe = QueryExpr [CatCreateTable "t0" [("a", "int4")
>                                        ,("b", "text")]
>                    ,CatCreateTable "t1" [("c", "int4")
>                                         ,("d", "text")]]


> {-
>       s "select * from (select 1 as a, 2 as b) a\n\
>         \  cross join (select true as c, 4.5 as d) b;"
>         $ Right [Just ([], [("a", typeInt)
>                            ,("b", typeInt)
>                            ,("c", typeBool)
>                            ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select true as c, 4.5 as d) b on true;"
>         $ Right [Just ([], [("a", typeInt)
>                            ,("b", typeInt)
>                            ,("c", typeBool)
>                            ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select 1 as a, 4.5 as d) b using(a);"
>         $ Right [Just ([], [("a", typeInt)
>                            ,("b", typeInt)
>                            ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a, 2 as b) a\n\
>         \ natural inner join (select 1 as a, 4.5 as d) b;"
>         $ Right [Just ([], [("a", typeInt)
>                            ,("b", typeInt)
>                            ,("d", typeNumeric)])]
>         --check the attribute order
>      ,s "select * from (select 2 as b, 1 as a) a\n\
>         \ natural inner join (select 4.5 as d, 1 as a) b;"
>         $ Right [Just ([], [("a", typeInt)
>                            ,("b", typeInt)
>                            ,("d", typeNumeric)])]

todo:
from cross join no common:
select *
select a.*
select b.*
select *,a.*,b.*
same with common attr
select a.x -- common attrs
select b.x
select x -> ambiguous
same all for natural join common attrs
unrecognised id
bogus qualifier for real id


>       --fixme: result set compatilibity check
>      {-,s "select * from (select 1 as a1, 2 as b) a\n\
>         \ natural inner join (select true as a1, 4.5 as d) b;"
>         $ Left [UnrecognisedCorrelationName ""
>                ,IncompatibleTypeSet [ScalarType "int4"
>                                     ,ScalarType "bool"]]-}
>       --fixme: result set compatilibity check
>       {-,s "select * from (select 1 as a1, 2 as b) a\n\
>         \ natural inner join (select true as a1, 4.5 as d) b;"
>         $ Left [UnrecognisedCorrelationName ""
>                ,IncompatibleTypeSet [ScalarType "int4"
>                                     ,ScalarType "bool"]]-}
>        ,s "select * from (select 1 as a1) a, (select 2 as a2) b;"
>         $ Right [Just ([], [("a1", typeInt)
>                                            ,("a2", typeInt)])]
>
>        ,s "select * from (select 1 as a1) a, (select 2 as a1) b;"
>         $ Right [Just ([], [("a1", typeInt)
>                                            ,("a1", typeInt)])]
>        --fixme: error detection (fixupids?)
>        {-,s "select a1 from (select 1 as a1) a,  (select 2 as a1) b;"
>         $ Left [AmbiguousIdentifier "a1"]-}
> -}