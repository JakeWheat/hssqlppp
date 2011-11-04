

> module Database.HsSqlPpp.Tests.TypeChecking.TrefIdentifiers
>     (trefIdentifiers) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog


> trefIdentifiers :: Item
> trefIdentifiers =
>   Group "trefIdentifiers"
>   [qenc "select * from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b",ScalarType "text")
>                               ,("c",typeInt)
>                               ,("d",ScalarType "text")]
>   ,qenc "select t0.* from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b",ScalarType "text")]
>   ,qenc "select t1.* from t0 cross join t1"
>       $ Right $ CompositeType [("c",typeInt)
>                               ,("d",ScalarType "text")]
>   ,qenc "select *,t0.*,t1.* from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b",ScalarType "text")
>                               ,("c",typeInt)
>                               ,("d",ScalarType "text")
>                               ,("a",typeInt)
>                               ,("b",ScalarType "text")
>                               ,("c",typeInt)
>                               ,("d",ScalarType "text")]

>   ,qec "select * from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b",ScalarType "text")
>                               ,("a",typeInt)
>                               ,("c",ScalarType "text")]
>   ,qec "select t0.* from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b",ScalarType "text")]
>   ,qec "select t1.* from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("c",ScalarType "text")]
>   ,qec "select *,t0.*,t1.* from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b",ScalarType "text")
>                               ,("a",typeInt)
>                               ,("c",ScalarType "text")
>                               ,("a",typeInt)
>                               ,("b",ScalarType "text")
>                               ,("a",typeInt)
>                               ,("c",ScalarType "text")]
>   ,qec "select t0.a from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)]
>   ,qec "select t1.a from t0 cross join t1"
>       $ Right $ CompositeType [("a",typeInt)]
>   ,qec "select a from t0 cross join t1"
>       $ Left [AmbiguousIdentifier "a"]

>   ,qec "select * from t0 natural inner join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b",ScalarType "text")
>                               ,("c",ScalarType "text")]
>   ,qec "select t0.* from t0 natural inner join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b",ScalarType "text")]
>   ,qec "select t1.* from t0 natural inner join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("c",ScalarType "text")]
>   ,qec "select *,t0.*,t1.* from t0 natural inner join t1"
>       $ Right $ CompositeType [("a",typeInt)
>                               ,("b",ScalarType "text")
>                               ,("c",ScalarType "text")
>                               ,("a",typeInt)
>                               ,("b",ScalarType "text")
>                               ,("a",typeInt)
>                               ,("c",ScalarType "text")]
>   ,qec "select t0.a from t0 natural inner join t1"
>       $ Right $ CompositeType [("a",typeInt)]
>   ,qec "select t1.a from t0 natural inner join t1"
>       $ Right $ CompositeType [("a",typeInt)]
>   ,qec "select a from t0 natural inner join t1"
>       $ Right $ CompositeType [("a",typeInt)]



>   ]
>   where
>     qenc = QueryExpr [CatCreateTable "t0" [("a", "int4")
>                                           ,("b", "text")]
>                      ,CatCreateTable "t1" [("c", "int4")
>                                           ,("d", "text")]]
>     qec = QueryExpr [CatCreateTable "t0" [("a", "int4")
>                                          ,("b", "text")]
>                     ,CatCreateTable "t1" [("a", "int4")
>                                          ,("c", "text")]]



todo:
unrecognised id
bogus qualifier for real id
