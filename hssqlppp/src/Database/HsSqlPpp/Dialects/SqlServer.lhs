
> {-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
> module Database.HsSqlPpp.Dialects.SqlServer (sqlServerDialect) where

> import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Internals.Catalog.CatalogBuilder
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> --import Database.HsSqlPpp.Dialects.BaseCatalog
> import Database.HsSqlPpp.Dialects.Postgres
> --import Database.HsSqlPpp.Internals.Dialect
> --import Data.List
> import Data.Generics.Uniplate.Data

The sql server dialect is a crap modification to the postgresql
dialect at the moment.  After a bunch more tests are written, it
should be reimplemented separately from scratch

> sqlServerDialect :: Dialect
> sqlServerDialect = Dialect
>     {diName = "sqlServer"
>     ,diSyntaxFlavour = SqlServer
>     ,diCanonicalTypeNames =  [("timestamp", ["datetime"])
>                              -- todo: temp before sqlserver dialect is done properly
>                              -- this hack should probably move to the ansi dialect first
>                              ,("int1", ["tinyint"])
>                              ,("int2", ["smallint"])
>                              ,("int4", ["integer","int"])
>                              ,("int8", ["bigint"])
>                              ,("float4", ["real"])
>                              ,("float8", ["double precision","float","double"])
>                              -- probably some missing here
>                              ,("varchar", ["character varying"])
>                              ,("char", ["character"])
>                              ,("bool", ["boolean"])]
>     ,diTextTypes = ["char","varchar", "nvarchar"]
>     ,diDatetimeTypes = ["date","time","timestamp","interval"]
>     ,diNumberTypes = ["int2","int4","int8","numeric","float4","float8"]
>     ,namesForAnsiTypes = [("char","char")
>                          ,("varchar","varchar")
>                          ,("nvarchar","nvarchar")
>                          ,("bigint","int8")
>                          ,("boolean","bool")
>                          ,("numeric","numeric")
>                          ,("int","int4")
>                          ,("date","date")
>                          ,("time","time")
>                          ,("timestamp","timestamp")
>                          ] -- todo: these are postgres names
>     ,diDefaultCatalog = tsqlCatalog
>     }

> tsqlCatalog :: Catalog
> tsqlCatalog = either (error . show) id catr
>   where
>     catr = updateCatalog
>               (alterUpdates (deconstructCatalog
>                              (diDefaultCatalog postgresDialect)
>                              ++ additionalEntries))
>               emptyCatalog
>     -- change the counts to return int instead of long
>     alterUpdates = map $ \u -> case u of
>         CatCreateAggregate "count" ["any"] "int8" ->
>             CatCreateAggregate "count" ["any"] "int4"
>         CatCreateAggregate f [e] _ | f `elem` ["sum","avg"]
>                                    , e `elem` ["int1"
>                                               ,"int2"
>                                               ,"int4"] ->
>             CatCreateAggregate f [e] "int4"
>         CatCreateAggregate f ["float4"] _ | f `elem` ["sum","avg"] ->
>             CatCreateAggregate f ["float4"] "float8"
>         CatCreateAggregate f ["int8"] _ | f `elem` ["sum","avg"] ->
>             CatCreateAggregate f ["int8"] "int8"
>         _ -> u

>     additionalEntries =
>         int1fns ++
>         int12fns ++
>         [CatCreateScalarType "nvarchar"
>         ,CatCreateTypeCategoryEntry "nvarchar" ("S", False)
>         ,CatCreateBinaryOp "+" "varchar" "varchar" "varchar"
>         ,CatCreateFunction "getdate" [] False "timestamp"
>         ,CatCreateFunction "isnumeric" ["anyelement"] False "int4"
>         ,CatCreateFunction "grt_lengthconv" ["int4"] False "int4"
>         ,CatCreateFunction "isnull" ["anyelement","anyelement"] False "anyelement"
>         -- put these in to stop use the text only version and a bunch of casts
>         ,CatCreateFunction "replace" ["char", "char", "char"] False "char"
>         ,CatCreateFunction "replace" ["varchar", "varchar", "varchar"] False "varchar"
>         ,CatCreateFunction "replace" ["nvarchar", "nvarchar", "nvarchar"] False "nvarchar"
>         ,CatCreateFunction "patindex" ["char","char"] False "int4"
>         ,CatCreateFunction "patindex" ["varchar","varchar"] False "int4"
>         ,CatCreateFunction "patindex" ["nvarchar","nvarchar"] False "int4"
>         ,CatCreateFunction "isdate" ["varchar"] False "bool"
>         ,CatCreateFunction "isdate" ["char"] False "int4"
>         ,CatCreateFunction "isdate" ["nvarchar"] False "int4"
>         ,CatCreateFunction "len" ["nvarchar"] False "int4"
>         ,CatCreateFunction "len" ["varchar"] False "int4"
>         ,CatCreateAggregate "count_big" ["any"] "int8"
>         ,CatCreateFunction "datediff" ["int4","date","date"] False "int4"
>         ,CatCreateFunction "datediff" ["int4","timestamp","timestamp"] False "int4"
>         ,CatCreateFunction "dateadd" ["int4","int4","date"] False "date"
>         ,CatCreateFunction "dateadd" ["int4","int4","timestamp"] False "timestamp"
>         ,CatCreateFunction "datepart" ["int4","date"] False "int4"
>         ,CatCreateFunction "datepart" ["int4","timestamp"] False "int4"
>         ,CatCreateFunction "trunc" ["timestamp"] False "timestamp"
>         ,CatCreateCast "char" "varchar" ImplicitCastContext

>         ,CatCreateFunction "substring" ["nvarchar","int4","int4"] False "nvarchar"
>         ,CatCreateFunction "like" [ "nvarchar" , "nvarchar" ] False "bool"

postponed until we have better design
  example of a problem: in "float4 < int4", both arguments are cast to text,
  because it has higher priority (see CatCreateTypeCategoryEntry)

>         --,CatCreateCast "int1" "varchar" ImplicitCastContext
>         --,CatCreateCast "int2" "varchar" ImplicitCastContext
>         --,CatCreateCast "int4" "varchar" ImplicitCastContext
>         --,CatCreateCast "int8" "varchar" ImplicitCastContext
>         --,CatCreateCast "float4" "varchar" ImplicitCastContext
>         --,CatCreateCast "float8" "varchar" ImplicitCastContext
>         --,CatCreateCast "int1" "text" ImplicitCastContext
>         --,CatCreateCast "int2" "text" ImplicitCastContext
>         --,CatCreateCast "int4" "text" ImplicitCastContext
>         --,CatCreateCast "int8" "text" ImplicitCastContext
>         --,CatCreateCast "float4" "text" ImplicitCastContext
>         --,CatCreateCast "float8" "text" ImplicitCastContext
>         ]
>     -- find all the functions on int2 and replace int2 with int1
>     -- then find all the functions with int2 and int4, and
>     -- replace int2 with int1 and int4 with int2
>     -- really hacky
>     int1fns = let s = filter (\x -> replaceItp x && hasInt2 x)
>                              (deconstructCatalog (diDefaultCatalog postgresDialect))
>               in flip transformBi s $ \x -> case (x :: CatName) of
>                                        "int2" -> "int1"
>                                        _ -> x
>     int12fns = let s = filter (\x -> replaceItp x && hasInt2Int4 x)
>                               (deconstructCatalog (diDefaultCatalog postgresDialect))
>                in flip transformBi s $ \x -> case (x :: CatName) of
>                                        "int2" -> "int1"
>                                        "int4" -> "int2"
>                                        _ -> x
>     hasInt2 x = not $ null [() | ("int2" :: CatName) <- universeBi x]
>     hasInt2Int4 x = not $ null [() | ("int2" :: CatName) <- universeBi x
>                                    , ("int4" :: CatName) <- universeBi x]
>     replaceItp x = case x of
>                      CatCreateScalarType {} -> True
>                      CatCreateArrayType {} -> True
>                      CatCreatePrefixOp {} -> True
>                      CatCreateBinaryOp {} -> True
>                      CatCreateFunction f _ _ _ | f `elem` ["abs","float4","float8","int2","int4","mod","numeric"] -> True
>                      CatCreateAggregate f _ _ | f `elem` ["avg","max","min","sum"] -> True
>                      CatCreateCast a b _ | a == "int2" || b == "int2" -> True
>                      CatCreateTypeCategoryEntry {} -> True
>                      _ -> False

comparisons with all ints
abs
cast functions
avg to numeric max min sum
catcreatecast: float4,float8, int4,int8,numeric, from

