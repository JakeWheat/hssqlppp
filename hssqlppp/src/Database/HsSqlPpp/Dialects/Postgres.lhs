

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Dialects.Postgres (postgresDialect) where

> import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Dialects.GeneratedPostgres
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> import Database.HsSqlPpp.Internals.Catalog.CatalogBuilder
> import Database.HsSqlPpp.Dialects.OdbcCatalog
> import Database.HsSqlPpp.Dialects.BaseCatalog

> postgresDialect :: Dialect
> postgresDialect = Dialect
>     {diName = "postgres"
>     ,diSyntaxFlavour = Postgres
>     ,diCanonicalTypeNames =  [("timestamp", ["datetime"])
>                              -- todo: temp before sqlserver dialect is done properly
>                              -- this hack should probably move to the ansi dialect first
>                              ,("int1", ["tinyint"])
>                              ,("int2", ["smallint"])
>                              ,("int4", ["integer","int"])
>                              ,("int8", ["bigint"])
>                              ,("numeric", ["decimal"])
>                              ,("float4", ["real"])
>                              ,("float8", ["double precision","float","double"])
>                              -- probably some missing here
>                              ,("varchar", ["character varying"])
>                              ,("char", ["character"])
>                              ,("bool", ["boolean"])]
>     ,diTextTypes = ["char","varchar","text"]
>     ,diDatetimeTypes = ["date","time","timestamp","interval"]
>     ,diNumberTypes = ["int2","int4","int8","numeric","float4","float8"]
>     ,namesForAnsiTypes = [("char","char")
>                          ,("varchar","varchar")
>                          ,("bigint","int8")
>                          ,("boolean","bool")
>                          ,("numeric","numeric")
>                          ,("int","int4")
>                          ,("date","date")
>                          ,("time","time")
>                          ,("timestamp","timestamp")
>                          ] -- todo: finish this
>     ,diDefaultCatalog = postgresCatalog
>     }

built in range types in postgresql

todo: maybe these are in the catalog somewhere and should come from
postgres?

> postgresCatalog :: Catalog
> postgresCatalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      flip updateCatalog emptyCatalog (
>           nonSpecialPseudoTypes ++ rangeTypes ++ generatedPostgresCatalogEntries
>           ++ extraDefs
>           ++ baseCatalog "bool" "int4" ["char", "varchar", "text"]
>           ++ odbcCatalog)
>   where
>      extraDefs = [CatCreateBinaryOp "=" "anyelement" "anyelement" "bool"
>                  ,CatCreateVariadicFunction "arrayctor" ["anyelement"] False "anyarray"
>                  ,CatCreateFunction "arraysub" ["anyarray","int4"] False "anyelement"
>                  ,CatCreateVariadicFunction "greatest" ["anyelement"] False "anyelement"
>                  ,CatCreateVariadicFunction "least" ["anyelement"] False "anyelement"
>                  ] ++ concat [ [CatCreateBinaryOp "rlike" t t "bool"
>                                ,CatCreateBinaryOp "notrlike" t t "bool"]
>                                | t <- ["char", "varchar", "text"] ]
>      rangeTypes = map CatCreateScalarType
>                   ["int4range", "int8range"
>                   ,"numrange","daterange"
>                   ,"tsrange","tstzrange"]
>      nonSpecialPseudoTypes = map CatCreateScalarType
>                              ["cstring"
>                              ,"trigger"
>                              ,"event_trigger"
>                              ,"_cstring"
>                              ,"internal"
>                              ,"language_handler"
>                              ,"opaque"
>                              ,"fdw_handler"]
