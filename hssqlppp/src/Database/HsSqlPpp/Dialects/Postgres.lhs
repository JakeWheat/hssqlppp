

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

> postgresCatalog :: Catalog
> postgresCatalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      flip updateCatalog (baseCatalog "bool"
>                                         "int4"
>                                         ["char"
>                                         ,"varchar"
>                                         ,"text"])
>           (nonSpecialPseudoTypes ++ generatedPostgresCatalogEntries ++ odbcCatalog)
>   where
>      nonSpecialPseudoTypes =
>         [CatCreateScalarType "cstring"
>         ,CatCreateScalarType "trigger"
>         ,CatCreateScalarType "event_trigger"
>         ,CatCreateScalarType "_cstring"
>         ,CatCreateScalarType "internal"
>         ,CatCreateScalarType "language_handler"
>         ,CatCreateScalarType "opaque"
>         ,CatCreateScalarType "fdw_handler"]
