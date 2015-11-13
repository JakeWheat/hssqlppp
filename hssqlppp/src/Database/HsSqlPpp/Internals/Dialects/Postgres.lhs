

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.Dialects.Postgres (postgresDialect) where

> import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Internals.Catalog.DefaultTemplate1Catalog

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
>     ,diDefaultCatalog = defaultTemplate1Catalog
>     }

