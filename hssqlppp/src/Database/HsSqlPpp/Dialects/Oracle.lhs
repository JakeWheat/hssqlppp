
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Dialects.Oracle (oracleDialect) where

> import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Dialects.Postgres

Oracle is a very small hack based on postgres. This should be fixed
after adding some more tests. It is particularly bad since we don't
use oracle style 'numeric' for ints, so it is not very compatible with
oracle (there are lots of other standard oracle types missing, which
is probably the biggest issue at the moment.

> oracleDialect :: Dialect
> oracleDialect = Dialect
>     {diName = "oracle"
>     ,diSyntaxFlavour = Oracle
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
>                          ] -- todo: these are postgres names
>     ,diDefaultCatalog = diDefaultCatalog postgresDialect
>     }

