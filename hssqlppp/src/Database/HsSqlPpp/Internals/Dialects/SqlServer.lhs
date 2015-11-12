
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.Dialects.SqlServer (sqlServerDialect) where

> import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Internals.Catalog.DefaultTSQLCatalog

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
>                              ,("real", ["real"])
>                              ,("float", ["double precision","float","double"])
>                              -- probably some missing here
>                              ,("varchar", ["character varying"])
>                              ,("char", ["character"])
>                              ,("boolean", ["boolean"])]
>     ,diTextTypes = ["char","varchar"]
>     ,diDatetimeTypes = ["date","time","timestamp","interval"]
>     ,diNumberTypes = ["int2","int4","int8","numeric","float4","float8"]
>     ,namesForAnsiTypes = [("char","char")
>                          ,("varchar","varchar")
>                          ,("bigint","int8")
>                          ,("boolean","bool")
>                          ,("numeric","numeric")
>                          ,("int","int4")] -- todo: these are postgres names
>     ,diDefaultCatalog = defaultTSQLCatalog
>     }
