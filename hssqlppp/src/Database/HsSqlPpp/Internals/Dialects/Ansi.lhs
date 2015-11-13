
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.Dialects.Ansi (ansiDialect) where
> import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Internals.Catalog.AnsiCatalog

> ansiDialect :: Dialect
> ansiDialect = Dialect
>     {diName = "ansi"
>     ,diSyntaxFlavour = Ansi
>     ,diCanonicalTypeNames = [("char",["character"])
>                             ,("varchar",["char varying","character varying"])
>                             ,("clob",["character large object","char large object"])
>                             ,("nchar",["national character","national char"])
>                             ,("nvarchar",["national character varying"
>                                          ,"national char varying"
>                                          ,"nchar varying"])
>                             ,("nclob",["national character large object"
>                                       ,"nchar large object"])
>                              -- todo: this list isn't complete
>                             ,("varbinary",["binary varying"])
>                             ,("blob",["binary large object"])
>                             ,("int",["integer"])
>                             ,("float",["double precision"])
>                             ]
>     ,diTextTypes = ["char","varchar","clob","nchar","nvarchar","nclob"]
>     ,diDatetimeTypes = ["date","time","timestamp","interval"]
>     ,diNumberTypes = ["smallint","int","bigint","decimal","numeric","float","real"]
>     ,namesForAnsiTypes = [("char","char")
>                          ,("varchar","varchar")
>                          ,("bigint","bigint")
>                          ,("boolean","boolean")
>                          ,("numeric","numeric")
>                          ,("int","int")
>                          ,("date","date")
>                          ,("time","time")
>                          ,("timestamp","timestamp")
>                          ] -- todo: finish this
>     ,diDefaultCatalog = ansiCatalog
>     }

