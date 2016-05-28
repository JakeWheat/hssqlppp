
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Dialects.Ansi (ansiDialect) where
> import Database.HsSqlPpp.Internals.Dialect

> import Database.HsSqlPpp.Internals.Catalog.CatalogBuilder
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> import Database.HsSqlPpp.Dialects.BaseCatalog


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


> ansiCatalog :: Catalog
> ansiCatalog =
>     (\l -> case l of
>              Left x -> error $ show x -- todo: what is the best way to handle this?
>              Right e -> e) $
>      flip updateCatalog emptyCatalog (
>                                        -- todo: duplicated list of text types
>        [CatCreateScalarType "char"
>        ,CatCreateScalarType "varchar"
>        ,CatCreateScalarType "clob"

>        ,CatCreateScalarType "nchar"
>        ,CatCreateScalarType "nvarchar"
>        ,CatCreateScalarType "nclob"

>        ,CatCreateScalarType "binary"
>        ,CatCreateScalarType "varbinary"
>        ,CatCreateScalarType "blob"

>        ,CatCreateScalarType "numeric"
>        ,CatCreateScalarType "decimal"

>        ,CatCreateScalarType "smallint"
>        ,CatCreateScalarType "int"
>        ,CatCreateScalarType "bigint"

>        ,CatCreateScalarType "float"
>        ,CatCreateScalarType "real"

>        ,CatCreateScalarType "boolean"

>        ,CatCreateScalarType "date"
>        ,CatCreateScalarType "time"
>        ,CatCreateScalarType "timestamp"
>        ,CatCreateScalarType "interval"
>        ]

what to do about these?

position
char_length
extract
substring
convert
translate
trim
overlay
nromalize
between
in
like
collate



>         ++ [CatCreateFunction charLen [t] False "int"
>            | t <- ["char","varchar","clob"
>                   ,"nchar","nvarchar","nclob"]
>            , charLen <- ["char_length", "character_length", "octet_length"] ]
>         ++ concat
>         -- unary +- all numeric types plus interval
>         [ [CatCreatePrefixOp "+" n n
>           ,CatCreatePrefixOp "-" n n]
>           | n <- ["numeric","decimal","smallint","int","bigint"
>                  ,"float","real","interval"] ]
>         -- binary  + - * / all numeric types
>         ++ concat
>         [ [ CatCreateBinaryOp "+" n n n
>            ,CatCreateBinaryOp "-" n n n
>            ,CatCreateBinaryOp "*" n n n
>            ,CatCreateBinaryOp "/" n n n]
>           | n <- ["numeric","decimal","smallint","int","bigint"
>                  ,"float","real"] ]
>         --  || concatenation on strings, binaries and arrays
>         -- not sure how arrays will work right now
>         ++ [CatCreateBinaryOp "||" s s s
>            | s <- ["char","varchar","clob"
>                   ,"nchar","nvarchar", "nclob"
>                   ,"binary","varbinary","blob" ] ]
>         -- comparisons, should some of these be generic?
>         -- maybe want the option of user defined types
>         -- not having comparisons? not sure about types without
>         -- equals makes any sense here
>         ++ [CatCreateBinaryOp op t t "boolean"
>            | op <- ["=","<>","<",">","<=",">="]
>            , t <- ["char","varchar","clob"
>                   ,"nchar","nvarchar","nclob"
>                   ,"binary","varbinary","blob"
>                   ,"numeric","decimal"
>                   ,"smallint","int","bigint"
>                   ,"float","real"
>                   ,"boolean"
>                   ,"date","time","timestamp","interval"
>                   ] ]
>
>         ++ [CatCreateFunction "abs" [t] False t
>            | t <- ["numeric", "decimal", "smallint", "int", "bigint", "float","real"]]

>         ++ [CatCreateFunction "mod" [t,t] False t
>            | t <- ["numeric", "smallint", "int", "bigint"]]

>         ++ [CatCreateFunction fn [t] False t
>            | t <- ["float","real"]
>            , fn <- ["ln","exp","pow","sqrt"] ]
>         ++ [CatCreateFunction fn [t] False t
>            | t <- ["float","real","numeric"]
>            , fn <- ["floor","ceil","ceiling"] ]

>         ++ [CatCreateFunction "width_bucket" [t,t,t,"numeric"] False "numeric"
>            | t <- ["char","varchar","clob"
>                   ,"nchar","nvarchar","nclob"
>                   ,"binary","varbinary","blob"
>                   ,"numeric","decimal"
>                   ,"smallint","int","bigint"
>                   ,"float","real"
>                   ,"boolean"
>                   ,"date","time","timestamp","interval"
>                   ] ]

>         ++ [CatCreateFunction fn [t] False t
>            | t <- ["char","varchar","clob"
>                   ,"nchar","nvarchar","nclob"]
>            , fn <- ["lower","upper"] ]

>         ++ concat
>         [ [ CatCreateBinaryOp "+" dt "interval" dt
>            ,CatCreateBinaryOp "+" "interval" dt dt
>            ,CatCreateBinaryOp "-" dt "interval" dt]
>           | dt <- ["date","time","timestamp"]]
>         ++ [CatCreateBinaryOp "+" "interval" "interval" "interval"
>            ,CatCreateBinaryOp "-" "interval" "interval" "interval"]

quick and dirty to get tpch working

>        ++
>        [ CatCreateAggregate "sum" [ "numeric" ] "numeric"
>        , CatCreateAggregate "sum" [ "smallint" ] "smallint"
>          --todo: figure out what the options for the types are here
>        , CatCreateAggregate "sum" [ "int" ] "bigint"
>        , CatCreateAggregate "sum" [ "bigint" ] "bigint"
>        , CatCreateAggregate "avg" [ "numeric" ] "numeric"
>        , CatCreateAggregate "max" [ "numeric" ] "numeric"
>        , CatCreateAggregate "min" [ "numeric" ] "numeric"
>        , CatCreateAggregate "count" [ "any" ] "bigint"]


>         ++ [
>           --todo: these are taken from postgres catalog
>           -- they need careful review to see if they are exactly
>           -- what we want in the ansi dialect
>         CatCreateCast "boolean" "int" ExplicitCastContext,
>         CatCreateCast "boolean" "varchar" AssignmentCastContext,
>         CatCreateCast "char" "int" ExplicitCastContext,
>         CatCreateCast "char" "varchar" AssignmentCastContext,
>         CatCreateCast "date" "timestamp" ImplicitCastContext,
>         CatCreateCast "real" "float" ImplicitCastContext,
>         CatCreateCast "real" "smallint" AssignmentCastContext,
>         CatCreateCast "real" "int" AssignmentCastContext,
>         CatCreateCast "real" "bigint" AssignmentCastContext,
>         CatCreateCast "real" "numeric" AssignmentCastContext,
>         CatCreateCast "float" "real" AssignmentCastContext,
>         CatCreateCast "float" "smallint" AssignmentCastContext,
>         CatCreateCast "float" "int" AssignmentCastContext,
>         CatCreateCast "float" "bigint" AssignmentCastContext,
>         CatCreateCast "float" "numeric" AssignmentCastContext,
>         CatCreateCast "smallint" "real" ImplicitCastContext,
>         CatCreateCast "smallint" "float" ImplicitCastContext,
>         CatCreateCast "smallint" "int" ImplicitCastContext,
>         CatCreateCast "smallint" "bigint" ImplicitCastContext,
>         CatCreateCast "smallint" "numeric" ImplicitCastContext,
>         CatCreateCast "int" "boolean" ExplicitCastContext,
>         CatCreateCast "int" "char" ExplicitCastContext,
>         CatCreateCast "int" "real" ImplicitCastContext,
>         CatCreateCast "int" "float" ImplicitCastContext,
>         CatCreateCast "int" "smallint" AssignmentCastContext,
>         CatCreateCast "int" "bigint" ImplicitCastContext,
>         CatCreateCast "int" "numeric" ImplicitCastContext,
>         CatCreateCast "bigint" "real" ImplicitCastContext,
>         CatCreateCast "bigint" "float" ImplicitCastContext,
>         CatCreateCast "bigint" "smallint" AssignmentCastContext,
>         CatCreateCast "bigint" "int" AssignmentCastContext,
>         CatCreateCast "bigint" "numeric" ImplicitCastContext,
>         CatCreateCast "interval" "interval" ImplicitCastContext,
>         CatCreateCast "interval" "time" AssignmentCastContext,
>         CatCreateCast "numeric" "real" ImplicitCastContext,
>         CatCreateCast "numeric" "float" ImplicitCastContext,
>         CatCreateCast "numeric" "smallint" AssignmentCastContext,
>         CatCreateCast "numeric" "int" AssignmentCastContext,
>         CatCreateCast "numeric" "bigint" AssignmentCastContext,
>         CatCreateCast "numeric" "numeric" ImplicitCastContext,
>         CatCreateCast "time" "interval" ImplicitCastContext,
>         CatCreateCast "time" "time" ImplicitCastContext,
>         CatCreateCast "timestamp" "date" AssignmentCastContext,
>         CatCreateCast "timestamp" "time" AssignmentCastContext,
>         CatCreateCast "timestamp" "timestamp" ImplicitCastContext,
>         CatCreateCast "varchar" "char" AssignmentCastContext,
>         CatCreateCast "varchar" "varchar" ImplicitCastContext,
>         CatCreateTypeCategoryEntry "boolean" ("B", True),
>         CatCreateTypeCategoryEntry "char" ("S", False),
>         CatCreateTypeCategoryEntry "date" ("D", False),
>         CatCreateTypeCategoryEntry "real" ("N", False),
>         CatCreateTypeCategoryEntry "float" ("N", True),
>         CatCreateTypeCategoryEntry "smallint" ("N", False),
>         CatCreateTypeCategoryEntry "int" ("N", False),
>         CatCreateTypeCategoryEntry "bigint" ("N", False),
>         CatCreateTypeCategoryEntry "interval" ("T", True),
>         CatCreateTypeCategoryEntry "numeric" ("N", False),
>         CatCreateTypeCategoryEntry "time" ("D", False),
>         CatCreateTypeCategoryEntry "timestamp" ("D", False),
>         CatCreateTypeCategoryEntry "varchar" ("S", False)]
>         ++ baseCatalog "boolean" "int" ["char","varchar","clob","nchar","nvarchar","nclob"]

>         )
