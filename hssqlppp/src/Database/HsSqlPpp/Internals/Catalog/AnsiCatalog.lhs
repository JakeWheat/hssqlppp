> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.Catalog.AnsiCatalog
>      (ansiCatalog) where
>
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal

> ansiCatalog :: Catalog
> ansiCatalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      flip updateCatalog (defaultCatalog "boolean" "int"
>                                          ["char" -- todo, add the rest?
>                                          ,"varchar"]) (

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

>         ]

>         ++ [CatCreateFunction charLen [t] False "int"
>            | t <- ["char","varchar","clob"
>                   ,"nchar","nvarchar","nclob"]
>            , charLen <- ["char_length", "character_length"] ]
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
>         -- || concatenation on strings, binaries and arrays
>         -- not sure how arrays will work right now
>         ++ [CatCreateBinaryOp "||" s s s
>            | s <- ["char","varchar","clob"
>                   ,"nchar","nvarchar", "nclob"
>                   ,"binary","varbinary","blob" ] ]
>         -- logical operators. the convention is to represent
>         -- non standard syntax using a prefix !
>         -- this will cause a problem if the user can create functions
>         -- or operators which overlap with these names, so a better
>         -- solution should be found (this hack is implemented in the
>         -- parser also
>         ++ [CatCreateBinaryOp "!and" "boolean" "boolean" "boolean"
>            ,CatCreateBinaryOp "!or" "boolean" "boolean" "boolean"
>            ,CatCreatePrefixOp "!not" "boolean" "boolean"]
>         -- truth value tests
>         --   ,CatCreateBinaryOp "!is true" "boolean" "boolean" "boolean" --truth value version
>         --   ,CatCreateBinaryOp "!is not true" "boolean" "boolean" "boolean"
>         --   ]
>         -- is/ is not null
>         {- ++ [CatCreatePostfixOp "!is null" "!any" "boolean"
>            ,CatCreatePostfixOp "!is not null" "!any" "boolean"]-}

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
>         -- TODO: overlaps, need to try to understand how this works better
>         -- so can add the right types
>         -- I think the args have to be some variation on a row value with
>         -- two components
>
>         -- is distinct from, supports any type
>         -- so I suppose this means every type must support
>         -- equals (or the error just isn't caught in this typechecker)
>         -- ++ [ CatCreateBinaryOp op "!any" "!any" "boolean"
>         --   | op <- ["!is distinct from","!is not distinct from"]
>         --   ]
>         )
