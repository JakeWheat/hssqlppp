
> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.TableRefs (tableRefs) where
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Syntax

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> tableRefs :: Item
> tableRefs =
>    Group "tableRefs"
>    [q "select a from tbl" stbl
>    ,q "select a from sc.tbl"
>     stbl {selTref = [qtref "sc" "tbl"]}
>    ,q "select a from tbl a"
>     stbl {selTref = [trefa "tbl" "a"]}
>    ,q "select a from tbl as a"
>     stbl {selTref = [trefa "tbl" "a"]}

>    ,q "select a from tbl as a(b,c)"
>     stbl {selTref = [treffa "tbl" "a" ["b","c"]]}

>    ,q "select a from gen();"
>     stbl {selTref = [FunTref ea (app "gen" [])]}
>    ,q "select a from gen() t;"
>     stbl {selTref = [ta $ FunTref ea (app "gen" [])]}
>    ,q "select a from gen() as t;"
>     stbl {selTref = [ta $ FunTref ea (app "gen" [])]}

>    ,q "select a from (select a from tbl) as t;"
>     stbl {selTref = [ta $ SubTref ea stbl]}

>    ,q "select a from (select a from tbl) t;"
>     stbl {selTref = [ta $ SubTref ea stbl]}

>    ]
>    where
>      ta = TableAlias ea (Nmc "t")
>      stbl = makeSelect
>             {selSelectList = sl [si $ ei "a"]
>             ,selTref = [tref "tbl"]}
>      q = QueryExpr
