
> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.TableRefs (tableRefs) where
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

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
>     stbl {selTref = [FunTref ea (app "gen" []) (NoAlias ea)]}
>    ,q "select a from gen() t;"
>     stbl {selTref = [FunTref ea (app "gen" []) (TableAlias ea $ Nmc "t")]}
>    ,q "select a from gen() as t;"
>     stbl {selTref = [FunTref ea (app "gen" []) (TableAlias ea $ Nmc "t")]}

>    ,q "select a from (select a from tbl) as d;"
>     stbl {selTref = [SubTref ea stbl (TableAlias ea "d")]}

>    ,q "select a from (select a from tbl) d;"
>     stbl {selTref = [SubTref ea stbl (TableAlias ea "d")]}

>    ]
>    where
>      stbl = makeSelect
>             {selSelectList = sl [si $ ei "a"]
>             ,selTref = [tref "tbl"]}
>      q = QueryExpr
