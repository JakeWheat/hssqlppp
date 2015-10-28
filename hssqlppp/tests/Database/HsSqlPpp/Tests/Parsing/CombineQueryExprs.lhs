

> {-# LANGUAGE QuasiQuotes,OverloadedStrings,TemplateHaskell #-}
>
> module Database.HsSqlPpp.Tests.Parsing.CombineQueryExprs (combineQueryExprs) where
>
> import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Syntax

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> import qualified Data.Text as T

> combineQueryExprs :: Item
> combineQueryExprs =
>    Group "combineSelects"
>    [q "select a from tbl\n\
>       \except\n\
>       \select a from tbl1;"
>     $ cq Except stbl stbl1
>    ,q "select a from tbl where true\n\
>         \except\n\
>         \select a from tbl1 where true;"
>       $ cq Except
>             (stbl {selWhere = Just lTrue})
>             (stbl1 {selWhere = Just lTrue})
>    ,q "select a from tbl\n\
>         \union\n\
>         \select a from tbl1;"
>       $ cq Union stbl stbl1
>    ,q "select a from tbl\n\
>         \union all\n\
>         \select a from tbl1;"
>       $ cq UnionAll stbl stbl1
>    ,q "(select 1 union select 2) union select 3;"
>       $ cq Union (cq Union (sel 1) (sel 2)) (sel 3)
>    ,q "select 1 union (select 2 union select 3);"
>       $ cq Union (sel 1) (cq Union (sel 2) (sel 3))
>    ,q [here|
>          with a as (select a from tbl),
>               b as (select a from tbl1)
>               select 1; |]
>          $ with [("a",stbl)
>                 ,("b",stbl1)] $ sel 1
>    ,q [here|
>          with a as (select a from tbl),
>               b as (select a from tbl1)
>               select 1
>               union select 2; |]
>          $ with [("a",stbl)
>                 ,("b",stbl1)] $ cq Union (sel 1) (sel 2)
>    ]
>    where
>      stbl = makeSelect
>             {selSelectList = sl [si $ ei "a"]
>             ,selTref = [tref "tbl"]}
>      stbl1 = makeSelect
>              {selSelectList = sl [si $ ei "a"]
>              ,selTref = [tref "tbl1"]}
>      sel :: Int -> QueryExpr
>      sel n = makeSelect {selSelectList = sl [si $ num $ T.pack $ show n]}
>      q = QueryExpr
>      cq = CombineQueryExpr ea
