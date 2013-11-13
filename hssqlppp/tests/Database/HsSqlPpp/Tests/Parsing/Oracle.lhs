
Tests for oracle syntax variations

> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.Oracle (oracle) where
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> oracle :: Item
> oracle =
>   Group "parse oracle"
>   [Oracle "select unique a from t;"
>       [qs $ makeSelect
>        {selSelectList = sl [si $ ei "a"]
>        ,selDistinct = Distinct
>        ,selTref = [tref "t"]}]
>   ,Oracle "select count(unique a) from t;"
>       [qs $ makeSelect
>        {selSelectList =
>            sl [si $ AggregateApp ea Distinct (app "count" [ei "a"]) []]
>        ,selTref = [tref "t"]}]

TODO: can't turn this into a regular outer join until type checking
since we need to know where the fields come from, so add a new scalar
expression syntax which represents a (+) suffix.

>   {-,Oracle "select a,b from t,u where a=b(+);"
>    [qs $ makeSelect
>        {selSelectList = sl [sia (eqi "r" "dn") $ QNmc "rg"]
>        ,selTref = [tref "tbl"]}]
>   ,Oracle "select a,b from t,u where a(+)=b;"
>    [qs $ makeSelect
>        {selSelectList = sl [sia (eqi "r" "dn") $ QNmc "rg"]
>        ,selTref = [tref "tbl"]}]-}
>   ]
>   where
>     qs = QueryStatement ea
