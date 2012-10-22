

> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.SqlServer (sqlServer) where
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> sqlServer :: Item
> sqlServer =
>   Group "parse sql server"
>   [TSQL "select top 3 * from a order by c;"
>       [qs $ makeSelect
>               {selSelectList = sl [si $ Star ea]
>               ,selTref = [tref "a"]
>               ,selOrderBy = [(ei "c", Asc)]
>               ,selLimit = Just $ num "3"}]
>   ,TSQL "select top(3) * from a order by c;"
>       [qs $ makeSelect
>               {selSelectList = sl [si $ Star ea]
>               ,selTref = [tref "a"]
>               ,selOrderBy = [(ei "c", Asc)]
>               ,selLimit = Just $ num "3"}]
>   ,TSQL "select r.dn as 'rg' from tbl;"
>    [qs $ makeSelect
>        {selSelectList = sl [sia (eqi "r" "dn") $ QNmc "rg"]
>        ,selTref = [tref "tbl"]}]
>   ,TSQL "select r.dn as 'check the pretty printing' from tbl;"
>    [qs $ makeSelect
>            {selSelectList = sl [sia (eqi "r" "dn") $ QNmc "check the pretty printing"]
>            ,selTref = [tref "tbl"]}]

syntax for identifiers is:
[[[server.] [database].][schema].] database-object]
so could have
server.db.sc.obj
->
server...obj
=> three dots max

>   ,TSQL "select a..b() from t;"
>    [qs $ makeSelect
>            {selSelectList = sl [SelExp ea (App ea (Name ea [Nmc "a",Nmc "", Nmc "b"])
>                                            [])]
>            ,selTref = [tref "t"]}]
>   ,TSQL "select a...b() from t;"
>    [qs $ makeSelect
>            {selSelectList = sl [SelExp ea (App ea (Name ea [Nmc "a",Nmc "", Nmc "", Nmc "b"])
>                                            [])]
>            ,selTref = [tref "t"]}]
>   ,TSQL "select * from a join x..b;"
>    [qs $ makeSelect
>            {selSelectList = sl [si $ Star ea]
>            ,selTref = [innerJoin (tref "a")
>                       (Tref ea (Name ea [Nmc "x",Nmc "",Nmc "b"])) Nothing]}]

>   ,TSQL "select * from a join x...b;"
>    [qs $ makeSelect
>            {selSelectList = sl [si $ Star ea]
>            ,selTref = [innerJoin (tref "a")
>                       (Tref ea (Name ea [Nmc "x",Nmc "",Nmc "",Nmc "b"])) Nothing]}]
>   ,TSQL "select a from t with(nolock);"
>     -- with is just (sort of) recognised, and not parsed to abstract
>     -- syntax
>    [qs $ makeSelect
>            {selSelectList = sl [si $ ei "a"]
>            ,selTref = [tref "t"]}]
>   ,TSQL "select a from #tbl;"
>    [qs $ makeSelect
>            {selSelectList = sl [si $ ei "a"]
>            ,selTref = [tref "#tbl"]}]

>   ,s "CREATE TABLE [schema].[table_name](\n\
>      \             [fieldname] [typename])"
>    $ [CreateTable ea (Name ea [QNmc "schema",QNmc "table_name"])
>       [AttributeDef ea (QNmc "fieldname")
>        (SimpleTypeName ea (Name ea [QNmc "typename"])) Nothing []] []]

>   ,s "select a from t  -- no semi colon\n\
>      \select b from t"
>    $ [qs $ makeSelect
>            {selSelectList = sl [si $ ei "a"]
>            ,selTref = [tref "t"]}
>      ,qs $ makeSelect
>            {selSelectList = sl [si $ ei "b"]
>            ,selTref = [tref "t"]}]

>   ,s "if 1=1\n\
>      \   drop table #temp\n\
>      \select b from t"
>    $ [If ea [(binop "=" (num "1") (num "1")
>              ,[DropSomething ea Table Require [name "#temp"] Restrict])] []
>      ,qs $ makeSelect
>            {selSelectList = sl [si $ ei "b"]
>            ,selTref = [tref "t"]}]

>   ,s "declare @nm int"
>      $ [DeclareStatement ea [("@nm"
>                              ,st "int"
>                              ,Nothing)]]

>   ,s "declare @nm int = 3, @nm2 datetime = '1/1/2000'"
>      $ [DeclareStatement ea [("@nm"
>                              ,st "int"
>                              ,Just (num "3"))
>                             ,("@nm2"
>                              ,st "datetime"
>                              ,Just $ str "1/1/2000")
>                             ]]

>   ,s "set @nm=3"
>      $ [Assignment ea (name "@nm") (num "3")]
>   ,s "select convert (INT,5) from t"
>      $ [qs $ makeSelect
>            {selSelectList = sl [si $ Cast ea (num "5") (st "INT")]
>            ,selTref = [tref "t"]}]

needs to be better: the style is lost

>   ,s "select convert (time,something,108) from t"
>      $ [qs $ makeSelect
>            {selSelectList = sl [si $ Cast ea (ei "something") (st "time")]
>            ,selTref = [tref "t"]}]

>   ,s "CREATE NONCLUSTERED INDEX idx ON tbl (col) INCLUDE (Gap)"
>      $ [CreateIndexTSQL ea (Nmc "idx") (name "tbl") [Nmc "col"]]

>   ,s "CREATE NONCLUSTERED INDEX idx ON [dbo].[#tmp] (col) INCLUDE (Gap)"
>      $ [CreateIndexTSQL ea (Nmc "idx")
>                             (Name ea [QNmc "dbo"
>                                      ,QNmc "#tmp"])
>                            [Nmc "col"]]
>   ,s "select y -@test from t"
>      $ [qs $ makeSelect
>            {selSelectList = sl [si $ binop "-" (ei "y") (ei "@test")]
>            ,selTref = [tref "t"]}]
>   ,s "select * from t natural inner hash join u"
>      [qs $ makeSelect
>        {selSelectList = sl [si $ Star ea]
>        ,selTref = [JoinTref ea (tref "t") Natural Inner
>                    (Just Hash) (tref "u") Nothing]}
>      ]
>   ,s "select * from t natural inner loop join u"
>      [qs $ makeSelect
>        {selSelectList = sl [si $ Star ea]
>        ,selTref = [JoinTref ea (tref "t") Natural Inner
>                    (Just Loop) (tref "u") Nothing]}
>      ]
>   ,s "select * from t natural inner merge join u"
>      [qs $ makeSelect
>        {selSelectList = sl [si $ Star ea]
>        ,selTref = [JoinTref ea (tref "t") Natural Inner
>                    (Just Merge) (tref "u") Nothing]}
>      ]

>   ]
>   where
>     s = TSQL
>     qs = QueryStatement ea

create index ++
parse select into to create table as (do this for postgresql non
  plpgsql also)
