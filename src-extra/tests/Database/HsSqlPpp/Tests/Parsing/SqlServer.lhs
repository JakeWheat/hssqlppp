

> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.SqlServer (sqlServerParseTests) where
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> sqlServerParseTests :: Item
> sqlServerParseTests =
>   Group "parse sql server"
>   [TSQL "select top 3 * from a order by c;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] (Just (NumberLit ea "3")) Nothing]
>   ,TSQL "select r.dn as 'rg' from tbl;"
>       [QueryStatement ea
>        $ selectFrom [SelectItem ea (eqi "r" "dn") $ QNmc "rg"]
>                     (Tref ea (i "tbl")
>                     (NoAlias ea))]
>   ,TSQL "select r.dn as 'check the pretty printing' from tbl;"
>       [QueryStatement ea
>        $ selectFrom [SelectItem ea (eqi "r" "dn") $ QNmc "check the pretty printing"]
>                     (Tref ea (i "tbl")
>                     (NoAlias ea))]

syntax for identifiers is:
[[[server.] [database].][schema].] database-object]
so could have
server.db.sc.obj
->
server...obj
=> three dots max

>   ,TSQL "select a..b() from t;"
>        [QueryStatement ea
>        $ selectFrom [SelExp ea (App ea (Name ea [Nmc "a",Nmc "", Nmc "b"])
>                                     [])]
>                     (Tref ea (i "t")
>                     (NoAlias ea))]
>   ,TSQL "select a...b() from t;"
>        [QueryStatement ea
>        $ selectFrom [SelExp ea (App ea (Name ea [Nmc "a",Nmc "", Nmc "", Nmc "b"])
>                                     [])]
>                     (Tref ea (i "t")
>                     (NoAlias ea))]
>   ,TSQL "select * from a join x..b;"
>        [QueryStatement ea
>        $ selectFrom (selEL [Star ea])
>           (JoinTref ea (Tref ea (Name ea [Nmc "a"]) (NoAlias ea))
>            Unnatural Inner
>            (Tref ea (Name ea [Nmc "x",Nmc "",Nmc "b"]) (NoAlias ea))
>            Nothing (NoAlias ea))
>        ]

>   ,TSQL "select * from a join x...b;"
>        [QueryStatement ea
>        $ selectFrom (selEL [Star ea])
>           (JoinTref ea (Tref ea (Name ea [Nmc "a"]) (NoAlias ea))
>            Unnatural Inner
>            (Tref ea (Name ea [Nmc "x",Nmc "",Nmc "",Nmc "b"]) (NoAlias ea))
>            Nothing (NoAlias ea))
>        ]
>   ,TSQL "select a from t with(nolock);"
>     -- with is just (sort of) recognised, and not parsed to abstract
>     -- syntax
>        [QueryStatement ea
>        $ selectFrom [SelExp ea (ei "a")]
>                     (Tref ea (i "t")
>                     (NoAlias ea))]
>   ,TSQL "select a from #tbl;"
>       [QueryStatement ea $ selectFrom (selIL ["a"]) (Tref ea (i "#tbl") (NoAlias ea))]

>   ,s "CREATE TABLE [schema].[table_name](\n\
>      \             [fieldname] [typename])"
>    $ [CreateTable ea (Name ea [QNmc "schema",QNmc "table_name"])
>       [AttributeDef ea (QNmc "fieldname")
>        (SimpleTypeName ea (Name ea [QNmc "typename"])) Nothing []] []]

>   ,s "select a from t  -- no semi colon\n\
>      \select b from t"
>    $ [QueryStatement ea
>       $ selectFrom [SelExp ea (ei "a")]
>                     (Tref ea (i "t")
>                     (NoAlias ea))
>      ,QueryStatement ea
>       $ selectFrom [SelExp ea (ei "b")]
>                     (Tref ea (i "t")
>                     (NoAlias ea))]


>   ,s "if 1=1\n\
>      \   drop table #temp\n\
>      \select b from t"
>    $ [If ea [(binop "=" (num "1") (num "1")
>              ,[DropSomething ea Table Require [Name ea [Nmc "#temp"]] Restrict])] []
>      ,QueryStatement ea
>       $ selectFrom [SelExp ea (ei "b")]
>                     (Tref ea (i "t")
>                     (NoAlias ea))]

>   ,s "declare @nm int"
>      $ [DeclareStatement ea "@nm" $ SimpleTypeName ea (Name ea [Nmc "int"])]

>   ,s "set @nm=3"
>      $ [Assignment ea (name "@nm") (num "3")]
>   ]
>   where
>     s = TSQL

create index ++
set
parse select into to create table as (do this for postgresql non
  plpgsql also)
