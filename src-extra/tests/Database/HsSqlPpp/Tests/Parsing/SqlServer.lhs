

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
>   [MSStmt "select top 3 * from a order by c;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] (Just (NumberLit ea "3")) Nothing]
>   ,MSStmt "select r.dn as 'rg' from tbl;"
>       [QueryStatement ea
>        $ selectFrom [SelectItem ea (eqi "r" "dn") $ QNmc "rg"]
>                     (Tref ea (i "tbl")
>                     (NoAlias ea))]
>   ,MSStmt "select r.dn as 'check the pretty printing' from tbl;"
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

>   ,MSStmt "select a..b() from t;"
>        [QueryStatement ea
>        $ selectFrom [SelExp ea (App ea (Name ea [Nmc "a",Nmc "", Nmc "b"])
>                                     [])]
>                     (Tref ea (i "t")
>                     (NoAlias ea))]
>   ,MSStmt "select a...b() from t;"
>        [QueryStatement ea
>        $ selectFrom [SelExp ea (App ea (Name ea [Nmc "a",Nmc "", Nmc "", Nmc "b"])
>                                     [])]
>                     (Tref ea (i "t")
>                     (NoAlias ea))]
>   ,MSStmt "select * from a join x..b;"
>        [QueryStatement ea
>        $ selectFrom (selEL [Star ea])
>           (JoinTref ea (Tref ea (Name ea [Nmc "a"]) (NoAlias ea))
>            Unnatural Inner
>            (Tref ea (Name ea [Nmc "x",Nmc "",Nmc "b"]) (NoAlias ea))
>            Nothing (NoAlias ea))
>        ]

>   ,MSStmt "select * from a join x...b;"
>        [QueryStatement ea
>        $ selectFrom (selEL [Star ea])
>           (JoinTref ea (Tref ea (Name ea [Nmc "a"]) (NoAlias ea))
>            Unnatural Inner
>            (Tref ea (Name ea [Nmc "x",Nmc "",Nmc "",Nmc "b"]) (NoAlias ea))
>            Nothing (NoAlias ea))
>        ]
>   ,MSStmt "select a from t with(nolock);"
>           -- with is just recognised, and not parsed to abstract syntax
>        [QueryStatement ea
>        $ selectFrom [SelExp ea (ei "a")]
>                     (Tref ea (i "t")
>                     (NoAlias ea))]
>   ,MSStmt "select a from #tbl;"
>       [QueryStatement ea $ selectFrom (selIL ["a"]) (Tref ea (i "#tbl") (NoAlias ea))]

>   ]

  #identifer
