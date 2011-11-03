

hack in some sql server support, will refactor to make it a bit cleaner,
and add in flag to parser and pretty printer

todo: temp table name:
select * from #something
with stuff
select * from t with (nolock)


> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.SqlServer (sqlServerParseTests) where
>
> import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> sqlServerParseTests :: Item
> sqlServerParseTests =
>   Group "parse sql server"
>   [Stmt "select top 3 * from a order by c;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] (Just (NumberLit ea "3")) Nothing]
>   ,Stmt "select r.dn as 'rg' from tbl;"
>       [QueryStatement ea
>        $ selectFrom [SelectItem ea (eqi "r" "dn") $ QNmc "rg"]
>                     (Tref ea (i "tbl")
>                     (NoAlias ea))]
>   ,Stmt "select x.y.z(a) from t;"
>         [QueryStatement ea
>          $ selectFrom [SelExp ea (App ea (Name ea ["x","y","z"]) [ei "a"])]
>                     (Tref ea (i "t")
>                     (NoAlias ea))]
>   ]

