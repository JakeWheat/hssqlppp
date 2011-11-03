

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
>   ]

a..b
select c from t a with(nolock)
  #identifer
add extra flag: quoted_identifiers
'identifier'