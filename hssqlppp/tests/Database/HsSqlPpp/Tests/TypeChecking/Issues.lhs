

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.Issues
>     (issues) where
>
> import Database.HsSqlPpp.Tests.TestTypes
> import Database.HsSqlPpp.TypeCheck
> import Database.HsSqlPpp.Catalog

> --import Database.HsSqlPpp.Types

todo: make sure type checking twice doesn't change the tree at all the
second time - including rewrite versions
issue with select t.x from tbl t, typecheck is mangling to
select tbl.x from tbl t.


>
>
> issues :: Item
> issues =
>   Group "issues"
>   [
>   -- check that quoted select list aliases don't lose their quotes
>    let s = "select t.a as \"Quoted\" from t as t(a,b);"
>    in RewriteQueryExpr defaultTypeCheckFlags
>         {tcfAddQualifiers = True
>         ,tcfAddSelectItemAliases = True
>         ,tcfExpandStars = True
>         ,tcfAddFullTablerefAliases = True} cat s s
>    -- check bug with some uuagc code not adding the
>    -- qualifiers for an identifier properly when
>    -- the identifier appears on its own at the top level of a select list item
>   ,r "select a from t as t(a,b);"
>      "select t.a as a from t as t(a,b);"
>   -- avoid some gratuitous case changes
>   ,r "select A from t as t(a,b);"
>      "select t.A as A from t as t(a,b);"
>   ,r "select T.A from t as t(a,b);"
>      "select T.A as A from t as t(a,b);"
>    -- check not using the alias correctly when qualifying ids
>   ,r "select tbl.a as a from t as tbl(a,b);"
>      "select tbl.a as a from t as tbl(a,b);"

>   --,r "select * from t u inner join t1 u1 on u.a=u1.c;"
>   --   "select * from t u inner join t1 u1 on u.a=u1.c;"
>   ]
>   where
>     r = RewriteQueryExpr defaultTypeCheckFlags
>         {tcfAddQualifiers = True
>         ,tcfAddSelectItemAliases = True
>         ,tcfExpandStars = True
>         ,tcfAddFullTablerefAliases = True}
>         cat
>     cat =
>         [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                             ,("b", mkCatNameExtra "int4")]
>         ,CatCreateTable ("public","t1") [("c", mkCatNameExtra "int4")
>                              ,("d", mkCatNameExtra "int4")]]


