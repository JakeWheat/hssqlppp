test adding qualifiers, tableref aliases, expanding *, adding select
item aliases

> module Database.HsSqlPpp.Tests.TypeChecking.Rewrites
>     (rewrites) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> --import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.TypeChecker



> rewrites :: Item
> rewrites =
>   Group "rewrites"
>   [RewriteQueryExpr defaultTypeCheckingFlags {tcfAddSelectItemAliases = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select a,b from t"
>    "select a as a,b as b from t"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfExpandStars = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select * from t"
>    "select t.a,t.b from t"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddQualifiers = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select b,count(a) from t group by b"
>    "select t.b,count(t.a) from t group by t.b"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddQualifiers = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select * from t where a > 3"
>    "select * from t where t.a > 3"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddQualifiers = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select * from t order by a"
>    "select * from t order by t.a"



>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddFullTablerefAliases = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select * from t"
>    "select * from t as t(a,b)"


>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddFullTablerefAliases = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select * from (select a,b from t) u"
>    "select * from (select a,b from t t(a,b)) u(a,b)"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddFullTablerefAliases = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]
>          ,CatCreateTable "u" [("c", "int4")
>                              ,("d", "text")]]
>    "select * from t cross join u"
>    -- can't add an alias to the join since the qualifiers
>    -- aren't all the same
>    "select * from t t(a,b) cross join u u(c,d)"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddFullTablerefAliases = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]
>          ,CatCreateTable "u" [("c", "int4")
>                              ,("d", "text")]]
>    "select * from t cross join u t"
>    "select * from (t t(a,b) cross join u t(c,d)) t(a,b,c,d)"




>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddQualifiers = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select a,b from t"
>    "select t.a,t.b from t"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddQualifiers = True
>                                              ,tcfAddSelectItemAliases = True
>                                              ,tcfExpandStars = True
>                                              ,tcfAddFullTablerefAliases = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "/*thisit*/select * from t"
>    "select t.a as a,t.b as b from t as t(a,b)"

>   ]
