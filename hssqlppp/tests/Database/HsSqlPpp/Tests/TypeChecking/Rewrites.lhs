test adding qualifiers, tableref aliases, expanding *, adding select
item aliases

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.Rewrites
>     (rewrites) where

> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.TypeCheck



> rewrites :: Item
> rewrites =
>   Group "rewrites"
>   [RewriteQueryExpr defaultTypeCheckFlags {tcfAddSelectItemAliases = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]]
>    "select a,b from t"
>    "select a as a,b as b from t"

>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfExpandStars = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]]
>    "select * from t"
>    "select t.a,t.b from t"

>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfAddQualifiers = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]]
>    "select b,count(a) from t group by b"
>    "select t.b,count(t.a) from t group by t.b"

>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfAddQualifiers = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]]
>    "select * from t where a > 3"
>    "select * from t where t.a > 3"

>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfAddQualifiers = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]]
>    "select * from t u where a > 3"
>    "select * from t u where u.a > 3"


>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfAddQualifiers = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]]
>    "select * from t order by a"
>    "select * from t order by t.a"



>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfAddFullTablerefAliases = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]]
>    "select * from t"
>    "select * from t as t(a,b)"


>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfAddFullTablerefAliases = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]]
>    "select * from (select a,b from t) u"
>    "select * from (select a,b from t t(a,b)) u(a,b)"

>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfAddFullTablerefAliases = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]
>          ,CatCreateTable ("public","u") [("c", mkCatNameExtra "int4")
>                              ,("d", mkCatNameExtra "text")]]
>    "select * from t cross join u"
>    -- can't add an alias to the join since the qualifiers
>    -- aren't all the same
>    "select * from t t(a,b) cross join u u(c,d)"

>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfAddFullTablerefAliases = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]
>          ,CatCreateTable ("public","u") [("c", mkCatNameExtra "int4")
>                              ,("d", mkCatNameExtra "text")]]
>    "select * from t cross join u t"
>    "select * from (t t(a,b) cross join u t(c,d)) t(a,b,c,d)"




>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfAddQualifiers = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]]
>    "select a,b from t"
>    "select t.a,t.b from t"

>   ,RewriteQueryExpr defaultTypeCheckFlags {tcfAddQualifiers = True
>                                              ,tcfAddSelectItemAliases = True
>                                              ,tcfExpandStars = True
>                                              ,tcfAddFullTablerefAliases = True}
>          [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                              ,("b", mkCatNameExtra "text")]]
>    "/*thisit*/select * from t"
>    "select t.a as a,t.b as b from t as t(a,b)"

>   ]
