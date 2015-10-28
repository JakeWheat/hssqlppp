

> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.Joins (joins) where
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Syntax

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> joins :: Item
> joins =
>   Group "joins"
>   [q "select a from t1,t2"
>    stbl {selTref = [tref "t1", tref "t2"]}
>   ,q "select a from t1 natural inner join t2"
>    stbl {selTref = [naturalInnerJoin (tref "t1") (tref "t2")]}
>   ,q "select a from t1 inner join t2 using (a)"
>    stbl {selTref = [usingInnerJoin (tref "t1") (tref "t2") ["a"]]}

todo: these aren't quite right: any join which isn't natural requires
a using or on clause. maybe the syntax should be fixed to represent
this?

>   ,q "select a from t1 left outer join t2"
>    stbl {selTref = [tjoin (tref "t1") LeftOuter (tref "t2") Nothing]}
>   ,q "select a from t1 right outer join t2"
>    stbl {selTref = [tjoin (tref "t1") RightOuter (tref "t2") Nothing]}
>   ,q "select a from t1 full outer join t2"
>    stbl {selTref = [tjoin (tref "t1") FullOuter (tref "t2") Nothing]}
>   ,q "select a from t1 cross join t2"
>    stbl {selTref = [tjoin (tref "t1") Cross (tref "t2") Nothing]}
>   ,q "select a from t1 join t2"
>    stbl {selTref = [tjoin (tref "t1") Inner (tref "t2") Nothing]}
>   ,q "select a from (b natural join c);"
>    stbl {selTref = [tfp $ naturalInnerJoin (tref "b") (tref "c")]}

>   ,q "select a from a cross join b cross join c;"
>    stbl {selTref = [tjoin
>                     (tjoin (tref "a") Cross (tref "b") Nothing)
>                     Cross
>                     (tref "c") Nothing]}
>   ,q "select a from (a cross join b) cross join c;"
>    stbl {selTref = [tjoin
>                     (tfp $ tjoin (tref "a") Cross (tref "b") Nothing)
>                     Cross
>                     (tref "c") Nothing]}
>   ,q "select a from ((a cross join b) cross join c);"
>    stbl {selTref = [tfp $ tjoin
>                     (tfp $ tjoin (tref "a") Cross (tref "b") Nothing)
>                     Cross
>                     (tref "c") Nothing]}

>   ,q "select a from a cross join (b cross join c);"
>    stbl {selTref = [tjoin
>                     (tref "a") Cross
>                     (tfp $ tjoin (tref "b") Cross (tref "c") Nothing)
>                     Nothing]}

>   ,q "select a from (a cross join (b cross join c));"
>    stbl {selTref = [tfp $ tjoin
>                     (tref "a") Cross
>                     (tfp $ tjoin (tref "b") Cross (tref "c") Nothing)
>                     Nothing]}

>   ,q "select a from ((a cross join b) cross join c) cross join d;"
>    stbl {selTref = [tjoin
>                     (tfp $ tjoin
>                      (tfp $ tjoin (tref "a") Cross (tref "b") Nothing)
>                      Cross
>                      (tref "c") Nothing)
>                     Cross
>                     (tref "d") Nothing]}

>   ,q "select a from a cross join b cross join c cross join d;"
>    stbl {selTref = [tjoin
>                     (tjoin
>                      (tjoin (tref "a") Cross (tref "b") Nothing)
>                      Cross
>                      (tref "c") Nothing)
>                     Cross
>                     (tref "d") Nothing]}
>   {-,q "select a from (t cross join u) x"
>    stbl {selTref = [TableAlias ea (Nmc "x") $ tfp
>                     $ (join (tref "t") Cross (tref "u") Nothing)]}
>   ,q "select a from (t as t(a, b) cross join u as t(c, d)) as t(a, b, c, d);"
>    stbl {selTref = [TableAlias ea (Nmc "x") $ tfp
>                     $ (join (tref "t") Cross (tref "u") Nothing)]}-}


>   ,q "select a from b\n\
>       \    inner join c\n\
>       \      on true\n\
>       \    inner join d\n\
>       \      on 1=1;"
>     stbl {selTref = [innerJoin
>                      (innerJoin (tref "b") (tref "c") (Just lTrue))
>                      (tref "d") (Just $ binop "=" (num "1") (num "1"))]}
>   ]

>    where
>      stbl = makeSelect
>             {selSelectList = sl [si $ ei "a"]
>             ,selTref = [tref "tbl"]}
>      q = QueryExpr
