
> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.Selects (selectParsingTestData) where
>
> import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> selectParsingTestData:: Item
> selectParsingTestData =
>    Group "parse selects" [
>    Group "simple select statements" [
>     Group "select no table" [
>       s "select 1;"
>       [qs $ makeSelect {selSelectList = sl [si $ num "1"]}]
>      ]
>    ,Group "select from table" [
>       s "select * from tbl;"
>       [qs $ makeSelect
>               {selSelectList = sl [si $ Star ea]
>               ,selTref = [tref "tbl"]}]
>      ,s "select a,b from tbl;"
>       [qs $ makeSelect
>               {selSelectList = sl [si $ ei "a"
>                                   ,si $ ei "b"]
>               ,selTref = [tref "tbl"]}]
>      ,s "select a,b from inf.tbl;"
>       [qs $ makeSelect
>               {selSelectList = sl [si $ ei "a"
>                                   ,si $ ei "b"]
>               ,selTref = [qtref "inf" "tbl"]}]
>      ,s "select distinct * from tbl;"
>       [qs $ makeSelect
>               {selDistinct = Distinct
>               ,selSelectList = sl [si $ Star ea]
>               ,selTref = [tref "tbl"]}]
>      ,s "select a from tbl where b=2;"
>       [qs $ makeSelect
>               {selSelectList = sl [si $ ei "a"]
>               ,selTref = [tref "tbl"]
>               ,selWhere = Just $ binop "=" (ei "b") (num "2")}]
>      ,s "select a from tbl where b=2 and c=3;"
>       [qs $ makeSelect
>               {selSelectList = sl [si $ ei "a"]
>               ,selTref = [tref "tbl"]
>               ,selWhere = Just $ binop "!and"
>                           (binop "=" (ei "b") (num "2"))
>                           (binop "=" (ei "c") (num "3"))}]
>      ,s "SELECT T.A::INT FROM TBL AS T;"
>         [qs $ makeSelect
>               {selSelectList = sl [si $ Cast ea (eqi "T" "A")
>                                           (st "INT")]
>               ,selTref = [trefa "TBL" "T"]}]
>      ]
>

>    ,Group "more select statements" [
>       s "select a from tbl\n\
>         \except\n\
>         \select a from tbl1;"
>       [qs $ CombineQueryExpr ea Except
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"]
>         ,selTref = [tref "tbl"]})
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"]
>         ,selTref = [tref "tbl1"]})]
>      ,s "select a from tbl where true\n\
>         \except\n\
>         \select a from tbl1 where true;"
>       [qs $ CombineQueryExpr ea Except
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"]
>         ,selTref = [tref "tbl"]
>         ,selWhere = Just lTrue})
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"]
>         ,selTref = [tref "tbl1"]
>         ,selWhere = Just lTrue})]
>      ,s "select a from tbl\n\
>         \union\n\
>         \select a from tbl1;"
>       [qs $ CombineQueryExpr ea Union
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"]
>         ,selTref = [tref "tbl"]})
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"]
>         ,selTref = [tref "tbl1"]})]
>      ,s "select a from tbl\n\
>         \union all\n\
>         \select a from tbl1;"
>       [qs $ CombineQueryExpr ea UnionAll
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"]
>         ,selTref = [tref "tbl"]})
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"]
>         ,selTref = [tref "tbl1"]})]
>      ,s "(select 1 union select 2) union select 3;"
>       [qs
>        (CombineQueryExpr ea Union
>         (CombineQueryExpr ea Union
>          (makeSelect {selSelectList = sl [si $ num "1"]})
>          (makeSelect {selSelectList = sl [si $ num "2"]}))
>         (makeSelect {selSelectList = sl [si $ num "3"]}))]
>      ,s "select 1 union (select 2 union select 3);"
>       [qs
>        (CombineQueryExpr ea Union
>         (makeSelect {selSelectList = sl [si $ num "1"]})
>         (CombineQueryExpr ea Union
>          (makeSelect {selSelectList = sl [si $ num "2"]})
>          (makeSelect {selSelectList = sl [si $ num "3"]})))]

>      ,s [$here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from b; |]
>          [qs $
>           with [("a",makeSelect {selSelectList = sl [sia (num "1") "a1"]})
>                ,("b",makeSelect {selSelectList = sl [si $ Star ea]
>                                 ,selTref = [tref "a"]})]
>           $ makeSelect {selSelectList = sl [si $ Star ea]
>                        ,selTref = [tref "b"]}]

>      ,s [$here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from a
>               union select * from b; |]
>          [qs $
>           with [("a",makeSelect {selSelectList = sl [sia (num "1") "a1"]})
>                ,("b",makeSelect {selSelectList = sl [si $ Star ea]
>                                 ,selTref = [tref "a"]})]
>           $ CombineQueryExpr ea Union
>             (makeSelect {selSelectList = sl [si $ Star ea]
>                        ,selTref = [tref "a"]})
>             (makeSelect {selSelectList = sl [si $ Star ea]
>                        ,selTref = [tref "b"]})]
>      ,s "select a as b from tbl;"
>       [qs $
>        makeSelect {selSelectList = sl [sia (ei "a") "b"]
>                   ,selTref = [tref "tbl"]}]
>      ,s "select a + b as b from tbl;"
>       [qs $
>        makeSelect {selSelectList = sl [sia (binop "+" (ei "a") (ei "b")) "b"]
>                   ,selTref = [tref "tbl"]}]
>      ,s "select a.* from tbl a;"
>       [qs $
>        makeSelect {selSelectList = sl [si $ QStar ea (Nmc "a")]
>                   ,selTref = [trefa "tbl" "a"]}]

>      ,s "select a.* from tbl a(b,c);"
>       [qs $
>        makeSelect {selSelectList = sl [si $ QStar ea (Nmc "a")]
>                   ,selTref = [Tref ea (name "tbl")
>                               $ (FullAlias ea (Nmc "a") [Nmc "b",Nmc "c"])]}]

>      ,s "select * from t1 a, t2 b;"
>             [qs
>              (Select ea Dupes
>               (SelectList ea
>                [SelExp ea (Star ea)])
>               [Tref ea (i "t1") (TableAlias ea (Nmc "a")),Tref ea (i "t2") (TableAlias ea (Nmc "b"))]
>               Nothing [] Nothing [] Nothing Nothing)]
>      ,s "select a from b inner join c on b.a=c.a;"
>       [qs $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (NoAlias ea))
>           (Just (JoinOn ea
>            (binop "=" (eqi "b" "a") (eqi "c" "a")))) (NoAlias ea))]
>      ,s "select a from b inner join c as d on b.a=d.a;"
>       [qs $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (TableAlias ea (Nmc "d")))
>           (Just (JoinOn ea
>            (binop "=" (eqi "b" "a") (eqi "d" "a")))) (NoAlias ea))]
>      ,s "select a from b inner join c using(d,e);"
>       [qs $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (NoAlias ea))
>           (Just (JoinUsing ea [Nmc "d",Nmc "e"])) (NoAlias ea))]
>      ,s "select a from b natural inner join c;"
>       [qs $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Natural Inner (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select a from b left outer join c;"
>       [qs $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural LeftOuter (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select a from b full outer join c;"
>       [qs $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural FullOuter (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select a from b right outer join c;"
>       [qs $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural RightOuter (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select a from b cross join c;"
>       [qs $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Cross (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select a from (b natural join c);"
>       [qs $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Natural Inner (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select x from a cross join b cross join c;"
>        [qs
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>          (JoinTref ea
>           (Tref ea (i "a") (NoAlias ea))
>            Unnatural Cross
>           (Tref ea (i "b") (NoAlias ea))
>           Nothing (NoAlias ea))
>          Unnatural Cross
>          (Tref ea (i "c") (NoAlias ea))
>          Nothing (NoAlias ea)))]
>      ,s "select x from ((a cross join b) cross join c);"
>        [qs
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>          (JoinTref ea
>           (Tref ea (i "a") (NoAlias ea))
>            Unnatural Cross
>           (Tref ea (i "b") (NoAlias ea))
>           Nothing (NoAlias ea))
>          Unnatural Cross
>          (Tref ea (i "c") (NoAlias ea))
>          Nothing (NoAlias ea)))]
>      ,s "select x from (a cross join (b cross join c));"
>        [qs
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>           (Tref ea (i "a") (NoAlias ea))
>           Unnatural Cross
>           (JoinTref ea
>            (Tref ea (i "b") (NoAlias ea))
>            Unnatural Cross
>            (Tref ea (i "c") (NoAlias ea))
>            Nothing (NoAlias ea))
>           Nothing (NoAlias ea)))]

>      ,s "select x from ((a cross join b) cross join c);"
>        [qs
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>          (JoinTref ea
>           (Tref ea (i "a") (NoAlias ea))
>            Unnatural Cross
>           (Tref ea (i "b") (NoAlias ea))
>           Nothing (NoAlias ea))
>          Unnatural Cross
>          (Tref ea (i "c") (NoAlias ea))
>          Nothing (NoAlias ea)))]
>      ,s "select x from (a cross join b) cross join c;"
>        [qs
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>          (JoinTref ea
>           (Tref ea (i "a") (NoAlias ea))
>            Unnatural Cross
>           (Tref ea (i "b") (NoAlias ea))
>           Nothing (NoAlias ea))
>          Unnatural Cross
>          (Tref ea (i "c") (NoAlias ea))
>          Nothing (NoAlias ea)))]
>      ,s "select x from ((a cross join b) cross join c) cross join d;"
>        [qs
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>           (JoinTref ea
>            (JoinTref ea
>             (Tref ea (i "a") (NoAlias ea))
>             Unnatural Cross
>             (Tref ea (i "b") (NoAlias ea))
>             Nothing (NoAlias ea))
>            Unnatural Cross
>            (Tref ea (i "c") (NoAlias ea))
>            Nothing (NoAlias ea))
>           Unnatural Cross
>           (Tref ea (i "d") (NoAlias ea))
>           Nothing (NoAlias ea)))]
>      ,s "select a from b\n\
>         \    inner join c\n\
>         \      on true\n\
>         \    inner join d\n\
>         \      on 1=1;"
>       [qs $ selectFrom
>        [SelExp ea (Identifier ea "a")]
>        (JoinTref ea
>         (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (NoAlias ea))
>          (Just $ JoinOn ea (BooleanLit ea True)) (NoAlias ea))
>         Unnatural Inner (Tref ea (i "d") (NoAlias ea))
>         (Just $ JoinOn ea (binop "=" (num "1") (num "1"))) (NoAlias ea))]

>      ,s "select row_number() over(order by a) as place from tbl;"
>       [qs $ selectFrom [SelectItem ea
>                    (WindowApp ea
>                     (App ea (name "row_number") [])
>                     []
>                     [(Identifier ea "a", Asc)] FrameUnboundedPreceding)
>                    (Nmc "place")]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select row_number() over(order by a asc) as place from tbl;"
>       [qs $ selectFrom [SelectItem ea
>                    (WindowApp ea
>                     (App ea (name "row_number") [])
>                     []
>                     [(Identifier ea "a",Asc)] FrameUnboundedPreceding)
>                    (Nmc "place")]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select row_number() over(order by a desc) as place from tbl;"
>       [qs $ selectFrom [SelectItem ea
>                    (WindowApp ea
>                     (App ea (name "row_number") [])
>                     []
>                     [(Identifier ea "a", Desc)] FrameUnboundedPreceding)
>                    (Nmc "place")]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select row_number()\n\
>         \over(partition by (a,b) order by c) as place\n\
>         \from tbl;"
>       [qs $ selectFrom [SelectItem ea
>                    (WindowApp ea
>                     (App ea (name "row_number") [])
>                     [SpecialOp ea (name "!rowctor") [Identifier ea "a",Identifier ea "b"]]
>                     [(Identifier ea "c", Asc)] FrameUnboundedPreceding)
>                    (Nmc "place")]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select * from a natural inner join (select * from b) as a;"
>       [qs $ selectFrom
>        (selEL [Star ea])
>        (JoinTref ea (Tref ea (i "a") (NoAlias ea)) Natural
>         Inner (SubTref ea (selectFrom
>                         (selEL [Star ea])
>                         (Tref ea (i "b") (NoAlias ea))) (TableAlias ea $ Nmc "a"))
>         Nothing (NoAlias ea))]
>      ,s "select * from a order by c;"
>       [qs $ Select ea  Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] Nothing Nothing]
>      ,s "select *\n\
>            \from Adventure\n\
>            \order by Clicks desc, AdventureID;"
>       [qs $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "Adventure") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "Clicks",Desc)
>                           ,(Identifier ea "AdventureID",Asc)] Nothing Nothing]
>      ,s "select * from a order by c,d asc;"
>       [qs $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c", Asc)
>                           ,(Identifier ea "d", Asc)] Nothing Nothing]
>      ,s "select * from a order by c,d desc;"
>       [qs $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c", Asc)
>                           ,(Identifier ea "d", Desc)] Nothing Nothing]
>      ,s "select * from a order by c limit 1;"
>       [qs $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] (Just (NumberLit ea "1")) Nothing]
>      ,s "select * from a order by c offset 3;"
>       [qs $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] Nothing (Just $ NumberLit ea "3")]
>      ,s "select a from (select b from c) as d;"
>         [qs $ selectFrom
>          (selIL ["a"])
>          (SubTref ea (selectFrom
>                    (selIL ["b"])
>                    (Tref ea (i "c") (NoAlias ea)))
>           (TableAlias ea $ Nmc "d"))]
>      ,s "select * from gen();"
>         [qs $ selectFrom (selEL [Star ea]) (FunTref ea (App ea (name "gen") []) (NoAlias ea))]
>      ,s "select * from gen() as t;"
>       [qs $ selectFrom
>        (selEL [Star ea])
>        (FunTref ea (App ea (name "gen") [])(TableAlias ea $ Nmc "t"))]
>      ,s "select count(distinct b) from c;"
>         [qs $ Select ea Dupes
>          (sl [SelExp ea (AggregateApp ea Distinct
>                          (App ea (name "count") [Identifier ea "b"])
>                          [])])
>          [Tref ea (i "c") (NoAlias ea)] Nothing []
>          Nothing [] Nothing Nothing]
>      ,s "select count(all b) from c;"
>         [qs $ Select ea Dupes
>          (sl [SelExp ea (AggregateApp ea Dupes
>                          (App ea (name "count") [Identifier ea "b"])
>                          [])])
>          [Tref ea (i "c") (NoAlias ea)] Nothing []
>          Nothing [] Nothing Nothing]
>      ,s "select string_agg(distinct relname,',' order by relname1) from pg_class;"
>         [qs $ Select ea Dupes
>          (sl [SelExp ea (AggregateApp ea Distinct
>                          (App ea (name "string_agg") [Identifier ea "relname"
>                                                   ,StringLit ea ","])
>                          [(Identifier ea "relname1", Asc)])])
>          [Tref ea (i "pg_class") (NoAlias ea)] Nothing []
>          Nothing [] Nothing Nothing]
>      ,s "select a, count(b) from c group by a;"
>         [qs $ Select ea Dupes
>          (sl [selI "a", SelExp ea (App ea (name "count") [Identifier ea "b"])])
>          [Tref ea (i "c") (NoAlias ea)] Nothing [Identifier ea "a"]
>          Nothing [] Nothing Nothing]
>      ,s "select a, count(b) as cnt from c group by a having cnt > 4;"
>         [qs $ Select ea Dupes
>          (sl [selI "a", SelectItem ea (App ea (name "count") [Identifier ea "b"]) $ Nmc "cnt"])
>          [Tref ea (i "c") (NoAlias ea)] Nothing [Identifier ea "a"]
>          (Just $ binop  ">" (ei "cnt") (num "4"))
>          [] Nothing Nothing]
>      ,s "select a from (select 1 as a, 2 as b) x;"
>         [qs $ selectFrom
>          [selI "a"]
>          (SubTref ea (selectE $ SelectList ea
>                                [SelectItem ea (NumberLit ea "1") $ Nmc "a"
>                                ,SelectItem ea (NumberLit ea "2") $ Nmc "b"])
>                   (TableAlias ea $ Nmc "x"))]
>      ]
>      ]
>    ,Group "some misc stuff" [
>       s "select (p).x, (p).y from pos;"
>         [qs $ selectFrom (selEL [parenQual (ei "p") (ei "x")
>                                                ,parenQual (ei "p") (ei "y")])
>                                          (Tref ea (i "pos") (NoAlias ea))]
>      ,s "select ($1).x, ($1).y from pos;"
>         [qs $ selectFrom
>          (selEL [member (Parens ea $ PositionalArg ea 1) (Identifier ea "x")
>                 ,member (Parens ea $ PositionalArg ea 1) (Identifier ea "y")])
>          (Tref ea (i "pos") (NoAlias ea))]
>      ,s "select row_number() over(), x from tb;"
>       [qs $ selectFrom
>        [SelExp ea
>                     (WindowApp ea
>                     (App ea (name "row_number") [])
>                     []
>                     [] FrameUnboundedPreceding)
>        , selI "x"]
>        (Tref ea (i "tb") (NoAlias ea))]
>      ]]
>  where
>    s = Stmt
>    qs = QueryStatement ea
