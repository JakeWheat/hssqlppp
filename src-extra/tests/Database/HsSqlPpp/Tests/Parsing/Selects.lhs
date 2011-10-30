
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
>       s "select 1;" [QueryStatement ea $ selectE (SelectList ea [SelExp ea (NumberLit ea "1")])]
>      ]
>    ,Group "select from table" [
>       s "select * from tbl;"
>       [QueryStatement ea $ selectFrom (selEL [Star ea]) (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select a,b from tbl;"
>       [QueryStatement ea $ selectFrom (selIL ["a", "b"]) (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select a,b from inf.tbl;"
>       [QueryStatement ea $ selectFrom (selIL ["a", "b"]) (Tref ea (qi "inf" "tbl") (NoAlias ea))]
>      ,s "select distinct * from tbl;"
>       [QueryStatement ea $ Select ea Distinct (SelectList ea (selEL [Star ea])) [Tref ea (i "tbl") (NoAlias ea)]
>        Nothing [] Nothing [] Nothing Nothing]
>      ,s "select a from tbl where b=2;"
>       [QueryStatement ea $ selectFromWhere
>         (selIL ["a"])
>         (Tref ea (i "tbl") (NoAlias ea))
>         (App ea (name "=")
>          [Identifier ea "b", NumberLit ea "2"])]
>      ,s "select a from tbl where b=2 and c=3;"
>       [QueryStatement ea $ selectFromWhere
>         (selIL ["a"])
>         (Tref ea (i "tbl") (NoAlias ea))
>         (App ea (name "!and")
>          [App ea (name "=")  [Identifier ea "b", NumberLit ea "2"]
>          ,App ea (name "=") [Identifier ea "c", NumberLit ea "3"]])]
>      {-,MSStmt "select a from t;\ngo"
>          [QueryStatement ea $ selectFrom (selIL ["a"]) (Tref ea (i "t") (NoAlias ea))]
>      ,MSStmt "select a from t;\nset rowcount -1\ngo"
>          [QueryStatement ea $ selectFrom (selIL ["a"]) (Tref ea (i "t") (NoAlias ea))]
>      ,MSStmt "select a from t;\nset rowcount 10\ngo"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selIL ["a"]))
>        [Tref ea (i "t") (NoAlias ea)]
>        Nothing [] Nothing [] (Just (NumberLit ea "10")) Nothing]-}
>      ,s "SELECT T.A::INT FROM TABLE AS T;"
>         [QueryStatement ea
>          (Select ea Dupes
>           (SelectList ea
>            [SelExp ea (Cast ea (QIdentifier ea [Nmc "T",Nmc "A"])
>                        (SimpleTypeName ea "int"))])
>           [Tref ea (Name ea [Nmc "TABLE"]) (TableAlias ea (Nmc "T"))]
>           Nothing [] Nothing [] Nothing Nothing)]
>      ]
>

>    ,Group "more select statements" [
>       s "select a from tbl\n\
>         \except\n\
>         \select a from tbl1;"
>       [QueryStatement ea $ CombineQueryExpr ea Except
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl") (NoAlias ea)))
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl1") (NoAlias ea)))]
>      ,s "select a from tbl where true\n\
>         \except\n\
>         \select a from tbl1 where true;"
>       [QueryStatement ea $ CombineQueryExpr ea Except
>        (selectFromWhere (selIL ["a"]) (Tref ea (i "tbl") (NoAlias ea)) (BooleanLit ea True))
>        (selectFromWhere (selIL ["a"]) (Tref ea (i "tbl1") (NoAlias ea)) (BooleanLit ea True))]
>      ,s "select a from tbl\n\
>         \union\n\
>         \select a from tbl1;"
>       [QueryStatement ea $ CombineQueryExpr ea Union
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl") (NoAlias ea)))
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl1") (NoAlias ea)))]
>      ,s "select a from tbl\n\
>         \union all\n\
>         \select a from tbl1;"
>       [QueryStatement ea $ CombineQueryExpr ea UnionAll
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl") (NoAlias ea)))
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl1") (NoAlias ea)))]
>      ,s "(select 1 union select 2) union select 3;"
>       [QueryStatement ea
>        (CombineQueryExpr ea Union
>         (CombineQueryExpr ea Union
>          (selectE (SelectList ea [SelExp ea (NumberLit ea "1")]))
>          (selectE (SelectList ea [SelExp ea (NumberLit ea "2")])))
>         (selectE (SelectList ea [SelExp ea (NumberLit ea "3")])))]
>      ,s "select 1 union (select 2 union select 3);"
>       [QueryStatement ea
>        (CombineQueryExpr ea Union
>         (selectE (SelectList ea [SelExp ea (NumberLit ea "1")]))
>         (CombineQueryExpr ea Union
>          (selectE (SelectList ea [SelExp ea (NumberLit ea "2")]))
>          (selectE (SelectList ea [SelExp ea (NumberLit ea "3")]))))]
>      ,s [here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from b; |]
>          [QueryStatement ea
>           (WithQueryExpr ea
>            [WithQuery ea (Nmc "a") Nothing (selectE $ SelectList ea
>                                             [SelectItem ea (NumberLit ea "1") (Nmc "a1")])
>            ,WithQuery ea (Nmc "b") Nothing (selectFrom (selEL [Star ea]) (Tref ea (i "a") (NoAlias ea)))]
>            (selectFrom (selEL [Star ea]) (Tref ea (i "b") (NoAlias ea))))]
>      ,s [here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from a
>               union select * from b; |]
>          [QueryStatement ea
>           (WithQueryExpr ea
>            [WithQuery ea (Nmc "a") Nothing (selectE $ SelectList ea
>                                             [SelectItem ea (NumberLit ea "1") (Nmc "a1")])
>            ,WithQuery ea (Nmc "b") Nothing (selectFrom (selEL [Star ea]) (Tref ea (i "a") (NoAlias ea)))]
>            (CombineQueryExpr ea Union
>              (selectFrom (selEL [Star ea]) (Tref ea (i "a") (NoAlias ea)))
>              (selectFrom (selEL [Star ea]) (Tref ea (i "b") (NoAlias ea)))))]
>      ,s "select a as b from tbl;"
>       [QueryStatement ea $ selectFrom [SelectItem ea (Identifier ea "a") (Nmc "b")] (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select a + b as b from tbl;"
>       [QueryStatement ea $ selectFrom
>        [SelectItem ea
>         (App ea (name "+")
>          [Identifier ea "a", Identifier ea "b"]) (Nmc "b")]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select a.* from tbl a;"
>       [QueryStatement ea $ selectFrom (selEL [QStar ea (Nmc "a")]) (Tref ea (i "tbl") (TableAlias ea (Nmc "a")))]
>      ,s "select a.* from tbl a(b,c);"
>       [QueryStatement ea $ selectFrom (selEL [QStar ea (Nmc "a")]) (Tref ea (i "tbl") (FullAlias ea (Nmc "a") [Nmc "b",Nmc "c"]))]

>      ,s "select * from t1 a, t2 b;"
>             [QueryStatement ea
>              (Select ea Dupes
>               (SelectList ea
>                [SelExp ea (Star ea)])
>               [Tref ea (i "t1") (TableAlias ea (Nmc "a")),Tref ea (i "t2") (TableAlias ea (Nmc "b"))]
>               Nothing [] Nothing [] Nothing Nothing)]
>      ,s "select a from b inner join c on b.a=c.a;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (NoAlias ea))
>           (Just (JoinOn ea
>            (App ea (name "=") [eqi "b" "a", eqi "c" "a"]))) (NoAlias ea))]
>      ,s "select a from b inner join c as d on b.a=d.a;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (TableAlias ea (Nmc "d")))
>           (Just (JoinOn ea
>            (App ea (name "=") [eqi "b" "a", eqi "d" "a"]))) (NoAlias ea))]
>      ,s "select a from b inner join c using(d,e);"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (NoAlias ea))
>           (Just (JoinUsing ea [Nmc "d",Nmc "e"])) (NoAlias ea))]
>      ,s "select a from b natural inner join c;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Natural Inner (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select a from b left outer join c;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural LeftOuter (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select a from b full outer join c;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural FullOuter (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select a from b right outer join c;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural RightOuter (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select a from b cross join c;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Cross (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select a from (b natural join c);"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Natural Inner (Tref ea (i "c") (NoAlias ea)) Nothing (NoAlias ea))]
>      ,s "select x from a cross join b cross join c;"
>        [QueryStatement ea
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
>        [QueryStatement ea
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
>        [QueryStatement ea
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
>        [QueryStatement ea
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
>        [QueryStatement ea
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
>        [QueryStatement ea
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
>       [QueryStatement ea $ selectFrom
>        [SelExp ea (Identifier ea "a")]
>        (JoinTref ea
>         (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (NoAlias ea))
>          (Just $ JoinOn ea (BooleanLit ea True)) (NoAlias ea))
>         Unnatural Inner (Tref ea (i "d") (NoAlias ea))
>         (Just $ JoinOn ea (App ea (name "=")
>                [NumberLit ea "1", NumberLit ea "1"])) (NoAlias ea))]

>      ,s "select row_number() over(order by a) as place from tbl;"
>       [QueryStatement ea $ selectFrom [SelectItem ea
>                    (WindowApp ea
>                     (App ea (name "row_number") [])
>                     []
>                     [(Identifier ea "a", Asc)] FrameUnboundedPreceding)
>                    (Nmc "place")]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select row_number() over(order by a asc) as place from tbl;"
>       [QueryStatement ea $ selectFrom [SelectItem ea
>                    (WindowApp ea
>                     (App ea (name "row_number") [])
>                     []
>                     [(Identifier ea "a",Asc)] FrameUnboundedPreceding)
>                    (Nmc "place")]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select row_number() over(order by a desc) as place from tbl;"
>       [QueryStatement ea $ selectFrom [SelectItem ea
>                    (WindowApp ea
>                     (App ea (name "row_number") [])
>                     []
>                     [(Identifier ea "a", Desc)] FrameUnboundedPreceding)
>                    (Nmc "place")]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select row_number()\n\
>         \over(partition by (a,b) order by c) as place\n\
>         \from tbl;"
>       [QueryStatement ea $ selectFrom [SelectItem ea
>                    (WindowApp ea
>                     (App ea (name "row_number") [])
>                     [App ea (name "!rowctor") [Identifier ea "a",Identifier ea "b"]]
>                     [(Identifier ea "c", Asc)] FrameUnboundedPreceding)
>                    (Nmc "place")]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select * from a natural inner join (select * from b) as a;"
>       [QueryStatement ea $ selectFrom
>        (selEL [Star ea])
>        (JoinTref ea (Tref ea (i "a") (NoAlias ea)) Natural
>         Inner (SubTref ea (selectFrom
>                         (selEL [Star ea])
>                         (Tref ea (i "b") (NoAlias ea))) (TableAlias ea $ Nmc "a"))
>         Nothing (NoAlias ea))]
>      ,s "select * from a order by c;"
>       [QueryStatement ea $ Select ea  Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] Nothing Nothing]
>      ,s "select *\n\
>            \from Adventure\n\
>            \order by Clicks desc, AdventureID;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "Adventure") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "Clicks",Desc)
>                           ,(Identifier ea "AdventureID",Asc)] Nothing Nothing]
>      ,s "select * from a order by c,d asc;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c", Asc)
>                           ,(Identifier ea "d", Asc)] Nothing Nothing]
>      ,s "select * from a order by c,d desc;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c", Asc)
>                           ,(Identifier ea "d", Desc)] Nothing Nothing]
>      ,s "select * from a order by c limit 1;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] (Just (NumberLit ea "1")) Nothing]
>      ,s "select top 3 * from a order by c;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] (Just (NumberLit ea "3")) Nothing]
>      ,s "select * from a order by c offset 3;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selEL [Star ea]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] Nothing (Just $ NumberLit ea "3")]
>      ,s "select a from (select b from c) as d;"
>         [QueryStatement ea $ selectFrom
>          (selIL ["a"])
>          (SubTref ea (selectFrom
>                    (selIL ["b"])
>                    (Tref ea (i "c") (NoAlias ea)))
>           (TableAlias ea $ Nmc "d"))]
>      ,s "select * from gen();"
>         [QueryStatement ea $ selectFrom (selEL [Star ea]) (FunTref ea (App ea (name "gen") []) (NoAlias ea))]
>      ,s "select * from gen() as t;"
>       [QueryStatement ea $ selectFrom
>        (selEL [Star ea])
>        (FunTref ea (App ea (name "gen") [])(TableAlias ea $ Nmc "t"))]
>      ,s "select count(distinct b) from c;"
>         [QueryStatement ea $ Select ea Dupes
>          (sl [SelExp ea (AggregateApp ea Distinct
>                          (App ea (name "count") [Identifier ea "b"])
>                          [])])
>          [Tref ea (i "c") (NoAlias ea)] Nothing []
>          Nothing [] Nothing Nothing]
>      ,s "select count(all b) from c;"
>         [QueryStatement ea $ Select ea Dupes
>          (sl [SelExp ea (AggregateApp ea Dupes
>                          (App ea (name "count") [Identifier ea "b"])
>                          [])])
>          [Tref ea (i "c") (NoAlias ea)] Nothing []
>          Nothing [] Nothing Nothing]
>      ,s "select string_agg(distinct relname,',' order by relname1) from pg_class;"
>         [QueryStatement ea $ Select ea Dupes
>          (sl [SelExp ea (AggregateApp ea Distinct
>                          (App ea (name "string_agg") [Identifier ea "relname"
>                                                   ,StringLit ea ","])
>                          [(Identifier ea "relname1", Asc)])])
>          [Tref ea (i "pg_class") (NoAlias ea)] Nothing []
>          Nothing [] Nothing Nothing]
>      ,s "select a, count(b) from c group by a;"
>         [QueryStatement ea $ Select ea Dupes
>          (sl [selI "a", SelExp ea (App ea (name "count") [Identifier ea "b"])])
>          [Tref ea (i "c") (NoAlias ea)] Nothing [Identifier ea "a"]
>          Nothing [] Nothing Nothing]
>      ,s "select a, count(b) as cnt from c group by a having cnt > 4;"
>         [QueryStatement ea $ Select ea Dupes
>          (sl [selI "a", SelectItem ea (App ea (name "count") [Identifier ea "b"]) $ Nmc "cnt"])
>          [Tref ea (i "c") (NoAlias ea)] Nothing [Identifier ea "a"]
>          (Just $ App ea (name ">") [Identifier ea "cnt", NumberLit ea "4"])
>          [] Nothing Nothing]
>      ,s "select a from (select 1 as a, 2 as b) x;"
>         [QueryStatement ea $ selectFrom
>          [selI "a"]
>          (SubTref ea (selectE $ SelectList ea
>                                [SelectItem ea (NumberLit ea "1") $ Nmc "a"
>                                ,SelectItem ea (NumberLit ea "2") $ Nmc "b"])
>                   (TableAlias ea $ Nmc "x"))]
>      ]
>      ]
>    ,Group "some misc stuff" [
>       s "select (p).x, (p).y from pos;"
>         [QueryStatement ea $ selectFrom (selEL [eqi "p" "x"
>                                                ,eqi "p" "y"])
>                                          (Tref ea (i "pos") (NoAlias ea))]
>      ,s "select ($1).x, ($1).y from pos;"
>         [QueryStatement ea $ selectFrom
>          (selEL [member (PositionalArg ea 1) (Identifier ea "x")
>                 ,member (PositionalArg ea 1) (Identifier ea "y")])
>          (Tref ea (i "pos") (NoAlias ea))]
>      ,s "select row_number() over(), x from tb;"
>       [QueryStatement ea $ selectFrom
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
