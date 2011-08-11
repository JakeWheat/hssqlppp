
The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written almost in tdd style, which the order/ coverage of these tests
reflects.

There are no tests for invalid syntax at the moment.

> {-# LANGUAGE QuasiQuotes #-}
>
> module Database.HsSqlPpp.Tests.ParserTests (parserTests, parserTestData, Item(..)) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Generics
>
> import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Pretty
>
> data Item = Expr String ScalarExpr
>           | Stmt String [Statement]
>           | MSStmt String [Statement]
>           | PgSqlStmt String [Statement]
>           | Group String [Item]
> parserTests :: Test.Framework.Test
> parserTests = itemToTft parserTestData
>
> parserTestData :: Item
> parserTestData =
>   Group "parserTests" [

--------------------------------------------------------------------------------

>    Group "parse expressions" [
>     Group "numbers" [
>       e "42" (NumberLit ea "42")
>      ,e "3.5" (NumberLit ea "3.5")
>      ,e "4." (NumberLit ea "4.")
>      ,e ".001" (NumberLit ea ".001")
>      ,e "5e2" (NumberLit ea "5e2")
>      ,e "1.925e-3" (NumberLit ea "1.925e-3")

>      ]
>    ,Group "basic expressions" [
>       e "1" (NumberLit ea "1")
>      ,e "-1" (FunCall ea "u-" [NumberLit ea "1"])
>      ,e "1.1" (NumberLit ea "1.1")
>      ,e "-1.1" (FunCall ea "u-" [NumberLit ea "1.1"])
>      ,e " 1 + 1 " (FunCall ea "+" [NumberLit ea "1"
>                                   ,NumberLit ea "1"])
>      ,e "1+1+1" (FunCall ea "+" [FunCall ea "+" [NumberLit ea "1"
>                                                 ,NumberLit ea "1"]
>                                 ,NumberLit ea "1"])
>      ]
>    ,Group "parens" [

check some basic parens use wrt naked values and row constructors
these tests reflect how pg seems to interpret the variants.

>       e "(1)" (NumberLit ea "1")
>      ,e "row ()" (FunCall ea "!rowctor" [])
>      ,e "row (1)" (FunCall ea "!rowctor" [NumberLit ea "1"])
>      ,e "row (1,2)" (FunCall ea "!rowctor" [NumberLit ea "1",NumberLit ea "2"])
>      ,e "(1,2)" (FunCall ea "!rowctor" [NumberLit ea "1",NumberLit ea "2"])
>      ]
>    ,Group "more basic expressions" [

test some more really basic expressions

>       e "'test'" (stringQ "test")
>      ,e "''" (stringQ "")
>      ,e "hello" (Identifier ea "hello")
>      ,e "helloTest" (Identifier ea "helloTest")
>      ,e "hello_test" (Identifier ea "hello_test")
>      ,e "\"this is an identifier\"" (Identifier ea "this is an identifier")
>      ,e "hello1234" (Identifier ea "hello1234")
>      ,e "true" (BooleanLit ea True)
>      ,e "false" (BooleanLit ea False)
>      ,e "null" (NullLit ea)
>      ]
>    ,Group "array ctor and selector" [
>       e "array[1,2]" (FunCall ea "!arrayctor" [NumberLit ea "1", NumberLit ea "2"])
>      ,e "a[1]" (FunCall ea "!arraysub" [Identifier ea "a", NumberLit ea "1"])
>      ]
>    ,Group "simple operators" [
>       e "1 + tst1" (FunCall ea "+" [NumberLit ea "1"
>                                ,Identifier ea "tst1"])
>      ,e "tst1 + 1" (FunCall ea "+" [Identifier ea "tst1"
>                                ,NumberLit ea "1"])
>      ,e "tst + tst1" (FunCall ea "+" [Identifier ea "tst"
>                                  ,Identifier ea "tst1"])
>      ,e "'a' || 'b'" (FunCall ea "||" [stringQ "a"
>                                   ,stringQ "b"])
>      ,e "'stuff'::text" (Cast ea (stringQ "stuff") (SimpleTypeName ea "text"))
>      ,e "245::float(24)" (Cast ea (NumberLit ea "245") (PrecTypeName ea "float" 24))
>      ,e "245.1::numeric(5,3)" (Cast ea (NumberLit ea "245.1") (Prec2TypeName ea "numeric" 5 3))
>      ,e "245::double precision" (Cast ea (NumberLit ea "245") (SimpleTypeName ea "double precision"))
>      ,e "date '1998-12-01'" (TypedStringLit ea (SimpleTypeName ea "date") "1998-12-01")
>      ,e "interval '63' day" (Interval ea "63" IntervalDay Nothing)
>      ,e "interval '63' day (3)" (Interval ea "63" IntervalDay $ Just 3)
>      ,e "extract(year from a)" (Extract ea ExtractYear $ Identifier ea "a")
>      ,e "a between 1 and 3"
>         (FunCall ea "!between" [Identifier ea "a", NumberLit ea "1", NumberLit ea "3"])
>      ,e "a between 7 - 1 and 7 + 1"
>         (FunCall ea "!between" [Identifier ea "a"
>                                ,FunCall ea "-" [NumberLit ea "7"
>                                                ,NumberLit ea "1"]
>                                ,FunCall ea "+" [NumberLit ea "7"
>                                                ,NumberLit ea "1"]])
>      ,e "cast(a as text)"
>         (Cast ea (Identifier ea "a") (SimpleTypeName ea "text"))
>      ,e "@ a"
>         (FunCall ea "@" [Identifier ea "a"])
>      ,e "substring(a from 0 for 3)"
>         (FunCall ea "!substring" [Identifier ea "a", NumberLit ea "0", NumberLit ea "3"])
>      ,e "substring(a from 0 for (5 - 3))"
>         (FunCall ea "!substring" [Identifier ea "a",NumberLit ea "0",
>          FunCall ea "-" [NumberLit ea "5",NumberLit ea "3"]])
>      ,e "a like b"
>         (FunCall ea "!like" [Identifier ea "a", Identifier ea "b"])
>      ,e "a not like b"
>         (FunCall ea "!notlike" [Identifier ea "a", Identifier ea "b"])
>      , e "a and b and c and d"
>         (FunCall ea "!and"
>          [FunCall ea "!and"
>           [FunCall ea "!and" [Identifier ea "a"
>                              ,Identifier ea "b"]
>           ,Identifier ea "c"]
>          ,Identifier ea "d"])
>      ]
>    ,Group "function calls" [
>       e "fn()" (FunCall ea "fn" [])
>      ,e "fn(1)" (FunCall ea "fn" [NumberLit ea "1"])
>      ,e "fn('test')" (FunCall ea "fn" [stringQ "test"])
>      ,e "fn(1,'test')" (FunCall ea "fn" [NumberLit ea "1", stringQ "test"])
>      ,e "fn('test')" (FunCall ea "fn" [stringQ "test"])
>      ]
>    ,Group "simple whitespace sanity checks" [
>       e "fn (1)" (FunCall ea "fn" [NumberLit ea "1"])
>      ,e "fn( 1)" (FunCall ea "fn" [NumberLit ea "1"])
>      ,e "fn(1 )" (FunCall ea "fn" [NumberLit ea "1"])
>      ,e "fn(1) " (FunCall ea "fn" [NumberLit ea "1"])
>      ]
>    ,Group "null stuff" [
>       e "not null" (FunCall ea "!not" [NullLit ea])
>      ,e "a is null" (FunCall ea "!isnull" [Identifier ea "a"])
>      ,e "a is not null" (FunCall ea "!isnotnull" [Identifier ea "a"])
>      ,e "not not true" (FunCall ea "!not"
>                          [FunCall ea "!not"
>                           [BooleanLit ea True]])
>      ]

>    ,Group "case expressions" [
>       e {-"case when a,b then 3\n\
>         \     when c then 4\n\
>         \     else 5\n\
>         \end" -}
>         [here|
>          case when a,b then 3
>               when c then 4
>               else 5
>          end
>          |]
>         (Case ea [([Identifier ea "a", Identifier ea "b"], NumberLit ea "3")
>               ,([Identifier ea "c"], NumberLit ea "4")]
>          (Just $ NumberLit ea "5"))
>      ,e  "case 1 when 2 then 3 else 4 end"
>         (CaseSimple ea (NumberLit ea "1")
>            [([NumberLit ea "2"], NumberLit ea "3")]
>          (Just $ NumberLit ea "4"))
>      ]
>    ,Group "positional args" [
>       e "$1" (PositionalArg ea 1)
>      ,e "?" (Placeholder ea)
>      ,e "a = ?" (FunCall ea "=" [Identifier ea "a",Placeholder ea])
>      ]
>    ,Group "exists" [
>       e "exists (select 1 from a)"
>       (Exists ea (selectFrom [SelExp ea (NumberLit ea "1")] (Tref ea (i "a") (NoAlias ea))))
>      ]
>    ,Group "in variants" [
>       e "t in (1,2)"
>       (InPredicate ea (Identifier ea "t") True (InList ea [NumberLit ea "1",NumberLit ea "2"]))
>      ,e "t not in (1,2)"
>       (InPredicate ea (Identifier ea "t") False (InList ea [NumberLit ea "1",NumberLit ea "2"]))
>      ,e "(t,u) in (1,2)"
>       (InPredicate ea (FunCall ea "!rowctor" [Identifier ea "t",Identifier ea "u"]) True
>        (InList ea [NumberLit ea "1",NumberLit ea "2"]))
>      ,e "3 = any (array[1,2])"
>       (LiftOperator ea "=" LiftAny [NumberLit ea "3"
>                                     ,FunCall ea "!arrayctor" [NumberLit ea "1"
>                                                              ,NumberLit ea "2"]])
>      ,e "3 = all (array[1,2,4])"
>       (LiftOperator ea "=" LiftAll [NumberLit ea "3"
>                                     ,FunCall ea "!arrayctor" [NumberLit ea "1"
>                                                              ,NumberLit ea "2"
>                                                              ,NumberLit ea "4"]])
>      ]
>    ,Group "comparison operators" [
>       e "a < b"
>       (FunCall ea "<" [Identifier ea "a", Identifier ea "b"])
>      ,e "a <> b"
>       (FunCall ea "<>" [Identifier ea "a", Identifier ea "b"])
>      ,e "a != b"
>       (FunCall ea "<>" [Identifier ea "a", Identifier ea "b"])
>      ]

test some string parsing, want to check single quote behaviour,
and dollar quoting, including nesting.

>    ,Group "string parsing" [
>       e "''" (stringQ "")
>      ,e "''''" (stringQ "'")
>      ,e "'test'''" (stringQ "test'")
>      ,e "'''test'" (stringQ "'test")
>      ,e "'te''st'" (stringQ "te'st")
>      ,e "$$test$$" (StringLit ea "test")
>      ,e "$$te'st$$" (StringLit ea "te'st")
>      ,e "$st$test$st$" (StringLit ea "test")
>      ,e "$outer$te$$yup$$st$outer$" (StringLit ea "te$$yup$$st")
>      ,e "'spl$$it'" (stringQ "spl$$it")
>      ]
>    ,Group "bracketed things" [
>       e "(p).x" (eqi "p" "x")
>      ,e "(select f(((a).x, y)::z))"
>         (ScalarSubQuery ea
>          (selectE (sl
>                    [SelExp ea
>                     (FunCall ea "f" [Cast ea
>                                      (FunCall ea "!rowctor"
>                                       [eqi "a" "x"
>                                       ,Identifier ea "y"])
>                                      (SimpleTypeName ea "z")])])))
>      ]
>      ]

--------------------------------------------------------------------------------

select statements

>   ,Group "simple select statements" [
>     Group "select no table" [
>       s "select 1;" [QueryStatement ea $ selectE (SelectList ea [SelExp ea (NumberLit ea "1")])]
>      ]
>    ,Group "select from table" [
>       s "select * from tbl;"
>       [QueryStatement ea $ selectFrom (selIL ["*"]) (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select a,b from tbl;"
>       [QueryStatement ea $ selectFrom (selIL ["a", "b"]) (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select a,b from inf.tbl;"
>       [QueryStatement ea $ selectFrom (selIL ["a", "b"]) (Tref ea (qi "inf" "tbl") (NoAlias ea))]
>      ,s "select distinct * from tbl;"
>       [QueryStatement ea $ Select ea Distinct (SelectList ea (selIL ["*"])) [Tref ea (i "tbl") (NoAlias ea)]
>        Nothing [] Nothing [] Nothing Nothing]
>      ,s "select a from tbl where b=2;"
>       [QueryStatement ea $ selectFromWhere
>         (selIL ["a"])
>         (Tref ea (i "tbl") (NoAlias ea))
>         (FunCall ea "="
>          [Identifier ea "b", NumberLit ea "2"])]
>      ,s "select a from tbl where b=2 and c=3;"
>       [QueryStatement ea $ selectFromWhere
>         (selIL ["a"])
>         (Tref ea (i "tbl") (NoAlias ea))
>         (FunCall ea "!and"
>          [FunCall ea "="  [Identifier ea "b", NumberLit ea "2"]
>          ,FunCall ea "=" [Identifier ea "c", NumberLit ea "3"]])]
>      ,MSStmt "select a from t;\ngo"
>          [QueryStatement ea $ selectFrom (selIL ["a"]) (Tref ea (i "t") (NoAlias ea))]
>      ,MSStmt "select a from t;\nset rowcount -1\ngo"
>          [QueryStatement ea $ selectFrom (selIL ["a"]) (Tref ea (i "t") (NoAlias ea))]
>      ,MSStmt "select a from t;\nset rowcount 10\ngo"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selIL ["a"]))
>        [Tref ea (i "t") (NoAlias ea)]
>        Nothing [] Nothing [] (Just (NumberLit ea "10")) Nothing]

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
>            [WithQuery ea "a" Nothing (selectE $ SelectList ea
>                                [SelectItem ea (NumberLit ea "1") "a1"])
>            ,WithQuery ea "b" Nothing (selectFrom (selIL ["*"]) (Tref ea (i "a") (NoAlias ea)))]
>            (selectFrom (selIL ["*"]) (Tref ea (i "b") (NoAlias ea))))]
>      ,s [here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from a
>               union select * from b; |]
>          [QueryStatement ea
>           (WithQueryExpr ea
>            [WithQuery ea "a" Nothing (selectE $ SelectList ea
>                                [SelectItem ea (NumberLit ea "1") "a1"])
>            ,WithQuery ea "b" Nothing (selectFrom (selIL ["*"]) (Tref ea (i "a") (NoAlias ea)))]
>            (CombineQueryExpr ea Union
>              (selectFrom (selIL ["*"]) (Tref ea (i "a") (NoAlias ea)))
>              (selectFrom (selIL ["*"]) (Tref ea (i "b") (NoAlias ea)))))]
>      ,s "select a as b from tbl;"
>       [QueryStatement ea $ selectFrom [SelectItem ea (Identifier ea "a") "b"] (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select a + b as b from tbl;"
>       [QueryStatement ea $ selectFrom
>        [SelectItem ea
>         (FunCall ea "+"
>          [Identifier ea "a", Identifier ea "b"]) "b"]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select a.* from tbl a;"
>       [QueryStatement ea $ selectFrom (selEL [eqi "a" "*"]) (Tref ea (i "tbl") (TableAlias ea "a"))]
>      ,s "select a.* from tbl a(b,c);"
>       [QueryStatement ea $ selectFrom (selEL [eqi "a" "*"]) (Tref ea (i "tbl") (FullAlias ea "a" ["b","c"]))]

>      ,s "select * from t1 a, t2 b;"
>             [QueryStatement ea
>              (Select ea Dupes
>               (SelectList ea
>                [SelExp ea (Identifier ea "*")])
>               [Tref ea (i "t1") (TableAlias ea "a"),Tref ea (i "t2") (TableAlias ea "b")]
>               Nothing [] Nothing [] Nothing Nothing)]
>      ,s "select a from b inner join c on b.a=c.a;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (NoAlias ea))
>           (Just (JoinOn ea
>            (FunCall ea "=" [eqi "b" "a", eqi "c" "a"]))) (NoAlias ea))]
>      ,s "select a from b inner join c as d on b.a=d.a;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (TableAlias ea "d"))
>           (Just (JoinOn ea
>            (FunCall ea "=" [eqi "b" "a", eqi "d" "a"]))) (NoAlias ea))]
>      ,s "select a from b inner join c using(d,e);"
>       [QueryStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") (NoAlias ea)) Unnatural Inner (Tref ea (i "c") (NoAlias ea))
>           (Just (JoinUsing ea ["d","e"])) (NoAlias ea))]
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
>         (Just $ JoinOn ea (FunCall ea "="
>                [NumberLit ea "1", NumberLit ea "1"])) (NoAlias ea))]

>      ,s "select row_number() over(order by a) as place from tbl;"
>       [QueryStatement ea $ selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [Identifier ea "a"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select row_number() over(order by a asc) as place from tbl;"
>       [QueryStatement ea $ selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [Identifier ea "a"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select row_number() over(order by a desc) as place from tbl;"
>       [QueryStatement ea $ selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [Identifier ea "a"] Desc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select row_number()\n\
>         \over(partition by (a,b) order by c) as place\n\
>         \from tbl;"
>       [QueryStatement ea $ selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     [FunCall ea "!rowctor" [Identifier ea "a",Identifier ea "b"]]
>                     [Identifier ea "c"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") (NoAlias ea))]
>      ,s "select * from a natural inner join (select * from b) as a;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["*"])
>        (JoinTref ea (Tref ea (i "a") (NoAlias ea)) Natural
>         Inner (SubTref ea (selectFrom
>                         (selIL ["*"])
>                         (Tref ea (i "b") (NoAlias ea))) (TableAlias ea "a"))
>         Nothing (NoAlias ea))]
>      ,s "select * from a order by c;"
>       [QueryStatement ea $ Select ea  Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] Nothing Nothing]
>      ,s "select *\n\
>            \from Adventure\n\
>            \order by Clicks desc, AdventureID;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "Adventure") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "Clicks",Desc)
>                           ,(Identifier ea "AdventureID",Asc)] Nothing Nothing]
>      ,s "select * from a order by c,d asc;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c", Asc)
>                           ,(Identifier ea "d", Asc)] Nothing Nothing]
>      ,s "select * from a order by c,d desc;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c", Asc)
>                           ,(Identifier ea "d", Desc)] Nothing Nothing]
>      ,s "select * from a order by c limit 1;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] (Just (NumberLit ea "1")) Nothing]
>      ,s "select * from a order by c offset 3;"
>       [QueryStatement ea $ Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") (NoAlias ea)]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] Nothing (Just $ NumberLit ea "3")]
>      ,s "select a from (select b from c) as d;"
>         [QueryStatement ea $ selectFrom
>          (selIL ["a"])
>          (SubTref ea (selectFrom
>                    (selIL ["b"])
>                    (Tref ea (i "c") (NoAlias ea)))
>           (TableAlias ea "d"))]
>      ,s "select * from gen();"
>         [QueryStatement ea $ selectFrom (selIL ["*"]) (FunTref ea (FunCall ea "gen" []) (NoAlias ea))]
>      ,s "select * from gen() as t;"
>       [QueryStatement ea $ selectFrom
>        (selIL ["*"])
>        (FunTref ea (FunCall ea "gen" [])(TableAlias ea  "t"))]
>      ,s "select a, count(b) from c group by a;"
>         [QueryStatement ea $ Select ea Dupes
>          (sl [selI "a", SelExp ea (FunCall ea "count" [Identifier ea "b"])])
>          [Tref ea (i "c") (NoAlias ea)] Nothing [Identifier ea "a"]
>          Nothing [] Nothing Nothing]
>      ,s "select a, count(b) as cnt from c group by a having cnt > 4;"
>         [QueryStatement ea $ Select ea Dupes
>          (sl [selI "a", SelectItem ea (FunCall ea "count" [Identifier ea "b"]) "cnt"])
>          [Tref ea (i "c") (NoAlias ea)] Nothing [Identifier ea "a"]
>          (Just $ FunCall ea ">" [Identifier ea "cnt", NumberLit ea "4"])
>          [] Nothing Nothing]
>      ,s "select a from (select 1 as a, 2 as b) x;"
>         [QueryStatement ea $ selectFrom
>          [selI "a"]
>          (SubTref ea (selectE $ SelectList ea
>                                [SelectItem ea (NumberLit ea "1") "a"
>                                ,SelectItem ea (NumberLit ea "2") "b"])
>                   (TableAlias ea "x"))]
>      ]

>    ,Group "multiple statements" [
>       s "select 1;\nselect 2;" [QueryStatement ea $ selectE $ sl [SelExp ea (NumberLit ea "1")]
>                                ,QueryStatement ea $ selectE $ sl [SelExp ea (NumberLit ea "2")]]
>      ]
>    ,Group "comments" [
>       s "" []
>      ,s "-- this is a test" []
>      ,s "/* this is\n\
>         \a test*/" []
>      ,s "select 1;\n\
>         \-- this is a test\n\
>         \select -- this is a test\n\
>         \2;" [QueryStatement ea $ selectE $ sl [SelExp ea (NumberLit ea "1")]
>              ,QueryStatement ea $ selectE $ sl [SelExp ea (NumberLit ea "2")]
>              ]
>      ,s "select 1;\n\
>         \/* this is\n\
>         \a test*/\n\
>         \select /* this is a test*/2;"
>                     [QueryStatement ea $ selectE $ sl [SelExp ea (NumberLit ea "1")]
>                     ,QueryStatement ea $ selectE $ sl [SelExp ea (NumberLit ea "2")]
>                     ]
>      ]
>    ,Group "some mis stuff" [
>       s "select (p).x, (p).y from pos;"
>         [QueryStatement ea $ selectFrom (selEL [eqi "p" "x"
>                                                ,eqi "p" "y"])
>                                          (Tref ea (i "pos") (NoAlias ea))]
>      ,s "select ($1).x, ($1).y from pos;"
>         [QueryStatement ea $ selectFrom (selEL [QIdentifier ea (PositionalArg ea 1) "x"
>                                                ,QIdentifier ea (PositionalArg ea 1) "y"])
>                                          (Tref ea (i "pos") (NoAlias ea))]
>      ,s "select row_number() over(), x from tb;"
>       [QueryStatement ea $ selectFrom
>        [SelExp ea
>                     (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [] Asc FrameUnboundedPreceding)
>        , selI "x"]
>        (Tref ea (i "tb") (NoAlias ea))]
>      ]
>      ]

TODO:

from in update, using in delete (+ type check these)

-------------------------------------------------------------------------------

dml statements

>    ,Group "dml" [
>      Group "insert" [
>       s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2);\n"
>        [Insert ea
>         (dqi "testtable")
>         ["columna", "columnb"]
>         (Values ea [[NumberLit ea "1", NumberLit ea "2"]])
>         Nothing]

multi row insert, test the stand alone values statement first, maybe
that should be in the select section?

>      ,s "values (1,2), (3,4);"
>      [QueryStatement ea $ Values ea [[NumberLit ea "1", NumberLit ea "2"]
>              ,[NumberLit ea "3", NumberLit ea "4"]]]
>
>      ,s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2), (3,4);\n"
>       [Insert ea
>         (dqi "testtable")
>         ["columna", "columnb"]
>         (Values ea [[NumberLit ea "1", NumberLit ea "2"]
>                 ,[NumberLit ea "3", NumberLit ea "4"]])
>         Nothing]

insert from select

>      ,s "insert into a\n\
>          \    select b from c;"
>       [Insert ea (dqi "a") []
>        (selectFrom [selI "b"] (Tref ea (i "c") (NoAlias ea)))
>        Nothing]
>
>      ,s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2) returning id;\n"
>       [Insert ea
>         (dqi "testtable")
>         ["columna", "columnb"]
>         (Values ea [[NumberLit ea "1", NumberLit ea "2"]])
>         (Just $ sl [selI "id"])]
>      ]
>
>     ,Group "update" [
>       s "update tb\n\
>         \  set x = 1, y = 2;"
>       [Update ea (dqi "tb") [FunCall ea "=" [Identifier ea "x", NumberLit ea "1"]
>                       ,FunCall ea "=" [Identifier ea "y", NumberLit ea "2"]]
>        [] Nothing Nothing]
>      ,s "update tb\n\
>         \  set x = 1, y = 2 where z = true;"
>       [Update ea (dqi "tb") [FunCall ea "=" [Identifier ea "x", NumberLit ea "1"]
>                       ,FunCall ea "=" [Identifier ea "y", NumberLit ea "2"]]
>        []
>        (Just $ FunCall ea "="
>         [Identifier ea "z", BooleanLit ea True])
>        Nothing]
>      ,s "update tb\n\
>         \  set x = 1, y = 2 returning id;"
>       [Update ea (dqi "tb") [FunCall ea "=" [Identifier ea "x", NumberLit ea "1"]
>                       ,FunCall ea "=" [Identifier ea "y", NumberLit ea "2"]]
>        [] Nothing (Just $ sl [selI "id"])]
>      ,s "update tb\n\
>         \  set (x,y) = (1,2);"
>       [Update ea (dqi "tb") [FunCall ea "="
>                        [FunCall ea "!rowctor" [Identifier ea "x"
>                                               ,Identifier ea "y"]
>                        ,FunCall ea "!rowctor" [NumberLit ea "1"
>                                               ,NumberLit ea "2"]]]
>        []
>        Nothing Nothing]
>      ]

FunCall ea "=" [FunCall ea "!rowctor" [Identifier ea "x",Identifier ea "y"],FunCall ea "!rowctor" [NumberLit ea "1",NumberLit ea "2"]])


>
>     ,Group "delete" [
>       s "delete from tbl1 where x = true;"
>       [Delete ea (dqi "tbl1") [] (Just $ FunCall ea "="
>                                [Identifier ea "x", BooleanLit ea True])
>        Nothing]
>      ,s "delete from tbl1 where x = true returning id;"
>       [Delete ea (dqi "tbl1") [] (Just $ FunCall ea "="
>                                [Identifier ea "x", BooleanLit ea True])
>        (Just $ sl [selI "id"])]
>      ]
>
>     ,Group "truncate" [
>       s "truncate test;"
>        [Truncate ea ["test"] ContinueIdentity Restrict]
>
>      ,s "truncate table test, test2 restart identity cascade;"
>        [Truncate ea ["test","test2"] RestartIdentity Cascade]
>      ]

copy, bit crap at the moment

>     ,Group "copy" [
>       s "copy tbl(a,b) from stdin;\n\
>         \bat\tt\n\
>         \bear\tf\n\
>         \\\.\n"
>       [Copy ea "tbl" ["a", "b"] Stdin
>        ,CopyData ea "\
>         \bat\tt\n\
>         \bear\tf\n"]
>      ]

--------------------------------------------------------------------------------

ddl statements

>    ,Group "ddl" [
>      Group "simple tables" [
>       s "create table test (\n\
>         \  fielda text,\n\
>         \  fieldb int\n\
>         \);"
>       [CreateTable ea
>        "test"
>        [att "fielda" "text"
>        ,att "fieldb" "int"
>        ]
>        []]
>      ,s "create table tbl (\n\
>         \  fld boolean default false);"
>       [CreateTable ea "tbl" [AttributeDef ea "fld" (SimpleTypeName ea "boolean")
>                           (Just $ BooleanLit ea False) []][]]
>
>      ,s "create table tbl as select 1;"
>       [CreateTableAs ea "tbl"
>        (selectE (SelectList ea [SelExp ea (NumberLit ea "1")]))]
>
>      ,s "alter table a alter column b set default 1;"
>       [AlterTable ea "a" [AlterColumnDefault ea "b" (NumberLit ea "1")]]
>
>      ,s "alter table a add constraint unique(b);"
>       [AlterTable ea "a" [AddConstraint ea (UniqueConstraint ea "" ["b"])]]
>      ]]

>
>     ,Group "others" [
>       s "create view v1 as\n\
>         \select a,b from t;"
>       [CreateView ea
>        "v1" Nothing
>        (selectFrom [selI "a", selI "b"] (Tref ea (i "t") (NoAlias ea)))]
>      ,s "create view v1(c,d) as\n\
>         \select a,b from t;"
>       [CreateView ea
>        "v1" (Just ["c","d"])
>        (selectFrom [selI "a", selI "b"] (Tref ea (i "t") (NoAlias ea)))]
>      ,s "create domain td as text check (value in ('t1', 't2'));"
>       [CreateDomain ea "td" (SimpleTypeName ea "text") ""
>        (Just (InPredicate ea (Identifier ea "value") True
>               (InList ea [stringQ "t1" ,stringQ "t2"])))]
>      ,s "create type tp1 as (\n\
>         \  f1 text,\n\
>         \  f2 text\n\
>         \);"
>       [CreateType ea "tp1" [TypeAttDef ea "f1" (SimpleTypeName ea "text")
>                         ,TypeAttDef ea "f2" (SimpleTypeName ea "text")]]
>
>      ,s "create sequence s start with 5 increment by 4 no maxvalue no minvalue cache 1;"
>         [CreateSequence ea "s" 4 1 ((2::Integer) ^ (63::Integer) - 1) 5 1]
>
>      ,s "alter sequence s owned by a.b;"
>         [AlterSequence ea "s" $ qi "a" "b"]
>
>      ,s "create trigger tr\n\
>          \after insert or delete on tb\n\
>          \for each statement\n\
>          \execute procedure fb();"
>         [CreateTrigger ea "tr" TriggerAfter [TInsert,TDelete] "tb" EachStatement "fb" []]
>      ]
>
>     ,Group "drops" [
>       s "drop domain t;"
>       [DropSomething ea Domain Require ["t"] Restrict]
>      ,s "drop domain if exists t,u cascade;"
>       [DropSomething ea Domain IfExists ["t", "u"] Cascade]
>      ,s "drop domain t restrict;"
>       [DropSomething ea Domain Require ["t"] Restrict]
>
>      ,s "drop type t;"
>       [DropSomething ea Type Require ["t"] Restrict]
>      ,s "drop table t;"
>       [DropSomething ea Table Require ["t"] Restrict]
>      ,s "drop view t;"
>       [DropSomething ea View Require ["t"] Restrict]
>      ]
>
>     ,Group "constraints" [
>       Group "nulls" [
>       s "create table t1 (\n\
>         \ a text null\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "a" (SimpleTypeName ea "text")
>                            Nothing [NullConstraint ea ""]]
>          []]
>      ,s "create table t1 (\n\
>         \ a text not null\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "a" (SimpleTypeName ea "text")
>                            Nothing [NotNullConstraint ea ""]]
>          []]
>      ]
>
>      ,Group "unique" [
>       s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ unique (x,y)\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [UniqueConstraint ea "" ["x","y"]]]

test arbitrary ordering

>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ unique (x),\n\
>         \ y int\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [UniqueConstraint ea "" ["x"]]]

unique row

>      ,s "create table t1 (\n\
>         \ x int unique\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowUniqueConstraint ea ""]][]]
>
>      ,s "create table t1 (\n\
>         \ x int unique not null\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowUniqueConstraint ea ""
>                            ,NotNullConstraint ea ""]][]]

quick sanity check

>      ,s "create table t1 (\n\
>         \ x int not null unique\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [NotNullConstraint ea ""
>                            ,RowUniqueConstraint ea ""]][]]
>      ]
>
>      ,Group "primary key" [
>       s "create table t1 (\n\
>         \ x int primary key\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowPrimaryKeyConstraint ea ""]][]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ primary key (x,y)\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [PrimaryKeyConstraint ea "" ["x", "y"]]]
>      ]
>
>      ,Group "check" [
>       s "create table t (\n\
>         \f text check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable ea "t"
>          [AttributeDef ea "f" (SimpleTypeName ea "text") Nothing
>           [RowCheckConstraint ea "" (InPredicate ea
>                                   (Identifier ea "f") True
>                                   (InList ea [stringQ "a", stringQ "b"]))]] []]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ check (x>y)\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [CheckConstraint ea "" (FunCall ea ">" [Identifier ea "x", Identifier ea "y"])]]
>      ]
>
>      ,Group "misc" [
>       s "create table t (\n\
>         \f text not null unique check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable ea "t"
>          [AttributeDef ea "f" (SimpleTypeName ea "text") Nothing
>           [NotNullConstraint ea ""
>            ,RowUniqueConstraint ea ""
>            ,RowCheckConstraint ea "" (InPredicate ea
>                                    (Identifier ea "f") True
>                                    (InList ea [stringQ "a"
>                                            ,stringQ "b"]))]] []]
>      ]

>      ,Group "references" [
>       s "create table t1 (\n\
>         \ x int references t2\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowReferenceConstraint ea "" "t2" Nothing
>                             Restrict Restrict]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2(y)\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowReferenceConstraint ea "" "t2" (Just "y")
>                             Restrict Restrict]][]]
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint ea "" ["x", "y"] "t2" []
>           Restrict Restrict]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2(z,w)\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint ea "" ["x", "y"] "t2" ["z", "w"]
>           Restrict Restrict]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on delete cascade\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowReferenceConstraint ea "" "t2" Nothing
>                             Cascade Restrict]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on update cascade\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowReferenceConstraint ea "" "t2" Nothing
>                             Restrict Cascade]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on delete cascade on update cascade\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowReferenceConstraint ea "" "t2" Nothing
>                             Cascade Cascade]][]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2 on update cascade on delete cascade\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint ea "" ["x", "y"] "t2" []
>           Cascade Cascade]]
>
>      ]
>      ]]

>    ,Group "functions" [
>      Group "basics" [
>       s "create function t1(text) returns text as $$\n\
>         \select a from t1 where b = $1;\n\
>         \$$ language sql stable;"
>       [CreateFunction ea "t1" [ParamDefTp ea $ SimpleTypeName ea "text"]
>        (SimpleTypeName ea "text") NoReplace Sql
>        (SqlFnBody ea
>         [QueryStatement ea $ selectFromWhere [SelExp ea (Identifier ea "a")] (Tref ea (i "t1") (NoAlias ea))
>          (FunCall ea "="
>           [Identifier ea "b", PositionalArg ea 1])])
>        Stable]
>      ,s "create function fn() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>       [CreateFunction ea "fn" [] (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [VarDef ea "a" (SimpleTypeName ea "int") Nothing
>                                            ,VarDef ea "b" (SimpleTypeName ea "text") Nothing]
>                           [NullStatement ea]))
>        Volatile]
>      ,s "create function fn() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>       [CreateFunction ea "fn" [] (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing [VarDef ea "a" (SimpleTypeName ea "int") Nothing
>                           ,VarDef ea "b" (SimpleTypeName ea "text") Nothing]
>         [NullStatement ea]))
>        Volatile]
>      ,s "create function fn(a text[]) returns int[] as $$\n\
>         \declare\n\
>         \  b xtype[] := '{}';\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql immutable;"
>       [CreateFunction ea "fn"
>        [ParamDef ea "a" $ ArrayTypeName ea $ SimpleTypeName ea "text"]
>        (ArrayTypeName ea $ SimpleTypeName ea "int") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [VarDef ea "b" (ArrayTypeName ea $ SimpleTypeName ea "xtype") (Just $ stringQ "{}")]
>          [NullStatement ea]))
>        Immutable]
>      ,s "create function fn() returns void as '\n\
>         \declare\n\
>         \  a int := 3;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction ea "fn" [] (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [VarDef ea "a" (SimpleTypeName ea "int") (Just $ NumberLit ea "3")]
>          [NullStatement ea]))
>        Stable]
>      ,s "create function fn(int) returns void as '\n\
>         \declare\n\
>         \  a alias for $1;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction ea "fn"
>        [ParamDefTp ea $ SimpleTypeName ea "int"]
>        (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [ParamAlias ea "a" 1]
>          [NullStatement ea]))
>        Stable]
>      ,s "create function fn(b int) returns void as '\n\
>         \declare\n\
>         \  a alias for b;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction ea "fn"
>        [ParamDef ea "b" $ SimpleTypeName ea "int"]
>        (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [VarAlias ea "a" "b"]
>          [NullStatement ea]))
>        Stable]
>      ,s "create function fn() returns setof int as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea "fn" []
>        (SetOfTypeName ea $ SimpleTypeName ea "int") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "create function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea "fn" []
>        (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "create or replace function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea "fn" []
>        (SimpleTypeName ea "void") Replace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "drop function test(text);"
>       [DropFunction ea Require [("test",[SimpleTypeName ea "text"])] Restrict]
>      ,s "drop function test(int,int);"
>       [DropFunction ea Require [("test",[SimpleTypeName ea "int"
>                                         ,SimpleTypeName ea "int"])] Restrict]
>      ,s "drop function if exists a(),test(text) cascade;"
>       [DropFunction ea IfExists [("a",[])
>                           ,("test",[SimpleTypeName ea "text"])] Cascade]
>     ]

>     ,Group "simple plpgsql statements" [
>       f "success := true;"
>       [Assignment ea (ei "success") (BooleanLit ea True)]
>      ,f "success = true;"
>       [Assignment ea (ei "success") (BooleanLit ea True)]
>      ,f "return true;"
>       [Return ea $ Just (BooleanLit ea True)]
>      ,f "return;"
>       [Return ea Nothing]
>      ,f "return next 1;"
>       [ReturnNext ea $ NumberLit ea "1"]
>      ,f "return query select a from b;"
>       [ReturnQuery ea $ selectFrom [selI "a"] (Tref ea (i "b") (NoAlias ea))]
>      ,f "raise notice 'stuff %', 1;"
>       [Raise ea RNotice "stuff %" [NumberLit ea "1"]]
>      ,f "perform test();"
>       [Perform ea $ FunCall ea "test" []]
>      ,f "perform test(a,b);"
>       [Perform ea $ FunCall ea "test" [Identifier ea "a", Identifier ea "b"]]
>      ,f "perform test(r.relvar_name || '_and_stuff');"
>       [Perform ea $ FunCall ea "test" [
>                     FunCall ea "||" [eqi "r" "relvar_name"
>                                     ,stringQ "_and_stuff"]]]
>      {-,f "select into a,b c,d from e;"
>       [Into ea False [ei "a", ei "b"]
>        $ QueryStatement ea $ Select ea Dupes (SelectList ea [selI "c", selI "d"])
>              [Tref ea (i "e") (NoAlias ea)] Nothing [] Nothing [] Nothing Nothing]-}
>      {-,f "select c,d into a,b from e;"
>       [Into ea False [ei "a", ei "b"]
>        $ QueryStatement ea $ Select ea Dupes (SelectList ea [selI "c", selI "d"])
>        [Tref ea (i "e") (NoAlias ea)] Nothing [] Nothing [] Nothing Nothing]-}
>      ,f "update pieces\n\
>         \set a=b returning tag into r.tag;"
>       [Into ea False [eqi "r" "tag"]
>          $ Update ea (dqi "pieces") [FunCall ea "=" [Identifier ea "a"
>                                                     ,Identifier ea "b"]]
>            []
>            Nothing (Just (SelectList ea
>                           [SelExp ea (Identifier ea "tag")]))]
>      ,f "insert into t(a) values (1) returning id into x;"
>       [Into ea False [ei "x"]
>        $ Insert ea
>         (dqi "t")
>         ["a"]
>         (Values ea [[NumberLit ea "1"]])
>         (Just $ sl [selI "id"])]

>      ,f "update t\n\
>         \  set x = 1 returning id into z;"
>       [Into ea False [ei "z"]
>       $ Update ea (dqi "t") [FunCall ea "=" [Identifier ea "x", NumberLit ea "1"]]
>         [] Nothing (Just $ sl [selI "id"])]

>      ,f "execute s;"
>       [Execute ea (Identifier ea "s")]
>      ,f "execute s into r;"
>       [Into ea False [ei "r"] (Execute ea (Identifier ea "s"))]
>     ,f "continue;" [ContinueStatement ea Nothing]
>     ]
>
>     ,Group "other plpgsql statements" [
>       f "for r in select a from tbl loop\n\
>         \null;\n\
>         \end loop;"
>       [ForQueryStatement ea Nothing (ei "r") (selectFrom  [selI "a"] (Tref ea (i "tbl") (NoAlias ea)))
>        [NullStatement ea]]
>      ,f "for r in select a from tbl where true loop\n\
>         \null;\n\
>         \end loop;"
>       [ForQueryStatement ea Nothing (ei "r")
>        (selectFromWhere [selI "a"] (Tref ea (i "tbl") (NoAlias ea)) (BooleanLit ea True))
>        [NullStatement ea]]
>      ,f "for r in 1 .. 10 loop\n\
>         \null;\n\
>         \end loop;"
>       [ForIntegerStatement ea Nothing (ei "r")
>        (NumberLit ea "1") (NumberLit ea "10")
>        [NullStatement ea]]
>
>      ,f "if a=b then\n\
>         \  update c set d = e;\n\
>         \end if;"
>       [If ea [(FunCall ea "=" [ei "a", ei "b"]
>               ,[Update ea (dqi "c") [FunCall ea "=" [ei "d"
>                                                     ,ei "e"]] [] Nothing Nothing])]
>        []]
>      ,f "if true then\n\
>         \  null;\n\
>         \else\n\
>         \  null;\n\
>         \end if;"
>       [If ea [((BooleanLit ea True),[NullStatement ea])]
>        [NullStatement ea]]
>      ,f "if true then\n\
>         \  null;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \end if;"
>       [If ea [((BooleanLit ea True), [NullStatement ea])
>           ,((BooleanLit ea False), [Return ea Nothing])]
>        []]
>      ,f "if true then\n\
>         \  null;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \elsif false then\n\
>         \  return;\n\
>         \else\n\
>         \  return;\n\
>         \end if;"
>       [If ea [((BooleanLit ea True), [NullStatement ea])
>           ,((BooleanLit ea False), [Return ea Nothing])
>           ,((BooleanLit ea False), [Return ea Nothing])]
>        [Return ea Nothing]]
>      ,f "case a\n\
>         \  when b then null;\n\
>         \  when c,d then null;\n\
>         \  else null;\n\
>         \end case;"
>      [CaseStatementSimple ea (Identifier ea "a")
>       [([Identifier ea "b"], [NullStatement ea])
>       ,([Identifier ea "c", Identifier ea "d"], [NullStatement ea])]
>       [NullStatement ea]]
>
>     ]

>    ,Group "misc" [
>       s "SET search_path TO my_schema, public;"
>         [Set ea "search_path" [SetId ea "my_schema"
>                               ,SetId ea "public"]]
>      ,s "SET t1 = 3;"
>         [Set ea "t1" [SetNum ea 3]]
>      ,s "SET t1 = 'stuff';"
>         [Set ea "t1" [SetStr ea "stuff"]]
>      ,s "create language plpgsql;"
>         [CreateLanguage ea "plpgsql"]
>
>     ]
>
>     ]]
>  where
>    e = Expr
>    s = Stmt
>    f = PgSqlStmt

-------------------------------------------------------------------------------

shortcuts for constructing test data and asts

> stringQ :: String -> ScalarExpr
> stringQ = StringLit ea
>
> selectFrom :: SelectItemList
>            -> TableRef
>            -> QueryExpr
> selectFrom selList frm = Select ea Dupes (SelectList ea selList)
>                            [frm] Nothing [] Nothing [] Nothing Nothing
>
> selectE :: SelectList -> QueryExpr
> selectE selList = Select ea Dupes selList
>                     [] Nothing [] Nothing [] Nothing Nothing
>
> selIL :: [String] -> [SelectItem]
> selIL = map selI
> selEL :: [ScalarExpr] -> [SelectItem]
> selEL = map (SelExp ea)
>
> i :: String -> SQIdentifier
> i = SQIdentifier ea . (:[])

> dqi :: String -> SQIdentifier
> dqi ii = SQIdentifier ea [ii]

> eqi :: String -> String -> ScalarExpr
> eqi c n = QIdentifier ea (Identifier ea c) n

> ei :: String -> ScalarExpr
> ei = Identifier ea
>
> qi :: String -> String -> SQIdentifier
> qi c n = SQIdentifier ea [c,n]
>
> selI :: String -> SelectItem
> selI = SelExp ea . Identifier ea
>
> sl :: SelectItemList -> SelectList
> sl a = SelectList ea a
>
> selectFromWhere :: SelectItemList
>                 -> TableRef
>                 -> ScalarExpr
>                 -> QueryExpr
> selectFromWhere selList frm whr =
>     Select ea Dupes (SelectList ea selList)
>                [frm] (Just whr) [] Nothing [] Nothing Nothing
>
> att :: String -> String -> AttributeDef
> att n t = AttributeDef ea n (SimpleTypeName ea t) Nothing []

> ea :: Annotation
> ea = emptyAnnotation

--------------------------------------------------------------------------------

Unit test helpers

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Expr a b) = testParseScalarExpr a b
> itemToTft (PgSqlStmt a b) = testParsePlpgsqlStatements a b
> itemToTft (Stmt a b) = testParseStatements a b
> itemToTft (MSStmt a b) = testParseMSStatements a b
> itemToTft (Group s is) = testGroup s $ map itemToTft is
>
> testParseScalarExpr :: String -> ScalarExpr -> Test.Framework.Test
> testParseScalarExpr src ast =
>   parseUtil src ast (parseScalarExpr "") (parseScalarExpr "") printScalarExpr
>
> testParseStatements :: String -> [Statement] -> Test.Framework.Test
> testParseStatements src ast =
>   parseUtil src ast (parseStatements "") (parseStatements "") printStatements
>
> testParseMSStatements :: String -> [Statement] -> Test.Framework.Test
> testParseMSStatements src ast =
>   parseUtil src ast parseMsQuery (parseStatements "") printStatements
>   where
>     parseMsQuery :: String -> Either ParseErrorExtra [Statement]
>     parseMsQuery s =
>       (\p' -> [QueryStatement ea p'])
>       `fmap` parseSqlServerQueryExpr "" s

>
> testParsePlpgsqlStatements :: String -> [Statement] -> Test.Framework.Test
> testParsePlpgsqlStatements src ast =
>   parseUtil src ast (parsePlpgsql "") (parsePlpgsql "") printStatements
>
> parseUtil :: (Show t, Eq b, Show b, Data b) =>
>              String
>           -> b
>           -> (String -> Either t b)
>           -> (String -> Either t b)
>           -> (b -> String)
>           -> Test.Framework.Test
> parseUtil src ast parser reparser printer = testCase ("parse " ++ src) $
>   case parser src of
>     Left er -> assertFailure $ show er
>     Right ast' -> do
>       assertEqual ("parse " ++ src) ast $ resetAnnotations ast'
>       case reparser (printer ast) of
>         Left er -> assertFailure $ "reparse\n" ++ show er ++ "\n" -- ++ pp ++ "\n"
>         Right ast'' -> assertEqual ("reparse " ++ printer ast) ast $ resetAnnotations ast''

~~~~
TODO
new idea for testing:
parsesql -> ast1
parse, pretty print, parse -> ast2
load into pg, pg_dump, parse -> ast3
parse, pretty print, load into pg, pg_dump, parse -> ast4
check all these asts are the same
~~~~
