
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
> import Database.HsSqlPpp.PrettyPrinter
>
> data Item = Expr String Expression
>           | Stmt String [Statement]
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
>     Group "basic expressions" [
>       e "1" (IntegerLit ea 1)
>      ,e "-1" (FunCall ea "u-" [IntegerLit ea 1])
>      ,e "1.1" (FloatLit ea 1.1)
>      ,e "-1.1" (FunCall ea "u-" [FloatLit ea 1.1])
>      ,e " 1 + 1 " (FunCall ea "+" [IntegerLit ea 1
>                                   ,IntegerLit ea 1])
>      ,e "1+1+1" (FunCall ea "+" [FunCall ea "+" [IntegerLit ea 1
>                                                 ,IntegerLit ea 1]
>                                 ,IntegerLit ea 1])
>      ]
>    ,Group "parens" [

check some basic parens use wrt naked values and row constructors
these tests reflect how pg seems to interpret the variants.

>       e "(1)" (IntegerLit ea 1)
>      ,e "row ()" (FunCall ea "!rowctor" [])
>      ,e "row (1)" (FunCall ea "!rowctor" [IntegerLit ea 1])
>      ,e "row (1,2)" (FunCall ea "!rowctor" [IntegerLit ea 1,IntegerLit ea 2])
>      ,e "(1,2)" (FunCall ea "!rowctor" [IntegerLit ea 1,IntegerLit ea 2])
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
>       e "array[1,2]" (FunCall ea "!arrayctor" [IntegerLit ea 1, IntegerLit ea 2])
>      ,e "a[1]" (FunCall ea "!arraysub" [Identifier ea "a", IntegerLit ea 1])
>      ]
>    ,Group "simple operators" [
>       e "1 + tst1" (FunCall ea "+" [IntegerLit ea 1
>                                ,Identifier ea "tst1"])
>      ,e "tst1 + 1" (FunCall ea "+" [Identifier ea "tst1"
>                                ,IntegerLit ea 1])
>      ,e "tst + tst1" (FunCall ea "+" [Identifier ea "tst"
>                                  ,Identifier ea "tst1"])
>      ,e "'a' || 'b'" (FunCall ea "||" [stringQ "a"
>                                   ,stringQ "b"])
>      ,e "'stuff'::text" (Cast ea (stringQ "stuff") (SimpleTypeName ea "text"))
>      ,e "245::float(24)" (Cast ea (IntegerLit ea 245) (PrecTypeName ea "float" 24))
>      ,e "245::double precision" (Cast ea (IntegerLit ea 245) (SimpleTypeName ea "double precision"))
>      ,e "a between 1 and 3"
>         (FunCall ea "!between" [Identifier ea "a", IntegerLit ea 1, IntegerLit ea 3])
>      ,e "cast(a as text)"
>         (Cast ea (Identifier ea "a") (SimpleTypeName ea "text"))
>      ,e "@ a"
>         (FunCall ea "@" [Identifier ea "a"])
>      ,e "substring(a from 0 for 3)"
>         (FunCall ea "!substring" [Identifier ea "a", IntegerLit ea 0, IntegerLit ea 3])
>      ,e "substring(a from 0 for (5 - 3))"
>         (FunCall ea "!substring" [Identifier ea "a",IntegerLit ea 0,
>          FunCall ea "-" [IntegerLit ea 5,IntegerLit ea 3]])
>      ,e "a like b"
>         (FunCall ea "!like" [Identifier ea "a", Identifier ea "b"])
>      ]
>    ,Group "function calls" [
>       e "fn()" (FunCall ea "fn" [])
>      ,e "fn(1)" (FunCall ea "fn" [IntegerLit ea 1])
>      ,e "fn('test')" (FunCall ea "fn" [stringQ "test"])
>      ,e "fn(1,'test')" (FunCall ea "fn" [IntegerLit ea 1, stringQ "test"])
>      ,e "fn('test')" (FunCall ea "fn" [stringQ "test"])
>      ]
>    ,Group "simple whitespace sanity checks" [
>       e "fn (1)" (FunCall ea "fn" [IntegerLit ea 1])
>      ,e "fn( 1)" (FunCall ea "fn" [IntegerLit ea 1])
>      ,e "fn(1 )" (FunCall ea "fn" [IntegerLit ea 1])
>      ,e "fn(1) " (FunCall ea "fn" [IntegerLit ea 1])
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
>         [$here|
>          case when a,b then 3
>               when c then 4
>               else 5
>          end
>          |]
>         (Case ea [([Identifier ea "a", Identifier ea "b"], IntegerLit ea 3)
>               ,([Identifier ea "c"], IntegerLit ea 4)]
>          (Just $ IntegerLit ea 5))
>      ,e  "case 1 when 2 then 3 else 4 end"
>         (CaseSimple ea (IntegerLit ea 1)
>            [([IntegerLit ea 2], IntegerLit ea 3)]
>          (Just $ IntegerLit ea 4))
>      ]
>    ,Group "positional args" [
>       e "$1" (PositionalArg ea 1)
>      ,e "?" (Placeholder ea)
>      ,e "a = ?" (FunCall ea "=" [Identifier ea "a",Placeholder ea])
>      ]
>    ,Group "exists" [
>       e "exists (select 1 from a)"
>       (Exists ea (selectFrom [SelExp ea (IntegerLit ea 1)] (Tref ea (i "a") NoAlias)))
>      ]
>    ,Group "in variants" [
>       e "t in (1,2)"
>       (InPredicate ea (Identifier ea "t") True (InList ea [IntegerLit ea 1,IntegerLit ea 2]))
>      ,e "t not in (1,2)"
>       (InPredicate ea (Identifier ea "t") False (InList ea [IntegerLit ea 1,IntegerLit ea 2]))
>      ,e "(t,u) in (1,2)"
>       (InPredicate ea (FunCall ea "!rowctor" [Identifier ea "t",Identifier ea "u"]) True
>        (InList ea [IntegerLit ea 1,IntegerLit ea 2]))
>      ,e "3 = any (array[1,2])"
>       (LiftOperator ea "=" LiftAny [IntegerLit ea 3
>                                     ,FunCall ea "!arrayctor" [IntegerLit ea 1
>                                                              ,IntegerLit ea 2]])
>      ,e "3 = all (array[1,2,4])"
>       (LiftOperator ea "=" LiftAll [IntegerLit ea 3
>                                     ,FunCall ea "!arrayctor" [IntegerLit ea 1
>                                                              ,IntegerLit ea 2
>                                                              ,IntegerLit ea 4]])
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
>       e "(p).x" (qi "p" "x")
>      ,e "(select f(((a).x, y)::z))"
>         (ScalarSubQuery ea
>          (selectE (sl
>                    [SelExp ea
>                     (FunCall ea "f" [Cast ea
>                                      (FunCall ea "!rowctor"
>                                       [qi "a" "x"
>                                       ,Identifier ea "y"])
>                                      (SimpleTypeName ea "z")])])))
>      ]
>      ]

--------------------------------------------------------------------------------

select statements

>   ,Group "simple select statements" [
>     Group "select no table" [
>       s "select 1;" [SelectStatement ea $ selectE (SelectList ea [SelExp ea (IntegerLit ea 1)] [])]
>      ]
>    ,Group "select from table" [
>       s "select * from tbl;"
>       [SelectStatement ea $ selectFrom (selIL ["*"]) (Tref ea (i "tbl") NoAlias)]
>      ,s "select a,b from tbl;"
>       [SelectStatement ea $ selectFrom (selIL ["a", "b"]) (Tref ea (i "tbl") NoAlias)]
>      ,s "select a,b from inf.tbl;"
>       [SelectStatement ea $ selectFrom (selIL ["a", "b"]) (Tref ea (qi "inf" "tbl") NoAlias)]
>      ,s "select distinct * from tbl;"
>       [SelectStatement ea $ Select ea Distinct (SelectList ea (selIL ["*"]) []) [Tref ea (i "tbl") NoAlias]
>        Nothing [] Nothing [] Nothing Nothing]
>      ,s "select a from tbl where b=2;"
>       [SelectStatement ea $ selectFromWhere
>         (selIL ["a"])
>         (Tref ea (i "tbl") NoAlias)
>         (FunCall ea "="
>          [Identifier ea "b", IntegerLit ea 2])]
>      ,s "select a from tbl where b=2 and c=3;"
>       [SelectStatement ea $ selectFromWhere
>         (selIL ["a"])
>         (Tref ea (i "tbl") NoAlias)
>         (FunCall ea "!and"
>          [FunCall ea "="  [Identifier ea "b", IntegerLit ea 2]
>          ,FunCall ea "=" [Identifier ea "c", IntegerLit ea 3]])]
>      ]
>

>    ,Group "more select statements" [
>       s "select a from tbl\n\
>         \except\n\
>         \select a from tbl1;"
>       [SelectStatement ea $ CombineSelect ea Except
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl") NoAlias))
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl1") NoAlias))]
>      ,s "select a from tbl where true\n\
>         \except\n\
>         \select a from tbl1 where true;"
>       [SelectStatement ea $ CombineSelect ea Except
>        (selectFromWhere (selIL ["a"]) (Tref ea (i "tbl") NoAlias) (BooleanLit ea True))
>        (selectFromWhere (selIL ["a"]) (Tref ea (i "tbl1") NoAlias) (BooleanLit ea True))]
>      ,s "select a from tbl\n\
>         \union\n\
>         \select a from tbl1;"
>       [SelectStatement ea $ CombineSelect ea Union
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl") NoAlias))
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl1") NoAlias))]
>      ,s "select a from tbl\n\
>         \union all\n\
>         \select a from tbl1;"
>       [SelectStatement ea $ CombineSelect ea UnionAll
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl") NoAlias))
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl1") NoAlias))]
>      ,s "(select 1 union select 2) union select 3;"
>       [SelectStatement ea
>        (CombineSelect ea Union
>         (CombineSelect ea Union
>          (selectE (SelectList ea [SelExp ea (IntegerLit ea 1)] []))
>          (selectE (SelectList ea [SelExp ea (IntegerLit ea 2)] [])))
>         (selectE (SelectList ea [SelExp ea (IntegerLit ea 3)] [])))]
>      ,s "select 1 union (select 2 union select 3);"
>       [SelectStatement ea
>        (CombineSelect ea Union
>         (selectE (SelectList ea [SelExp ea (IntegerLit ea 1)] []))
>         (CombineSelect ea Union
>          (selectE (SelectList ea [SelExp ea (IntegerLit ea 2)] []))
>          (selectE (SelectList ea [SelExp ea (IntegerLit ea 3)] []))))]
>      ,s [$here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from b; |]
>          [SelectStatement ea
>           (WithSelect ea
>            [WithQuery ea "a" (selectE $ SelectList ea
>                                [SelectItem ea (IntegerLit ea 1) "a1"] [])
>            ,WithQuery ea "b" (selectFrom (selIL ["*"]) (Tref ea (i "a") NoAlias))]
>            (selectFrom (selIL ["*"]) (Tref ea (i "b") NoAlias)))]
>      ,s [$here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from a
>               union select * from b; |]
>          [SelectStatement ea
>           (WithSelect ea
>            [WithQuery ea "a" (selectE $ SelectList ea
>                                [SelectItem ea (IntegerLit ea 1) "a1"] [])
>            ,WithQuery ea "b" (selectFrom (selIL ["*"]) (Tref ea (i "a") NoAlias))]
>            (CombineSelect ea Union
>              (selectFrom (selIL ["*"]) (Tref ea (i "a") NoAlias))
>              (selectFrom (selIL ["*"]) (Tref ea (i "b") NoAlias))))]
>      ,s "select a as b from tbl;"
>       [SelectStatement ea $ selectFrom [SelectItem ea (Identifier ea "a") "b"] (Tref ea (i "tbl") NoAlias)]
>      ,s "select a + b as b from tbl;"
>       [SelectStatement ea $ selectFrom
>        [SelectItem ea
>         (FunCall ea "+"
>          [Identifier ea "a", Identifier ea "b"]) "b"]
>        (Tref ea (i "tbl") NoAlias)]
>      ,s "select a.* from tbl a;"
>       [SelectStatement ea $ selectFrom (selEL [qi "a" "*"]) (Tref ea (i "tbl") (TableAlias "a"))]
>      ,s "select a.* from tbl a(b,c);"
>       [SelectStatement ea $ selectFrom (selEL [qi "a" "*"]) (Tref ea (i "tbl") (FullAlias "a" ["b","c"]))]

>      ,s "select * from t1 a, t2 b;"
>             [SelectStatement ea
>              (Select ea Dupes
>               (SelectList ea
>                [SelExp ea (Identifier ea "*")] [])
>               [Tref ea (i "t1") (TableAlias "a"),Tref ea (i "t2") (TableAlias "b")]
>               Nothing [] Nothing [] Nothing Nothing)]
>      ,s "select a from b inner join c on b.a=c.a;"
>       [SelectStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinedTref ea (Tref ea (i "b") NoAlias) Unnatural Inner (Tref ea (i "c") NoAlias)
>           (Just (JoinOn ea
>            (FunCall ea "=" [qi "b" "a", qi "c" "a"]))) NoAlias)]
>      ,s "select a from b inner join c as d on b.a=d.a;"
>       [SelectStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinedTref ea (Tref ea (i "b") NoAlias) Unnatural Inner (Tref ea (i "c") (TableAlias "d"))
>           (Just (JoinOn ea
>            (FunCall ea "=" [qi "b" "a", qi "d" "a"]))) NoAlias)]
>      ,s "select a from b inner join c using(d,e);"
>       [SelectStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinedTref ea (Tref ea (i "b") NoAlias) Unnatural Inner (Tref ea (i "c") NoAlias)
>           (Just (JoinUsing ea ["d","e"])) NoAlias)]
>      ,s "select a from b natural inner join c;"
>       [SelectStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinedTref ea (Tref ea (i "b") NoAlias) Natural Inner (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select a from b left outer join c;"
>       [SelectStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinedTref ea (Tref ea (i "b") NoAlias) Unnatural LeftOuter (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select a from b full outer join c;"
>       [SelectStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinedTref ea (Tref ea (i "b") NoAlias) Unnatural FullOuter (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select a from b right outer join c;"
>       [SelectStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinedTref ea (Tref ea (i "b") NoAlias) Unnatural RightOuter (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select a from b cross join c;"
>       [SelectStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinedTref ea (Tref ea (i "b") NoAlias) Unnatural Cross (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select a from (b natural join c);"
>       [SelectStatement ea $ selectFrom
>        (selIL ["a"])
>        (JoinedTref ea (Tref ea (i "b") NoAlias) Natural Inner (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select x from a cross join b cross join c;"
>        [SelectStatement ea
>         (selectFrom (selIL ["x"])
>          (JoinedTref ea
>          (JoinedTref ea
>           (Tref ea (i "a") NoAlias)
>            Unnatural Cross
>           (Tref ea (i "b") NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref ea (i "c") NoAlias)
>          Nothing NoAlias))]
>      ,s "select x from ((a cross join b) cross join c);"
>        [SelectStatement ea
>         (selectFrom (selIL ["x"])
>          (JoinedTref ea
>          (JoinedTref ea
>           (Tref ea (i "a") NoAlias)
>            Unnatural Cross
>           (Tref ea (i "b") NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref ea (i "c") NoAlias)
>          Nothing NoAlias))]
>      ,s "select x from (a cross join (b cross join c));"
>        [SelectStatement ea
>         (selectFrom (selIL ["x"])
>          (JoinedTref ea
>           (Tref ea (i "a") NoAlias)
>           Unnatural Cross
>           (JoinedTref ea
>            (Tref ea (i "b") NoAlias)
>            Unnatural Cross
>            (Tref ea (i "c") NoAlias)
>            Nothing NoAlias)
>           Nothing NoAlias))]

>      ,s "select x from ((a cross join b) cross join c);"
>        [SelectStatement ea
>         (selectFrom (selIL ["x"])
>          (JoinedTref ea
>          (JoinedTref ea
>           (Tref ea (i "a") NoAlias)
>            Unnatural Cross
>           (Tref ea (i "b") NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref ea (i "c") NoAlias)
>          Nothing NoAlias))]
>      ,s "select x from (a cross join b) cross join c;"
>        [SelectStatement ea
>         (selectFrom (selIL ["x"])
>          (JoinedTref ea
>          (JoinedTref ea
>           (Tref ea (i "a") NoAlias)
>            Unnatural Cross
>           (Tref ea (i "b") NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref ea (i "c") NoAlias)
>          Nothing NoAlias))]
>      ,s "select x from ((a cross join b) cross join c) cross join d;"
>        [SelectStatement ea
>         (selectFrom (selIL ["x"])
>          (JoinedTref ea
>           (JoinedTref ea
>            (JoinedTref ea
>             (Tref ea (i "a") NoAlias)
>             Unnatural Cross
>             (Tref ea (i "b") NoAlias)
>             Nothing NoAlias)
>            Unnatural Cross
>            (Tref ea (i "c") NoAlias)
>            Nothing NoAlias)
>           Unnatural Cross
>           (Tref ea (i "d") NoAlias)
>           Nothing NoAlias))]
>      ,s "select a from b\n\
>         \    inner join c\n\
>         \      on true\n\
>         \    inner join d\n\
>         \      on 1=1;"
>       [SelectStatement ea $ selectFrom
>        [SelExp ea (Identifier ea "a")]
>        (JoinedTref ea
>         (JoinedTref ea (Tref ea (i "b") NoAlias) Unnatural Inner (Tref ea (i "c") NoAlias)
>          (Just $ JoinOn ea (BooleanLit ea True)) NoAlias)
>         Unnatural Inner (Tref ea (i "d") NoAlias)
>         (Just $ JoinOn ea (FunCall ea "="
>                [IntegerLit ea 1, IntegerLit ea 1])) NoAlias)]

>      ,s "select row_number() over(order by a) as place from tbl;"
>       [SelectStatement ea $ selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [Identifier ea "a"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") NoAlias)]
>      ,s "select row_number() over(order by a asc) as place from tbl;"
>       [SelectStatement ea $ selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [Identifier ea "a"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") NoAlias)]
>      ,s "select row_number() over(order by a desc) as place from tbl;"
>       [SelectStatement ea $ selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [Identifier ea "a"] Desc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") NoAlias)]
>      ,s "select row_number()\n\
>         \over(partition by (a,b) order by c) as place\n\
>         \from tbl;"
>       [SelectStatement ea $ selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     [FunCall ea "!rowctor" [Identifier ea "a",Identifier ea "b"]]
>                     [Identifier ea "c"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") NoAlias)]
>      ,s "select * from a natural inner join (select * from b) as a;"
>       [SelectStatement ea $ selectFrom
>        (selIL ["*"])
>        (JoinedTref ea (Tref ea (i "a") NoAlias) Natural
>         Inner (SubTref ea (selectFrom
>                         (selIL ["*"])
>                         (Tref ea (i "b") NoAlias)) (TableAlias "a"))
>         Nothing NoAlias)]
>      ,s "select * from a order by c;"
>       [SelectStatement ea $ Select ea  Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") NoAlias]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] Nothing Nothing]
>      ,s "select *\n\
>            \from Adventure\n\
>            \order by Clicks desc, AdventureID;"
>       [SelectStatement ea $ Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "Adventure") NoAlias]
>        Nothing [] Nothing [(Identifier ea "Clicks",Desc)
>                           ,(Identifier ea "AdventureID",Asc)] Nothing Nothing]
>      ,s "select * from a order by c,d asc;"
>       [SelectStatement ea $ Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") NoAlias]
>        Nothing [] Nothing [(Identifier ea "c", Asc)
>                           ,(Identifier ea "d", Asc)] Nothing Nothing]
>      ,s "select * from a order by c,d desc;"
>       [SelectStatement ea $ Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") NoAlias]
>        Nothing [] Nothing [(Identifier ea "c", Asc)
>                           ,(Identifier ea "d", Desc)] Nothing Nothing]
>      ,s "select * from a order by c limit 1;"
>       [SelectStatement ea $ Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") NoAlias]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] (Just (IntegerLit ea 1)) Nothing]
>      ,s "select * from a order by c offset 3;"
>       [SelectStatement ea $ Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") NoAlias]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] Nothing (Just $ IntegerLit ea 3)]
>      ,s "select a from (select b from c) as d;"
>         [SelectStatement ea $ selectFrom
>          (selIL ["a"])
>          (SubTref ea (selectFrom
>                    (selIL ["b"])
>                    (Tref ea (i "c") NoAlias))
>           (TableAlias "d"))]
>      ,s "select * from gen();"
>         [SelectStatement ea $ selectFrom (selIL ["*"]) (TrefFun ea (FunCall ea "gen" []) NoAlias)]
>      ,s "select * from gen() as t;"
>       [SelectStatement ea $ selectFrom
>        (selIL ["*"])
>        (TrefFun ea (FunCall ea "gen" [])(TableAlias  "t"))]
>      ,s "select a, count(b) from c group by a;"
>         [SelectStatement ea $ Select ea Dupes
>          (sl [selI "a", SelExp ea (FunCall ea "count" [Identifier ea "b"])])
>          [Tref ea (i "c") NoAlias] Nothing [Identifier ea "a"]
>          Nothing [] Nothing Nothing]
>      ,s "select a, count(b) as cnt from c group by a having cnt > 4;"
>         [SelectStatement ea $ Select ea Dupes
>          (sl [selI "a", SelectItem ea (FunCall ea "count" [Identifier ea "b"]) "cnt"])
>          [Tref ea (i "c") NoAlias] Nothing [Identifier ea "a"]
>          (Just $ FunCall ea ">" [Identifier ea "cnt", IntegerLit ea 4])
>          [] Nothing Nothing]
>      ,s "select a from (select 1 as a, 2 as b) x;"
>         [SelectStatement ea $ selectFrom
>          [selI "a"]
>          (SubTref ea (selectE $ SelectList ea
>                                [SelectItem ea (IntegerLit ea 1) "a"
>                                ,SelectItem ea (IntegerLit ea 2) "b"] [])
>                   (TableAlias "x"))]
>      ]

>    ,Group "multiple statements" [
>       s "select 1;\nselect 2;" [SelectStatement ea $ selectE $ sl [SelExp ea (IntegerLit ea 1)]
>                                ,SelectStatement ea $ selectE $ sl [SelExp ea (IntegerLit ea 2)]]
>      ]
>    ,Group "comments" [
>       s "" []
>      ,s "-- this is a test" []
>      ,s "/* this is\n\
>         \a test*/" []
>      ,s "select 1;\n\
>         \-- this is a test\n\
>         \select -- this is a test\n\
>         \2;" [SelectStatement ea $ selectE $ sl [SelExp ea (IntegerLit ea 1)]
>              ,SelectStatement ea $ selectE $ sl [SelExp ea (IntegerLit ea 2)]
>              ]
>      ,s "select 1;\n\
>         \/* this is\n\
>         \a test*/\n\
>         \select /* this is a test*/2;"
>                     [SelectStatement ea $ selectE $ sl [SelExp ea (IntegerLit ea 1)]
>                     ,SelectStatement ea $ selectE $ sl [SelExp ea (IntegerLit ea 2)]
>                     ]
>      ]
>    ,Group "some mis stuff" [
>       s "select (p).x, (p).y from pos;"
>         [SelectStatement ea $ selectFrom (selEL [qi "p" "x"
>                                                 ,qi "p" "y"])
>                                          (Tref ea (i "pos") NoAlias)]
>      ,s "select ($1).x, ($1).y from pos;"
>         [SelectStatement ea $ selectFrom (selEL [QIdentifier ea (PositionalArg ea 1) "x"
>                                                 ,QIdentifier ea (PositionalArg ea 1) "y"])
>                                          (Tref ea (i "pos") NoAlias)]
>      ,s "select row_number() over(), x from tb;"
>       [SelectStatement ea $ selectFrom
>        [SelExp ea
>                     (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [] Asc FrameUnboundedPreceding)
>        , selI "x"]
>        (Tref ea (i "tb") NoAlias)]
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
>         (i "testtable")
>         ["columna", "columnb"]
>         (Values ea [[IntegerLit ea 1, IntegerLit ea 2]])
>         Nothing]

multi row insert, test the stand alone values statement first, maybe
that should be in the select section?

>      ,s "values (1,2), (3,4);"
>      [SelectStatement ea $ Values ea [[IntegerLit ea 1, IntegerLit ea 2]
>              ,[IntegerLit ea 3, IntegerLit ea 4]]]
>
>      ,s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2), (3,4);\n"
>       [Insert ea
>         (i "testtable")
>         ["columna", "columnb"]
>         (Values ea [[IntegerLit ea 1, IntegerLit ea 2]
>                 ,[IntegerLit ea 3, IntegerLit ea 4]])
>         Nothing]

insert from select

>      ,s "insert into a\n\
>          \    select b from c;"
>       [Insert ea (i "a") []
>        (selectFrom [selI "b"] (Tref ea (i "c") NoAlias))
>        Nothing]
>
>      ,s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2) returning id;\n"
>       [Insert ea
>         (i "testtable")
>         ["columna", "columnb"]
>         (Values ea [[IntegerLit ea 1, IntegerLit ea 2]])
>         (Just $ sl [selI "id"])]
>      ]
>
>     ,Group "update" [
>       s "update tb\n\
>         \  set x = 1, y = 2;"
>       [Update ea (i "tb") [FunCall ea "=" [Identifier ea "x", IntegerLit ea 1]
>                       ,FunCall ea "=" [Identifier ea "y", IntegerLit ea 2]]
>        [] Nothing Nothing]
>      ,s "update tb\n\
>         \  set x = 1, y = 2 where z = true;"
>       [Update ea (i "tb") [FunCall ea "=" [Identifier ea "x", IntegerLit ea 1]
>                       ,FunCall ea "=" [Identifier ea "y", IntegerLit ea 2]]
>        []
>        (Just $ FunCall ea "="
>         [Identifier ea "z", BooleanLit ea True])
>        Nothing]
>      ,s "update tb\n\
>         \  set x = 1, y = 2 returning id;"
>       [Update ea (i "tb") [FunCall ea "=" [Identifier ea "x", IntegerLit ea 1]
>                       ,FunCall ea "=" [Identifier ea "y", IntegerLit ea 2]]
>        [] Nothing (Just $ sl [selI "id"])]
>      ,s "update pieces\n\
>         \set a=b returning tag into r.tag;"
>       [Update ea (i "pieces") [FunCall ea "=" [Identifier ea "a"
>                                           ,Identifier ea "b"]]
>        []
>        Nothing (Just (SelectList ea
>                       [SelExp ea (Identifier ea "tag")]
>                       [qi "r" "tag"]))]
>      ,s "update tb\n\
>         \  set (x,y) = (1,2);"
>       [Update ea (i "tb") [FunCall ea "="
>                        [FunCall ea "!rowctor" [Identifier ea "x"
>                                               ,Identifier ea "y"]
>                        ,FunCall ea "!rowctor" [IntegerLit ea 1
>                                               ,IntegerLit ea 2]]]
>        []
>        Nothing Nothing]
>      ]

FunCall ea "=" [FunCall ea "!rowctor" [Identifier ea "x",Identifier ea "y"],FunCall ea "!rowctor" [IntegerLit ea 1,IntegerLit ea 2]])


>
>     ,Group "delete" [
>       s "delete from tbl1 where x = true;"
>       [Delete ea (i "tbl1") [] (Just $ FunCall ea "="
>                                [Identifier ea "x", BooleanLit ea True])
>        Nothing]
>      ,s "delete from tbl1 where x = true returning id;"
>       [Delete ea (i "tbl1") [] (Just $ FunCall ea "="
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
>        (selectE (SelectList ea [SelExp ea (IntegerLit ea 1)] []))]
>
>      ,s "alter table a alter column b set default 1;"
>       [AlterTable ea "a" [AlterColumnDefault ea "b" (IntegerLit ea 1)]]
>
>      ,s "alter table a add constraint unique(b);"
>       [AlterTable ea "a" [AddConstraint ea (UniqueConstraint ea "" ["b"])]]
>      ]]

>
>     ,Group "others" [
>       s "create view v1 as\n\
>         \select a,b from t;"
>       [CreateView ea
>        "v1"
>        (selectFrom [selI "a", selI "b"] (Tref ea (i "t") NoAlias))]
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
>         [SelectStatement ea $ selectFromWhere [SelExp ea (Identifier ea "a")] (Tref ea (i "t1") NoAlias)
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
>          [VarDef ea "a" (SimpleTypeName ea "int") (Just $ IntegerLit ea 3)]
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
>       [Assignment ea (i "success") (BooleanLit ea True)]
>      ,f "success = true;"
>       [Assignment ea (i "success") (BooleanLit ea True)]
>      ,f "return true;"
>       [Return ea $ Just (BooleanLit ea True)]
>      ,f "return;"
>       [Return ea Nothing]
>      ,f "return next 1;"
>       [ReturnNext ea $ IntegerLit ea 1]
>      ,f "return query select a from b;"
>       [ReturnQuery ea $ selectFrom [selI "a"] (Tref ea (i "b") NoAlias)]
>      ,f "raise notice 'stuff %', 1;"
>       [Raise ea RNotice "stuff %" [IntegerLit ea 1]]
>      ,f "perform test();"
>       [Perform ea $ FunCall ea "test" []]
>      ,f "perform test(a,b);"
>       [Perform ea $ FunCall ea "test" [Identifier ea "a", Identifier ea "b"]]
>      ,f "perform test(r.relvar_name || '_and_stuff');"
>       [Perform ea $ FunCall ea "test" [
>                     FunCall ea "||" [qi "r" "relvar_name"
>                                     ,stringQ "_and_stuff"]]]
>      ,f "select into a,b c,d from e;"
>       [SelectStatement ea $ Select ea Dupes (SelectList ea [selI "c", selI "d"] [i "a", i "b"])
>                   [Tref ea (i "e") NoAlias] Nothing [] Nothing [] Nothing Nothing]
>      ,f "select c,d into a,b from e;"
>       [SelectStatement ea $ Select ea Dupes (SelectList ea [selI "c", selI "d"] [i "a", i "b"])
>                   [Tref ea (i "e") NoAlias] Nothing [] Nothing [] Nothing Nothing]
>
>      ,f "execute s;"
>       [Execute ea (Identifier ea "s")]
>      ,f "execute s into r;"
>       [ExecuteInto ea (Identifier ea "s") ["r"]]
>
>      ,f "continue;" [ContinueStatement ea Nothing]
>     ]
>
>     ,Group "other plpgsql statements" [
>       f "for r in select a from tbl loop\n\
>         \null;\n\
>         \end loop;"
>       [ForSelectStatement ea Nothing (i "r") (selectFrom  [selI "a"] (Tref ea (i "tbl") NoAlias))
>        [NullStatement ea]]
>      ,f "for r in select a from tbl where true loop\n\
>         \null;\n\
>         \end loop;"
>       [ForSelectStatement ea Nothing (i "r")
>        (selectFromWhere [selI "a"] (Tref ea (i "tbl") NoAlias) (BooleanLit ea True))
>        [NullStatement ea]]
>      ,f "for r in 1 .. 10 loop\n\
>         \null;\n\
>         \end loop;"
>       [ForIntegerStatement ea Nothing (i "r")
>        (IntegerLit ea 1) (IntegerLit ea 10)
>        [NullStatement ea]]
>
>      ,f "if a=b then\n\
>         \  update c set d = e;\n\
>         \end if;"
>       [If ea [(FunCall ea "=" [Identifier ea "a", Identifier ea "b"]
>               ,[Update ea (i "c") [FunCall ea "=" [Identifier ea "d"
>                                                   ,Identifier ea "e"]] [] Nothing Nothing])]
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

> stringQ :: String -> Expression
> stringQ = StringLit ea
>
> selectFrom :: SelectItemList
>            -> TableRef
>            -> SelectExpression
> selectFrom selList frm = Select ea Dupes (SelectList ea selList [])
>                            [frm] Nothing [] Nothing [] Nothing Nothing
>
> selectE :: SelectList -> SelectExpression
> selectE selList = Select ea Dupes selList
>                     [] Nothing [] Nothing [] Nothing Nothing
>
> selIL :: [String] -> [SelectItem]
> selIL = map selI
> selEL :: [Expression] -> [SelectItem]
> selEL = map (SelExp ea)
>
> i :: String -> Expression
> i = Identifier ea
>
> qi :: String -> String -> Expression
> qi c n = QIdentifier ea (i c) n
>
> selI :: String -> SelectItem
> selI = SelExp ea . Identifier ea
>
> sl :: SelectItemList -> SelectList
> sl a = SelectList ea a []
>
> selectFromWhere :: SelectItemList
>                 -> TableRef
>                 -> Expression
>                 -> SelectExpression
> selectFromWhere selList frm whr =
>     Select ea Dupes (SelectList ea selList [])
>                [frm] (Just whr) [] Nothing [] Nothing Nothing
>
> att :: String -> String -> AttributeDef
> att n t = AttributeDef ea n (SimpleTypeName ea t) Nothing []

> ea :: Annotation
> ea = emptyAnnotation

--------------------------------------------------------------------------------

Unit test helpers

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Expr a b) = testParseExpression a b
> itemToTft (PgSqlStmt a b) = testParsePlpgsqlStatements a b
> itemToTft (Stmt a b) = testParseStatements a b
> itemToTft (Group s is) = testGroup s $ map itemToTft is
>
> testParseExpression :: String -> Expression -> Test.Framework.Test
> testParseExpression src ast = parseUtil src ast
>                                  (parseExpression "") printExpression
>
> testParseStatements :: String -> [Statement] -> Test.Framework.Test
> testParseStatements src ast = parseUtil src ast (parseSql "") printSql
>
> testParsePlpgsqlStatements :: String -> [Statement] -> Test.Framework.Test
> testParsePlpgsqlStatements src ast = parseUtil src ast (parsePlpgsql "") printSql
>
> parseUtil :: (Show t, Eq b, Show b, Data b) =>
>              String
>           -> b
>           -> (String -> Either t b)
>           -> (b -> String)
>           -> Test.Framework.Test
> parseUtil src ast parser printer = testCase ("parse " ++ src) $
>   case parser src of
>     Left er -> assertFailure $ show er
>     Right ast' -> do
>       assertEqual ("parse " ++ src) ast $ resetAnnotations ast'
>       case parser (printer ast) of
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
