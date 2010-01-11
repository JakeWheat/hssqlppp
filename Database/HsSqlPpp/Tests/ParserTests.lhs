Copyright 2009 Jake Wheat

The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written in a tdd style, which the coverage of the tests reflects.

There are no tests for invalid sql at the moment.

TODO
new idea for testing:
parsesql -> ast1
parse, pretty print, parse -> ast2
load into pg, pg_dump, parse -> ast3
parse, pretty print, load into pg, pg_dump, parse -> ast4
check all these asts are the same

> {-# LANGUAGE QuasiQuotes #-}

> module Database.HsSqlPpp.Tests.ParserTests (parserTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char
> import Data.Generics

> import Database.HsSqlPpp.Here

> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.PrettyPrinter.PrettyPrinter


> data Item = Expressions [(String, Expression)]
>           | Statements [(String, [Statement])]
>           | PlpgsqlStatements [(String, [Statement])]
>           | Group String [Item]

> parserTests :: [Test.Framework.Test]
> parserTests = itemToTft parserTestData

> parserTestData :: Item
> parserTestData =
>   Group "parserTests" [

================================================================================

expressions

>    Group "parse expressions" [
>     Group "basic expressions" [Expressions [
>       p "1" (IntegerLit [] 1)
>      ,p "-1" (FunCall [] "u-" [IntegerLit [] 1])
>      ,p "1.1" (FloatLit [] 1.1)
>      ,p "-1.1" (FunCall [] "u-" [FloatLit [] 1.1])
>      ,p " 1 + 1 " (FunCall [] "+" [IntegerLit [] 1
>                               ,IntegerLit [] 1])
>      ,p "1+1+1" (FunCall [] "+" [FunCall [] "+" [IntegerLit [] 1
>                                         ,IntegerLit [] 1]
>                             ,IntegerLit [] 1])
>      ]]


>    ,Group "parens" [Expressions [

check some basic parens use wrt naked values and row constructors
these tests reflect how pg seems to interpret the variants.

>       p "(1)" (IntegerLit [] 1)
>      ,p "row ()" (FunCall [] "!rowctor" [])
>      ,p "row (1)" (FunCall [] "!rowctor" [IntegerLit [] 1])
>      ,p "row (1,2)" (FunCall [] "!rowctor" [IntegerLit [] 1,IntegerLit [] 2])
>      ,p "(1,2)" (FunCall [] "!rowctor" [IntegerLit [] 1,IntegerLit [] 2])
>      ]]

>    ,Group "more basic expressions" [Expressions [

test some more really basic expressions

>       p "'test'" (stringQ "test")
>      ,p "''" (stringQ "")
>      ,p "hello" (Identifier [] "hello")
>      ,p "helloTest" (Identifier [] "helloTest")
>      ,p "hello_test" (Identifier [] "hello_test")
>      ,p "\"this is an identifier\"" (Identifier [] "this is an identifier")
>      ,p "hello1234" (Identifier [] "hello1234")
>      ,p "true" (BooleanLit [] True)
>      ,p "false" (BooleanLit [] False)
>      ,p "null" (NullLit [])
>      ]]

>    ,Group "array ctor and selector" [Expressions [
>       p "array[1,2]" (FunCall [] "!arrayctor" [IntegerLit [] 1, IntegerLit [] 2])
>      ,p "a[1]" (FunCall [] "!arraysub" [Identifier [] "a", IntegerLit [] 1])

>      ]]

>    ,Group "simple operators" [Expressions [
>       p "1 + tst1" (FunCall [] "+" [IntegerLit [] 1
>                                ,Identifier [] "tst1"])
>      ,p "tst1 + 1" (FunCall [] "+" [Identifier [] "tst1"
>                                ,IntegerLit [] 1])
>      ,p "tst + tst1" (FunCall [] "+" [Identifier [] "tst"
>                                  ,Identifier [] "tst1"])
>      ,p "'a' || 'b'" (FunCall [] "||" [stringQ "a"
>                                   ,stringQ "b"])
>      ,p "'stuff'::text" (Cast [] (stringQ "stuff") (SimpleTypeName [] "text"))
>      ,p "245::float(24)" (Cast [] (IntegerLit [] 245) (PrecTypeName [] "float" 24))

>      ,p "245::double precision" (Cast [] (IntegerLit [] 245) (SimpleTypeName [] "double precision"))

>      ,p "a between 1 and 3"
>         (FunCall [] "!between" [Identifier [] "a", IntegerLit [] 1, IntegerLit [] 3])
>      ,p "cast(a as text)"
>         (Cast [] (Identifier [] "a") (SimpleTypeName [] "text"))
>      ,p "@ a"
>         (FunCall [] "@" [Identifier [] "a"])

>      ,p "substring(a from 0 for 3)"
>         (FunCall [] "!substring" [Identifier [] "a", IntegerLit [] 0, IntegerLit [] 3])

>      ,p "substring(a from 0 for (5 - 3))"
>         (FunCall [] "!substring" [Identifier [] "a",IntegerLit [] 0,
>          FunCall [] "-" [IntegerLit [] 5,IntegerLit [] 3]])
>      ,p "a like b"
>         (FunCall [] "!like" [Identifier [] "a", Identifier [] "b"])
>      ]]

>    ,Group "function calls" [Expressions [
>       p "fn()" (FunCall [] "fn" [])
>      ,p "fn(1)" (FunCall [] "fn" [IntegerLit [] 1])
>      ,p "fn('test')" (FunCall [] "fn" [stringQ "test"])
>      ,p "fn(1,'test')" (FunCall [] "fn" [IntegerLit [] 1, stringQ "test"])
>      ,p "fn('test')" (FunCall [] "fn" [stringQ "test"])
>      ]]

>    ,Group "simple whitespace sanity checks" [Expressions [
>       p "fn (1)" (FunCall [] "fn" [IntegerLit [] 1])
>      ,p "fn( 1)" (FunCall [] "fn" [IntegerLit [] 1])
>      ,p "fn(1 )" (FunCall [] "fn" [IntegerLit [] 1])
>      ,p "fn(1) " (FunCall [] "fn" [IntegerLit [] 1])

>      ]]

>    ,Group "null stuff" [Expressions [
>       p "not null" (FunCall [] "!not" [NullLit []])
>      ,p "a is null" (FunCall [] "!isnull" [Identifier [] "a"])
>      ,p "a is not null" (FunCall [] "!isnotnull" [Identifier [] "a"])

>      ,p "not not true" (FunCall [] "!not"
>                          [FunCall [] "!not"
>                           [BooleanLit [] True]])
>      ]]

>    ,Group "case expressions" [Expressions [
>       p [$here|
>          case when a,b then 3
>               when c then 4
>               else 5
>          end
>          |]
>         (Case [] [([Identifier [] "a", Identifier [] "b"], IntegerLit [] 3)
>               ,([Identifier [] "c"], IntegerLit [] 4)]
>          (Just $ IntegerLit [] 5))

>      ,p  "case 1 when 2 then 3 else 4 end"
>         (CaseSimple [] (IntegerLit [] 1)
>            [([IntegerLit [] 2], IntegerLit [] 3)]
>          (Just $ IntegerLit [] 4))
>      ]]

>    ,Group "positional args" [Expressions [
>       p "$1" (PositionalArg [] 1)
>      ]]

>    ,Group "exists" [Expressions [
>       p "exists (select 1 from a)"
>       (Exists [] (selectFrom [SelExp [] (IntegerLit [] 1)] (Tref [] "a" NoAlias)))
>      ]]

>    ,Group "in variants" [Expressions [
>       p "t in (1,2)"
>       (InPredicate [] (Identifier [] "t") True (InList [] [IntegerLit [] 1,IntegerLit [] 2]))
>      ,p "t not in (1,2)"
>       (InPredicate [] (Identifier [] "t") False (InList [] [IntegerLit [] 1,IntegerLit [] 2]))
>      ,p "(t,u) in (1,2)"
>       (InPredicate [] (FunCall [] "!rowctor" [Identifier [] "t",Identifier [] "u"]) True
>        (InList [] [IntegerLit [] 1,IntegerLit [] 2]))
>      ,p "3 = any (array[1,2])"
>       (LiftOperator [] "=" LiftAny [IntegerLit [] 3
>                                     ,FunCall [] "!arrayctor" [IntegerLit [] 1
>                                                              ,IntegerLit [] 2]])
>      ,p "3 = all (array[1,2,4])"
>       (LiftOperator [] "=" LiftAll [IntegerLit [] 3
>                                     ,FunCall [] "!arrayctor" [IntegerLit [] 1
>                                                              ,IntegerLit [] 2
>                                                              ,IntegerLit [] 4]])
>      ]]

>    ,Group "comparison operators" [Expressions [
>       p "a < b"
>       (FunCall [] "<" [Identifier [] "a", Identifier [] "b"])
>      ,p "a <> b"
>       (FunCall [] "<>" [Identifier [] "a", Identifier [] "b"])
>      ,p "a != b"
>       (FunCall [] "<>" [Identifier [] "a", Identifier [] "b"])
>      ]]

test some string parsing, want to check single quote behaviour,
and dollar quoting, including nesting.

>    ,Group "string parsing" [Expressions [
>       p "''" (stringQ "")
>      ,p "''''" (stringQ "'")
>      ,p "'test'''" (stringQ "test'")
>      ,p "'''test'" (stringQ "'test")
>      ,p "'te''st'" (stringQ "te'st")
>      ,p "$$test$$" (StringLit [] "$$" "test")
>      ,p "$$te'st$$" (StringLit [] "$$" "te'st")
>      ,p "$st$test$st$" (StringLit [] "$st$" "test")
>      ,p "$outer$te$$yup$$st$outer$" (StringLit [] "$outer$" "te$$yup$$st")
>      ,p "'spl$$it'" (stringQ "spl$$it")
>      ]]
>      ]

================================================================================

select statements

>   ,Group "simple select statements" [
>     Group "select no table" [Statements [
>       p "select 1;" [SelectStatement [] $ selectE (SelectList [] [SelExp [] (IntegerLit [] 1)] [])]
>      ]]
>    ,Group "select from table" [Statements [
>       p "select * from tbl;"
>       [SelectStatement [] $ selectFrom (selIL ["*"]) (Tref [] "tbl" NoAlias)]
>      ,p "select a,b from tbl;"
>       [SelectStatement [] $ selectFrom (selIL ["a", "b"]) (Tref [] "tbl" NoAlias)]

>      ,p "select a,b from inf.tbl;"
>       [SelectStatement [] $ selectFrom (selIL ["a", "b"]) (Tref [] "inf.tbl" NoAlias)]

>      ,p "select distinct * from tbl;"
>       [SelectStatement [] $ Select [] Distinct (SelectList [] (selIL ["*"]) []) [Tref [] "tbl" NoAlias]
>        Nothing [] Nothing [] Nothing Nothing]

>      ,p "select a from tbl where b=2;"
>       [SelectStatement [] $ selectFromWhere
>         (selIL ["a"])
>         (Tref [] "tbl" NoAlias)
>         (FunCall [] "="
>          [Identifier [] "b", IntegerLit [] 2])]
>      ,p "select a from tbl where b=2 and c=3;"
>       [SelectStatement [] $ selectFromWhere
>         (selIL ["a"])
>         (Tref [] "tbl" NoAlias)
>         (FunCall [] "!and"
>          [FunCall [] "="  [Identifier [] "b", IntegerLit [] 2]
>          ,FunCall [] "=" [Identifier [] "c", IntegerLit [] 3]])]

>      ]]

>    ,Group "more select statements" [Statements [
>       p [$here|
>          select a from tbl
>          except
>          select a from tbl1;|]
>       [SelectStatement [] $ CombineSelect [] Except
>        (selectFrom (selIL ["a"]) (Tref [] "tbl" NoAlias))
>        (selectFrom (selIL ["a"]) (Tref [] "tbl1" NoAlias))]
>      ,p "select a from tbl where true\n\
>         \except\n\
>         \select a from tbl1 where true;"
>       [SelectStatement [] $ CombineSelect [] Except
>        (selectFromWhere (selIL ["a"]) (Tref [] "tbl" NoAlias) (BooleanLit [] True))
>        (selectFromWhere (selIL ["a"]) (Tref [] "tbl1" NoAlias) (BooleanLit [] True))]
>      ,p "select a from tbl\n\
>         \union\n\
>         \select a from tbl1;"
>       [SelectStatement [] $ CombineSelect [] Union
>        (selectFrom (selIL ["a"]) (Tref [] "tbl" NoAlias))
>        (selectFrom (selIL ["a"]) (Tref [] "tbl1" NoAlias))]
>      ,p "select a from tbl\n\
>         \union all\n\
>         \select a from tbl1;"
>       [SelectStatement [] $ CombineSelect [] UnionAll
>        (selectFrom (selIL ["a"]) (Tref [] "tbl" NoAlias))
>        (selectFrom (selIL ["a"]) (Tref [] "tbl1" NoAlias))]

>      ,p "(select 1 union select 2) union select 3;"
>       [SelectStatement []
>        (CombineSelect [] Union
>         (CombineSelect [] Union
>          (selectE (SelectList [] [SelExp [] (IntegerLit [] 1)] []))
>          (selectE (SelectList [] [SelExp [] (IntegerLit [] 2)] [])))
>         (selectE (SelectList [] [SelExp [] (IntegerLit [] 3)] [])))]

>      ,p "select 1 union (select 2 union select 3);"
>       [SelectStatement []
>        (CombineSelect [] Union
>         (selectE (SelectList [] [SelExp [] (IntegerLit [] 1)] []))
>         (CombineSelect [] Union
>          (selectE (SelectList [] [SelExp [] (IntegerLit [] 2)] []))
>          (selectE (SelectList [] [SelExp [] (IntegerLit [] 3)] []))))]

>      ,p "select a as b from tbl;"
>       [SelectStatement [] $ selectFrom [SelectItem [] (Identifier [] "a") "b"] (Tref [] "tbl" NoAlias)]
>      ,p "select a + b as b from tbl;"
>       [SelectStatement [] $ selectFrom
>        [SelectItem []
>         (FunCall [] "+"
>          [Identifier [] "a", Identifier [] "b"]) "b"]
>        (Tref [] "tbl" NoAlias)]
>      ,p "select a.* from tbl a;"
>       [SelectStatement [] $ selectFrom (selIL ["a.*"]) (Tref [] "tbl" (TableAlias "a"))]

>      ,p "select a.* from tbl a(b,c);"
>       [SelectStatement [] $ selectFrom (selIL ["a.*"]) (Tref [] "tbl" (FullAlias "a" ["b","c"]))]

>      ,p "select * from t1 a, t2 b;"
>             [SelectStatement []
>              (Select [] Dupes
>               (SelectList []
>                [SelExp [] (Identifier [] "*")] [])
>               [Tref [] "t1" (TableAlias "a"),Tref [] "t2" (TableAlias "b")]
>               Nothing [] Nothing [] Nothing Nothing)]
>      ,p "select a from b inner join c on b.a=c.a;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b" NoAlias) Unnatural Inner (Tref [] "c" NoAlias)
>           (Just (JoinOn []
>            (FunCall [] "=" [Identifier [] "b.a", Identifier [] "c.a"]))) NoAlias)]
>      ,p "select a from b inner join c as d on b.a=d.a;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b" NoAlias) Unnatural Inner (Tref [] "c" (TableAlias "d"))
>           (Just (JoinOn []
>            (FunCall [] "=" [Identifier [] "b.a", Identifier [] "d.a"]))) NoAlias)]

>      ,p "select a from b inner join c using(d,e);"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b" NoAlias) Unnatural Inner (Tref [] "c" NoAlias)
>           (Just (JoinUsing [] ["d","e"])) NoAlias)]

>      ,p "select a from b natural inner join c;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b" NoAlias) Natural Inner (Tref [] "c" NoAlias) Nothing NoAlias)]
>      ,p "select a from b left outer join c;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b" NoAlias) Unnatural LeftOuter (Tref [] "c" NoAlias) Nothing NoAlias)]
>      ,p "select a from b full outer join c;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b" NoAlias) Unnatural FullOuter (Tref [] "c" NoAlias) Nothing NoAlias)]
>      ,p "select a from b right outer join c;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b" NoAlias) Unnatural RightOuter (Tref [] "c" NoAlias) Nothing NoAlias)]
>      ,p "select a from b cross join c;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b" NoAlias) Unnatural Cross (Tref [] "c" NoAlias) Nothing NoAlias)]

>      ,p "select a from (b natural join c);"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b" NoAlias) Natural Inner (Tref [] "c" NoAlias) Nothing NoAlias)]

>      ,p "select x from ((a cross join b) cross join c);"
>        [SelectStatement []
>         (selectFrom (selIL ["x"])
>          (JoinedTref []
>          (JoinedTref []
>           (Tref [] "a" NoAlias)
>            Unnatural Cross
>           (Tref [] "b" NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref [] "c" NoAlias)
>          Nothing NoAlias))]

>      ,p "select x from (a cross join (b cross join c));"
>        [SelectStatement []
>         (selectFrom (selIL ["x"])
>          (JoinedTref []
>           (Tref [] "a" NoAlias)
>           Unnatural Cross
>           (JoinedTref []
>            (Tref [] "b" NoAlias)
>            Unnatural Cross
>            (Tref [] "c" NoAlias)
>            Nothing NoAlias)
>           Nothing NoAlias))]

>      ,p "select x from ((a cross join b) cross join c);"
>        [SelectStatement []
>         (selectFrom (selIL ["x"])
>          (JoinedTref []
>          (JoinedTref []
>           (Tref [] "a" NoAlias)
>            Unnatural Cross
>           (Tref [] "b" NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref [] "c" NoAlias)
>          Nothing NoAlias))]

>      ,p "select x from (a cross join b) cross join c;"
>        [SelectStatement []
>         (selectFrom (selIL ["x"])
>          (JoinedTref []
>          (JoinedTref []
>           (Tref [] "a" NoAlias)
>            Unnatural Cross
>           (Tref [] "b" NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref [] "c" NoAlias)
>          Nothing NoAlias))]

>      ,p "select x from ((a cross join b) cross join c) cross join d;"
>        [SelectStatement []
>         (selectFrom (selIL ["x"])
>          (JoinedTref []
>           (JoinedTref []
>            (JoinedTref []
>             (Tref [] "a" NoAlias)
>             Unnatural Cross
>             (Tref [] "b" NoAlias)
>             Nothing NoAlias)
>            Unnatural Cross
>            (Tref [] "c" NoAlias)
>            Nothing NoAlias)
>           Unnatural Cross
>           (Tref [] "d" NoAlias)
>           Nothing NoAlias))]


>      ,p "select a from b\n\
>         \    inner join c\n\
>         \      on true\n\
>         \    inner join d\n\
>         \      on 1=1;"
>       [SelectStatement [] $ selectFrom
>        [SelExp [] (Identifier [] "a")]
>        (JoinedTref []
>         (JoinedTref [] (Tref [] "b" NoAlias) Unnatural Inner (Tref [] "c" NoAlias)
>          (Just $ JoinOn [] (BooleanLit [] True)) NoAlias)
>         Unnatural Inner (Tref [] "d" NoAlias)
>         (Just  $ JoinOn [] (FunCall [] "="
>                [IntegerLit [] 1, IntegerLit [] 1])) NoAlias)]

>      ,p "select row_number() over(order by a) as place from tbl;"
>       [SelectStatement [] $ selectFrom [SelectItem []
>                    (WindowFn []
>                     (FunCall [] "row_number" [])
>                     []
>                     [Identifier [] "a"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref [] "tbl" NoAlias)]
>      ,p "select row_number() over(order by a asc) as place from tbl;"
>       [SelectStatement [] $ selectFrom [SelectItem []
>                    (WindowFn []
>                     (FunCall [] "row_number" [])
>                     []
>                     [Identifier [] "a"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref [] "tbl" NoAlias)]
>      ,p "select row_number() over(order by a desc) as place from tbl;"
>       [SelectStatement [] $ selectFrom [SelectItem []
>                    (WindowFn []
>                     (FunCall [] "row_number" [])
>                     []
>                     [Identifier [] "a"] Desc FrameUnboundedPreceding)
>                    "place"]
>        (Tref [] "tbl" NoAlias)]
>      ,p "select row_number()\n\
>         \over(partition by (a,b) order by c) as place\n\
>         \from tbl;"
>       [SelectStatement [] $ selectFrom [SelectItem []
>                    (WindowFn []
>                     (FunCall [] "row_number" [])
>                     [FunCall [] "!rowctor" [Identifier [] "a",Identifier [] "b"]]
>                     [Identifier [] "c"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref [] "tbl" NoAlias)]

>      ,p "select * from a natural inner join (select * from b) as a;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["*"])
>        (JoinedTref [] (Tref [] "a" NoAlias) Natural
>         Inner (SubTref [] (selectFrom
>                         (selIL ["*"])
>                         (Tref [] "b" NoAlias)) (TableAlias "a"))
>         Nothing NoAlias)]

>      ,p "select * from a order by c;"
>       [SelectStatement [] $ Select []  Dupes
>        (sl (selIL ["*"]))
>        [Tref [] "a" NoAlias]
>        Nothing [] Nothing [(Identifier [] "c",Asc)] Nothing Nothing]

>      ,p "select *\n\
>            \from Adventure\n\
>            \order by Clicks desc, AdventureID;"
>       [SelectStatement [] $ Select [] Dupes
>        (sl (selIL ["*"]))
>        [Tref [] "Adventure" NoAlias]
>        Nothing [] Nothing [(Identifier [] "Clicks",Desc)
>                           ,(Identifier [] "AdventureID",Asc)] Nothing Nothing]

>      ,p "select * from a order by c,d asc;"
>       [SelectStatement [] $ Select [] Dupes
>        (sl (selIL ["*"]))
>        [Tref [] "a" NoAlias]
>        Nothing [] Nothing [(Identifier [] "c", Asc)
>                           ,(Identifier [] "d", Asc)] Nothing Nothing]

>      ,p "select * from a order by c,d desc;"
>       [SelectStatement [] $ Select [] Dupes
>        (sl (selIL ["*"]))
>        [Tref [] "a" NoAlias]
>        Nothing [] Nothing [(Identifier [] "c", Asc)
>                           ,(Identifier [] "d", Desc)] Nothing Nothing]

>      ,p "select * from a order by c limit 1;"
>       [SelectStatement [] $ Select [] Dupes
>        (sl (selIL ["*"]))
>        [Tref [] "a" NoAlias]
>        Nothing [] Nothing [(Identifier [] "c",Asc)] (Just (IntegerLit [] 1)) Nothing]

>      ,p "select * from a order by c offset 3;"
>       [SelectStatement [] $ Select [] Dupes
>        (sl (selIL ["*"]))
>        [Tref [] "a" NoAlias]
>        Nothing [] Nothing [(Identifier [] "c",Asc)] Nothing (Just $ IntegerLit [] 3)]

>      ,p "select a from (select b from c) as d;"
>         [SelectStatement [] $ selectFrom
>          (selIL ["a"])
>          (SubTref [] (selectFrom
>                    (selIL ["b"])
>                    (Tref [] "c" NoAlias))
>           (TableAlias "d"))]

>      ,p "select * from gen();"
>         [SelectStatement [] $ selectFrom (selIL ["*"]) (TrefFun [] (FunCall [] "gen" []) NoAlias)]
>      ,p "select * from gen() as t;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["*"])
>        (TrefFun [] (FunCall [] "gen" [])(TableAlias  "t"))]

>      ,p "select a, count(b) from c group by a;"
>         [SelectStatement [] $ Select [] Dupes
>          (sl [selI "a", SelExp [] (FunCall [] "count" [Identifier [] "b"])])
>          [Tref [] "c" NoAlias] Nothing [Identifier [] "a"]
>          Nothing [] Nothing Nothing]

>      ,p "select a, count(b) as cnt from c group by a having cnt > 4;"
>         [SelectStatement [] $ Select [] Dupes
>          (sl [selI "a", SelectItem [] (FunCall [] "count" [Identifier [] "b"]) "cnt"])
>          [Tref [] "c" NoAlias] Nothing [Identifier [] "a"]
>          (Just $ FunCall [] ">" [Identifier [] "cnt", IntegerLit [] 4])
>          [] Nothing Nothing]

>      ,p "select a from (select 1 as a, 2 as b) x;"
>         [SelectStatement [] $ selectFrom
>          [selI "a"]
>          (SubTref [] (selectE $ SelectList []
>                                [SelectItem [] (IntegerLit [] 1) "a"
>                                ,SelectItem [] (IntegerLit [] 2) "b"] [])
>                   (TableAlias "x"))]
>      ]]

>    ,Group "multiple statements" [Statements [
>       p "select 1;\nselect 2;" [SelectStatement [] $ selectE $ sl [SelExp [] (IntegerLit [] 1)]
>                                ,SelectStatement [] $ selectE $ sl [SelExp [] (IntegerLit [] 2)]]
>      ]]

>    ,Group "comments" [Statements [
>       p "" []
>      ,p "-- this is a test" []
>      ,p "/* this is\n\
>         \a test*/" []
>      ,p "select 1;\n\
>         \-- this is a test\n\
>         \select -- this is a test\n\
>         \2;" [SelectStatement [] $ selectE $ sl [SelExp [] (IntegerLit [] 1)]
>              ,SelectStatement [] $ selectE $ sl [SelExp [] (IntegerLit [] 2)]
>              ]
>      ,p "select 1;\n\
>         \/* this is\n\
>         \a test*/\n\
>         \select /* this is a test*/2;"
>                     [SelectStatement [] $ selectE $ sl [SelExp [] (IntegerLit [] 1)]
>                     ,SelectStatement [] $ selectE $ sl [SelExp [] (IntegerLit [] 2)]
>                     ]
>      ]]
>      ]

================================================================================

dml statements

>    ,Group "dml" [
>      Group "insert" [Statements [
>       p "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2);\n"
>        [Insert []
>         "testtable"
>         ["columna", "columnb"]
>         (Values [] [[IntegerLit [] 1, IntegerLit [] 2]])
>         Nothing]

multi row insert, test the stand alone values statement first, maybe
that should be in the select section?

>      ,p "values (1,2), (3,4);"
>      [SelectStatement [] $ Values [] [[IntegerLit [] 1, IntegerLit [] 2]
>              ,[IntegerLit [] 3, IntegerLit [] 4]]]

>      ,p "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2), (3,4);\n"
>       [Insert []
>         "testtable"
>         ["columna", "columnb"]
>         (Values [] [[IntegerLit [] 1, IntegerLit [] 2]
>                 ,[IntegerLit [] 3, IntegerLit [] 4]])
>         Nothing]

insert from select

>      ,p "insert into a\n\
>          \    select b from c;"
>       [Insert [] "a" []
>        (selectFrom [selI "b"] (Tref [] "c" NoAlias))
>        Nothing]

>      ,p "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2) returning id;\n"
>       [Insert []
>         "testtable"
>         ["columna", "columnb"]
>         (Values [] [[IntegerLit [] 1, IntegerLit [] 2]])
>         (Just $ sl [selI "id"])]
>      ]]

>     ,Group "update" [Statements [
>       p "update tb\n\
>         \  set x = 1, y = 2;"
>       [Update [] "tb" [SetClause [] "x" (IntegerLit [] 1)
>                    ,SetClause [] "y" (IntegerLit [] 2)]
>        Nothing Nothing]
>      ,p "update tb\n\
>         \  set x = 1, y = 2 where z = true;"
>       [Update [] "tb" [SetClause [] "x" (IntegerLit [] 1)
>                    ,SetClause [] "y" (IntegerLit [] 2)]
>        (Just $ FunCall [] "="
>         [Identifier [] "z", BooleanLit [] True])
>        Nothing]
>      ,p "update tb\n\
>         \  set x = 1, y = 2 returning id;"
>       [Update [] "tb" [SetClause [] "x" (IntegerLit [] 1)
>                    ,SetClause [] "y" (IntegerLit [] 2)]
>        Nothing (Just $ sl [selI "id"])]
>      ,p "update pieces\n\
>         \set a=b returning tag into r.tag;"
>       [Update [] "pieces" [SetClause [] "a" (Identifier [] "b")]
>        Nothing (Just (SelectList []
>                       [SelExp [] (Identifier [] "tag")]
>                       ["r.tag"]))]
>      ,p "update tb\n\
>         \  set (x,y) = (1,2);"
>       [Update [] "tb" [RowSetClause []
>                     ["x","y"]
>                     [IntegerLit [] 1,IntegerLit [] 2]]
>        Nothing Nothing]
>      ]]

>     ,Group "delete" [Statements [
>       p "delete from tbl1 where x = true;"
>       [Delete [] "tbl1" (Just $ FunCall [] "="
>                                [Identifier [] "x", BooleanLit [] True])
>        Nothing]
>      ,p "delete from tbl1 where x = true returning id;"
>       [Delete [] "tbl1" (Just $ FunCall [] "="
>                                [Identifier [] "x", BooleanLit [] True])
>        (Just $ sl [selI "id"])]
>      ]]

>     ,Group "truncate" [Statements [
>       p "truncate test;"
>        [Truncate [] ["test"] ContinueIdentity Restrict]

>      ,p "truncate table test, test2 restart identity cascade;"
>        [Truncate [] ["test","test2"] RestartIdentity Cascade]
>      ]]

copy, bit crap at the moment

>     ,Group "copy" [Statements [
>       p "copy tbl(a,b) from stdin;\n\
>         \bat\tt\n\
>         \bear\tf\n\
>         \\\.\n"
>       [Copy [] "tbl" ["a", "b"] Stdin
>        ,CopyData [] "\
>         \bat\tt\n\
>         \bear\tf\n"]
>      ]]

================================================================================

ddl statements

>    ,Group "ddl" [
>      Group "simple tables" [Statements [
>       p "create table test (\n\
>         \  fielda text,\n\
>         \  fieldb int\n\
>         \);"
>       [CreateTable []
>        "test"
>        [att "fielda" "text"
>        ,att "fieldb" "int"
>        ]
>        []]
>      ,p "create table tbl (\n\
>         \  fld boolean default false);"
>       [CreateTable [] "tbl" [AttributeDef [] "fld" (SimpleTypeName [] "boolean")
>                           (Just $ BooleanLit [] False) []][]]

>      ,p "create table tbl as select 1;"
>       [CreateTableAs [] "tbl"
>        (selectE (SelectList [] [SelExp [] (IntegerLit [] 1)] []))]


>      ,p "alter table a alter column b set default 1;"
>       [AlterTable [] "a" [AlterColumnDefault [] "b" (IntegerLit [] 1)]]

>      ,p "alter table a add constraint unique(b);"
>       [AlterTable [] "a" [AddConstraint [] (UniqueConstraint [] "" ["b"])]]
>      ]]

>     ,Group "others" [Statements [
>       p "create view v1 as\n\
>         \select a,b from t;"
>       [CreateView []
>        "v1"
>        (selectFrom [selI "a", selI "b"] (Tref [] "t" NoAlias))]
>      ,p "create domain td as text check (value in ('t1', 't2'));"
>       [CreateDomain [] "td" (SimpleTypeName [] "text") ""
>        (Just (InPredicate [] (Identifier [] "value") True
>               (InList [] [stringQ "t1" ,stringQ "t2"])))]
>      ,p "create type tp1 as (\n\
>         \  f1 text,\n\
>         \  f2 text\n\
>         \);"
>       [CreateType [] "tp1" [TypeAttDef [] "f1" (SimpleTypeName [] "text")
>                         ,TypeAttDef [] "f2" (SimpleTypeName [] "text")]]

>      ,p "create sequence s start with 5 increment by 4 no maxvalue no minvalue cache 1;"
>         [CreateSequence [] "s" 4 1 ((2::Integer) ^ (63::Integer) - 1) 5 1]

>      ,p "alter sequence s owned by a.b;"
>         [AlterSequence [] "s" "a.b"]

>      ,p "create trigger tr\n\
>          \after insert or delete on tb\n\
>          \for each statement\n\
>          \execute procedure fb();"
>         [CreateTrigger [] "tr" TriggerAfter [TInsert,TDelete] "tb" EachStatement "fb" []]
>      ]]

>     ,Group "drops" [Statements [
>       p "drop domain t;"
>       [DropSomething [] Domain Require ["t"] Restrict]
>      ,p "drop domain if exists t,u cascade;"
>       [DropSomething [] Domain IfExists ["t", "u"] Cascade]
>      ,p "drop domain t restrict;"
>       [DropSomething [] Domain Require ["t"] Restrict]

>      ,p "drop type t;"
>       [DropSomething [] Type Require ["t"] Restrict]
>      ,p "drop table t;"
>       [DropSomething [] Table Require ["t"] Restrict]
>      ,p "drop view t;"
>       [DropSomething [] View Require ["t"] Restrict]

>      ]]

>     ,Group "constraints" [
>       Group "nulls" [Statements [
>       p "create table t1 (\n\
>         \ a text null\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef [] "a" (SimpleTypeName [] "text")
>                            Nothing [NullConstraint [] ""]]
>          []]
>      ,p "create table t1 (\n\
>         \ a text not null\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef [] "a" (SimpleTypeName [] "text")
>                            Nothing [NotNullConstraint [] ""]]
>          []]
>      ]]

>      ,Group "unique" [Statements [
>       p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ unique (x,y)\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [UniqueConstraint [] "" ["x","y"]]]

test arbitrary ordering

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ unique (x),\n\
>         \ y int\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [UniqueConstraint [] "" ["x"]]]

unique row

>      ,p "create table t1 (\n\
>         \ x int unique\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef [] "x" (SimpleTypeName [] "int") Nothing
>                            [RowUniqueConstraint [] ""]][]]

>      ,p "create table t1 (\n\
>         \ x int unique not null\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef [] "x" (SimpleTypeName [] "int") Nothing
>                            [RowUniqueConstraint [] ""
>                            ,NotNullConstraint [] ""]][]]

quick sanity check

>      ,p "create table t1 (\n\
>         \ x int not null unique\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef [] "x" (SimpleTypeName [] "int") Nothing
>                            [NotNullConstraint [] ""
>                            ,RowUniqueConstraint [] ""]][]]
>      ]]

>      ,Group "primary key" [Statements [
>       p "create table t1 (\n\
>         \ x int primary key\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef [] "x" (SimpleTypeName [] "int") Nothing
>                            [RowPrimaryKeyConstraint [] ""]][]]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ primary key (x,y)\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [PrimaryKeyConstraint [] "" ["x", "y"]]]

>      ]]

>      ,Group "check" [Statements [
>       p "create table t (\n\
>         \f text check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable [] "t"
>          [AttributeDef [] "f" (SimpleTypeName [] "text") Nothing
>           [RowCheckConstraint [] "" (InPredicate []
>                                   (Identifier [] "f") True
>                                   (InList [] [stringQ "a", stringQ "b"]))]] []]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ check (x>y)\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [CheckConstraint [] "" (FunCall [] ">" [Identifier [] "x", Identifier [] "y"])]]
>      ]]

>      ,Group "misc" [Statements [
>       p "create table t (\n\
>         \f text not null unique check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable [] "t"
>          [AttributeDef [] "f" (SimpleTypeName [] "text") Nothing
>           [NotNullConstraint [] ""
>            ,RowUniqueConstraint [] ""
>            ,RowCheckConstraint [] "" (InPredicate []
>                                    (Identifier [] "f") True
>                                    (InList [] [stringQ "a"
>                                            ,stringQ "b"]))]] []]

>      ]]

>      ,Group "references" [Statements [
>       p "create table t1 (\n\
>         \ x int references t2\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef [] "x" (SimpleTypeName [] "int") Nothing
>                            [RowReferenceConstraint [] "" "t2" Nothing
>                             Restrict Restrict]][]]

>      ,p "create table t1 (\n\
>         \ x int references t2(y)\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef [] "x" (SimpleTypeName [] "int") Nothing
>                            [RowReferenceConstraint [] "" "t2" (Just "y")
>                             Restrict Restrict]][]]


>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint [] "" ["x", "y"] "t2" []
>           Restrict Restrict]]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2(z,w)\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint [] "" ["x", "y"] "t2" ["z", "w"]
>           Restrict Restrict]]

>      ,p "create table t1 (\n\
>         \ x int references t2 on delete cascade\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef [] "x" (SimpleTypeName [] "int") Nothing
>                            [RowReferenceConstraint [] "" "t2" Nothing
>                             Cascade Restrict]][]]

>      ,p "create table t1 (\n\
>         \ x int references t2 on update cascade\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef [] "x" (SimpleTypeName [] "int") Nothing
>                            [RowReferenceConstraint [] "" "t2" Nothing
>                             Restrict Cascade]][]]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2 on update cascade on delete cascade\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint [] "" ["x", "y"] "t2" []
>           Cascade Cascade]]

>      ]]
>      ]]
>    ,Group "functions" [
>      Group "basics" [Statements [
>       p "create function t1(text) returns text as $$\n\
>         \select a from t1 where b = $1;\n\
>         \$$ language sql stable;"
>       [CreateFunction [] "t1" [ParamDefTp [] $ SimpleTypeName [] "text"]
>        (SimpleTypeName [] "text") Sql "$$"
>        (SqlFnBody []
>         [SelectStatement [] $ selectFromWhere [SelExp [] (Identifier [] "a")] (Tref [] "t1" NoAlias)
>          (FunCall [] "="
>           [Identifier [] "b", PositionalArg [] 1])])
>        Stable]
>      ,p "create function fn() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>       [CreateFunction [] "fn" [] (SimpleTypeName [] "void") Plpgsql "$$"
>        (PlpgsqlFnBody [] [VarDef [] "a" (SimpleTypeName [] "int") Nothing
>                          ,VarDef [] "b" (SimpleTypeName [] "text") Nothing]
>         [NullStatement []])
>        Volatile]
>      ,p "create function fn() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>       [CreateFunction [] "fn" [] (SimpleTypeName [] "void") Plpgsql "$$"
>        (PlpgsqlFnBody [] [VarDef [] "a" (SimpleTypeName [] "int") Nothing
>                          ,VarDef [] "b" (SimpleTypeName [] "text") Nothing]
>         [NullStatement []])
>        Volatile]
>      ,p "create function fn(a text[]) returns int[] as $$\n\
>         \declare\n\
>         \  b xtype[] := '{}';\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql immutable;"
>       [CreateFunction [] "fn"
>        [ParamDef [] "a" $ ArrayTypeName [] $ SimpleTypeName [] "text"]
>        (ArrayTypeName [] $ SimpleTypeName [] "int") Plpgsql "$$"
>        (PlpgsqlFnBody []
>         [VarDef [] "b" (ArrayTypeName [] $ SimpleTypeName [] "xtype") (Just $ stringQ "{}")]
>         [NullStatement []])
>        Immutable]
>      ,p "create function fn() returns void as '\n\
>         \declare\n\
>         \  a int := 3;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction [] "fn" [] (SimpleTypeName [] "void") Plpgsql "'"
>        (PlpgsqlFnBody [] [VarDef [] "a" (SimpleTypeName [] "int") (Just $ IntegerLit [] 3)]
>         [NullStatement []])
>        Stable]
>      ,p "create function fn() returns setof int as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction [] "fn" []
>        (SetOfTypeName [] $ SimpleTypeName [] "int") Plpgsql "$$"
>        (PlpgsqlFnBody [] [] [NullStatement []])
>        Stable]
>      ,p "create function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction [] "fn" []
>        (SimpleTypeName [] "void") Plpgsql "$$"
>        (PlpgsqlFnBody [] [] [NullStatement []])
>        Stable]
>      ,p "drop function test(text);"
>       [DropFunction [] Require [("test",[SimpleTypeName [] "text"])] Restrict]
>      ,p "drop function if exists a(),test(text) cascade;"
>       [DropFunction [] IfExists [("a",[])
>                           ,("test",[SimpleTypeName [] "text"])] Cascade]
>     ]]

>     ,Group "simple plpgsql statements" [PlpgsqlStatements [
>       p "success := true;"
>       [Assignment [] "success" (BooleanLit [] True)]
>      ,p "success = true;"
>       [Assignment [] "success" (BooleanLit [] True)]
>      ,p "return true;"
>       [Return [] $ Just (BooleanLit [] True)]
>      ,p "return;"
>       [Return [] Nothing]
>      ,p "return next 1;"
>       [ReturnNext [] $ IntegerLit [] 1]
>      ,p "return query select a from b;"
>       [ReturnQuery [] $ selectFrom [selI "a"] (Tref [] "b" NoAlias)]
>      ,p "raise notice 'stuff %', 1;"
>       [Raise [] RNotice "stuff %" [IntegerLit [] 1]]
>      ,p "perform test();"
>       [Perform [] $ FunCall [] "test" []]
>      ,p "perform test(a,b);"
>       [Perform [] $ FunCall [] "test" [Identifier [] "a", Identifier [] "b"]]
>      ,p "perform test(r.relvar_name || '_and_stuff');"
>       [Perform [] $ FunCall [] "test" [
>                     FunCall [] "||" [Identifier [] "r.relvar_name"
>                                 ,stringQ "_and_stuff"]]]
>      ,p "select into a,b c,d from e;"
>       [SelectStatement [] $ Select [] Dupes (SelectList [] [selI "c", selI "d"] ["a", "b"])
>                   [Tref [] "e" NoAlias] Nothing [] Nothing [] Nothing Nothing]
>      ,p "select c,d into a,b from e;"
>       [SelectStatement [] $ Select [] Dupes (SelectList [] [selI "c", selI "d"] ["a", "b"])
>                   [Tref [] "e" NoAlias] Nothing [] Nothing [] Nothing Nothing]

>      ,p "execute s;"
>       [Execute [] (Identifier [] "s")]
>      ,p "execute s into r;"
>       [ExecuteInto [] (Identifier [] "s") ["r"]]

>      ,p "continue;" [ContinueStatement []]
>     ]]

>     ,Group "other plpgsql statements" [PlpgsqlStatements [
>       p "for r in select a from tbl loop\n\
>         \null;\n\
>         \end loop;"
>       [ForSelectStatement [] "r" (selectFrom  [selI "a"] (Tref [] "tbl" NoAlias))
>        [NullStatement []]]
>      ,p "for r in select a from tbl where true loop\n\
>         \null;\n\
>         \end loop;"
>       [ForSelectStatement [] "r"
>        (selectFromWhere [selI "a"] (Tref [] "tbl" NoAlias) (BooleanLit [] True))
>        [NullStatement []]]
>      ,p "for r in 1 .. 10 loop\n\
>         \null;\n\
>         \end loop;"
>       [ForIntegerStatement [] "r"
>        (IntegerLit [] 1) (IntegerLit [] 10)
>        [NullStatement []]]

>      ,p "if a=b then\n\
>         \  update c set d = e;\n\
>         \end if;"
>       [If [] [((FunCall [] "=" [Identifier [] "a", Identifier [] "b"])
>           ,[Update [] "c" [SetClause [] "d" (Identifier [] "e")] Nothing Nothing])]
>        []]
>      ,p "if true then\n\
>         \  null;\n\
>         \else\n\
>         \  null;\n\
>         \end if;"
>       [If [] [((BooleanLit [] True),[NullStatement []])]
>        [NullStatement []]]
>      ,p "if true then\n\
>         \  null;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \end if;"
>       [If [] [((BooleanLit [] True), [NullStatement []])
>           ,((BooleanLit [] False), [Return [] Nothing])]
>        []]
>      ,p "if true then\n\
>         \  null;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \else\n\
>         \  return;\n\
>         \end if;"
>       [If [] [((BooleanLit [] True), [NullStatement []])
>           ,((BooleanLit [] False), [Return [] Nothing])
>           ,((BooleanLit [] False), [Return [] Nothing])]
>        [Return [] Nothing]]
>      ,p "case a\n\
>         \  when b then null;\n\
>         \  when c,d then null;\n\
>         \  else null;\n\
>         \end case;"
>      [CaseStatement [] (Identifier [] "a")
>       [([Identifier [] "b"], [NullStatement []])
>       ,([Identifier [] "c", Identifier [] "d"], [NullStatement []])]
>       [NullStatement []]]

>     ]]

>    ,Group "misc" [Statements [
>       p "SET search_path TO my_schema, public;"
>         [Set [] "search_path" [SetId [] "my_schema"
>                               ,SetId [] "public"]]
>      ,p "SET t1 = 3;"
>         [Set [] "t1" [SetNum [] 3]]
>      ,p "SET t1 = 'stuff';"
>         [Set [] "t1" [SetStr [] "stuff"]]
>      ,p "create language plpgsql;"
>         [CreateLanguage [] "plpgsql"]

>     ]]

>     ]]]

================================================================================

shortcuts for constructing test data and asts

> p :: t -> t1 -> (t, t1)
> p a b = (a,b)

> stringQ :: String -> Expression
> stringQ = StringLit [] "'"

> selectFrom :: SelectItemList
>            -> TableRef
>            -> SelectExpression
> selectFrom selList frm = Select [] Dupes (SelectList [] selList [])
>                            [frm] Nothing [] Nothing [] Nothing Nothing

> selectE :: SelectList -> SelectExpression
> selectE selList = Select [] Dupes selList
>                     [] Nothing [] Nothing [] Nothing Nothing

> selIL :: [String] -> [SelectItem]
> selIL = map selI

> selI :: String -> SelectItem
> selI = SelExp [] . Identifier []

> sl :: SelectItemList -> SelectList
> sl a = SelectList [] a []

> selectFromWhere :: SelectItemList
>                 -> TableRef
>                 -> Expression
>                 -> SelectExpression
> selectFromWhere selList frm whr =
>     Select [] Dupes (SelectList [] selList [])
>                [frm] (Just whr) [] Nothing [] Nothing Nothing

> att :: String -> String -> AttributeDef
> att n t = AttributeDef [] n (SimpleTypeName [] t) Nothing []


================================================================================

Unit test helpers

> itemToTft :: Item -> [Test.Framework.Test]
> itemToTft (Expressions es) = map (uncurry testParseExpression) es
> itemToTft (PlpgsqlStatements es) = map (uncurry testParsePlpgsqlStatements) es
> itemToTft (Statements es) = map (uncurry testParseStatements) es
> itemToTft (Group s is) = [testGroup s $ concatMap itemToTft is]


> testParseExpression :: String -> Expression -> Test.Framework.Test
> testParseExpression src ast = parseUtil src ast
>                                  (parseExpression "") printExpression

> testParseStatements :: String -> [Statement] -> Test.Framework.Test
> testParseStatements src ast = parseUtil src ast (parseSql "") printSql

> testParsePlpgsqlStatements :: String -> [Statement] -> Test.Framework.Test
> testParsePlpgsqlStatements src ast = parseUtil src ast (parsePlpgsql "") printSql

> parseUtil :: (Show t, Eq b, Show b, Data b) =>
>              String
>           -> b
>           -> ([Char] -> Either t b)
>           -> (b -> [Char])
>           -> Test.Framework.Test
> parseUtil src ast parser printer = testCase ("parse " ++ src) $ do
>   case parser src of
>     Left er -> assertFailure $ show er
>     Right ast' -> do
>       assertEqual ("parse " ++ src) ast $ stripAnnotations ast'
>       case parser (printer ast) of
>         Left er -> assertFailure $ "reparse\n" ++ show er ++ "\n" -- ++ pp ++ "\n"
>         Right ast'' -> assertEqual ("reparse " ++ (printer ast)) ast $ stripAnnotations ast''
