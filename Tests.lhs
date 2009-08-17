#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written in a tdd style, which the coverage of the tests reflects.

Also had some quickcheck stuff, but it got disabled since it failed
depressingly often and the code has now gone very stale. The idea with
this was to generate random parse trees, pretty print then parse them,
and check the new parse tree was the same as the original.

There are no tests for invalid sql at the moment.

> import Test.HUnit

 > import Test.QuickCheck

> import Test.Framework
> import Test.Framework.Providers.HUnit

> import Text.Parsec.Error (ParseError)

> import Data.Char

 > import Test.Framework.Providers.QuickCheck2

 > import Control.Monad

> import Tree
> import Parser
> import PrettyPrinter

> main :: IO ()
> main =
>   defaultMain [

================================================================================

uses a whole bunch of shortcuts (at the bottom of main) to make this
code more concise. Could probably use a few more.

>     testGroup "parse expression"
>     (mapExpr [

start with some really basic expressions, we just use the expression
parser rather than the full sql statement parser. (the expression parser
requires a single expression followed by eof.)

>       p "1" (IntegerL 1)
>      ,p "-1" (UnOpCall Neg (IntegerL 1))
>      ,p "1.1" (FloatL 1.1)
>      ,p "-1.1" (UnOpCall Neg (FloatL 1.1))
>      ,p " 1 + 1 " (BinOpCall Plus (IntegerL 1) (IntegerL 1))
>      ,p "1+1+1" (BinOpCall
>                  Plus
>                  (BinOpCall Plus (IntegerL 1) (IntegerL 1))
>                  (IntegerL 1))

check some basic parens use wrt naked values and row constructors
these tests reflect how pg seems to intrepret the variants.

>      ,p "(1)" (IntegerL 1)
>      ,p "row ()" (Row [])
>      ,p "row (1)" (Row [IntegerL 1])
>      ,p "row (1,2)" (Row [IntegerL 1,IntegerL 2])
>      ,p "(1,2)" (Row [IntegerL 1,IntegerL 2])

test some more really basic expressions

>      ,p "'test'" (StringL "test")
>      ,p "''" (StringL "")
>      ,p "hello" (Identifier "hello")
>      ,p "helloTest" (Identifier "helloTest")
>      ,p "hello_test" (Identifier "hello_test")
>      ,p "hello1234" (Identifier "hello1234")
>      ,p "true" (BooleanL True)
>      ,p "false" (BooleanL False)
>      ,p "null" NullL

array selector

>      ,p "array[1,2]" (ArrayL [IntegerL 1, IntegerL 2])

array subscripting

>      ,p "a[1]" (ArraySub (Identifier "a") [IntegerL 1])

we just produce a parse tree, so no type checking or anything like
that is done

some operator tests

>      ,p "1 + tst1" (BinOpCall
>                     Plus (IntegerL 1) (Identifier "tst1"))
>      ,p "tst1 + 1" (BinOpCall
>                     Plus (Identifier "tst1") (IntegerL 1))
>      ,p "tst + tst1" (BinOpCall
>                       Plus (Identifier "tst") (Identifier "tst1"))
>      ,p "'a' || 'b'" (BinOpCall Conc (StringL "a")
>                                              (StringL "b"))
>      ,p "'stuff'::text" (BinOpCall
>                          Cast (StringL "stuff") (Identifier "text"))
>      ,p "245::float(24)" (BinOpCall
>                          Cast (IntegerL 245)
>                           (FunCall "float" [IntegerL 24]))

>      ,p "a between 1 and 3"
>         (Between (Identifier "a") (IntegerL 1) (IntegerL 3))
>      ,p "cast(a as text)"
>         (CastKeyword (Identifier "a") (SimpleType "text"))
>      ,p "@ a"
>         (UnOpCall Abs (Identifier "a"))

>      ,p "substring(a from 0 for 3)"
>         (Substring (Identifier "a") (IntegerL 0) (IntegerL 3))

>      ,p "substring(a from 0 for (5 - 3))"
>         (Substring (Identifier "a") (IntegerL 0)
>          (BinOpCall Minus (IntegerL 5) (IntegerL 3)))


some function call tests

>      ,p "fn()" (FunCall "fn" [])
>      ,p "fn(1)" (FunCall "fn" [IntegerL 1])
>      ,p "fn('test')" (FunCall "fn" [StringL "test"])
>      ,p "fn(1,'test')" (FunCall "fn" [IntegerL 1, StringL "test"])
>      ,p "fn('test')" (FunCall "fn" [StringL "test"])

simple whitespace sanity checks

>      ,p "fn (1)" (FunCall "fn" [IntegerL 1])
>      ,p "fn( 1)" (FunCall "fn" [IntegerL 1])
>      ,p "fn(1 )" (FunCall "fn" [IntegerL 1])
>      ,p "fn(1) " (FunCall "fn" [IntegerL 1])

null stuff

>      ,p "not null" (UnOpCall Not NullL)
>      ,p "a is null" (UnOpCall IsNull (Identifier "a"))
>      ,p "a is not null" (UnOpCall
>                          IsNotNull (Identifier "a"))

some slightly more complex stuff

>      ,p "case when a then 3\n\
>         \     when b then 4\n\
>         \     else 5\n\
>         \end"
>         (Case [(Identifier "a", IntegerL 3)
>               ,(Identifier "b", IntegerL 4)]
>          (Just $ IntegerL 5))

positional args used in sql and sometimes plpgsql functions

>      ,p "$1" (PositionalArg 1)

>      ,p "exists (select 1 from a)"
>       (Exists (selectFrom [SelExp (IntegerL 1)] (Tref "a")))

in variants, including using row constructors

>      ,p "t in (1,2)"
>       (InPredicate (Identifier "t") True (InList [IntegerL 1,IntegerL 2]))
>      ,p "t not in (1,2)"
>       (InPredicate (Identifier "t") False (InList [IntegerL 1,IntegerL 2]))
>      ,p "(t,u) in (1,2)"
>       (InPredicate (Row [Identifier "t",Identifier "u"]) True
>        (InList [IntegerL 1,IntegerL 2]))

operator issues:
<> appears below < in the precedence table, this caused
<> to not parse properly

>      ,p "a < b"
>       (BinOpCall Lt (Identifier "a") (Identifier "b"))
>      ,p "a <> b"
>       (BinOpCall NotEql (Identifier "a") (Identifier "b"))
>      ,p "a != b"
>       (BinOpCall NotEql (Identifier "a") (Identifier "b"))

>      ])


================================================================================

test some string parsing, want to check single quote behaviour,
and dollar quoting, including nesting.

>     ,testGroup "string parsing"
>     (mapExpr [
>       p "''" (StringL "")
>      ,p "''''" (StringL "'")
>      ,p "'test'''" (StringL "test'")
>      ,p "'''test'" (StringL "'test")
>      ,p "'te''st'" (StringL "te'st")
>      ,p "$$test$$" (StringLD "" "test")
>      ,p "$$te'st$$" (StringLD "" "te'st")
>      ,p "$st$test$st$" (StringLD "st" "test")
>      ,p "$outer$te$$yup$$st$outer$" (StringLD "outer" "te$$yup$$st")
>      ,p "'spl$$it'" (StringL "spl$$it")
>      ])

================================================================================

first statement, pretty simple

>     ,testGroup "select expression"
>     (mapSql [
>       p "select 1;" [selectE (SelectList [SelExp (IntegerL 1)] [])]
>      ])

================================================================================

test a whole bunch more select statements

>     ,testGroup "select from table"
>     (mapSql [
>       p "select * from tbl;"
>       [selectFrom (selIL ["*"]) (Tref "tbl")]
>      ,p "select a,b from tbl;"
>       [selectFrom (selIL ["a", "b"]) (Tref "tbl")]

>      ,p "select a,b from inf.tbl;"
>       [selectFrom (selIL ["a", "b"]) (Tref "inf.tbl")]

>      ,p "select distinct * from tbl;"
>       [Select Distinct (SelectList (selIL ["*"]) []) (Just $ Tref "tbl")
>        Nothing [] [] Asc Nothing Nothing]

>      ,p "select a from tbl where b=2;"
>       [selectFromWhere
>         (selIL ["a"])
>         (Tref "tbl")
>         (BinOpCall Eql
>          (Identifier "b") (IntegerL 2))]
>      ,p "select a from tbl where b=2 and c=3;"
>       [selectFromWhere
>         (selIL ["a"])
>         (Tref "tbl")
>         (BinOpCall And
>          (BinOpCall Eql
>           (Identifier "b") (IntegerL 2))
>          (BinOpCall Eql
>           (Identifier "c") (IntegerL 3)))]

>      ,p "select a from tbl\n\
>         \except\n\
>         \select a from tbl1;"
>       [CombineSelect Except
>        (selectFrom (selIL ["a"]) (Tref "tbl"))
>        (selectFrom (selIL ["a"]) (Tref "tbl1"))]
>      ,p "select a from tbl where true\n\
>         \except\n\
>         \select a from tbl1 where true;"
>       [CombineSelect Except
>        (selectFromWhere (selIL ["a"]) (Tref "tbl") (BooleanL True))
>        (selectFromWhere (selIL ["a"]) (Tref "tbl1") (BooleanL True))]
>      ,p "select a from tbl\n\
>         \union\n\
>         \select a from tbl1;"
>       [CombineSelect Union
>        (selectFrom (selIL ["a"]) (Tref "tbl"))
>        (selectFrom (selIL ["a"]) (Tref "tbl1"))]
>      ,p "select a from tbl\n\
>         \union all\n\
>         \select a from tbl1;"
>       [CombineSelect UnionAll
>        (selectFrom (selIL ["a"]) (Tref "tbl"))
>        (selectFrom (selIL ["a"]) (Tref "tbl1"))]

>      ,p "select a as b from tbl;"
>       [selectFrom [SelectItem (Identifier "a") "b"] (Tref "tbl")]
>      ,p "select a + b as b from tbl;"
>       [selectFrom
>        [SelectItem
>         (BinOpCall Plus
>          (Identifier "a") (Identifier "b")) "b"]
>        (Tref "tbl")]
>      ,p "select a.* from tbl a;"
>       [selectFrom (selIL ["a.*"]) (TrefAlias "tbl" "a")]

>      ,p "select a from b inner join c on b.a=c.a;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") Unnatural Inner (Tref "c")
>           (Just (JoinOn
>            (BinOpCall Eql (Identifier "b.a") (Identifier "c.a")))))]
>      ,p "select a from b inner join c as d on b.a=d.a;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") Unnatural Inner (TrefAlias "c" "d")
>           (Just (JoinOn
>            (BinOpCall Eql (Identifier "b.a") (Identifier "d.a")))))]

>      ,p "select a from b inner join c using(d,e);"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") Unnatural Inner (Tref "c")
>           (Just (JoinUsing ["d","e"])))]

>      ,p "select a from b natural inner join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") Natural Inner (Tref "c") Nothing)]
>      ,p "select a from b left outer join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") Unnatural LeftOuter (Tref "c") Nothing)]
>      ,p "select a from b full outer join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") Unnatural FullOuter (Tref "c") Nothing)]
>      ,p "select a from b right outer join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") Unnatural RightOuter (Tref "c") Nothing)]
>      ,p "select a from b cross join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") Unnatural Cross (Tref "c") Nothing)]

>      ,p "select a from b\n\
>         \    inner join c\n\
>         \      on true\n\
>         \    inner join d\n\
>         \      on 1=1;"
>       [selectFrom
>        [SelExp (Identifier "a")]
>        (JoinedTref
>         (JoinedTref (Tref "b") Unnatural Inner (Tref "c")
>          (Just $ JoinOn (BooleanL True)))
>         Unnatural Inner (Tref "d")
>         (Just  $ JoinOn (BinOpCall Eql
>                (IntegerL 1) (IntegerL 1))))]

>      ,p "select row_number() over(order by a) as place from tbl;"
>       [selectFrom [SelectItem
>                    (WindowFn
>                     (FunCall "row_number" [])
>                     []
>                     [Identifier "a"] Asc)
>                    "place"]
>        (Tref "tbl")]
>      ,p "select row_number() over(order by a asc) as place from tbl;"
>       [selectFrom [SelectItem
>                    (WindowFn
>                     (FunCall "row_number" [])
>                     []
>                     [Identifier "a"] Asc)
>                    "place"]
>        (Tref "tbl")]
>      ,p "select row_number() over(order by a desc) as place from tbl;"
>       [selectFrom [SelectItem
>                    (WindowFn
>                     (FunCall "row_number" [])
>                     []
>                     [Identifier "a"] Desc)
>                    "place"]
>        (Tref "tbl")]
>      ,p "select row_number()\n\
>         \over(partition by (a,b) order by c) as place\n\
>         \from tbl;"
>       [selectFrom [SelectItem
>                    (WindowFn
>                     (FunCall "row_number" [])
>                     [Row [Identifier "a",Identifier "b"]]
>                     [Identifier "c"] Asc)
>                    "place"]
>        (Tref "tbl")]

>      ,p "select * from a natural inner join (select * from b) as a;"
>       [selectFrom
>        (selIL ["*"])
>        (JoinedTref (Tref "a") Natural
>         Inner (SubTref (selectFrom
>                         (selIL ["*"])
>                         (Tref "b")) "a")
>         Nothing)]

>      ,p "select * from a order by c;"
>       [Select Dupes
>        (sl (selIL ["*"]))
>        (Just $ Tref "a")
>        Nothing [] [Identifier "c"] Asc Nothing Nothing]

>      ,p "select * from a order by c,d asc;"
>       [Select Dupes
>        (sl (selIL ["*"]))
>        (Just $ Tref "a")
>        Nothing [] [Identifier "c", Identifier "d"] Asc Nothing Nothing]

>      ,p "select * from a order by c,d desc;"
>       [Select Dupes
>        (sl (selIL ["*"]))
>        (Just $ Tref "a")
>        Nothing [] [Identifier "c", Identifier "d"] Desc Nothing Nothing]

>      ,p "select * from a order by c limit 1;"
>       [Select Dupes
>        (sl (selIL ["*"]))
>        (Just $ Tref "a")
>        Nothing [] [Identifier "c"] Asc (Just (IntegerL 1)) Nothing]

>      ,p "select * from a order by c offset 3;"
>       [Select Dupes
>        (sl (selIL ["*"]))
>        (Just $ Tref "a")
>        Nothing [] [Identifier "c"] Asc Nothing (Just $ IntegerL 3)]

>      ,p "select a from (select b from c) as d;"
>         [selectFrom
>          (selIL ["a"])
>          (SubTref (selectFrom
>                    (selIL ["b"])
>                    (Tref "c"))
>           "d")]

>      ,p "select * from gen();"
>         [selectFrom (selIL ["*"]) (TrefFun $ FunCall "gen" [])]
>      ,p "select * from gen() as t;"
>       [selectFrom
>        (selIL ["*"])
>        (TrefFunAlias (FunCall "gen" []) "t")]

>      ,p "select a, count(b) from c group by a;"
>         [Select Dupes
>          (sl [selI "a", SelExp (FunCall "count" [Identifier "b"])])
>          (Just $ Tref "c") Nothing [Identifier "a"]
>          [] Asc Nothing Nothing]

>      ])

================================================================================

one sanity check for parsing multiple statements

>     ,testGroup "multiple statements"
>     (mapSql [
>       p "select 1;\nselect 2;" [selectE $ sl [SelExp (IntegerL 1)]
>                                ,selectE $ sl [SelExp (IntegerL 2)]]
>      ])

================================================================================

test comment behaviour

>     ,testGroup "comments"
>     (mapSql [
>       p "" []
>      ,p "-- this is a test" []
>      ,p "/* this is\n\
>         \a test*/" []

maybe some people actually put block comments inside parts of
statements when they program?

>      ,p "select 1;\n\
>         \-- this is a test\n\
>         \select -- this is a test\n\
>         \2;" [selectE $ sl [SelExp (IntegerL 1)]
>              ,selectE $ sl [SelExp (IntegerL 2)]
>              ]
>      ,p "select 1;\n\
>         \/* this is\n\
>         \a test*/\n\
>         \select /* this is a test*/2;"
>                     [selectE $ sl [SelExp (IntegerL 1)]
>                     ,selectE $ sl [SelExp (IntegerL 2)]
>                     ]
>      ])

================================================================================

dml statements

>     ,testGroup "dml"
>     (mapSql [

simple insert

>       p "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2);\n"
>       [Insert
>         "testtable"
>         ["columna", "columnb"]
>         (Values [[IntegerL 1, IntegerL 2]])
>         Nothing]

multi row insert, test the stand alone values statement first, maybe
that should be in the select section?

>      ,p "values (1,2), (3,4);"
>      [Values [[IntegerL 1, IntegerL 2]
>              ,[IntegerL 3, IntegerL 4]]]

>      ,p "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2), (3,4);\n"
>       [Insert
>         "testtable"
>         ["columna", "columnb"]
>         (Values [[IntegerL 1, IntegerL 2]
>                 ,[IntegerL 3, IntegerL 4]])
>         Nothing]

insert from select

>      ,p "insert into a\n\
>          \    select b from c;"
>       [Insert "a" []
>        (selectFrom [selI "b"] (Tref "c"))
>        Nothing]

>      ,p "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2) returning id;\n"
>       [Insert
>         "testtable"
>         ["columna", "columnb"]
>         (Values [[IntegerL 1, IntegerL 2]])
>         (Just $ sl [selI "id"])]

updates

>      ,p "update tb\n\
>         \  set x = 1, y = 2;"
>       [Update "tb" [SetClause "x" (IntegerL 1)
>                    ,SetClause "y" (IntegerL 2)]
>        Nothing Nothing]
>      ,p "update tb\n\
>         \  set x = 1, y = 2 where z = true;"
>       [Update "tb" [SetClause "x" (IntegerL 1)
>                    ,SetClause "y" (IntegerL 2)]
>        (Just $ BinOpCall Eql
>         (Identifier "z") (BooleanL True))
>        Nothing]
>      ,p "update tb\n\
>         \  set x = 1, y = 2 returning id;"
>       [Update "tb" [SetClause "x" (IntegerL 1)
>                    ,SetClause "y" (IntegerL 2)]
>        Nothing (Just $ sl [selI "id"])]
>      ,p "update pieces\n\
>         \set a=b returning tag into r.tag;"
>       [Update "pieces" [SetClause "a" (Identifier "b")]
>        Nothing (Just (SelectList
>                       [SelExp (Identifier "tag")]
>                       ["r.tag"]))]
>      ,p "update tb\n\
>         \  set (x,y) = (1,2);"
>       [Update "tb" [RowSetClause
>                     ["x","y"]
>                     [IntegerL 1,IntegerL 2]]
>        Nothing Nothing]

delete

>      ,p "delete from tbl1 where x = true;"
>       [Delete "tbl1" (Just $ BinOpCall Eql
>                                (Identifier "x") (BooleanL True))
>        Nothing]
>      ,p "delete from tbl1 where x = true returning id;"
>       [Delete "tbl1" (Just $ BinOpCall Eql
>                                (Identifier "x") (BooleanL True))
>        (Just $ sl [selI "id"])]

>     ,p "truncate test;"
>        [Truncate ["test"] ContinueIdentity Restrict]

>     ,p "truncate table test, test2 restart identity cascade;"
>        [Truncate ["test","test2"] RestartIdentity Cascade]

copy, bit crap at the moment

>      ,p "copy tbl(a,b) from stdin;\n\
>         \bat	t\n\
>         \bear	f\n\
>         \\\.\n"
>       [Copy "tbl(a,b) from stdin;\n\
>         \bat	t\n\
>         \bear	f\n\
>             \\\.\n"]
>      ])

================================================================================

some ddl

>     ,testGroup "create"
>     (mapSql [

create table tests

>       p "create table test (\n\
>         \  fielda text,\n\
>         \  fieldb int\n\
>         \);"
>       [CreateTable
>        "test"
>        [AttributeDef "fielda" "text" Nothing []
>        ,AttributeDef "fieldb" "int" Nothing []
>        ]
>        []]
>      ,p "create table tbl (\n\
>         \  fld boolean default false);"
>       [CreateTable "tbl" [AttributeDef "fld" "boolean"
>                           (Just $ BooleanL False) []][]]

>      ,p "create table tbl as select 1;"
>       [CreateTableAs "tbl"
>        (selectE (SelectList [SelExp (IntegerL 1)] []))]

other creates

>      ,p "create view v1 as\n\
>         \select a,b from t;"
>       [CreateView
>        "v1"
>        (selectFrom [selI "a", selI "b"] (Tref "t"))]
>      ,p "create domain td as text check (value in ('t1', 't2'));"
>       [CreateDomain "td" "text"
>        (Just (InPredicate (Identifier "value") True
>               (InList [StringL "t1" ,StringL "t2"])))]
>      ,p "create type tp1 as (\n\
>         \  f1 text,\n\
>         \  f2 text\n\
>         \);"
>       [CreateType "tp1" [TypeAttDef "f1" "text"
>                         ,TypeAttDef "f2" "text"]]

drops

>      ,p "drop domain t;"
>       [DropSomething Domain Require ["t"] Restrict]
>      ,p "drop domain if exists t,u cascade;"
>       [DropSomething Domain IfExists ["t", "u"] Cascade]
>      ,p "drop domain t restrict;"
>       [DropSomething Domain Require ["t"] Restrict]

>      ,p "drop type t;"
>       [DropSomething Type Require ["t"] Restrict]
>      ,p "drop table t;"
>       [DropSomething Table Require ["t"] Restrict]
>      ,p "drop view t;"
>       [DropSomething View Require ["t"] Restrict]

>      ])

constraints

>     ,testGroup "constraints"
>     (mapSql [

nulls

>       p "create table t1 (\n\
>         \ a text null\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "a" "text"
>                            Nothing [NullConstraint]]
>          []]
>      ,p "create table t1 (\n\
>         \ a text not null\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "a" "text"
>                            Nothing [NotNullConstraint]]
>          []]

unique table

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ unique (x,y)\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing []
>                           ,AttributeDef "y" "int" Nothing []]
>          [UniqueConstraint ["x","y"]]]

test arbitrary ordering

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ unique (x),\n\
>         \ y int\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing []
>                           ,AttributeDef "y" "int" Nothing []]
>          [UniqueConstraint ["x"]]]

unique row

>      ,p "create table t1 (\n\
>         \ x int unique\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing
>                            [RowUniqueConstraint]][]]

>      ,p "create table t1 (\n\
>         \ x int unique not null\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing
>                            [RowUniqueConstraint
>                            ,NotNullConstraint]][]]

quick sanity check

>      ,p "create table t1 (\n\
>         \ x int not null unique\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing
>                            [NotNullConstraint
>                            ,RowUniqueConstraint]][]]

primary key row, table

>      ,p "create table t1 (\n\
>         \ x int primary key\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing
>                            [RowPrimaryKeyConstraint]][]]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ primary key (x,y)\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing []
>                           ,AttributeDef "y" "int" Nothing []]
>          [PrimaryKeyConstraint ["x", "y"]]]

check row, table

>      ,p "create table t (\n\
>         \f text check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable "t"
>          [AttributeDef "f" "text" Nothing
>           [RowCheckConstraint (InPredicate
>                                   (Identifier "f") True
>                                   (InList [StringL "a", StringL "b"]))]] []]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ check (x>y)\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing []
>                           ,AttributeDef "y" "int" Nothing []]
>          [CheckConstraint (BinOpCall Gt (Identifier "x") (Identifier "y"))]]

row, whole load of constraints, todo: add reference here

>      ,p "create table t (\n\
>         \f text not null unique check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable "t"
>          [AttributeDef "f" "text" Nothing
>           [NotNullConstraint
>            ,RowUniqueConstraint
>            ,RowCheckConstraint (InPredicate
>                                    (Identifier "f") True
>                                    (InList [StringL "a"
>                                            ,StringL "b"]))]] []]

reference row, table

>      ,p "create table t1 (\n\
>         \ x int references t2\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing
>                            [RowReferenceConstraint "t2" []
>                             Restrict Restrict]][]]

>      ,p "create table t1 (\n\
>         \ x int references t2(y)\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing
>                            [RowReferenceConstraint "t2" ["y"]
>                             Restrict Restrict]][]]


>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing []
>                           ,AttributeDef "y" "int" Nothing []]
>          [ReferenceConstraint ["x", "y"] "t2" []
>           Restrict Restrict]]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2(z,w)\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing []
>                           ,AttributeDef "y" "int" Nothing []]
>          [ReferenceConstraint ["x", "y"] "t2" ["z", "w"]
>           Restrict Restrict]]

>      ,p "create table t1 (\n\
>         \ x int references t2 on delete cascade\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing
>                            [RowReferenceConstraint "t2" []
>                             Cascade Restrict]][]]

>      ,p "create table t1 (\n\
>         \ x int references t2 on update cascade\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing
>                            [RowReferenceConstraint "t2" []
>                             Restrict Cascade]][]]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2 on delete cascade on update cascade\n\
>         \);"
>         [CreateTable "t1" [AttributeDef "x" "int" Nothing []
>                           ,AttributeDef "y" "int" Nothing []]
>          [ReferenceConstraint ["x", "y"] "t2" []
>           Cascade Cascade]]

>      ])

================================================================================

test functions

>     ,testGroup "functions"
>     (mapSql [
>       p "create function t1(text) returns text as $$\n\
>         \select a from t1 where b = $1;\n\
>         \$$ language sql stable;"
>       [CreateFunction Sql "t1" [ParamDefTp $ SimpleType "text"]
>        (SimpleType "text") "$$"
>        (SqlFnBody
>         [selectFromWhere [SelExp (Identifier "a")] (Tref "t1")
>          ((BinOpCall Eql
>            (Identifier "b") (PositionalArg 1)))])
>        Stable]
>      ,p "create function fn() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>       [CreateFunction Plpgsql "fn" [] (SimpleType "void") "$$"
>        (PlpgsqlFnBody [VarDef "a" (SimpleType "int") Nothing
>                       ,VarDef "b" (SimpleType "text") Nothing]
>         [NullStatement])
>        Volatile]
>      ,p "create function fn() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>       [CreateFunction Plpgsql "fn" [] (SimpleType "void") "$$"
>        (PlpgsqlFnBody [VarDef "a" (SimpleType "int") Nothing
>                       ,VarDef "b" (SimpleType "text") Nothing]
>         [NullStatement])
>        Volatile]
>      ,p "create function fn(a text[]) returns int[] as $$\n\
>         \declare\n\
>         \  b xtype[] := '{}';\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql immutable;"
>       [CreateFunction Plpgsql "fn"
>        [ParamDef "a" $ ArrayType $ SimpleType "text"]
>        (ArrayType $ SimpleType "int") "$$"
>        (PlpgsqlFnBody
>         [VarDef "b" (ArrayType $ SimpleType "xtype") (Just $ StringL "{}")]
>         [NullStatement])
>        Immutable]
>      ,p "create function fn() returns void as '\n\
>         \declare\n\
>         \  a int := 3;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction Plpgsql "fn" [] (SimpleType "void") "'"
>        (PlpgsqlFnBody [VarDef "a" (SimpleType "int") (Just $ IntegerL 3)]
>         [NullStatement])
>        Stable]
>      ,p "create function fn() returns setof int as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction Plpgsql "fn" []
>        (SetOfType $ SimpleType "int") "$$"
>        (PlpgsqlFnBody [] [NullStatement])
>        Stable]
>      ,p "create function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction Plpgsql "fn" []
>        (SimpleType "void") "$$"
>        (PlpgsqlFnBody [] [NullStatement])
>        Stable]
>      ,p "drop function test(text);"
>       [DropFunction Require [("test",["text"])] Restrict]
>      ,p "drop function if exists a(),test(text) cascade;"
>       [DropFunction IfExists [("a",[])
>                           ,("test",["text"])] Cascade]
>      ])

================================================================================

test non sql plpgsql statements

>     ,testGroup "plpgsqlStatements"
>     (mapPlpgsql [

simple statements

>       p "success := true;"
>       [Assignment "success" (BooleanL True)]
>      ,p "success = true;"
>       [Assignment "success" (BooleanL True)]
>      ,p "return true;"
>       [Return $ Just (BooleanL True)]
>      ,p "return;"
>       [Return Nothing]
>      ,p "return next 1;"
>       [ReturnNext $ IntegerL 1]
>      ,p "return query select a from b;"
>       [ReturnQuery $ selectFrom [selI "a"] (Tref "b")]
>      ,p "raise notice 'stuff %', 1;"
>       [Raise RNotice "stuff %" [IntegerL 1]]
>      ,p "perform test();"
>       [Perform $ FunCall "test" []]
>      ,p "perform test(a,b);"
>       [Perform $ FunCall "test" [Identifier "a", Identifier "b"]]
>      ,p "perform test(r.relvar_name || '_and_stuff');"
>       [Perform $ FunCall "test" [
>                     BinOpCall Conc (Identifier "r.relvar_name")
>                                            (StringL "_and_stuff")]]
>      ,p "select into a,b c,d from e;"
>       [Select Dupes (SelectList [selI "c", selI "d"] ["a", "b"])
>                   (Just $ Tref "e") Nothing [] [] Asc Nothing Nothing]
>      ,p "select c,d into a,b from e;"
>       [Select Dupes (SelectList [selI "c", selI "d"] ["a", "b"])
>                   (Just $ Tref "e") Nothing [] [] Asc Nothing Nothing]

>      ,p "execute s;"
>       [Execute (Identifier "s")]
>      ,p "execute s into r;"
>       [ExecuteInto (Identifier "s") ["r"]]

>      ,p "continue;" [ContinueStatement]

complicated statements

>      ,p "for r in select a from tbl loop\n\
>         \null;\n\
>         \end loop;"
>       [ForSelectStatement "r" (selectFrom  [selI "a"] (Tref "tbl"))
>        [NullStatement]]
>      ,p "for r in select a from tbl where true loop\n\
>         \null;\n\
>         \end loop;"
>       [ForSelectStatement "r"
>        (selectFromWhere [selI "a"] (Tref "tbl") (BooleanL True))
>        [NullStatement]]
>      ,p "for r in 1 .. 10 loop\n\
>         \null;\n\
>         \end loop;"
>       [ForIntegerStatement "r"
>        (IntegerL 1) (IntegerL 10)
>        [NullStatement]]

>      ,p "if a=b then\n\
>         \  update c set d = e;\n\
>         \end if;"
>       [If [((BinOpCall  Eql (Identifier "a") (Identifier "b"))
>           ,[Update "c" [SetClause "d" (Identifier "e")] Nothing Nothing])]
>        []]
>      ,p "if true then\n\
>         \  null;\n\
>         \else\n\
>         \  null;\n\
>         \end if;"
>       [If [((BooleanL True),[NullStatement])]
>        [NullStatement]]
>      ,p "if true then\n\
>         \  null;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \end if;"
>       [If [((BooleanL True), [NullStatement])
>           ,((BooleanL False), [Return Nothing])]
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
>       [If [((BooleanL True), [NullStatement])
>           ,((BooleanL False), [Return Nothing])
>           ,((BooleanL False), [Return Nothing])]
>        [Return Nothing]]
>      ,p "case a\n\
>         \  when b then null;\n\
>         \  when c then null;\n\
>         \  else null;\n\
>         \end case;"
>      [CaseStatement (Identifier "a")
>       [(Identifier "b", [NullStatement])
>       ,(Identifier "c", [NullStatement])]
>       [NullStatement]]
>      ])
>        --,testProperty "random expression" prop_expression_ppp
>        -- ,testProperty "random statements" prop_statements_ppp
>     ]
>         where
>           mapExpr = map $ uncurry checkParseExpression
>           mapSql = map $ uncurry checkParse
>           mapPlpgsql = map $ uncurry checkParsePlpgsql
>           p a b = (a,b)
>           selIL = map selI
>           selI = SelExp . Identifier
>           sl a = SelectList a []
>           selectE selList = Select Dupes selList Nothing Nothing [] [] Asc Nothing Nothing
>           selectFrom selList frm =
>             Select Dupes (SelectList selList [])
>                    (Just frm) Nothing [] [] Asc Nothing Nothing
>           selectFromWhere selList frm whr =
>             Select Dupes (SelectList selList [])
>                    (Just frm) (Just whr) [] [] Asc Nothing Nothing

================================================================================

Unit test helpers

parse and then pretty print and parse a statement

> checkParse :: String -> [Statement] -> Test.Framework.Test
> checkParse src ast = parseUtil src ast parseSql printSql

parse and then pretty print and parse an expression

> checkParseExpression :: String -> Expression -> Test.Framework.Test
> checkParseExpression src ast = parseUtil src ast
>                                  parseExpression printExpression

> checkParsePlpgsql :: String -> [Statement] -> Test.Framework.Test
> checkParsePlpgsql src ast = parseUtil src ast parsePlpgsql printSql

> parseUtil :: (Eq b, Show b) =>
>              String
>           -> b
>           -> (String -> Either ParseError b)
>           -> (b -> String)
>           -> Test.Framework.Test
> parseUtil src ast parser printer = testCase ("parse " ++ src) $ do
>   let ast' = case parser src of
>               Left er -> error $ showEr er src
>               Right l -> l
>   assertEqual ("parse " ++ src) ast ast'
>   -- pretty print then parse to check
>   let pp = printer ast
>   let ast'' = case parser pp of
>               Left er -> error $ "reparse " ++ showEr er pp ++ "\n" ++ pp ++ "\n"
>               Right l -> l
>   assertEqual ("reparse " ++ pp) ast ast''

 > parseSqlThrow :: String -> [Statement]
 > parseSqlThrow s =
 >     case parseSql s of
 >       Left er -> error $ "parse " ++ showEr er s ++ "****" ++ s ++ "****"
 >       Right l -> l

================================================================================

Properties

WELL STALE

Scaffolding to generate random asts which are checked by pretty printing then
parsing them

property

-- > prop_statements_ppp :: [Statement] -> Bool
-- > prop_statements_ppp s = parseSqlThrow (printSql s) == s

-- > prop_expression_ppp :: Expression -> Bool
-- > prop_expression_ppp s = parseExpressionThrow (printExpression s) == s

-- > parseExpressionThrow :: String -> Expression
-- > parseExpressionThrow s =
-- >     case parseExpression s of
-- >       Left er -> error $ "parse " ++ show er ++ "****" ++ s ++ "****"
-- >       Right l -> l



-- arbitrary instances

-- > instance Arbitrary Expression where
-- >     arbitrary = oneof [
-- >                  liftM3 BinOpCall arbitrary arbitrary arbitrary
-- >                 ,liftM IntegerL arbitrary
-- >                 ,liftM StringL aString
-- >                 ,liftM BooleanL arbitrary
-- >                 ,liftM Identifier aIdentifier
-- >                 ,liftM2 FunCall aIdentifier arbitrary
-- >                 ]

-- > instance Arbitrary Statement where
-- >     arbitrary = oneof [
-- >                  liftM selectE arbitrary
-- >                 ,liftM3 Select arbitrary aIdentifier arbitrary
-- >                 ,liftM2 CreateTable aIdentifier arbitrary
-- >                 ,liftM3 Insert aIdentifier arbitrary arbitrary
-- >                 ,liftM3 Update aIdentifier arbitrary arbitrary
-- >                 ]


-- > instance Arbitrary Op where
-- >     arbitrary = elements [Plus, Minus, Mult, Div, Pow, Mod, Eql]

-- > instance Arbitrary SelectList where
-- >     arbitrary = oneof [
-- >                  liftM SelectList arbitrary
-- >                 ,return Star
-- >                 ]

-- > instance Arbitrary AttributeDef where
-- >     arbitrary = liftM3 AttributeDef aIdentifier aIdentifier arbitrary

-- > instance Arbitrary SetClause where
-- >     arbitrary = liftM2 SetClause aIdentifier arbitrary

-- > instance Arbitrary Where where
-- >     arbitrary = liftM Where arbitrary

-- some gen helpers

-- > aString :: Gen String
-- > aString = listOf1 $ choose ('\32', '\126')

-- > aIdentifier :: Gen String
-- > aIdentifier = do
-- >   start <- elements letter
-- >   suffix <- listOf $ elements $ letter ++ "_" ++ ['0' .. '9']
-- >   return (start : suffix)
-- >               where letter = ['A'..'Z'] ++ ['a' .. 'z']
