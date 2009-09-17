Copyright 2009 Jake Wheat

The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written in a tdd style, which the coverage of the tests reflects.

Also had some quickcheck stuff, but it got disabled since it failed
depressingly often and the code has now gone very stale. The idea with
this was to generate random asts, pretty print then parse them, and
check the new ast was the same as the original. Probably worth having
another go at this some time soon.

There are no tests for invalid sql at the moment.

> module Database.HsSqlPpp.ParserTests (parserTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char

> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.PrettyPrinter

> parserTests :: Test.Framework.Test
> parserTests =
>   testGroup "parserTests" [

================================================================================

uses a whole bunch of shortcuts (at the bottom of main) to make this
code more concise. Could probably use a few more.

>     testGroup "parse expression"
>     (mapExpr [

start with some really basic expressions, we just use the expression
parser rather than the full sql statement parser. (the expression parser
requires a single expression followed by eof.)

>       p "1" (IntegerLit [] 1)
>      ,p "-1" (FunCall [] "u-" [IntegerLit [] 1])
>      ,p "1.1" (FloatLit [] 1.1)
>      ,p "-1.1" (FunCall [] "u-" [FloatLit [] 1.1])
>      ,p " 1 + 1 " (FunCall [] "+" [IntegerLit [] 1
>                               ,IntegerLit [] 1])
>      ,p "1+1+1" (FunCall [] "+" [FunCall [] "+" [IntegerLit [] 1
>                                         ,IntegerLit [] 1]
>                             ,IntegerLit [] 1])

check some basic parens use wrt naked values and row constructors
these tests reflect how pg seems to interpret the variants.

>      ,p "(1)" (IntegerLit [] 1)
>      ,p "row ()" (FunCall [] "!rowCtor" [])
>      ,p "row (1)" (FunCall [] "!rowCtor" [IntegerLit [] 1])
>      ,p "row (1,2)" (FunCall [] "!rowCtor" [IntegerLit [] 1,IntegerLit [] 2])
>      ,p "(1,2)" (FunCall [] "!rowCtor" [IntegerLit [] 1,IntegerLit [] 2])

test some more really basic expressions

>      ,p "'test'" (stringQ "test")
>      ,p "''" (stringQ "")
>      ,p "hello" (Identifier [] "hello")
>      ,p "helloTest" (Identifier [] "helloTest")
>      ,p "hello_test" (Identifier [] "hello_test")
>      --,p "\"this is an identifier\"" (Identifier [] "this is an identifier")
>      ,p "hello1234" (Identifier [] "hello1234")
>      ,p "true" (BooleanLit [] True)
>      ,p "false" (BooleanLit [] False)
>      ,p "null" (NullLit [])

array selector

>      ,p "array[1,2]" (FunCall [] "!arrayCtor" [IntegerLit [] 1, IntegerLit [] 2])

array subscripting

>      ,p "a[1]" (FunCall [] "!arraySub" [Identifier [] "a", IntegerLit [] 1])

we just produce a ast, so no type checking or anything like that is
done

some operator tests

>      ,p "1 + tst1" (FunCall [] "+" [IntegerLit [] 1
>                                ,Identifier [] "tst1"])
>      ,p "tst1 + 1" (FunCall [] "+" [Identifier [] "tst1"
>                                ,IntegerLit [] 1])
>      ,p "tst + tst1" (FunCall [] "+" [Identifier [] "tst"
>                                  ,Identifier [] "tst1"])
>      ,p "'a' || 'b'" (FunCall [] "||" [stringQ "a"
>                                   ,stringQ "b"])
>      ,p "'stuff'::text" (Cast [] (stringQ "stuff") (SimpleTypeName "text"))
>      ,p "245::float(24)" (Cast [] (IntegerLit [] 245) (PrecTypeName "float" 24))

>      ,p "a between 1 and 3"
>         (FunCall [] "!between" [Identifier [] "a", IntegerLit [] 1, IntegerLit [] 3])
>      ,p "cast(a as text)"
>         (Cast [] (Identifier [] "a") (SimpleTypeName "text"))
>      ,p "@ a"
>         (FunCall [] "@" [Identifier [] "a"])

>      ,p "substring(a from 0 for 3)"
>         (FunCall [] "!substring" [Identifier [] "a", IntegerLit [] 0, IntegerLit [] 3])

>      ,p "substring(a from 0 for (5 - 3))"
>         (FunCall [] "!substring" [Identifier [] "a",IntegerLit [] 0,
>          FunCall [] "-" [IntegerLit [] 5,IntegerLit [] 3]])
>      ,p "a like b"
>         (FunCall [] "!like" [Identifier [] "a", Identifier [] "b"])

some function call tests

>      ,p "fn()" (FunCall [] "fn" [])
>      ,p "fn(1)" (FunCall [] "fn" [IntegerLit [] 1])
>      ,p "fn('test')" (FunCall [] "fn" [stringQ "test"])
>      ,p "fn(1,'test')" (FunCall [] "fn" [IntegerLit [] 1, stringQ "test"])
>      ,p "fn('test')" (FunCall [] "fn" [stringQ "test"])

simple whitespace sanity checks

>      ,p "fn (1)" (FunCall [] "fn" [IntegerLit [] 1])
>      ,p "fn( 1)" (FunCall [] "fn" [IntegerLit [] 1])
>      ,p "fn(1 )" (FunCall [] "fn" [IntegerLit [] 1])
>      ,p "fn(1) " (FunCall [] "fn" [IntegerLit [] 1])

null stuff

>      ,p "not null" (FunCall [] "!not" [NullLit []])
>      ,p "a is null" (FunCall [] "!isNull" [Identifier [] "a"])
>      ,p "a is not null" (FunCall [] "!isNotNull" [Identifier [] "a"])

some slightly more complex stuff

>      ,p "case when a,b then 3\n\
>         \     when c then 4\n\
>         \     else 5\n\
>         \end"
>         (Case [] [([Identifier [] "a", Identifier [] "b"], IntegerLit [] 3)
>               ,([Identifier [] "c"], IntegerLit [] 4)]
>          (Just $ IntegerLit [] 5))

>      ,p  "case 1 when 2 then 3 else 4 end"
>         (CaseSimple [] (IntegerLit [] 1)
>            [([IntegerLit [] 2], IntegerLit [] 3)]
>          (Just $ IntegerLit [] 4))


positional args used in sql and sometimes plpgsql functions

>      ,p "$1" (PositionalArg [] 1)

>      ,p "exists (select 1 from a)"
>       (Exists [] (selectFrom [SelExp (IntegerLit [] 1)] (Tref [] "a")))

 >       (Exists (makeSelect {
 >                 selSelectList = sle [(IntegerLit [] 1)]
 >                ,selTref = Just $ Tref "a"}))


selectFrom [SelExp (IntegerLit [] 1)] (Tref "a")))

in variants, including using row constructors

>      ,p "t in (1,2)"
>       (InPredicate [] (Identifier [] "t") True (InList [IntegerLit [] 1,IntegerLit [] 2]))
>      ,p "t not in (1,2)"
>       (InPredicate [] (Identifier [] "t") False (InList [IntegerLit [] 1,IntegerLit [] 2]))
>      ,p "(t,u) in (1,2)"
>       (InPredicate [] (FunCall [] "!rowCtor" [Identifier [] "t",Identifier [] "u"]) True
>        (InList [IntegerLit [] 1,IntegerLit [] 2]))

>      ,p "a < b"
>       (FunCall [] "<" [Identifier [] "a", Identifier [] "b"])
>      ,p "a <> b"
>       (FunCall [] "<>" [Identifier [] "a", Identifier [] "b"])
>      ,p "a != b"
>       (FunCall [] "<>" [Identifier [] "a", Identifier [] "b"])

>      ])


================================================================================

test some string parsing, want to check single quote behaviour,
and dollar quoting, including nesting.

>     ,testGroup "string parsing"
>     (mapExpr [
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
>      ])

================================================================================

first statement, pretty simple

>     ,testGroup "select expression"
>     (mapSql [
>       p "select 1;" [SelectStatement [] $ selectE (SelectList [SelExp (IntegerLit [] 1)] [])]
>      ])

================================================================================

test a whole bunch more select statements

>     ,testGroup "select from table"
>     (mapSql [
>       p "select * from tbl;"
>       [SelectStatement [] $ selectFrom (selIL ["*"]) (Tref [] "tbl")]
>      ,p "select a,b from tbl;"
>       [SelectStatement [] $ selectFrom (selIL ["a", "b"]) (Tref [] "tbl")]

>      ,p "select a,b from inf.tbl;"
>       [SelectStatement [] $ selectFrom (selIL ["a", "b"]) (Tref [] "inf.tbl")]

>      ,p "select distinct * from tbl;"
>       [SelectStatement [] $ Select [] Distinct (SelectList (selIL ["*"]) []) (Just $ Tref [] "tbl")
>        Nothing [] Nothing [] Asc Nothing Nothing]

>      ,p "select a from tbl where b=2;"
>       [SelectStatement [] $ selectFromWhere
>         (selIL ["a"])
>         (Tref [] "tbl")
>         (FunCall [] "="
>          [Identifier [] "b", IntegerLit [] 2])]
>      ,p "select a from tbl where b=2 and c=3;"
>       [SelectStatement [] $ selectFromWhere
>         (selIL ["a"])
>         (Tref [] "tbl")
>         (FunCall [] "!and"
>          [FunCall [] "="  [Identifier [] "b", IntegerLit [] 2]
>          ,FunCall [] "=" [Identifier [] "c", IntegerLit [] 3]])]

>      ,p "select a from tbl\n\
>         \except\n\
>         \select a from tbl1;"
>       [SelectStatement [] $ CombineSelect [] Except
>        (selectFrom (selIL ["a"]) (Tref [] "tbl"))
>        (selectFrom (selIL ["a"]) (Tref [] "tbl1"))]
>      ,p "select a from tbl where true\n\
>         \except\n\
>         \select a from tbl1 where true;"
>       [SelectStatement [] $ CombineSelect [] Except
>        (selectFromWhere (selIL ["a"]) (Tref [] "tbl") (BooleanLit [] True))
>        (selectFromWhere (selIL ["a"]) (Tref [] "tbl1") (BooleanLit [] True))]
>      ,p "select a from tbl\n\
>         \union\n\
>         \select a from tbl1;"
>       [SelectStatement [] $ CombineSelect [] Union
>        (selectFrom (selIL ["a"]) (Tref [] "tbl"))
>        (selectFrom (selIL ["a"]) (Tref [] "tbl1"))]
>      ,p "select a from tbl\n\
>         \union all\n\
>         \select a from tbl1;"
>       [SelectStatement [] $ CombineSelect [] UnionAll
>        (selectFrom (selIL ["a"]) (Tref [] "tbl"))
>        (selectFrom (selIL ["a"]) (Tref [] "tbl1"))]

>      ,p "select a as b from tbl;"
>       [SelectStatement [] $ selectFrom [SelectItem (Identifier [] "a") "b"] (Tref [] "tbl")]
>      ,p "select a + b as b from tbl;"
>       [SelectStatement [] $ selectFrom
>        [SelectItem
>         (FunCall [] "+"
>          [Identifier [] "a", Identifier [] "b"]) "b"]
>        (Tref [] "tbl")]
>      ,p "select a.* from tbl a;"
>       [SelectStatement [] $ selectFrom (selIL ["a.*"]) (TrefAlias [] "tbl" "a")]

>      ,p "select a from b inner join c on b.a=c.a;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b") Unnatural Inner (Tref [] "c")
>           (Just (JoinOn
>            (FunCall [] "=" [Identifier [] "b.a", Identifier [] "c.a"]))))]
>      ,p "select a from b inner join c as d on b.a=d.a;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b") Unnatural Inner (TrefAlias [] "c" "d")
>           (Just (JoinOn
>            (FunCall [] "=" [Identifier [] "b.a", Identifier [] "d.a"]))))]

>      ,p "select a from b inner join c using(d,e);"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b") Unnatural Inner (Tref [] "c")
>           (Just (JoinUsing ["d","e"])))]

>      ,p "select a from b natural inner join c;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b") Natural Inner (Tref [] "c") Nothing)]
>      ,p "select a from b left outer join c;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b") Unnatural LeftOuter (Tref [] "c") Nothing)]
>      ,p "select a from b full outer join c;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b") Unnatural FullOuter (Tref [] "c") Nothing)]
>      ,p "select a from b right outer join c;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b") Unnatural RightOuter (Tref [] "c") Nothing)]
>      ,p "select a from b cross join c;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["a"])
>        (JoinedTref [] (Tref [] "b") Unnatural Cross (Tref [] "c") Nothing)]

>      ,p "select a from b\n\
>         \    inner join c\n\
>         \      on true\n\
>         \    inner join d\n\
>         \      on 1=1;"
>       [SelectStatement [] $ selectFrom
>        [SelExp (Identifier [] "a")]
>        (JoinedTref [] 
>         (JoinedTref [] (Tref [] "b") Unnatural Inner (Tref [] "c")
>          (Just $ JoinOn (BooleanLit [] True)))
>         Unnatural Inner (Tref [] "d")
>         (Just  $ JoinOn (FunCall [] "="
>                [IntegerLit [] 1, IntegerLit [] 1])))]

>      ,p "select row_number() over(order by a) as place from tbl;"
>       [SelectStatement [] $ selectFrom [SelectItem
>                    (WindowFn []
>                     (FunCall [] "row_number" [])
>                     []
>                     [Identifier [] "a"] Asc)
>                    "place"]
>        (Tref [] "tbl")]
>      ,p "select row_number() over(order by a asc) as place from tbl;"
>       [SelectStatement [] $ selectFrom [SelectItem
>                    (WindowFn []
>                     (FunCall [] "row_number" [])
>                     []
>                     [Identifier [] "a"] Asc)
>                    "place"]
>        (Tref [] "tbl")]
>      ,p "select row_number() over(order by a desc) as place from tbl;"
>       [SelectStatement [] $ selectFrom [SelectItem
>                    (WindowFn []
>                     (FunCall [] "row_number" [])
>                     []
>                     [Identifier [] "a"] Desc)
>                    "place"]
>        (Tref [] "tbl")]
>      ,p "select row_number()\n\
>         \over(partition by (a,b) order by c) as place\n\
>         \from tbl;"
>       [SelectStatement [] $ selectFrom [SelectItem
>                    (WindowFn []
>                     (FunCall [] "row_number" [])
>                     [FunCall [] "!rowCtor" [Identifier [] "a",Identifier [] "b"]]
>                     [Identifier [] "c"] Asc)
>                    "place"]
>        (Tref [] "tbl")]

>      ,p "select * from a natural inner join (select * from b) as a;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["*"])
>        (JoinedTref [] (Tref [] "a") Natural
>         Inner (SubTref [] (selectFrom
>                         (selIL ["*"])
>                         (Tref [] "b")) "a")
>         Nothing)]

>      ,p "select * from a order by c;"
>       [SelectStatement [] $ Select []  Dupes
>        (sl (selIL ["*"]))
>        (Just $ Tref [] "a")
>        Nothing [] Nothing [Identifier [] "c"] Asc Nothing Nothing]

>      ,p "select * from a order by c,d asc;"
>       [SelectStatement [] $ Select [] Dupes
>        (sl (selIL ["*"]))
>        (Just $ Tref [] "a")
>        Nothing [] Nothing [Identifier [] "c", Identifier [] "d"] Asc Nothing Nothing]

>      ,p "select * from a order by c,d desc;"
>       [SelectStatement [] $ Select [] Dupes
>        (sl (selIL ["*"]))
>        (Just $ Tref [] "a")
>        Nothing [] Nothing [Identifier [] "c", Identifier [] "d"] Desc Nothing Nothing]

>      ,p "select * from a order by c limit 1;"
>       [SelectStatement [] $ Select [] Dupes
>        (sl (selIL ["*"]))
>        (Just $ Tref [] "a")
>        Nothing [] Nothing [Identifier [] "c"] Asc (Just (IntegerLit [] 1)) Nothing]

>      ,p "select * from a order by c offset 3;"
>       [SelectStatement [] $ Select [] Dupes
>        (sl (selIL ["*"]))
>        (Just $ Tref [] "a")
>        Nothing [] Nothing [Identifier [] "c"] Asc Nothing (Just $ IntegerLit [] 3)]

>      ,p "select a from (select b from c) as d;"
>         [SelectStatement [] $ selectFrom
>          (selIL ["a"])
>          (SubTref [] (selectFrom
>                    (selIL ["b"])
>                    (Tref [] "c"))
>           "d")]

>      ,p "select * from gen();"
>         [SelectStatement [] $ selectFrom (selIL ["*"]) (TrefFun [] $ FunCall [] "gen" [])]
>      ,p "select * from gen() as t;"
>       [SelectStatement [] $ selectFrom
>        (selIL ["*"])
>        (TrefFunAlias [] (FunCall [] "gen" []) "t")]

>      ,p "select a, count(b) from c group by a;"
>         [SelectStatement [] $ Select [] Dupes
>          (sl [selI "a", SelExp (FunCall [] "count" [Identifier [] "b"])])
>          (Just $ Tref [] "c") Nothing [Identifier [] "a"]
>          Nothing [] Asc Nothing Nothing]

>      ,p "select a, count(b) as cnt from c group by a having cnt > 4;"
>         [SelectStatement [] $ Select [] Dupes
>          (sl [selI "a", SelectItem (FunCall [] "count" [Identifier [] "b"]) "cnt"])
>          (Just $ Tref [] "c") Nothing [Identifier [] "a"]
>          (Just $ FunCall [] ">" [Identifier [] "cnt", IntegerLit [] 4])
>          [] Asc Nothing Nothing]

>      ,p "select a from (select 1 as a, 2 as b) x;"
>         [SelectStatement [] $ selectFrom
>          [selI "a"]
>          (SubTref [] (selectE $ SelectList
>                                [SelectItem (IntegerLit [] 1) "a"
>                                ,SelectItem (IntegerLit [] 2) "b"] [])
>                   "x")]
>      ])

================================================================================

one sanity check for parsing multiple statements

>     ,testGroup "multiple statements"
>     (mapSql [
>       p "select 1;\nselect 2;" [SelectStatement [] $ selectE $ sl [SelExp (IntegerLit [] 1)]
>                                ,SelectStatement [] $ selectE $ sl [SelExp (IntegerLit [] 2)]]
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
>         \2;" [SelectStatement [] $ selectE $ sl [SelExp (IntegerLit [] 1)]
>              ,SelectStatement [] $ selectE $ sl [SelExp (IntegerLit [] 2)]
>              ]
>      ,p "select 1;\n\
>         \/* this is\n\
>         \a test*/\n\
>         \select /* this is a test*/2;"
>                     [SelectStatement [] $ selectE $ sl [SelExp (IntegerLit [] 1)]
>                     ,SelectStatement [] $ selectE $ sl [SelExp (IntegerLit [] 2)]
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
>       [Insert []
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
>        (selectFrom [selI "b"] (Tref [] "c"))
>        Nothing]

>      ,p "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2) returning id;\n"
>       [Insert []
>         "testtable"
>         ["columna", "columnb"]
>         (Values [] [[IntegerLit [] 1, IntegerLit [] 2]])
>         (Just $ sl [selI "id"])]

updates

>      ,p "update tb\n\
>         \  set x = 1, y = 2;"
>       [Update [] "tb" [SetClause "x" (IntegerLit [] 1)
>                    ,SetClause "y" (IntegerLit [] 2)]
>        Nothing Nothing]
>      ,p "update tb\n\
>         \  set x = 1, y = 2 where z = true;"
>       [Update [] "tb" [SetClause "x" (IntegerLit [] 1)
>                    ,SetClause "y" (IntegerLit [] 2)]
>        (Just $ FunCall [] "="
>         [Identifier [] "z", BooleanLit [] True])
>        Nothing]
>      ,p "update tb\n\
>         \  set x = 1, y = 2 returning id;"
>       [Update [] "tb" [SetClause "x" (IntegerLit [] 1)
>                    ,SetClause "y" (IntegerLit [] 2)]
>        Nothing (Just $ sl [selI "id"])]
>      ,p "update pieces\n\
>         \set a=b returning tag into r.tag;"
>       [Update [] "pieces" [SetClause "a" (Identifier [] "b")]
>        Nothing (Just (SelectList
>                       [SelExp (Identifier [] "tag")]
>                       ["r.tag"]))]
>      ,p "update tb\n\
>         \  set (x,y) = (1,2);"
>       [Update [] "tb" [RowSetClause
>                     ["x","y"]
>                     [IntegerLit [] 1,IntegerLit [] 2]]
>        Nothing Nothing]

delete

>      ,p "delete from tbl1 where x = true;"
>       [Delete [] "tbl1" (Just $ FunCall [] "="
>                                [Identifier [] "x", BooleanLit [] True])
>        Nothing]
>      ,p "delete from tbl1 where x = true returning id;"
>       [Delete [] "tbl1" (Just $ FunCall [] "="
>                                [Identifier [] "x", BooleanLit [] True])
>        (Just $ sl [selI "id"])]

>     ,p "truncate test;"
>        [Truncate [] ["test"] ContinueIdentity Restrict]

>     ,p "truncate table test, test2 restart identity cascade;"
>        [Truncate [] ["test","test2"] RestartIdentity Cascade]

copy, bit crap at the moment

>      ,p "copy tbl(a,b) from stdin;\n\
>         \bat	t\n\
>         \bear	f\n\
>         \\\.\n"
>       [Copy [] "tbl" ["a", "b"] Stdin
>        ,CopyData [] "\
>         \bat	t\n\
>         \bear	f\n"]
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
>       [CreateTable []
>        "test"
>        [att "fielda" "text"
>        ,att "fieldb" "int"
>        ]
>        []]
>      ,p "create table tbl (\n\
>         \  fld boolean default false);"
>       [CreateTable [] "tbl" [AttributeDef "fld" (SimpleTypeName "boolean")
>                           (Just $ BooleanLit [] False) []][]]

>      ,p "create table tbl as select 1;"
>       [CreateTableAs [] "tbl"
>        (selectE (SelectList [SelExp (IntegerLit [] 1)] []))]

other creates

>      ,p "create view v1 as\n\
>         \select a,b from t;"
>       [CreateView []
>        "v1"
>        (selectFrom [selI "a", selI "b"] (Tref [] "t"))]
>      ,p "create domain td as text check (value in ('t1', 't2'));"
>       [CreateDomain [] "td" (SimpleTypeName "text")
>        (Just (InPredicate [] (Identifier [] "value") True
>               (InList [stringQ "t1" ,stringQ "t2"])))]
>      ,p "create type tp1 as (\n\
>         \  f1 text,\n\
>         \  f2 text\n\
>         \);"
>       [CreateType [] "tp1" [TypeAttDef "f1" (SimpleTypeName "text")
>                         ,TypeAttDef "f2" (SimpleTypeName "text")]]

drops

>      ,p "drop domain t;"
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

>      ])

constraints

>     ,testGroup "constraints"
>     (mapSql [

nulls

>       p "create table t1 (\n\
>         \ a text null\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef "a" (SimpleTypeName "text")
>                            Nothing [NullConstraint]]
>          []]
>      ,p "create table t1 (\n\
>         \ a text not null\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef "a" (SimpleTypeName "text")
>                            Nothing [NotNullConstraint]]
>          []]

unique table

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ unique (x,y)\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [UniqueConstraint ["x","y"]]]

test arbitrary ordering

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ unique (x),\n\
>         \ y int\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [UniqueConstraint ["x"]]]

unique row

>      ,p "create table t1 (\n\
>         \ x int unique\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef "x" (SimpleTypeName "int") Nothing
>                            [RowUniqueConstraint]][]]

>      ,p "create table t1 (\n\
>         \ x int unique not null\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef "x" (SimpleTypeName "int") Nothing
>                            [RowUniqueConstraint
>                            ,NotNullConstraint]][]]

quick sanity check

>      ,p "create table t1 (\n\
>         \ x int not null unique\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef "x" (SimpleTypeName "int") Nothing
>                            [NotNullConstraint
>                            ,RowUniqueConstraint]][]]

primary key row, table

>      ,p "create table t1 (\n\
>         \ x int primary key\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef "x" (SimpleTypeName "int") Nothing
>                            [RowPrimaryKeyConstraint]][]]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ primary key (x,y)\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [PrimaryKeyConstraint ["x", "y"]]]

check row, table

>      ,p "create table t (\n\
>         \f text check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable [] "t"
>          [AttributeDef "f" (SimpleTypeName "text") Nothing
>           [RowCheckConstraint (InPredicate []
>                                   (Identifier [] "f") True
>                                   (InList [stringQ "a", stringQ "b"]))]] []]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ check (x>y)\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [CheckConstraint (FunCall [] ">" [Identifier [] "x", Identifier [] "y"])]]

row, whole load of constraints, todo: add reference here

>      ,p "create table t (\n\
>         \f text not null unique check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable [] "t"
>          [AttributeDef "f" (SimpleTypeName "text") Nothing
>           [NotNullConstraint
>            ,RowUniqueConstraint
>            ,RowCheckConstraint (InPredicate []
>                                    (Identifier [] "f") True
>                                    (InList [stringQ "a"
>                                            ,stringQ "b"]))]] []]

reference row, table

>      ,p "create table t1 (\n\
>         \ x int references t2\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef "x" (SimpleTypeName "int") Nothing
>                            [RowReferenceConstraint "t2" Nothing
>                             Restrict Restrict]][]]

>      ,p "create table t1 (\n\
>         \ x int references t2(y)\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef "x" (SimpleTypeName "int") Nothing
>                            [RowReferenceConstraint "t2" (Just "y")
>                             Restrict Restrict]][]]


>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint ["x", "y"] "t2" []
>           Restrict Restrict]]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2(z,w)\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint ["x", "y"] "t2" ["z", "w"]
>           Restrict Restrict]]

>      ,p "create table t1 (\n\
>         \ x int references t2 on delete cascade\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef "x" (SimpleTypeName "int") Nothing
>                            [RowReferenceConstraint "t2" Nothing
>                             Cascade Restrict]][]]

>      ,p "create table t1 (\n\
>         \ x int references t2 on update cascade\n\
>         \);"
>         [CreateTable [] "t1" [AttributeDef "x" (SimpleTypeName "int") Nothing
>                            [RowReferenceConstraint "t2" Nothing
>                             Restrict Cascade]][]]

>      ,p "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2 on delete cascade on update cascade\n\
>         \);"
>         [CreateTable [] "t1" [att "x" "int"
>                           ,att "y" "int"]
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
>       [CreateFunction [] Sql "t1" [ParamDefTp $ SimpleTypeName "text"]
>        (SimpleTypeName "text") "$$"
>        (SqlFnBody
>         [SelectStatement [] $ selectFromWhere [SelExp (Identifier [] "a")] (Tref [] "t1")
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
>       [CreateFunction [] Plpgsql "fn" [] (SimpleTypeName "void") "$$"
>        (PlpgsqlFnBody [VarDef "a" (SimpleTypeName "int") Nothing
>                       ,VarDef "b" (SimpleTypeName "text") Nothing]
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
>       [CreateFunction [] Plpgsql "fn" [] (SimpleTypeName "void") "$$"
>        (PlpgsqlFnBody [VarDef "a" (SimpleTypeName "int") Nothing
>                       ,VarDef "b" (SimpleTypeName "text") Nothing]
>         [NullStatement []])
>        Volatile]
>      ,p "create function fn(a text[]) returns int[] as $$\n\
>         \declare\n\
>         \  b xtype[] := '{}';\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql immutable;"
>       [CreateFunction [] Plpgsql "fn"
>        [ParamDef "a" $ ArrayTypeName $ SimpleTypeName "text"]
>        (ArrayTypeName $ SimpleTypeName "int") "$$"
>        (PlpgsqlFnBody
>         [VarDef "b" (ArrayTypeName $ SimpleTypeName "xtype") (Just $ stringQ "{}")]
>         [NullStatement []])
>        Immutable]
>      ,p "create function fn() returns void as '\n\
>         \declare\n\
>         \  a int := 3;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction [] Plpgsql "fn" [] (SimpleTypeName "void") "'"
>        (PlpgsqlFnBody [VarDef "a" (SimpleTypeName "int") (Just $ IntegerLit [] 3)]
>         [NullStatement []])
>        Stable]
>      ,p "create function fn() returns setof int as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction [] Plpgsql "fn" []
>        (SetOfTypeName $ SimpleTypeName "int") "$$"
>        (PlpgsqlFnBody [] [NullStatement []])
>        Stable]
>      ,p "create function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction [] Plpgsql "fn" []
>        (SimpleTypeName "void") "$$"
>        (PlpgsqlFnBody [] [NullStatement []])
>        Stable]
>      ,p "drop function test(text);"
>       [DropFunction [] Require [("test",["text"])] Restrict]
>      ,p "drop function if exists a(),test(text) cascade;"
>       [DropFunction [] IfExists [("a",[])
>                           ,("test",["text"])] Cascade]
>      ])

================================================================================

test non sql plpgsql statements

>     ,testGroup "plpgsqlStatements"
>     (mapPlpgsql [

simple statements

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
>       [ReturnQuery [] $ selectFrom [selI "a"] (Tref [] "b")]
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
>       [SelectStatement [] $ Select [] Dupes (SelectList [selI "c", selI "d"] ["a", "b"])
>                   (Just $ Tref [] "e") Nothing [] Nothing [] Asc Nothing Nothing]
>      ,p "select c,d into a,b from e;"
>       [SelectStatement [] $ Select [] Dupes (SelectList [selI "c", selI "d"] ["a", "b"])
>                   (Just $ Tref [] "e") Nothing [] Nothing [] Asc Nothing Nothing]

>      ,p "execute s;"
>       [Execute [] (Identifier [] "s")]
>      ,p "execute s into r;"
>       [ExecuteInto [] (Identifier [] "s") ["r"]]

>      ,p "continue;" [ContinueStatement []]

complicated statements

>      ,p "for r in select a from tbl loop\n\
>         \null;\n\
>         \end loop;"
>       [ForSelectStatement [] "r" (selectFrom  [selI "a"] (Tref [] "tbl"))
>        [NullStatement []]]
>      ,p "for r in select a from tbl where true loop\n\
>         \null;\n\
>         \end loop;"
>       [ForSelectStatement [] "r"
>        (selectFromWhere [selI "a"] (Tref [] "tbl") (BooleanLit [] True))
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
>           ,[Update [] "c" [SetClause "d" (Identifier [] "e")] Nothing Nothing])]
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
>           selI = SelExp . (Identifier [])
>           sl a = SelectList a []
>           selectE selList = Select [] Dupes selList
>                             Nothing Nothing [] Nothing [] Asc Nothing Nothing
>           selectFrom selList frm =
>             Select [] Dupes (SelectList selList [])
>                    (Just frm) Nothing [] Nothing [] Asc Nothing Nothing
>           selectFromWhere selList frm whr =
>             Select [] Dupes (SelectList selList [])
>                    (Just frm) (Just whr) [] Nothing [] Asc Nothing Nothing
>           stringQ = StringLit [] "'"
>           att n t = AttributeDef n (SimpleTypeName t) Nothing []

================================================================================

Unit test helpers

parse and then pretty print and parse a statement

> checkParse :: String -> [Statement] -> Test.Framework.Test
> checkParse src ast = parseUtil1 src ast parseSql

parse and then pretty print and parse an expression

> checkParseExpression :: String -> Expression -> Test.Framework.Test
> checkParseExpression src ast = parseUtil src ast
>                                  parseExpression printExpression

> checkParsePlpgsql :: String -> [Statement] -> Test.Framework.Test
> checkParsePlpgsql src ast = parseUtil1 src ast parsePlpgsql

> parseUtil :: (Show t, Eq b, Show b) =>
>              String
>           -> b
>           -> (String -> Either t b)
>           -> (b -> String)
>           -> Test.Framework.Test
> parseUtil src ast parser printer = testCase ("parse " ++ src) $ do
>   let ast' = case parser src of
>               Left er -> error $ show er
>               Right l -> l
>   assertEqual ("parse " ++ src) ast ast'
>   -- pretty print then parse to check
>   let pp = printer ast
>   let ast'' = case parser pp of
>               Left er -> error $ "reparse\n" ++ show er ++ "\n" -- ++ pp ++ "\n"
>               Right l -> l
>   assertEqual ("reparse " ++ pp) ast ast''

> parseUtil1 :: (Show a) => String
>            -> [Statement]
>            -> (String -> Either a StatementList)
>            -> Test.Framework.Test
> parseUtil1 src ast parser = testCase ("parse " ++ src) $ do
>   let ast' = case parser src of
>               Left er -> error $ show er
>               Right l -> l
>   assertEqual ("parse " ++ src) ast $ map stripAnnotations ast'
>   -- pretty print then parse to check
>   let pp = printSql ast'
>   let ast'' = case parser pp of
>               Left er -> error $ "reparse\n" ++ show er ++ "\n" -- ++ pp ++ "\n"
>               Right l -> l
>   assertEqual ("reparse " ++ pp) ast $ map stripAnnotations ast''

