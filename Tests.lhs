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

The only ddl supported is creates, no alters or drops at the moment.

> import Test.HUnit

 > import Test.QuickCheck

> import Test.Framework
> import Test.Framework.Providers.HUnit

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
>      ,p " 1 + 1 " (BinaryOperatorCall Plus (IntegerL 1) (IntegerL 1))
>      ,p "1+1+1" (BinaryOperatorCall
>                  Plus
>                  (BinaryOperatorCall Plus (IntegerL 1) (IntegerL 1))
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
>      ,p "array[1,2]" (ArrayL [IntegerL 1, IntegerL 2])


we just produce a parse tree, so no type checking or anything like
that is done

some operator tests

>      ,p "1 + tst1" (BinaryOperatorCall
>                     Plus (IntegerL 1) (Identifier "tst1"))
>      ,p "tst1 + 1" (BinaryOperatorCall
>                     Plus (Identifier "tst1") (IntegerL 1))
>      ,p "tst + tst1" (BinaryOperatorCall
>                       Plus (Identifier "tst") (Identifier "tst1"))
>      ,p "'a' || 'b'" (BinaryOperatorCall Conc (StringL "a")
>                                              (StringL "b"))
>      ,p "'stuff'::text" (BinaryOperatorCall
>                          Cast (StringL "stuff") (Identifier "text"))
>      ,p "245::float(24)" (BinaryOperatorCall
>                          Cast (IntegerL 245)
>                           (FunctionCall "float" [IntegerL 24]))

some function call tests

>      ,p "fn()" (FunctionCall "fn" [])
>      ,p "fn(1)" (FunctionCall "fn" [IntegerL 1])
>      ,p "fn('test')" (FunctionCall "fn" [StringL "test"])
>      ,p "fn(1,'test')" (FunctionCall "fn" [IntegerL 1, StringL "test"])
>      ,p "fn('test')" (FunctionCall "fn" [StringL "test"])

simple whitespace sanity checks

>      ,p "fn (1)" (FunctionCall "fn" [IntegerL 1])
>      ,p "fn( 1)" (FunctionCall "fn" [IntegerL 1])
>      ,p "fn(1 )" (FunctionCall "fn" [IntegerL 1])
>      ,p "fn(1) " (FunctionCall "fn" [IntegerL 1])

null stuff

>      ,p "not null" (BinaryOperatorCall Not NullL NullL)
>      ,p "a is null" (BinaryOperatorCall IsNull NullL (Identifier "a"))
>      ,p "a is not null" (BinaryOperatorCall
>                          IsNotNull NullL (Identifier "a"))

some slightly more complex stuff

>      ,p "case when a then 3\n\
>         \     when b then 4\n\
>         \     else 5\n\
>         \end"
>         (Case [When (Identifier "a") (IntegerL 3)
>               ,When (Identifier "b") (IntegerL 4)]
>          (Just $ Else (IntegerL 5)))

positional args used in sql and sometimes plpgsql functions

>      ,p "$1" (PositionalArg 1)

>      ,p "exists (select 1 from a)"
>       (Exists (selectFrom (SelectList [SelExp (IntegerL 1)]) (Tref "a")))

in variants, including using row constructors

>      ,p "t in (1,2)"
>       (InPredicate (Identifier "t") (InList [IntegerL 1,IntegerL 2]))
>      ,p "t not in (1,2)"
>       (BinaryOperatorCall Not NullL $ InPredicate (Identifier "t")
>        (InList [IntegerL 1,IntegerL 2]))
>      ,p "(t,u) in (1,2)"
>       (InPredicate (Row [Identifier "t",Identifier "u"])
>        (InList [IntegerL 1,IntegerL 2]))
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
>       p "select 1;" [selectE (SelectList [SelExp (IntegerL 1)])]
>      ])

================================================================================

test a whole bunch more select statements

>     ,testGroup "select from table"
>     (mapSql [
>       p "select * from tbl;"
>       [selectFrom (selIL ["*"]) (Tref "tbl")]
>      ,p "select a,b from tbl;"
>       [selectFrom (selIL ["a", "b"]) (Tref "tbl")]
>      ,p "select a from tbl where b=2;"
>       [selectFromWhere
>         (selIL ["a"])
>         (Tref "tbl")
>         (BinaryOperatorCall Eql
>          (Identifier "b") (IntegerL 2))]
>      ,p "select a from tbl where b=2 and c=3;"
>       [selectFromWhere
>         (selIL ["a"])
>         (Tref "tbl")
>         (BinaryOperatorCall And
>          (BinaryOperatorCall Eql
>           (Identifier "b") (IntegerL 2))
>          (BinaryOperatorCall Eql
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
>      ,p "select a as b from tbl;"
>       [selectFrom (SelectList [SelectItem (Identifier "a") "b"]) (Tref "tbl")]
>      ,p "select a + b as b from tbl;"
>       [selectFrom
>        (SelectList [SelectItem
>                     (BinaryOperatorCall Plus
>                      (Identifier "a") (Identifier "b")) "b"])
>        (Tref "tbl")]
>      ,p "select a.* from tbl a;"
>       [selectFrom (SelectList [SelExp (qi "a" "*")]) (TrefAlias "tbl" "a")]
>      ,p "select a from b inner join c on b.a=c.a;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") False Inner (Tref "c")
>           (Just (JoinOn
>            (BinaryOperatorCall Eql (qi "b" "a") (qi "c" "a")))))]
>      ,p "select a from b inner join c using(d,e);"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") False Inner (Tref "c")
>           (Just (JoinUsing ["d","e"])))]
>      ,p "select a from b natural inner join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") True Inner (Tref "c") Nothing)]
>      ,p "select a from b left outer join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") False LeftOuter (Tref "c") Nothing)]
>      ,p "select a from b full outer join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") False FullOuter (Tref "c") Nothing)]
>      ,p "select a from b right outer join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") False RightOuter (Tref "c") Nothing)]
>      ,p "select a from b cross join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinedTref (Tref "b") False Cross (Tref "c") Nothing)]
>      ,p "select a from b\n\
>         \    inner join c\n\
>         \      on true\n\
>         \    inner join d\n\
>         \      on 1=1;"
>       [selectFrom
>        (SelectList [SelExp (Identifier "a")])
>        (JoinedTref
>         (JoinedTref (Tref "b") False Inner (Tref "c")
>          (Just $ JoinOn (BooleanL True)))
>         False Inner (Tref "d")
>         (Just  $ JoinOn (BinaryOperatorCall Eql
>                (IntegerL 1) (IntegerL 1))))]
>      ,p "select row_number() over(order by a) as place from tbl;"
>       [selectFrom (SelectList [SelectItem
>                                (WindowFn
>                                 (FunctionCall "row_number" [])
>                                 Nothing
>                                 (Just [Identifier "a"]))
>                                "place"])
>        (Tref "tbl")]
>      ,p "select row_number()\n\
>         \over(partition by (a,b) order by c) as place\n\
>         \from tbl;"
>       [selectFrom (SelectList [SelectItem
>                                (WindowFn
>                                 (FunctionCall "row_number" [])
>                                 (Just [Row [Identifier "a",Identifier "b"]])
>                                 (Just [Identifier "c"]))
>                                "place"])
>        (Tref "tbl")]
>      ,p "select * from a natural inner join (select * from b) as a;"
>       [selectFrom
>        (selIL ["*"])
>        (JoinedTref (Tref "a") True
>         Inner (SubTref (selectFrom
>                         (selIL ["*"])
>                         (Tref "b")) "a")
>         Nothing)]
>      ,p "select * from a order by c;"
>       [Select
>        (selIL ["*"])
>        (Just (From (Tref "a")))
>        Nothing (Just [Identifier "c"]) Nothing]
>      ,p "select * from a order by c limit 1;"
>       [Select
>        (selIL ["*"])
>        (Just (From (Tref "a")))
>        Nothing (Just [Identifier "c"]) (Just (IntegerL 1))]
>      ,p "select a from (select b from c) as d;"
>         [selectFrom
>          (selIL ["a"])
>          (SubTref (selectFrom
>                    (selIL ["b"])
>                    (Tref "c"))
>           "d")]
>      ,p "select * from gen();"
>         [selectFrom (selIL ["*"]) (TrefFun $ FunctionCall "gen" [])]
>      ,p "select * from gen() as t;"
>       [selectFrom
>        (selIL ["*"])
>        (TrefFunAlias (FunctionCall "gen" []) "t")]
>      ])

================================================================================

one sanity check for parsing multiple statements

>     ,testGroup "multiple statements"
>     (mapSql [
>       p "select 1;\nselect 2;" [selectE $ SelectList [SelExp (IntegerL 1)]
>                                ,selectE $ SelectList [SelExp (IntegerL 2)]]
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
>         \2;" [selectE $ SelectList [SelExp (IntegerL 1)]
>              ,selectE $ SelectList [SelExp (IntegerL 2)]
>              ]
>      ,p "select 1;\n\
>         \/* this is\n\
>         \a test*/\n\
>         \select /* this is a test*/2;"
>                     [selectE $ SelectList [SelExp (IntegerL 1)]
>                     ,selectE $ SelectList [SelExp (IntegerL 2)]
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
>         (Just ["columna", "columnb"])
>         (InsertData [[IntegerL 1, IntegerL 2]])]

multi row insert

>      ,p "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2), (3,4);\n"
>       [Insert
>         "testtable"
>         (Just ["columna", "columnb"])
>         (InsertData [[IntegerL 1, IntegerL 2]
>                     ,[IntegerL 3, IntegerL 4]])]

insert from select

>      ,p "insert into a\n\
>          \    select b from c;"
>       [Insert "a" Nothing
>        (InsertQuery (Select
>                      (SelectList [selI "b"])
>                      (Just $ From $ Tref "c")
>                      Nothing Nothing Nothing))]

updates

>      ,p "update tb\n\
>         \  set x = 1, y = 2;"
>       [Update "tb" [SetClause "x" (IntegerL 1)
>                    ,SetClause "y" (IntegerL 2)]
>        Nothing]
>      ,p "update tb\n\
>         \  set x = 1, y = 2 where z = true;"
>       [Update "tb" [SetClause "x" (IntegerL 1)
>                    ,SetClause "y" (IntegerL 2)]
>        (Just $ Where $ BinaryOperatorCall Eql
>         (Identifier "z") (BooleanL True))]
>      ,p "delete from tbl1 where x = true;"

delete

>       [Delete "tbl1" (Just $ Where $ BinaryOperatorCall Eql
>                                (Identifier "x") (BooleanL True))]
>      ,p "copy tbl(a,b) from stdin;\n\
>         \bat	t\n\
>         \bear	f\n\
>         \\\.\n"

copy, bit crap at the moment

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
>        [AttributeDef "fielda" "text" Nothing Nothing
>        ,AttributeDef "fieldb" "int" Nothing Nothing
>        ]]
>      ,p "create table test (\n\
>         \type text check (type in('a', 'b')));"
>       [CreateTable
>        "test" [AttributeDef "type" "text" Nothing
>                (Just (InPredicate
>                       (Identifier "type")
>                       (InList [StringL "a"
>                               ,StringL "b"])))]]
>      ,p "create table tb (\n\
>         \a text not null,\n\
>         \b boolean null);"
>       [CreateTable
>        "tb"
>        [AttributeDef "a" "text" Nothing
>         (Just (BinaryOperatorCall Not NullL NullL))
>        ,AttributeDef "b" "boolean"  Nothing (Just NullL)]]
>      ,p "create table tbl (\n\
>         \  fld boolean default false);"
>       [CreateTable "tbl" [AttributeDef "fld" "boolean"
>                           (Just $ BooleanL False)
>                           Nothing]]
>      ,p "create view v1 as\n\
>         \select a,b from t;"

other creates

>       [CreateView
>        "v1"
>        (Select
>         (SelectList [selI "a", selI "b"])
>         (Just $ From $ Tref "t")
>         Nothing Nothing Nothing)]
>      ,p "create domain td as text check (value in ('t1', 't2'));"
>       [CreateDomain "td" "text"
>        (Just (InPredicate (Identifier "value")
>               (InList [StringL "t1" ,StringL "t2"])))]
>      ,p "create type tp1 as (\n\
>         \  f1 text,\n\
>         \  f2 text\n\
>         \);"
>       [CreateType "tp1" [TypeAttDef "f1" "text"
>                         ,TypeAttDef "f2" "text"]]
>      ])

================================================================================

test functions

>     ,testGroup "functions"
>     (mapSql [
>       p "create function t1(text) returns text as $$\n\
>         \select a from t1 where b = $1;\n\
>         \$$ language sql stable;"
>       [CreateFunction Sql "t1" [ParamDefTp "text"] (Identifier "text") "$$"
>        (SqlFnBody
>         [Select
>          (SelectList [SelExp (Identifier "a")])
>          (Just (From (Tref "t1")))
>          (Just (Where (BinaryOperatorCall Eql
>                        (Identifier "b") (PositionalArg 1))))
>          Nothing Nothing])
>        Stable]
>      ,p "create function fn() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>       [CreateFunction Plpgsql "fn" [] (Identifier "void") "$$"
>        (PlpgsqlFnBody [VarDef "a" "int"
>                       ,VarDef "b" "text"]
>         [NullStatement])
>        Volatile]
>      ,p "create function fn() returns void as '\n\
>         \declare\n\
>         \  a int;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction Plpgsql "fn" [] (Identifier "void") "'"
>        (PlpgsqlFnBody [VarDef "a" "int"] [NullStatement])
>        Stable]
>      ])

================================================================================

test non sql plpgsql statements

>     ,testGroup "plpgsqlStatements"
>     (mapSql [

simple statements

>       p "success := true;"
>       [Assignment "success" (BooleanL True)]
>      ,p "return true;"
>       [Return (BooleanL True)]
>      ,p "raise notice 'stuff %', 1;"
>       [Raise RNotice "stuff %" [IntegerL 1]]
>      ,p "perform test();"
>       [Perform $ FunctionCall "test" []]
>      ,p "perform test(a,b);"
>       [Perform $ FunctionCall "test" [Identifier "a", Identifier "b"]]
>      ,p "perform test(r.relvar_name || '_and_stuff');"
>       [Perform $ FunctionCall "test" [
>                     BinaryOperatorCall Conc (qi "r" "relvar_name")
>                                            (StringL "_and_stuff")]]
>      ,p "select into a,b c,d from e;"
>       [SelectInto ["a", "b"]
>                       (Select (SelectList [selI "c", selI "d"])
>                        (Just $ From $ Tref "e") Nothing Nothing Nothing)]

complicated statements

>      ,p "for r in select a from tbl loop\n\
>         \null;\n\
>         \end loop;"
>       [ForStatement "r" (Select
>                          (SelectList [selI "a"])
>                          (Just $ From $ Tref "tbl")
>                          Nothing Nothing Nothing)
>        [NullStatement]]
>      ,p "for r in select a from tbl where true loop\n\
>         \null;\n\
>         \end loop;"
>       [ForStatement "r" (Select
>                          (SelectList [selI "a"])
>                          (Just $ From $ Tref "tbl")
>                          (Just $ Where $ BooleanL True)
>                          Nothing Nothing)
>        [NullStatement]]
>      ,p "if a=b then\n\
>         \  update c set d = e;\n\
>         \end if;"
>       [If (BinaryOperatorCall  Eql (Identifier "a") (Identifier "b"))
>               [Update "c" [SetClause "d" (Identifier "e")] Nothing]
>        Nothing]
>      ,p "if true then\n\
>         \  null;\n\
>         \else\n\
>         \  null;\n\
>         \end if;"
>       [If (BooleanL True)
>        [NullStatement]
>        (Just [NullStatement])]
>      ])
>        --,testProperty "random expression" prop_expression_ppp
>        -- ,testProperty "random statements" prop_statements_ppp
>     ]
>         where
>           mapExpr = map $ uncurry checkParseExpression
>           mapSql = map $ uncurry checkParse
>           p a b = (a,b)
>           selIL as = SelectList $ map selI as
>           selI = SelExp . Identifier
>           selectE selList = Select selList Nothing Nothing Nothing Nothing
>           qi a b = BinaryOperatorCall Qual (Identifier a) (Identifier b)
>           selectFrom selList frm =
>             Select selList (Just $ From frm) Nothing Nothing Nothing
>           selectFromWhere selList frm whr =
>             Select selList (Just $ From frm) (Just $ Where whr) Nothing Nothing

================================================================================

Unit test helpers

parse and then pretty print and parse a statement

> checkParse :: String -> [Statement] -> Test.Framework.Test
> checkParse src ast = testCase ("parse " ++ src) $ do
>   let ast' = case parseSql src of
>               Left er -> error $ showEr er src
>               Right l -> l
>   assertEqual ("parse " ++ src) ast ast'
>   -- pretty print then parse to check
>   let pp = printSql ast
>   let ast'' = case parseSql pp of
>               Left er -> error $ "reparse " ++ showEr er pp ++ "\n" ++ pp ++ "\n"
>               Right l -> l
>   assertEqual ("reparse " ++ pp) ast ast''

parse and then pretty print and parse an expression

> checkParseExpression :: String -> Expression -> Test.Framework.Test
> checkParseExpression src ast = testCase ("parse " ++ src) $ do
>   let ast' = case parseExpression src of
>               Left er -> error $ showEr er src
>               Right l -> l
>   assertEqual ("parse " ++ src) ast ast'
>   let pp = printExpression ast
>   let ast'' = case parseExpression pp of
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
-- >                  liftM3 BinaryOperatorCall arbitrary arbitrary arbitrary
-- >                 ,liftM IntegerL arbitrary
-- >                 ,liftM StringL aString
-- >                 ,liftM BooleanL arbitrary
-- >                 ,liftM Identifier aIdentifier
-- >                 ,liftM2 FunctionCall aIdentifier arbitrary
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
