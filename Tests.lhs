#!/usr/bin/env runghc


> import Test.HUnit

> import Test.QuickCheck

> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char

> import Test.Framework.Providers.QuickCheck2

> import Control.Monad

> import Grammar
> import Parser
> import PrettyPrinter

> main :: IO ()
> main =
>   defaultMain [

>         testGroup "parse expression" [
>                        checkParseExpression "1" (IntegerL 1)
>                       ,checkParseExpression " 1 + 1 " (BinaryOperatorCall Plus (IntegerL 1) (IntegerL 1))
>                       ,checkParseExpression "1+1+1" (BinaryOperatorCall Plus (BinaryOperatorCall Plus (IntegerL 1) (IntegerL 1)) (IntegerL 1))
>                       ,checkParseExpression "(1)" (IntegerL 1)
>                       ,checkParseExpression "'test'" (StringL "test")
>                       ,checkParseExpression "''" (StringL "")
>                       ,checkParseExpression "hello" (Identifier "hello")
>                       ,checkParseExpression "helloTest" (Identifier "helloTest")
>                       ,checkParseExpression "hello_test" (Identifier "hello_test")
>                       ,checkParseExpression "hello1234" (Identifier "hello1234")
>                       ,checkParseExpression "1 + tst1" (BinaryOperatorCall Plus
>                                                           (IntegerL 1)
>                                                           (Identifier "tst1"))
>                       ,checkParseExpression "tst1 + 1" (BinaryOperatorCall Plus
>                                                         (Identifier "tst1")
>                                                         (IntegerL 1))
>                       ,checkParseExpression "tst + tst1" (BinaryOperatorCall Plus
>                                                           (Identifier "tst")
>                                                           (Identifier "tst1"))
>                       ,checkParseExpression "fn()" (FunctionCall "fn" [])
>                       ,checkParseExpression "fn(1)" (FunctionCall "fn" [IntegerL 1])
>                       ,checkParseExpression "fn('test')" (FunctionCall "fn" [StringL "test"])
>                       ,checkParseExpression "fn(1,'test')" (FunctionCall "fn" [IntegerL 1, StringL "test"])
>                       ,checkParseExpression "true" (BooleanL True)
>                       ,checkParseExpression "false" (BooleanL False)
>                       ,checkParseExpression "fn (1)" (FunctionCall "fn" [IntegerL 1])
>                       ,checkParseExpression "fn( 1)" (FunctionCall "fn" [IntegerL 1])
>                       ,checkParseExpression "fn(1 )" (FunctionCall "fn" [IntegerL 1])
>                       ,checkParseExpression "fn(1) " (FunctionCall "fn" [IntegerL 1])
>                       ,checkParseExpression "fn('test')" (FunctionCall "fn" [StringL "test"])
>                       ,checkParseExpression "'a' || 'b'" (BinaryOperatorCall Conc (StringL "a")
>                                                           (StringL "b"))
>                       ,checkParseExpression "null" (NullL)
>                       ,checkParseExpression "not null" (BinaryOperatorCall Not (NullL) (NullL))
>                       ,checkParseExpression "a is null" (BinaryOperatorCall IsNull (NullL) (Identifier "a"))
>                       ,checkParseExpression "a is not null" (BinaryOperatorCall IsNotNull (NullL) (Identifier "a"))
>                       ,checkParseExpression "'stuff'::text" (BinaryOperatorCall
>                                                                Cast (StringL "stuff") (Identifier "text"))
>                       ,checkParseExpression "array[1,2]" (ArrayL [IntegerL 1, IntegerL 2])
>                       ,checkParseExpression "case when a then 3\n\
>                                             \     when b then 4\n\
>                                             \     else 5\n\
>                                             \end"
>                                             (Case [When (Identifier "a") (IntegerL 3)
>                                                   ,When (Identifier "b") (IntegerL 4)]
>                                              (Just $ Else (IntegerL 5)))
>                       ,checkParseExpression "$1" (PositionalArg 1)
>                       ]
>        ,testGroup  "string parsing" [
>                         checkParseExpression "''" (StringL "")
>                        ,checkParseExpression "''''" (StringL "'")
>                        ,checkParseExpression "'test'''" (StringL "test'")
>                        ,checkParseExpression "'''test'" (StringL "'test")
>                        ,checkParseExpression "'te''st'" (StringL "te'st")
>                        ,checkParseExpression "$$test$$" (StringLD "" "test")
>                        ,checkParseExpression "$$te'st$$" (StringLD "" "te'st")
>                        ,checkParseExpression "$st$test$st$" (StringLD "st" "test")
>                        ,checkParseExpression "$outer$te$$yup$$st$outer$" (StringLD "outer" "te$$yup$$st")
>                        ,checkParseExpression "'spl$$it'" (StringL "spl$$it")
>                        ]

>        ,testGroup "select expression" [
>                        checkParse "select 1;" [selectE (SelectList [SelExp (IntegerL 1)])]
>                       --,checkParse "select 1+1;" [(selectE $ BinaryOperatorCall Plus (IntegerL 1) (IntegerL 1))]
>                       --,checkParse "select 'test';" [(selectE $ StringL "test")]
>                       --,checkParse "select fn(1, 'test');"
>                       -- [(selectE $ FunctionCall "fn" [IntegerL 1, StringL "test"])]
>                       --,checkParse "select 3 as tst, 4 as tst1;" [selectE [SelectItem (IntegerL 3) "tst"
>                       --                                                   ,SelectItem (IntegerL 4) "tst1"]]
>                       ]
>        ,testGroup "select from table" [
>                        checkParse "select * from tbl;" [(Select (SelectList [
>                                                              selI "*"]) (Just $ From $ Tref "tbl") Nothing)]
>                       ,checkParse "select a,b from tbl;" [(Select
>                                                            (SelectList [
>                                                              selI "a"
>                                                             ,selI "b"]) (Just $ From $ Tref "tbl") Nothing)]
>                       ,checkParse "select a from tbl where b=2;"
>                                   [(Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl")
>                                            (Just (Where $ BinaryOperatorCall Eql
>                                                  (Identifier "b") (IntegerL 2))))]
>                       ,checkParse "select a from tbl where b=2 and c=3;"
>                                   [(Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl")
>                                            (Just (Where $
>                                                   BinaryOperatorCall And
>                                                    (BinaryOperatorCall Eql
>                                                    (Identifier "b") (IntegerL 2))
>                                                    (BinaryOperatorCall Eql
>                                                    (Identifier "c") (IntegerL 3))
>                                                   )))]
>                       ,checkParse "select a from tbl\n\
>                                   \except\n\
>                                   \select a from tbl1;"
>                                   [(CombineSelect Except
>                                     (Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl") Nothing)
>                                     (Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl1") Nothing))]
>                       ,checkParse "select a from tbl where true\n\
>                                   \except\n\
>                                   \select a from tbl1 where true;"
>                                   [(CombineSelect Except
>                                     (Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl") (Just $ Where $ BooleanL True))
>                                     (Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl1") (Just $ Where $ BooleanL True)))]
>                       ,checkParse "select a from tbl\n\
>                                   \union\n\
>                                   \select a from tbl1;"
>                                   [(CombineSelect Union
>                                     (Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl") Nothing)
>                                     (Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl1") Nothing))]
>                       ,checkParse "select a as b from tbl;"
>                                   [(Select (SelectList [SelectItem (Identifier "a") "b"]) (Just $ From $ Tref "tbl") Nothing)]
>                       ,checkParse "select a + b as b from tbl;"
>                                   [(Select (SelectList [SelectItem (BinaryOperatorCall Plus (Identifier "a") (Identifier "b")) "b"]) (Just $ From $ Tref "tbl") Nothing)]
>                       ,checkParse "select a.* from tbl a;"
>                                   [Select (SelectList [SelExp (qi "a" "*")])
>                                           (Just $ From $ TrefAlias "tbl" "a") Nothing]
>                       ,checkParse "select a from b inner join c on b.a=c.a;"
>                                   [Select (SelectList [SelExp (Identifier "a")])
>                                      (Just $ From $ JoinedTref (Tref "b") False Inner (Tref "c")
>                                      (Just $ (BinaryOperatorCall Eql
>                                                 (qi "b" "a")
>                                                 (qi "c" "a"))))
>                                      Nothing]
>                       ,checkParse "select a from b natural inner join c;"
>                        [Select (SelectList [SelExp (Identifier "a")])
>                         (Just (From (JoinedTref (Tref "b") True Inner (Tref "c") Nothing))) Nothing]
>                       ,checkParse "select a from b left outer join c;"
>                        [Select (SelectList [SelExp (Identifier "a")])
>                         (Just (From (JoinedTref (Tref "b") False LeftOuter (Tref "c") Nothing))) Nothing]
>                       ,checkParse "select a from b full outer join c;"
>                        [Select (SelectList [SelExp (Identifier "a")])
>                         (Just (From (JoinedTref (Tref "b") False FullOuter (Tref "c") Nothing))) Nothing]
>                       ,checkParse "select a from b right outer join c;"
>                        [Select (SelectList [SelExp (Identifier "a")])
>                         (Just (From (JoinedTref (Tref "b") False RightOuter (Tref "c") Nothing))) Nothing]
>                       ,checkParse "select a from b cross join c;"
>                        [Select (SelectList [SelExp (Identifier "a")])
>                         (Just (From (JoinedTref (Tref "b") False Cross (Tref "c") Nothing))) Nothing]
>                       ,checkParse "select a from b\n\
>                                   \    inner join c\n\
>                                   \      on true\n\
>                                   \    inner join d\n\
>                                   \      on 1=1;"
>                                   [Select
>                                    (SelectList [SelExp (Identifier "a")])
>                                    (Just (From (JoinedTref
>                                                 (JoinedTref (Tref "b") False Inner (Tref "c")
>                                                  (Just (BooleanL True)))
>                                                 False Inner
>                                                 (Tref "d")
>                                                 (Just (BinaryOperatorCall Eql
>                                                        (IntegerL 1) (IntegerL 1))))))
>                                    Nothing]
>                       ,checkParse "select row_number() over(order by a) as place from tbl;"
>                                   [Select (SelectList [SelectItem
>                                                        (WindowFn
>                                                         (FunctionCall "row_number" [])
>                                                         (Just [Identifier "a"]))
>                                                        ("place")])
>                                      (Just $ From $ Tref "tbl")
>                                      Nothing]
>                       ,checkParse "select * from a natural inner join (select * from b) as a;"
>                                   [Select
>                                    (SelectList [SelExp (Identifier "*")])
>                                    (Just (From (JoinedTref (Tref "a") True Inner
>                                                 (SubTref (Select
>                                                           (SelectList [SelExp (Identifier "*")])
>                                                           (Just (From (Tref "b")))
>                                                           Nothing) "a")
>                                                 Nothing)))
>                                    Nothing]
>                       ]
>        ,testGroup "multiple statements" [
>                         checkParse "select 1;\nselect 2;" [selectE $ SelectList [SelExp (IntegerL 1)]
>                                                           ,selectE $ SelectList [SelExp (IntegerL 2)]
>                                                           ]
>                         ]
>        ,testGroup "more expressions" [
>                       checkParseExpression "(select a from tbl where id = 3)"
>                          (ScalarSubQuery $ Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl")
>                             (Just $ Where $ BinaryOperatorCall Eql (Identifier "id") (IntegerL 3)))
>                       ]
>        ,testGroup "comments" [
>                        checkParse "" []
>                       ,checkParse "-- this is a test" []
>                       ,checkParse "/* this is\n\
>                                   \a test*/" []
>                       ,checkParse "select 1;\n\
>                                   \-- this is a test\n\
>                                   \select -- this is a test\n\
>                                   \2;" [selectE $ SelectList [SelExp (IntegerL 1)]
>                                        ,selectE $ SelectList [SelExp (IntegerL 2)]
>                                        ]
>                       ,checkParse "select 1;\n\
>                                   \/* this is\n\
>                                   \a test*/\n\
>                                   \select /* this is a test*/2;"
>                                   [selectE $ SelectList [SelExp (IntegerL 1)]
>                                   ,selectE $ SelectList [SelExp (IntegerL 2)]
>                                   ]
>                       ]
>        ,testGroup "rud" [
>                        checkParse "insert into testtable\n\
>                                   \(columna,columnb)\n\
>                                   \values (1,2);\n" [(Insert "testtable"
>                                                              (Just ["columna", "columnb"])
>                                                              [IntegerL 1,
>                                                               IntegerL 2])]
>                       ,checkParse "update tb\n\
>                                   \  set x = 1, y = 2;"
>                                   [Update "tb" [SetClause "x" (IntegerL 1)
>                                                ,SetClause "y" (IntegerL 2)]
>                                      Nothing]
>                       ,checkParse "update tb\n\
>                                   \  set x = 1, y = 2 where z = true;"
>                                   [Update "tb" [SetClause "x" (IntegerL 1)
>                                                ,SetClause "y" (IntegerL 2)]
>                                      (Just $ Where $ BinaryOperatorCall Eql
>                                                      (Identifier "z") (BooleanL True))]
>                       ,checkParse "delete from tbl1 where x = true;"
>                                   [Delete "tbl1" (Just $ Where $ BinaryOperatorCall Eql
>                                                      (Identifier "x") (BooleanL True))]
>                                   ]
>        ,checkParse "copy tbl(a,b) from stdin;\n\
>                    \bat	t\n\
>                    \bear	f\n\
>                    \\\.\n"
>                    [Copy "tbl(a,b) from stdin;\n\
>                    \bat	t\n\
>                    \bear	f\n\
>                    \\\.\n"]
>        ,testGroup "create" [
>                        checkParse "create table test (\n\
>                                    \  fielda text,\n\
>                                    \  fieldb int\n\
>                                    \);" [(CreateTable "test" [
>                                                            AttributeDef "fielda" "text" Nothing Nothing
>                                                           ,AttributeDef "fieldb" "int" Nothing Nothing
>                                                           ])]
>                        ,checkParse "create table test (\n\
>                                    \type text check (type in('a', 'b')));"
>                                    [(CreateTable "test" [
>                                                       AttributeDef "type" "text" Nothing
>                                                         (Just (InPredicate
>                                                               "type"
>                                                               [StringL "a"
>                                                               ,StringL "b"]))])]
>                        ,checkParse "create table tb (\n\
>                                    \a text not null,\n\
>                                    \b boolean null);"
>                                    [(CreateTable "tb" [AttributeDef "a" "text" Nothing
>                                                          (Just (BinaryOperatorCall Not NullL NullL))
>                                                       ,AttributeDef "b" "boolean"  Nothing (Just NullL)])]
>                        ,checkParse "create table tbl (\n\
>                                    \  fld boolean default false);"
>                                    [CreateTable "tbl" [AttributeDef "fld" "boolean"
>                                                           (Just $ BooleanL False)
>                                                           Nothing]]
>                        ,checkParse "create view v1 as\n\
>                                    \select a,b from t;"
>                                    [(CreateView "v1"
>                                        (Select (SelectList [selI "a", selI "b"]) (Just $ From $ Tref "t") Nothing))]
>                        ,checkParse "create domain td as text check (value in ('t1', 't2'));"
>                                    [(CreateDomain "td" "text"
>                                        (Just (InPredicate
>                                               "value"
>                                               [StringL "t1"
>                                               ,StringL "t2"])))]
>                        ,checkParse "create type tp1 as (\n\
>                                    \  f1 text,\n\
>                                    \  f2 text\n\
>                                    \);"
>                                    [CreateType "tp1" [TypeAttDef "f1" "text"
>                                                      ,TypeAttDef "f2" "text"]]
>                        ]

>        ,testGroup "functions" [
>                         checkParse "create function t1(text) returns text as $$\n\
>                                     \select a from t1 where b = $1;\n\
>                                     \$$ language sql stable;"
>                                     [CreateFunction Sql "t1"
>                                      [ParamDefTp "text"] "text" "$$"
>                                      (SqlFnBody
>                                       [Select (SelectList [SelExp (Identifier "a")])
>                                        (Just (From (Tref "t1")))
>                                        (Just (Where (BinaryOperatorCall Eql
>                                                      (Identifier "b") (PositionalArg 1))))]) Stable]
>                        ,checkParse "create function fn() returns void as $$\n\
>                                    \declare\n\
>                                    \  a int;\n\
>                                    \  b text;\n\
>                                    \begin\n\
>                                    \  null;\n\
>                                    \end;\n\
>                                    \$$ language plpgsql volatile;"
>                                    [CreateFunction Plpgsql "fn" [] "void" "$$"
>                                       (PlpgsqlFnBody [VarDef "a" "int"
>                                                      ,VarDef "b" "text"]
>                                        [NullStatement])
>                                       Volatile
>                                      ]
>                        ,checkParse "create function fn() returns void as '\n\
>                                    \declare\n\
>                                    \  a int;\n\
>                                    \begin\n\
>                                    \  null;\n\
>                                    \end;\n\
>                                    \' language plpgsql stable;"
>                                    [CreateFunction Plpgsql "fn" [] "void" "'"
>                                      (PlpgsqlFnBody [VarDef "a" "int"]
>                                                     [NullStatement])
>                                      Stable
>                                      ]
>                       ]
>        ,testGroup "plpgsqlStatements" [
>                        checkParse "success := true;"
>                                    [Assignment "success" (BooleanL True)]
>                       ,checkParse "return true;"
>                                    [Return (BooleanL True)]
>                       ,checkParse "raise notice 'stuff %', 1;"
>                                    [Raise RNotice "stuff %" [IntegerL 1]]
>                       ,checkParse "for r in select a from tbl loop\n\
>                                   \null;\n\
>                                   \end loop;"
>                                    [ForStatement "r" (Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl") Nothing)
>                                         [NullStatement]]
>                       ,checkParse "for r in select a from tbl where true loop\n\
>                                   \null;\n\
>                                   \end loop;"
>                                    [ForStatement "r" (Select (SelectList [selI "a"]) (Just $ From $ Tref "tbl")
>                                                         (Just $ Where $ BooleanL True))
>                                         [NullStatement]]
>                       ,checkParse "perform test();"
>                                    [Perform $ FunctionCall "test" []]
>                       ,checkParse "perform test(a,b);"
>                                    [Perform $ FunctionCall "test" [Identifier "a", Identifier "b"]]
>                       ,checkParse "perform test(r.relvar_name || '_and_stuff');"
>                                    [Perform $ FunctionCall "test" [
>                                       BinaryOperatorCall Conc (qi "r" "relvar_name")
>                                                               (StringL "_and_stuff")]]
>                       ,checkParse "select into a,b c,d from e;"
>                                   [SelectInto ["a", "b"]
>                                    (Select (SelectList [selI "c", selI "d"])
>                                     (Just $ From $ Tref "e") Nothing)]
>                       ,checkParse "if a=b then\n\
>                                   \  update c set d = e;\n\
>                                   \end if;"
>                                   [If (BinaryOperatorCall  Eql (Identifier "a") (Identifier "b"))
>                                       [Update "c" [SetClause "d" (Identifier "e")] Nothing]]

>                       ]
>        --,testProperty "random expression" prop_expression_ppp
>        -- ,testProperty "random statements" prop_statements_ppp
>        ]

> selI :: String -> SelectItem
> selI i = SelExp (Identifier i)

> selectE :: SelectList -> Statement
> selectE selList = Select selList Nothing Nothing

> qi :: String -> String -> Expression
> qi a b = BinaryOperatorCall Qual (Identifier a) (Identifier b)

================================================================================

Unit test helpers

parse and then pretty print and parse a statement

> checkParse :: String -> [Statement] -> Test.Framework.Test
> checkParse src ast = testCase ("parse " ++ src) $ do
>   let ast' = case parseSql src of
>               Left er -> error $ show er
>               Right l -> l
>   assertEqual ("parse " ++ src) ast ast'
>   -- pretty print then parse to check
>   let pp = printSql ast
>   let ast'' = case parseSql pp of
>               Left er -> error $ "reparse " ++ show er ++ "\n" ++ pp ++ "\n"
>               Right l -> l
>   assertEqual ("reparse " ++ pp) ast ast''


parse and then pretty print and parse an expression

> checkParseExpression :: String -> Expression -> Test.Framework.Test
> checkParseExpression src ast = testCase ("parse " ++ src) $ do
>   let ast' = case parseExpression src of
>               Left er -> error $ show er
>               Right l -> l
>   assertEqual ("parse " ++ src) ast ast'
>   let pp = printExpression ast
>   let ast'' = case parseExpression pp of
>               Left er -> error $ "reparse " ++ show er ++ "\n" ++ pp ++ "\n"
>               Right l -> l
>   assertEqual ("reparse " ++ pp) ast ast''

================================================================================

Properties

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

> parseSqlThrow :: String -> [Statement]
> parseSqlThrow s =
>     case parseSql s of
>       Left er -> error $ "parse " ++ show er ++ "****" ++ s ++ "****"
>       Right l -> l


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

> x = "create function adjust_world_alignment() returns void as $$\n\
> \declare\n\
> \  abs_change float;\n\
> \begin\n\
> \  --select into abs_change\n\
> \  -- min(abs(get_cast_alignment()) / 2, 2);\n\
> \  --update world_alignment_table\n\
> \  --  set world_alignment = world_alignment\n\
> \  --    + trunc(abs_change) * sign(get_cast_alignment());\n\
> \  --get fractional part\n\
> \  --if (random() < abs_change - trunc(abs_change)) then\n\
> \  if a then\n\
> \    update world_alignment_table\n\
> \      set world_alignment = world_alignment +\n\
> \        sign(get_cast_alignment());\n\
> \  end if;\n\
> \  --update cast_alignment_table set cast_alignment = 0;\n\
> \end;\n\
> \$$ language plpgsql volatile;"
