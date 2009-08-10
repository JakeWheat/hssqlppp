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

> x = "--create view chaos_base_relvars as\n\
>   \select object_name,object_type from public_database_objects\n\
>   \where object_type = 'base_relvar'\n\
>   \except\n\
>   \      select object_name,object_type from module_objects\n\
>   \      where module_name = 'catalog' and object_type='base_relvar';"


> main :: IO ()
> main = do
>   defaultMain [

>         testGroup "parse expression" [
>                        checkParseExpression "1" (IntegerL 1)
>                       ,checkParseExpression " 1 + 1 " (BinaryOperatorCall Plus (IntegerL 1) (IntegerL 1))
>                       ,checkParseExpression "1+1+1" (BinaryOperatorCall Plus (BinaryOperatorCall Plus (IntegerL 1) (IntegerL 1)) (IntegerL 1))
>                       ,checkParseExpression "(1)" (IntegerL 1)
>                       ,checkParseExpression "'test'" (StringL "test")
>                       ,checkParseExpression "''" (StringL "")
>                       ,checkParseExpression "hello" (Identifier "hello")
>                       --,checkParseExpression "''''" (StringL "'")
>                       --,checkParseExpression "'test'''" (StringL "test'")
>                       --,checkParseExpression "'''test'" (StringL "'test")
>                       --,checkParseExpression "'te''st'" (StringL "te'st")
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
>                       ]
>        ,testGroup "select expression" [
>                        checkParse "select 1;" [(SelectE $ IntegerL 1)]
>                       ,checkParse "select 1+1;" [(SelectE $ BinaryOperatorCall Plus (IntegerL 1) (IntegerL 1))]
>                       ,checkParse "select 'test';" [(SelectE $ StringL "test")]
>                       ,checkParse "select fn(1, 'test');"
>                        [(SelectE $ FunctionCall "fn" [IntegerL 1, StringL "test"])]
>                       ]
>        ,testGroup "select from table" [
>                        checkParse "select * from tbl;" [(Select Star "tbl" Nothing)]
>                       ,checkParse "select a,b from tbl;" [(Select (SelectList ["a","b"]) "tbl" Nothing)]
>                       ,checkParse "select a from tbl where b=2;"
>                                   [(Select (SelectList ["a"]) "tbl"
>                                            (Just (Where $ BinaryOperatorCall Eql
>                                                  (Identifier "b") (IntegerL 2))))]
>                       ,checkParse "select a from tbl where b=2 and c=3;"
>                                   [(Select (SelectList ["a"]) "tbl"
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
>                                   [(ExceptSelect
>                                     (Select (SelectList ["a"]) "tbl" Nothing)
>                                     (Select (SelectList ["a"]) "tbl1" Nothing))]
>                       ,checkParse "select a from tbl where true\n\
>                                   \except\n\
>                                   \select a from tbl1 where true;"
>                                   [(ExceptSelect
>                                     (Select (SelectList ["a"]) "tbl" (Just $ Where $ BooleanL True))
>                                     (Select (SelectList ["a"]) "tbl1" (Just $ Where $ BooleanL True)))]
>                       ]
>        ,testGroup "multiple statements" [
>                         checkParse "select 1;\nselect 2;" [(SelectE $ IntegerL 1)
>                                                           ,(SelectE $ IntegerL 2)
>                                                           ]
>                         ]
>        ,testGroup "comments" [
>                        checkParse "" []
>                       ,checkParse "-- this is a test" []
>                       ,checkParse "/* this is\n\
>                                    \a test*/" []
>                       ,checkParse "select 1;\n\
>                                    \-- this is a test\n\
>                                    \select -- this is a test\n\
>                                    \2;" [(SelectE $ IntegerL 1)
>                                                           ,(SelectE $ IntegerL 2)
>                                                           ]
>                       ,checkParse "select 1;\n\
>                                   \/* this is\n\
>                                   \a test*/\n\
>                                   \select /* this is a test*/2;"
>                                   [(SelectE $ IntegerL 1)
>                                   ,(SelectE $ IntegerL 2)
>                                   ]
>                       ]
>        ,testGroup "rud" [
>                        checkParse "insert into testtable\n\
>                                   \(columna,columnb)\n\
>                                   \values (1,2);\n" [(Insert "testtable"
>                                                              ["columna", "columnb"]
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
>        ,testGroup "create" [
>                        checkParse "create table test (\n\
>                                    \  fielda text,\n\
>                                    \  fieldb int\n\
>                                    \);" [(CreateTable "test" [
>                                                            AttributeDef "fielda" "text" Nothing
>                                                           ,AttributeDef "fieldb" "int" Nothing
>                                                           ])]
>                        ,checkParse "create table test (\n\
>                                    \type text check (type in('a', 'b')));"
>                                    [(CreateTable "test" [
>                                                       AttributeDef "type" "text"
>                                                         (Just (InPredicate
>                                                               "type"
>                                                               [StringL "a"
>                                                               ,StringL "b"]))])]
>                        ,checkParse "create function fn() returns void as $$\n\
>                                    \begin\n\
>                                    \  null;\n\
>                                    \end;\n\
>                                    \$$ language plpgsql volatile;"
>                                    [(CreateFunction "fn" [] "void" [NullStatement])]
>                        ,checkParse "create view v1 as\n\
>                                    \select a,b from t;"
>                                    [(CreateView "v1"
>                                        (Select (SelectList ["a","b"]) "t" Nothing))]
>                        ]
>        --,testProperty "random expression" prop_expression_ppp
>        -- ,testProperty "random statements" prop_statements_ppp
>        ]

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

> prop_statements_ppp :: [Statement] -> Bool
> prop_statements_ppp s = (parseSqlThrow (printSql s)) == s

> prop_expression_ppp :: Expression -> Bool
> prop_expression_ppp s = (parseExpressionThrow (printExpression s)) == s

> parseExpressionThrow :: String -> Expression
> parseExpressionThrow s =
>     case parseExpression s of
>       Left er -> error $ "parse " ++ show er ++ "****" ++ s ++ "****"
>       Right l -> l

> parseSqlThrow :: String -> [Statement]
> parseSqlThrow s =
>     case parseSql s of
>       Left er -> error $ "parse " ++ show er ++ "****" ++ s ++ "****"
>       Right l -> l


arbitrary instances

> instance Arbitrary Expression where
>     arbitrary = oneof [
>                  liftM3 BinaryOperatorCall arbitrary arbitrary arbitrary
>                 ,liftM IntegerL arbitrary
>                 ,liftM StringL aString
>                 ,liftM BooleanL arbitrary
>                 ,liftM Identifier aIdentifier
>                 ,liftM2 FunctionCall aIdentifier arbitrary
>                 ]

> instance Arbitrary Statement where
>     arbitrary = oneof [
>                  liftM SelectE arbitrary
>                 ,liftM3 Select arbitrary aIdentifier arbitrary
>                 ,liftM2 CreateTable aIdentifier arbitrary
>                 ,liftM3 Insert aIdentifier arbitrary arbitrary
>                 ,liftM3 Update aIdentifier arbitrary arbitrary
>                 ]


> instance Arbitrary Op where
>     arbitrary = elements [Plus, Minus, Mult, Div, Pow, Mod, Eql]

> instance Arbitrary SelectList where
>     arbitrary = oneof [
>                  liftM SelectList arbitrary
>                 ,return Star
>                 ]

> instance Arbitrary AttributeDef where
>     arbitrary = liftM3 AttributeDef aIdentifier aIdentifier arbitrary

> instance Arbitrary SetClause where
>     arbitrary = liftM2 SetClause aIdentifier arbitrary

> instance Arbitrary Where where
>     arbitrary = liftM Where arbitrary

some gen helpers

> aString :: Gen [Char]
> aString = listOf1 $ choose ('\32', '\126')

> aIdentifier :: Gen [Char]
> aIdentifier = do
>   start <- elements $ letter
>   suffix <- listOf $ elements $ letter ++ ['_'] ++ ['0' .. '9']
>   return (start : suffix)
>               where letter = ['A'..'Z'] ++ ['a' .. 'z']
