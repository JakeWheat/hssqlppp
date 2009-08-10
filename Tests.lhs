#!/usr/bin/env runghc

insert
update
delete
create table base_relvar_metadata (
  relvar_name text,
  type text check (type in('readonly', 'data', 'stack'))
);

  insert into base_relvar_metadata (relvar_name, type)
    values (vname, vtype);

create function set_relvar_type(vname text, vtype text) returns void as $$
begin
  insert into base_relvar_metadata (relvar_name, type)
    values (vname, vtype);
end;
$$ language plpgsql volatile;

create view chaos_base_relvars as
  select object_name,object_type from public_database_objects
  where object_type = 'base_relvar'
  except
        select object_name,object_type from module_objects
        where module_name = 'catalog' and object_type='base_relvar';

 update wizard_spell_choices_mr
        set imaginary = false
        where wizard_name = get_current_wizard();

  delete from wizard_spell_choices_mr where wizard_name = get_current_wizard();

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
>        ,testGroup "multiple statements" [
>                         checkParse "select 1;\nselect 2;" [(SelectE $ IntegerL 1)
>                                                           ,(SelectE $ IntegerL 2)
>                                                           ]
>                         ]
>        ,testGroup "create" [
>                        checkParse "create table test (\n\
>                                    \  fielda text,\n\
>                                    \  fieldb int\n\
>                                    \);" [(CreateTable "test" [
>                                                            AttributeDef "fielda" "text"
>                                                           ,AttributeDef "fieldb" "int"
>                                                           ])]
>                        ]
>        ,testGroup "select from table" [
>                        checkParse "select * from tbl;" [(Select Star "tbl")]
>                       ,checkParse "select a,b from tbl;" [(Select (SelectList ["a","b"]) "tbl")]
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
>                 ,liftM2 Select arbitrary aIdentifier
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
>     arbitrary = liftM2 AttributeDef aIdentifier aIdentifier

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
