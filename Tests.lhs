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
>                       ,checkParseExpression "'test'" (StringL "test")
>                       ,checkParseExpression "''" (StringL "")
>                       ,checkParseExpression "hello" (Identifier "hello")
>                       ,checkParseExpression "helloTest" (Identifier "helloTest")
>                       ,checkParseExpression "hello_test" (Identifier "hello_test")
>                       ,checkParseExpression "hello1234" (Identifier "hello1234")
>                       ,checkParseExpression "1 + tst1" (BinaryOperatorCall Plus
>                                                           (IntegerL 1)
>                                                           (StringL "tst1"))
>                       ,checkParseExpression "tst1 + 1" (BinaryOperatorCall Plus
>                                                         (Identifier "tst1")
>                                                         (IntegerL 1))
>                       ,checkParseExpression "tst + tst1" (BinaryOperatorCall Plus
>                                                           (StringL "tst")
>                                                           (StringL "tst1"))
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
>        ,testGroup "insert" [
>                        checkParse "insert into testtable\n\
>                                   \(columna,columnb)\n\
>                                   \values (1,2);\n" [(Insert "testtable"
>                                                              ["columna", "columnb"]
>                                                              [IntegerL 1,
>                                                               IntegerL 2])]
>                       ]
>        ,testGroup "update" [
>                        checkParse "update tb\n\
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
>                       ]

> --        ,testProperty "random expression" prop_expression_ppp
>        -- ,testProperty "random  statement" prop_select_ppp
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

 > prop_select_ppp :: [Statement] -> Bool
 > prop_select_ppp s = (parseThrowError (printSql s)) == s

> prop_expression_ppp :: Expression -> Bool
> prop_expression_ppp s = (parseExpressionThrow (printExpression s)) == s

> parseExpressionThrow :: String -> Expression
> parseExpressionThrow s =
>     case parseExpression s of
>       Left er -> error $ "parse " ++ show er ++ "****" ++ s ++ "****"
>       Right l -> l

arbitrary instances

> instance Arbitrary Expression where
>     arbitrary = oneof [
>                  liftM3 BinaryOperatorCall arbitrary arbitrary arbitrary
>                 ,liftM IntegerL arbitrary
>                 ,liftM StringL arbitrary
>                 ,liftM BooleanL arbitrary
>                 ,liftM Identifier aIdentifier
>                 ,liftM2 FunctionCall aIdentifier arbitrary
>                 ]

> instance Arbitrary Op where
>     arbitrary = elements [Plus, Minus, Mult, Div, Pow, Mod, Eql]

 > instance Arbitrary Char where
 >     arbitrary     = choose ('\32', '\128')
 >     coarbitrary c = variant (ord c `rem` 4)

 > instance Arbitrary Identifier where
 >     arbitrary = liftM Identifier $ listOf' $ choose ('\97', '\122')

 > instance Arbitrary Statement where
 >     arbitrary = oneof [
 >                  liftM SelectE arbitrary
 >                 ,liftM2 CreateTable aIdentifier arbitrary
 >                 ]

 > instance Arbitrary AttributeDef where
 >     arbitrary = liftM2 AttributeDef aIdentifier aIdentifier

 > instance Arbitrary Expression where
 >     arbitrary = oneof [
 >                  liftM Identifier aIdentifier
 >                 ,liftM IntegerL arbitrary
 >                 ,liftM StringL $ listOf1 $ arbitrary
 >                 ,liftM2 FunctionCall aIdentifier arbitrary
 >                 ,liftM3 BinaryOperatorCall aBinaryOp arbitrary arbitrary
 >                 ]

some gen helpers

 > aString :: Gen [Char]
 > aString = listOf1 $ choose ('\32', '\128')

> aIdentifier :: Gen [Char]
> aIdentifier = do
>   start <- elements $ letter
>   suffix <- listOf $ elements $ letter ++ ['_'] ++ ['0' .. '9']
>   return (start : suffix)
>               where letter = ['A'..'Z'] ++ ['a' .. 'z']

 > aBinaryOp :: Gen [Char]
 > aBinaryOp = elements ["+", "-"]

 > listOf' :: Gen a -> Gen [a]
 > listOf' gen = sized $ \n ->
 >   do k <- choose (1,n)
 >      vectorOf' k gen

 > vectorOf' :: Int -> Gen a -> Gen [a]
 > vectorOf' k gen = sequence [ gen | _ <- [0..k] ]
