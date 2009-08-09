#!/usr/bin/env runghc

select fn();
select fn(1);
select fn('test');

create table test (
  fielda text,
  fieldb int
);

select * from test;
insert
update
delete
select 1+1;

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
>         checkParse "select 1;" (Select $ IntegerLiteral 1)
>        ,checkParse "select 1+1;"
>           (Select $ BinaryOperatorCall "+" (IntegerLiteral 1) (IntegerLiteral 1))
>        ,checkParse "select 'test';" (Select $ StringLiteral "test")
>        ,checkParse "select fn();" (Select $ FunctionCall "fn" [])
>        ,checkParse "select fn(1);" (Select $ FunctionCall "fn" [IntegerLiteral 1])
>        ,checkParse "select fn('test');" (Select $ FunctionCall "fn" [StringLiteral "test"])
>        ,checkParse "select fn(1, 'test');"
>           (Select $ FunctionCall "fn" [IntegerLiteral 1, StringLiteral "test"])
>        ,testProperty "basic select function call" prop_select_ppp
>        ]

> checkParse :: String -> Select -> Test.Framework.Test
> checkParse src ast = testCase ("parse " ++ src) $ do
>   let ast' = case parseSql src of
>               Left er -> error $ show er
>               Right l -> l
>   assertEqual ("parse " ++ src) ast ast'

 > instance Arbitrary Char where
 >     arbitrary     = choose ('\32', '\128')
 >     coarbitrary c = variant (ord c `rem` 4)

 > instance Arbitrary Identifier where
 >     arbitrary = liftM Identifier $ listOf' $ choose ('\97', '\122')

> instance Arbitrary Select where
>     arbitrary = liftM Select arbitrary

> instance Arbitrary Expression where
>     arbitrary = oneof [
>                  liftM Identifier arbitrary
>                 ,liftM IntegerLiteral arbitrary
>                 ,liftM StringLiteral arbitrary
>                 ,liftM2 FunctionCall arbitrary arbitrary
>                 ,liftM3 BinaryOperatorCall arbitrary arbitrary arbitrary
>                 ]

> parseThrowError :: String -> Select
> parseThrowError s = case parseSql s of
>               Left er -> error $ show er
>               Right l -> l

> prop_select_ppp :: Select -> Bool
> prop_select_ppp s = (parseThrowError (printSql s)) == s

> listOf' :: Gen a -> Gen [a]
> listOf' gen = sized $ \n ->
>   do k <- choose (1,n)
>      vectorOf' k gen

> vectorOf' :: Int -> Gen a -> Gen [a]
> vectorOf' k gen = sequence [ gen | _ <- [0..k] ]
