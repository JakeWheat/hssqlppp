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
>          testGroup "select expression" [
>                        checkParse "select 1;" [(SelectE $ IntegerLiteral 1)]
>                       ,checkParse "select 1+1;"
>                        [(SelectE $ BinaryOperatorCall "+" (IntegerLiteral 1) (IntegerLiteral 1))]
>                       ,checkParse "select 1 + 1;"
>                        [(SelectE $ BinaryOperatorCall "+" (IntegerLiteral 1) (IntegerLiteral 1))]
>                       ,checkParse "select 1+1+1;"
>                        [(SelectE $ BinaryOperatorCall "+" (IntegerLiteral 1) (IntegerLiteral 1))]
>                       ,checkParse "select 'test';" [(SelectE $ StringLiteral "test")]
>                       ,checkParse "select fn();" [(SelectE $ FunctionCall "fn" [])]
>                       ,checkParse "select fn(1);" [(SelectE $ FunctionCall "fn" [IntegerLiteral 1])]
>                       ,checkParse "select fn (1);" [(SelectE $ FunctionCall "fn" [IntegerLiteral 1])]
>                       ,checkParse "select fn( 1);" [(SelectE $ FunctionCall "fn" [IntegerLiteral 1])]
>                       ,checkParse "select fn(1 );" [(SelectE $ FunctionCall "fn" [IntegerLiteral 1])]
>                       ,checkParse "select fn(1) ;" [(SelectE $ FunctionCall "fn" [IntegerLiteral 1])]
>                       ,checkParse "select fn('test');" [(SelectE $ FunctionCall "fn" [StringLiteral "test"])]
>                       ,checkParse "select fn(1, 'test');"
>                        [(SelectE $ FunctionCall "fn" [IntegerLiteral 1, StringLiteral "test"])]
>                       ,checkParse "select 1;\nselect 2;" [(SelectE $ IntegerLiteral 1)
>                                                          ,(SelectE $ IntegerLiteral 2)
>                                                          ]
>                       ]
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
>                                                              [IntegerLiteral 1,
>                                                               IntegerLiteral 2])]
>                       ]
>        ,testGroup "update" [
>                        checkParse "update tb\n\
>                                   \  set x = 1, y = 2;"
>                                   [Update "tb" [SetClause "x" (IntegerLiteral 1)
>                                                ,SetClause "y" (IntegerLiteral 2)]
>                                      Nothing]
>                       ,checkParse "update tb\n\
>                                   \  set x = 1, y = 2 where z = true;"
>                                   [Update "tb" [SetClause "x" (IntegerLiteral 1)
>                                                ,SetClause "y" (IntegerLiteral 2)]
>                                      (Just $ Where $ BinaryOperatorCall "="
>                                                      (Identifier "z") (BooleanLiteral True))]

>                       ]
>        -- ,testProperty "random  statement" prop_select_ppp
>        ]

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
>                 ,liftM IntegerLiteral arbitrary
>                 ,liftM StringLiteral $ listOf1 $ arbitrary
>                 ,liftM2 FunctionCall aIdentifier arbitrary
>                 ,liftM3 BinaryOperatorCall aBinaryOp arbitrary arbitrary
>                 ]

> aString :: Gen [Char]
> aString = listOf1 $ choose ('\32', '\128')

> aIdentifier :: Gen [Char]
> aIdentifier = listOf1 $ choose ('\97', '\122')

> aBinaryOp :: Gen [Char]
> aBinaryOp = elements ["+", "-"]

> parseThrowError :: String -> [Statement]
> parseThrowError s = case parseSql s of
>               Left er -> error $ show er
>               Right l -> l

> prop_select_ppp :: [Statement] -> Bool
> prop_select_ppp s = (parseThrowError (printSql s)) == s

> listOf' :: Gen a -> Gen [a]
> listOf' gen = sized $ \n ->
>   do k <- choose (1,n)
>      vectorOf' k gen

> vectorOf' :: Int -> Gen a -> Gen [a]
> vectorOf' k gen = sequence [ gen | _ <- [0..k] ]
