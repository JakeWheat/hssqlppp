#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

Mainly test error messages from check failures from bad sql checks the
source positions, even though they are a bit fragile and checks the
types of error message received.

> module AstCheckTests (astCheckTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char

> import Ast
> import Parser

> astCheckTests :: Test.Framework.Test
> astCheckTests = testGroup "astCheckTests" [
>     testGroup "test test"
>     (mapAttr [
>       p "select 1;" []
>      ])
>    ,testGroup "loop tests"
>     (mapAttr [
>       p "create function cont_test1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \begin\n\
>         \  for r in select a from b loop\n\
>         \    null;\n\
>         \    continue;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;\n" []
>      ,p "create function cont_test2() returns void as $$\n\
>         \begin\n\
>         \    continue;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;\n" [Error ("",3,5) ContinueNotInLoop]
>      ])

>    ,testGroup "basic literal types"
>     (mapExprType [
>       p "1" (ScalarType "integer")
>      ,p "1.0" (ScalarType "float")
>      ,p "'test'" (ScalarType "text")
>      ,p "true" (ScalarType "boolean")
>      ,p "array[1,2,3]" (ArrayType (ScalarType "integer"))
>      ,p "array['a','b']" (ArrayType (ScalarType "text"))
>      ,p "array[1,'b']" (TypeError ("",0,0)
>                         (WrongTypes
>                          (ScalarType "integer")
>                          [ScalarType "integer",ScalarType "text"]))
>      ])

>    ,testGroup "some expressions"
>     (mapExprType [
>       p "1=1" (ScalarType "boolean")
>      ,p "1='test'" (TypeError ("",0,0)
>                     (NoMatchingOperator "=" [ScalarType "integer",ScalarType "text"]))
>      ,p "substring('aqbc' from 2 for 2)" (ScalarType "text")

>      ,p "substring(3 from 2 for 2)" (TypeError ("",0,0)
>                     (WrongTypeList [ScalarType "text"
>                                    ,ScalarType "integer"
>                                    ,ScalarType "integer"]
>                                    [ScalarType "integer"
>                                    ,ScalarType "integer"
>                                    ,ScalarType "integer"]))
>      ,p "substring('aqbc' from 2 for 'test')" (TypeError ("",0,0)
>                     (WrongTypeList [ScalarType "text"
>                                    ,ScalarType "integer"
>                                    ,ScalarType "integer"]
>                                    [ScalarType "text"
>                                    ,ScalarType "integer"
>                                    ,ScalarType "text"]))

>      ,p "3 between 2 and 4" (ScalarType "boolean")
>      ,p "3 between '2' and 4" (TypeError ("",0,0)
>                                (WrongTypes (ScalarType "integer")
>                                 [ScalarType "integer"
>                                 ,ScalarType "text"
>                                 ,ScalarType "integer"]))

>      ,p "array[1,2,3][2]" (ScalarType "integer")
>      ,p "array['a','b'][1]" (ScalarType "text")
>      ,p "array['a','b']['test']" (TypeError ("",0,0)
>                                   (WrongType
>                                    (ScalarType "integer")
>                                    (ScalarType "text")))

>      ,p "not true" (ScalarType "boolean")
>      ,p "not 1" (TypeError ("",0,0)
>                  (NoMatchingKOperator Not [ScalarType "integer"]))

>      ,p "@ 3" (ScalarType "integer")
>      ,p "@ 'a'" (TypeError ("",0,0)
>                  (NoMatchingOperator "@" [ScalarType "text"]))

>      ,p "-3" (ScalarType "integer")
>      ,p "-'a'" (TypeError ("",0,0)
>                  (NoMatchingOperator "-" [ScalarType "text"]))

>      ,p "4-3" (ScalarType "integer")

>      --,p "1 is null" (ScalarType "boolean")
>      --,p "1 is not null" (ScalarType "boolean")

>      ,p "1+1" (ScalarType "integer")
>      ,p "1+1" (ScalarType "integer")
>      ,p "31*511" (ScalarType "integer")
>      ,p "5/2" (ScalarType "integer")
>      --,p "2^10" (ScalarType "integer")
>      ,p "17%5" (ScalarType "integer")

>      ,p "3 and 4" (TypeError ("",0,0)
>                   (NoMatchingKOperator And [ScalarType "integer",ScalarType "integer"]))

>      ,p "True and False" (ScalarType "boolean")
>      ,p "false or true" (ScalarType "boolean")

>      ,p "lower('TEST')" (ScalarType "text")
>      ,p "lower(1)" (TypeError nsp (NoMatchingOperator "lower" [ScalarType "integer"]))

>      ])

>    ,testGroup "case expressions"
>     (mapExprType [
>       p "case\n\
>         \ when true then 1\n\
>         \end" (ScalarType "integer")
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" (ScalarType "text")
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 'test'=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" (TypeError ("",0,0)
>                (NoMatchingOperator "=" [ScalarType "text",ScalarType "integer"]))
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 'blah'\n\
>         \ else 1\n\
>         \end" (TypeError ("",0,0)
>                (WrongTypes (ScalarType "text")
>                  [ScalarType "text"
>                  ,ScalarType "text"
>                  ,ScalarType "integer"]))
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 1\n\
>         \ else 'else'\n\
>         \end" (TypeError ("",0,0)
>                (WrongTypes (ScalarType "text")
>                 [ScalarType "text"
>                 ,ScalarType "integer"
>                 ,ScalarType "text"]))
>      ])

>    ,testGroup "case expressions"
>     (mapExprType [
>       p "cast ('1' as integer)"
>         (ScalarType "integer")
>      ,p "cast ('1' as baz)"
>         (TypeError nsp (UnknownTypeError "baz"))


>      ])

TODO:
function calls
rowctor
casts
in list

select expressions:
select 1
select 1,2
select a from table
select a,b from table
select * from table
select a,b from table where a = 1
select except, union, intersect
select a as b from table
select a+b from table
joins
window functions
qualifier tests
select * from gen()
select count(b) from t
select a, count(b) from c group by a
select a, count(b) as cnt from c group by a having cnt > 4

exists
in query
scalar subquery

functions:
identifiers used in functions, refering to positional args, aliased
args, declared variables
etc.



>    ]
>         where
>           mapAttr = map $ uncurry checkAttrs
>           p a b = (a,b)
>           mapExprType = map $ uncurry checkExpressionType

> checkAttrs :: String -> [Message] -> Test.Framework.Test
> checkAttrs src msgs = testCase ("check " ++ src) $ do
>   let ast = case parseSql src of
>                Left er -> error $ show er
>                Right l -> l
>       msgs1 = checkAst ast
>   assertEqual ("check " ++ src) msgs msgs1

> checkExpressionType :: String -> Type -> Test.Framework.Test
> checkExpressionType src typ = testCase ("typecheck " ++ src) $ do
>   let ast = case parseExpression src of
>                Left er -> error $ show er
>                Right l -> l
>       typ1 = getExpressionType ast
>   assertEqual ("typecheck " ++ src) typ typ1
