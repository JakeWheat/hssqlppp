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
>       p "1" (ScalarType "Integer")
>      ,p "1.0" (ScalarType "Float")
>      ,p "'test'" (ScalarType "String")
>      ,p "true" (ScalarType "Boolean")
>      ,p "array[1,2,3]" (ArrayType (ScalarType "Integer"))
>      ,p "array['a','b']" (ArrayType (ScalarType "String"))
>      ,p "array[1,'b']" (TypeError ("",0,0)
>                         (WrongTypes
>                          (ScalarType "Integer")
>                          [ScalarType "Integer",ScalarType "String"]))
>      ])

>    ,testGroup "some expressions"
>     (mapExprType [
>       p "1=1" (ScalarType "Boolean")
>      ,p "1='test'" (TypeError ("",0,0)
>                     (WrongTypes
>                     (ScalarType "Integer")
>                     [ScalarType "Integer",ScalarType "String"]))
>      ,p "substring('aqbc' from 2 for 2)" (ScalarType "String")

>      ,p "substring(3 from 2 for 2)" (TypeError ("",0,0)
>                     (WrongTypeList [ScalarType "String"
>                                    ,ScalarType "Integer"
>                                    ,ScalarType "Integer"]
>                                    [ScalarType "Integer"
>                                    ,ScalarType "Integer"
>                                    ,ScalarType "Integer"]))
>      ,p "substring('aqbc' from 2 for 'test')" (TypeError ("",0,0)
>                     (WrongTypeList [ScalarType "String"
>                                    ,ScalarType "Integer"
>                                    ,ScalarType "Integer"]
>                                    [ScalarType "String"
>                                    ,ScalarType "Integer"
>                                    ,ScalarType "String"]))

>      ,p "3 between 2 and 4" (ScalarType "Boolean")
>      ,p "3 between '2' and 4" (TypeError ("",0,0)
>                                (WrongTypes (ScalarType "Integer")
>                                 [ScalarType "Integer"
>                                 ,ScalarType "String"
>                                 ,ScalarType "Integer"]))

>      ,p "array[1,2,3][2]" (ScalarType "Integer")
>      ,p "array['a','b'][1]" (ScalarType "String")
>      ,p "array['a','b']['test']" (TypeError ("",0,0)
>                                   (WrongType
>                                    (ScalarType "Integer")
>                                    (ScalarType "String")))

>      ,p "not true" (ScalarType "Boolean")
>      ,p "not 1" (TypeError ("",0,0)
>                  (WrongTypeList
>                   [ScalarType "Boolean"]
>                   [ScalarType "Integer"]))

>      ,p "@ 3" (ScalarType "Integer")
>      ,p "@ 'a'" (TypeError ("",0,0)
>                  (WrongTypeList
>                   [ScalarType "Integer"]
>                   [ScalarType "String"]))

>      ,p "-3" (ScalarType "Integer")
>      ,p "-'a'" (TypeError ("",0,0)
>                  (WrongTypeList
>                   [ScalarType "Integer"]
>                   [ScalarType "String"]))

>      ,p "4-3" (ScalarType "Integer")
>      ,p "4-'a'" (TypeError ("",0,0)
>                  (WrongTypeList
>                   [ScalarType "Integer",ScalarType "Integer"]
>                   [ScalarType "Integer",ScalarType "String"]))

>      ,p "1 is null" (ScalarType "Boolean")
>      ,p "1 is not null" (ScalarType "Boolean")

>      ,p "1+1" (ScalarType "Integer")
>      ,p "1+True" (TypeError ("",0,0)
>                   (WrongTypeList
>                    [ScalarType "Integer",ScalarType "Integer"]
>                    [ScalarType "Integer",ScalarType "Boolean"]))
>      ,p "1+1" (ScalarType "Integer")
>      ,p "31*511" (ScalarType "Integer")
>      ,p "5/2" (ScalarType "Integer")
>      ,p "2^10" (ScalarType "Integer")
>      ,p "17%5" (ScalarType "Integer")

>      ,p "3 and 4" (TypeError ("",0,0)
>                   (WrongTypeList
>                    [ScalarType "Boolean",ScalarType "Boolean"]
>                    [ScalarType "Integer",ScalarType "Integer"]))

>      ,p "True and False" (ScalarType "Boolean")
>      ,p "false or true" (ScalarType "Boolean")


funs
rowctor


>      ])



>    ,testGroup "case expressions"
>     (mapExprType [
>       p "case\n\
>         \ when true then 1\n\
>         \end" (ScalarType "Integer")
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" (ScalarType "String")
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 'test'=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" (TypeError ("",0,0)
>                (WrongTypes (ScalarType "String")
>                 [ScalarType "String",ScalarType "Integer"]))
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 'blah'\n\
>         \ else 1\n\
>         \end" (TypeError ("",0,0)
>                (WrongTypes (ScalarType "String")
>                  [ScalarType "String"
>                  ,ScalarType "String"
>                  ,ScalarType "Integer"]))
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 1\n\
>         \ else 'else'\n\
>         \end" (TypeError ("",0,0)
>                (WrongTypes (ScalarType "String")
>                 [ScalarType "String"
>                 ,ScalarType "Integer"
>                 ,ScalarType "String"]))
>      ])

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
