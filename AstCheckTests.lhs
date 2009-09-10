#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

Set of tests to check the type checking code

> module AstCheckTests (astCheckTests, parseAndGetType) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char

> import Ast
> import Parser
> import Scope
> import TypeType

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
>       p "1" typeInt
>      ,p "1.0" typeNumeric
>      ,p "'test'" UnknownStringLit
>      ,p "true" typeBool
>      ,p "array[1,2,3]" (ArrayType typeInt)
>      ,p "array['a','b']" (ArrayType (ScalarType "text"))
>      ,p "array[1,'b']" (ArrayType typeInt)
>      ,p "array[1,true]" (TypeError ("",0,0)
>                         (IncompatibleTypes [typeInt,typeBool]))
>      ])

>    ,testGroup "some expressions"
>     (mapExprType [
>       p "1=1" typeBool
>      ,p "1=true" (TypeError ("",0,0)
>                     (NoMatchingOperator "=" [typeInt,typeBool]))
>      ,p "substring('aqbc' from 2 for 2)" (ScalarType "text")

>      ,p "substring(3 from 2 for 2)" (TypeError ("",0,0)
>                                      (NoMatchingOperator "!substring"
>                                       [ScalarType "int4"
>                                       ,ScalarType "int4"
>                                       ,ScalarType "int4"]))
>      ,p "substring('aqbc' from 2 for true)" (TypeError ("",0,0)
>                     (NoMatchingOperator "!substring"
>                      [UnknownStringLit
>                      ,ScalarType "int4"
>                      ,ScalarType "bool"]))

>      ,p "3 between 2 and 4" typeBool
>      ,p "3 between true and 4" (TypeError ("",0,0)
>                                (NoMatchingOperator ">="
>                                 [typeInt
>                                 ,typeBool]))

>      ,p "array[1,2,3][2]" typeInt
>      ,p "array['a','b'][1]" (ScalarType "text")

 >      ,p "array['a','b'][true]" (TypeError ("",0,0)
 >                                   (WrongType
 >                                    typeInt
 >                                    UnknownStringLit))

>      ,p "not true" typeBool
>      ,p "not 1" (TypeError ("",0,0)
>                  (NoMatchingOperator "!not" [typeInt]))

>      ,p "@ 3" typeInt
>      ,p "@ true" (TypeError ("",0,0)
>                  (NoMatchingOperator "@" [ScalarType "bool"]))

>      ,p "-3" typeInt
>      ,p "-'a'" (TypeError ("",0,0)
>                  (NoMatchingOperator "-" [UnknownStringLit]))

>      ,p "4-3" typeInt

>      --,p "1 is null" typeBool
>      --,p "1 is not null" typeBool

>      ,p "1+1" typeInt
>      ,p "1+1" typeInt
>      ,p "31*511" typeInt
>      ,p "5/2" typeInt
>      ,p "2^10" typeFloat8
>      ,p "17%5" typeInt

>      ,p "3 and 4" (TypeError ("",0,0)
>                   (NoMatchingOperator "!and" [typeInt,typeInt]))

>      ,p "True and False" typeBool
>      ,p "false or true" typeBool

>      ,p "lower('TEST')" (ScalarType "text")
>      ,p "lower(1)" (TypeError nsp (NoMatchingOperator "lower" [typeInt]))

>      ])

implicit casting and function/operator choice tests:
check when multiple implicit and one exact match on num args
check multiple matches with num args, only one with implicit conversions
check multiple implicit matches with one preferred
check multiple implicit matches with one preferred highest count
check casts from unknown string lits

>    ,testGroup "some expressions"
>     (mapExprType [
>       p "3 + '4'" typeInt
>      ,p "3.0 + '4'" typeNumeric
>      ,p "'3' + '4'" (TypeError ("",0,0)
>                       (NoMatchingOperator "+" [UnknownStringLit
>                                               ,UnknownStringLit]))
>      ])
>

>    ,testGroup "expressions and scope"
>     (mapExprScopeType [
>      t "a" (makeScope [("test", [("a", typeInt)])]) typeInt
>     ,t "b" (makeScope [("test", [("a", typeInt)])]) (TypeError nsp (UnrecognisedIdentifier "b"))
>     ])



>    ,testGroup "case expressions"
>     (mapExprType [
>       p "case\n\
>         \ when true then 1\n\
>         \end" typeInt
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" (ScalarType "text")
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when true=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" (TypeError ("",0,0)
>                (NoMatchingOperator "=" [typeBool,typeInt]))
>      ,p "case\n\
>         \ when 1=2 then true\n\
>         \ when 2=3 then false\n\
>         \ else 1\n\
>         \end" (TypeError ("",0,0)
>                (IncompatibleTypes [typeBool
>                                   ,typeBool
>                                   ,typeInt]))
>      ,p "case\n\
>         \ when 1=2 then false\n\
>         \ when 2=3 then 1\n\
>         \ else true\n\
>         \end" (TypeError ("",0,0)
>                (IncompatibleTypes [typeBool
>                                   ,typeInt
>                                   ,typeBool]))
>      ])

>    ,testGroup "polymorphic functions"
>     (mapExprType [
>       p "array_append(ARRAY[1,2], 3)"
>         (ArrayType typeInt)
>      ,p "array_append(ARRAY['a','b'], 'c')"
>         (ArrayType $ ScalarType "text")
>      ,p "array_append(ARRAY['a'::int,'b'], 'c')"
>         (ArrayType typeInt)
>      ])

todo:



>    ,testGroup "cast expressions"
>     (mapExprType [
>       p "cast ('1' as integer)"
>         typeInt
>      ,p "cast ('1' as baz)"
>         (TypeError nsp (UnknownTypeError $ ScalarType "baz"))
>      ,p "array[]"
>         (TypeError nsp TypelessEmptyArray)
>      --todo: figure out how to do this
>      --,p "array[] :: text[]"
>      --   (ArrayType (ScalarType "text"))

>      ])


>    ,testGroup "simple selects"
>     (mapStatementType [
>       p "select 1;" [SetOfType $ UnnamedCompositeType [("?column?", typeInt)]]
>      ,p "select 1 as a;" [SetOfType $ UnnamedCompositeType [("a", typeInt)]]
>      ,p "select 1,2;" [SetOfType $ UnnamedCompositeType [("?column?", typeInt)
>                                                         ,("?column?", typeInt)]]
>      ,p "select 1 as a, 2 as b;" [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                                                    ,("b", typeInt)]]
>      ,p "select 1+2 as a, 'a' || 'b';" [SetOfType $
>                                         UnnamedCompositeType [("a", typeInt)
>                                                              ,("?column?", ScalarType "text")]]
>      ,p "values (1,2);" [SetOfType $
>                          UnnamedCompositeType
>                          [("column1", typeInt)
>                          ,("column2", typeInt)]]
>      ,p "values (1,2),('3', '4');" [SetOfType $
>                                      UnnamedCompositeType
>                                      [("column1", typeInt)
>                                      ,("column2", typeInt)]]
>      ,p "values (1,2),('a', true);" [TypeError ("",1,1)
>                                      (IncompatibleTypes [typeInt
>                                                         ,typeBool])]
>      ,p "values ('3', '4'),(1,2);" [SetOfType $
>                                      UnnamedCompositeType
>                                      [("column1", typeInt)
>                                      ,("column2", typeInt)]]
>      ,p "values ('a', true),(1,2);" [TypeError ("",1,1)
>                                      (IncompatibleTypes [typeBool
>                                                         ,typeInt])]
>      ,p "values ('a'::text, '2'::int2),('1','2');" [SetOfType $
>                                      UnnamedCompositeType
>                                      [("column1", ScalarType "text")
>                                      ,("column2", typeSmallInt)]]
>      ,p "values (1,2,3),(1,2);" [TypeError ("",1,1) ValuesListsMustBeSameLength]
>      ])

>    ,testGroup "simple selects from"
>     (mapStatementType [
>       p "select a from (select 1 as a, 2 as b) x;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)]]
>      ,p "select b from (select 1 as a, 2 as b) x;"
>         [SetOfType $ UnnamedCompositeType [("b", typeInt)]]
>      ,p "select c from (select 1 as a, 2 as b) x;"
>         [TypeError ("",1,1) (UnrecognisedIdentifier "c")]
>      ,p "select typlen from pg_type;"
>         [SetOfType $ UnnamedCompositeType [("typlen", typeSmallInt)]]
>      ,p "select typlen from nope;"
>         [TypeError ("",1,1) (UnrecognisedRelation "nope")]
>      ,p "select generate_series from generate_series(1,7);"
>         [SetOfType $ UnnamedCompositeType [("generate_series", typeInt)]]
>      ,p "select * from pg_attrdef;"
>         [SetOfType $ UnnamedCompositeType
>          [("adrelid",ScalarType "oid")
>          ,("adnum",ScalarType "int2")
>          ,("adbin",ScalarType "text")
>          ,("adsrc",ScalarType "text")]]
>      ,p "select abs from abs(3);"
>         [SetOfType $ UnnamedCompositeType [("abs", typeInt)]]
>         --todo: these are both valid,
>         --the second one means select 3+generate_series from generate_series(1,7)
>         --  select generate_series(1,7);
>         -- select 3 + generate_series(1,7);
>      ])

>    ,testGroup "simple selects from 2"
>     (mapStatementTypeScope [
>
>       t "select a,b from testfunc();"
>         (let fn = ("testfunc", [], SetOfType $ CompositeType "testType")
>          in emptyScope {scopeFunctions = [fn]
>                        ,scopeAllFns = [fn]
>                        ,scopeAttrDefs =
>                         [("testType"
>                          ,Composite
>                          ,UnnamedCompositeType
>                             [("a", ScalarType "text")
>                             ,("b", typeInt)
>                             ,("c", typeInt)])]})
>         [SetOfType $ UnnamedCompositeType
>          [("a",ScalarType "text"),("b",ScalarType "int4")]]
>      ])

>    ,testGroup "simple join selects"
>     (mapStatementType [
>       p "select * from (select 1 as a, 2 as b) a\n\
>         \  cross join (select true as c, 4.5 as d) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("c", typeBool)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select true as c, 4.5 as d) b on true;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("c", typeBool)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select 1 as a, 4.5 as d) b using(a);"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \ natural inner join (select 1 as a, 4.5 as d) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>         --check the attribute order
>      ,p "select * from (select 2 as b, 1 as a) a\n\
>         \ natural inner join (select 4.5 as d, 1 as a) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \ natural inner join (select true as a, 4.5 as d) b;"
>         [TypeError ("",1,1) (IncompatibleTypes [ScalarType "int4"
>                                                ,ScalarType "bool"])]
>      ])

>    ,testGroup "simple scalar identifier qualification"
>     (mapStatementType [
>       p "select a.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)]]
>      ,p "select a.b,b.c from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         [SetOfType $ UnnamedCompositeType [("b", typeInt)
>                                           ,("c", typeInt)]]
>      ,p "select a.a,b.a from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("a", typeInt)]]

>      ,p "select pg_attrdef.adsrc from pg_attrdef;"
>         [SetOfType $ UnnamedCompositeType [("adsrc", ScalarType "text")]]

>      ,p "select a.adsrc from pg_attrdef a;"
>         [SetOfType $ UnnamedCompositeType [("adsrc", ScalarType "text")]]

>      ,p "select pg_attrdef.adsrc from pg_attrdef a;"
>         [TypeError ("",1,1) (UnrecognisedAlias "pg_attrdef")]

select g.fn from fn() g


>      ])

>    ,testGroup "simple wheres"
>     (mapStatementType [
>       p "select 1 from pg_type where true;"
>         [SetOfType $ UnnamedCompositeType [("?column?", typeInt)]]
>      ,p "select 1 from pg_type where 1;"
>         [TypeError ("",1,1) ExpressionMustBeBool]
>      ,p "select typname from pg_type where typbyval;"
>         [SetOfType $ UnnamedCompositeType [("typname", ScalarType "name")]]
>      ,p "select typname from pg_type where typtype = 'b';"
>         [SetOfType $ UnnamedCompositeType [("typname", ScalarType "name")]]
>      ,p "select typname from pg_type where what = 'b';"
>         [TypeError ("",1,1) (UnrecognisedIdentifier "what")]
>      ])

>    ]
>         where
>           mapAttr = map $ uncurry checkAttrs
>           p a b = (a,b)
>           t a b c = (a,b,c)
>           mapExprType = map $ uncurry $ checkExpressionType emptyScope
>           mapStatementType = map $ uncurry checkStatementType
>           mapExprScopeType = map (\(a,b,c) -> checkExpressionType b a c)
>           makeScope l = scopeReplaceIds defaultScope l []
>           mapStatementTypeScope = map (\(a,b,c) -> checkStatementTypeScope b a c)

> checkAttrs :: String -> [Message] -> Test.Framework.Test
> checkAttrs src msgs = testCase ("check " ++ src) $ do
>   let ast = case parseSql src of
>                Left er -> error $ show er
>                Right l -> l
>       msgs1 = checkAst ast
>   assertEqual ("check " ++ src) msgs msgs1

> checkExpressionType :: Scope -> String -> Type -> Test.Framework.Test
> checkExpressionType scope src typ = testCase ("typecheck " ++ src) $
>   assertEqual ("typecheck " ++ src) typ (parseAndGetExpressionType scope src)

> checkStatementType :: String -> [Type] -> Test.Framework.Test
> checkStatementType src typ = testCase ("typecheck " ++ src) $
>   assertEqual ("typecheck " ++ src) typ (parseAndGetType src)

> checkStatementTypeScope ::  Scope -> String -> [Type] -> Test.Framework.Test
> checkStatementTypeScope scope src typ = testCase ("typecheck " ++ src) $
>   assertEqual ("typecheck " ++ src) typ (parseAndGetTypeScope scope src)


> parseAndGetType :: String -> [Type]
> parseAndGetType src =
>   let ast = case parseSql src of
>                               Left er -> error $ show er
>                               Right l -> l
>   in getStatementsType ast

> parseAndGetTypeScope :: Scope -> String -> [Type]
> parseAndGetTypeScope scope src =
>   let ast = case parseSql src of
>                               Left er -> error $ show er
>                               Right l -> l
>   in getStatementsTypeScope scope ast


> parseAndGetExpressionType :: Scope -> String -> Type
> parseAndGetExpressionType scope src =
>   let ast = case parseExpression src of
>                                      Left er -> error $ show er
>                                      Right l -> l
>   in getExpressionType scope ast
