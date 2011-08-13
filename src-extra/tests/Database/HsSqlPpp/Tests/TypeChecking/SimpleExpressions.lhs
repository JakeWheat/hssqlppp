
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.TypeChecking.SimpleExpressions
>     (tcSimpleExpressionTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
>
> tcSimpleExpressionTestData :: Item
> tcSimpleExpressionTestData =
>    Group "simple expressions" [
>       e "1=1" $ Right typeBool
>      ,e "1=true" $ Left [NoMatchingOperator "=" [typeInt,typeBool]]
>      ,e "substring('aqbc' from 2 for 2)" $ Right (ScalarType "text")
>
>      ,e "substring(3 from 2 for 2)" $ Left
>                                      [NoMatchingOperator "!substring"
>                                       [ScalarType "int4"
>                                       ,ScalarType "int4"
>                                       ,ScalarType "int4"]]
>      ,e "substring('aqbc' from 2 for true)" $ Left
>                     [NoMatchingOperator "!substring"
>                      [UnknownType
>                      ,ScalarType "int4"
>                      ,ScalarType "bool"]]
>
>      ,e "3 between 2 and 4" $ Right typeBool
>      ,e "3 between true and 4" $ Left
>                                [IncompatibleTypeSet [ScalarType "int4"
>                                                     ,ScalarType "bool"
>                                                     ,ScalarType "int4"]]
>
>      ,e "array[1,2,3][2]" $ Right typeInt
>      ,e "array['a','b'][1]" $ Right UnknownType
>      ,e "array['a'::text,'b'][1]" $ Right (ScalarType "text")
>      {-,p "array['a','b'][true]" (TypeError ("",0,0)
>                                   (WrongType
>                                    typeInt
>                                    UnknownType))-}
>      ,e "not true" $ Right typeBool
>      ,e "not 1" $ Left
>                  [NoMatchingOperator "!not" [typeInt]]
>
>      ,e "@ 3" $ Right typeInt
>      ,e "@ true" $ Left
>                  [NoMatchingOperator "@" [ScalarType "bool"]]
>
>      ,e "-3" $ Right typeInt
>      ,e "-'a'" $ Left
>                  [NoMatchingOperator "-" [UnknownType]]
>
>      ,e "4-3" $ Right typeInt
>
>      ,e "1 is null" $ Right typeBool
>      ,e "1 is not null" $ Right typeBool
>
>      ,e "1+1" $ Right typeInt
>      ,e "1+1" $ Right typeInt
>      ,e "31*511" $ Right typeInt
>      ,e "5/2" $ Right typeInt
>      ,e "2^10" $ Right typeFloat8
>      ,e "17%5" $ Right typeInt
>      ,e "3 and 4" $ Left
>                   [NoMatchingOperator "!and" [typeInt,typeInt]]
>      ,e "True and False" $ Right typeBool
>      ,e "false or true" $ Right typeBool
>      ,e "lower('TEST')" $ Right (ScalarType "text")
>      ,e "lower(1)" $ Left [NoMatchingOperator "lower" [typeInt]]
>      ]
>  where
>    e = Expr
>    --s = StmtType
>    --c = CatStmtType
>    --d = Ddl

