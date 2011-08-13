
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.TypeChecking.Literals
>     (tcLiteralTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> tcLiteralTestData :: Item
> tcLiteralTestData =
>    Group "basic literal types" [
>       e "1" $ Right typeInt
>      ,e "1.0" $ Right typeNumeric
>      ,e "'test'" $ Right UnknownType
>      ,e "true" $ Right typeBool
>      ,e "array[1,2,3]" $ Right (ArrayType typeInt)
>      ,e "array['a','b']" $ Right (ArrayType UnknownType)
>      ,e "array['a'::text,'b']" $ Right (ArrayType (ScalarType "text"))
>      ,e "array['a','b'::text]" $ Right (ArrayType (ScalarType "text"))
>      ,e "array[1,'b']" $ Right (ArrayType typeInt)
>      ,e "array[1,true]" $ Left [NoMatchingOperator "!arrayctor" [ScalarType "int4",ScalarType "bool"]]
>      ]
>  where
>    e = Expr

