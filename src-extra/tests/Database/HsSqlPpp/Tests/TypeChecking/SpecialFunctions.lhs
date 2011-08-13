
> module Database.HsSqlPpp.Tests.TypeChecking.SpecialFunctions
>     (tcSpecialFunctionsTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
>
> tcSpecialFunctionsTestData :: Item
> tcSpecialFunctionsTestData =
>   Group "special functions" [
>       e "coalesce(null,1,2,null)" $ Right typeInt
>      ,e "coalesce('3',1,2,null)" $ Right typeInt
>      ,e "coalesce('3',1,true,null)"
>             $ Left [NoMatchingOperator "coalesce" [UnknownType
>                                                   ,ScalarType "int4"
>                                                   ,ScalarType "bool"
>                                                   ,UnknownType]]
>      ,e "nullif('hello','hello')" $ Right UnknownType
>      ,e "nullif('hello'::text,'hello')" $ Right (ScalarType "text")
>      ,e "nullif(3,4)" $ Right typeInt
>      ,e "nullif(true,3)"
>             $ Left [NoMatchingOperator "nullif" [ScalarType "bool"
>                                           ,ScalarType "int4"]]
>      ,e "greatest(3,5,6,4,3)" $ Right typeInt
>      ,e "least(3,5,6,4,3)" $ Right typeInt
>      ,e "least(5,true)"
>             $ Left [NoMatchingOperator "least" [ScalarType "int4",ScalarType "bool"]]
>      ]
>  where
>    e = Expr
