
> module Database.HsSqlPpp.Tests.TypeChecking.MiscExpressions
>     (miscExpressionsTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
>
> miscExpressionsTestData :: Item
> miscExpressionsTestData =
>   Group "misc expressions" [

~~~~
implicit casting and function/operator choice tests:
check when multiple implicit and one exact match on num args
check multiple matches with num args, only one with implicit conversions
check multiple implicit matches with one preferred
check multiple implicit matches with one preferred highest count
check casts from unknown string lits
~~~~

>   Group "some expressions" [
>       e "3 + '4'" $ Right typeInt
>      ,e "3.0 + '4'" $ Right typeNumeric
>      ,e "'3' + '4'" $ Left [NoMatchingOperator "+" [UnknownType
>                                                    ,UnknownType]]
>      ]
>   ,Group "exists expressions" [
>       e "exists (select 1 from pg_type)" $ Right typeBool
>      ,e "exists (select testit from pg_type)"
>        $ Left [UnrecognisedIdentifier "testit"]
>      ]

>   ,Group "polymorphic functions" [
>       e "array_append(ARRAY[1,2], 3)"
>         $ Right (ArrayType typeInt)
>      ,e "array_append(ARRAY['a','b'], 'c')"
>         $ Right (ArrayType UnknownType)
>      ,e "array_append(ARRAY['a','b'], 'c'::text)"
>         $ Right (ArrayType $ ScalarType "text")
>      ,e "array_append(ARRAY['a','b'::text], 'c')"
>         $ Right (ArrayType $ ScalarType "text")
>      ,e "array_append(ARRAY['a'::int,'b'], 'c')"
>         $ Right (ArrayType typeInt)
>      ]

>   ,Group "cast expressions" [
>       e "cast ('1' as integer)"
>         $ Right typeInt
>      ,e "cast ('1' as baz)"
>         $ Left [UnknownTypeName "baz"]
>      ,e "array[]" -- FIXME: this isn't quite right but not sure how to do it atm
>                   -- no point fixing this case since need a load of other
>                   -- test cases where the behaviour is different
>         $ Right (Pseudo AnyArray) -- Left [TypelessEmptyArray]
>      ,e "array[] :: text[]"
>         $ Right (ArrayType (ScalarType "text"))
>      ]

>      ]
>  where
>    e = Expr
