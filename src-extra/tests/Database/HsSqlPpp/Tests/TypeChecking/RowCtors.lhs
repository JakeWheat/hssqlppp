
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.TypeChecking.RowCtors
>     (tcRowCtorsTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
>
> tcRowCtorsTestData :: Item
> tcRowCtorsTestData =
>   Group "row comparison expressions" [

~~~~
rows different lengths
rows match types pairwise, same and different types
rows implicit cast from unknown
rows don't match types
~~~~

>       e "row(1)" $ Right (AnonymousRecordType [typeInt])
>      ,e "row(1,2)" $ Right (AnonymousRecordType [typeInt,typeInt])
>      ,e "row('t1','t2')" $ Right (AnonymousRecordType [UnknownType,UnknownType])
>      ,e "row(true,1,'t3')" $ Right (AnonymousRecordType [typeBool, typeInt,UnknownType])
>      ,e "(true,1,'t3',75.3)" $ Right (AnonymousRecordType [typeBool,typeInt
>                                       ,UnknownType,typeNumeric])
>      ,e "row(1) = row(2)" $ Right typeBool
>      ,e "row(1,2) = row(2,1)" $ Right typeBool
>      ,e "(1,2) = (2,1)" $ Right typeBool
>      ,e "(1,2,3) = (2,1)" $ Left [NoMatchingOperator "="
>                                   [AnonymousRecordType [ScalarType "int4"
>                                                        ,ScalarType "int4"
>                                                        ,ScalarType "int4"]
>                                    ,AnonymousRecordType [ScalarType "int4"
>                                                         ,ScalarType "int4"]]]
>      ,e "('1',2) = (2,'1')" $ Right typeBool
>      ,e "(1,true) = (2,1)" $ Left [NoMatchingOperator "="
>                                    [AnonymousRecordType [ScalarType "int4"
>                                                         ,ScalarType "bool"]
>                                    ,AnonymousRecordType [ScalarType "int4"
>                                                         ,ScalarType "int4"]]]
>      ,e "(1,2) <> (2,1)" $ Right typeBool
>      ,e "(1,2) >= (2,1)" $ Right typeBool
>      ,e "(1,2) <= (2,1)" $ Right typeBool
>      ,e "(1,2) > (2,1)" $ Right typeBool
>      ,e "(1,2) < (2,1)" $ Right typeBool
>      ]
>  where
>    e = Expr
>    --s = StmtType
>    --c = CatStmtType
>    --d = Ddl

