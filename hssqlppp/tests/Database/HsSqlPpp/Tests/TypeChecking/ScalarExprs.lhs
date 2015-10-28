
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.ScalarExprs
>     (scalarExprs) where

> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Tests.TestTypes

> scalarExprs :: Item
> scalarExprs =
>   Group "scalarExprs"
>   [Group "simple literals"
>    [ScalExpr "true" $ Right typeBool
>    ,ScalExpr "false" $ Right typeBool
>    ,ScalExpr "41" $ Right typeInt
>    ,ScalExpr "2147483648" $ Right typeBigInt
>    ,ScalExpr "9223372036854775808" $ Right typeNumeric
>    ,ScalExpr "1.6" $ Right typeNumeric
>    ,ScalExpr "'test'" $ Right UnknownType
>    ,ScalExpr "null" $ Right UnknownType
>    ]
>   ,Group "other simple scalexprs"
>     [ScalExpr "'1'::int" $ Right typeInt
>     ,ScalExpr "date '2000-01-01'" $ Right $ ScalarType "date"
>     ,ScalExpr "interval '90' day" $ Right $ ScalarType "interval"
>     ,ScalExpr "?" $ Right UnknownType
>     ]
>   ,Group "function application"
>     [ScalExpr "length('test')" $ Right typeInt
>     ,ScalExpr "-5" $ Right typeInt
>     ]
>   ,Group "function application like"
>     [ScalExpr "extract(year from date '2000-01-01')" $ Right typeInt
>     ,ScalExpr "extract(year from 3)" $ Left [NoMatchingOperator "extract" [typeInt]]
>     ]
>   ]
