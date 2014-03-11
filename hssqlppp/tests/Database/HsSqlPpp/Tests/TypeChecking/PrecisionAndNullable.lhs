
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.PrecisionAndNullable
>     (precisionAndNullable) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types

> precisionAndNullable :: Item
> precisionAndNullable =
>   Group "scalarExprs"
>   [Group "simple literals"
>    [ScalExpr "true" $ Right typeBool
>    ,ScalExpr "false" $ Right typeBool
>    ,ScalExpr "41" $ Right typeInt
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
>     [ScalExpr "extract(year from date '2000-01-01')" $ Right typeFloat8
>     ,ScalExpr "extract(year from 3)" $ Left [NoMatchingOperator "extract" [typeInt]]
>     ]
>   ]
