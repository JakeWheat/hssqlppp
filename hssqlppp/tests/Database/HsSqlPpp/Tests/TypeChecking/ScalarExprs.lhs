
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.ScalarExprs
>     (scalarExprs) where

> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Internals.TypesInternal hiding (mkTypeExtra,mkTypeExtraNN)

> import Database.HsSqlPpp.Tests.TestTypes

> scalarExprs :: Item
> scalarExprs =
>   Group "scalarExprs"
>   [Group "simple literals"
>    [scalExpr "true" $ Right typeBool
>    ,scalExpr "false" $ Right typeBool
>    ,scalExpr "41" $ Right typeInt
>    ,scalExpr "2147483648" $ Right typeBigInt
>    ,scalExpr "9223372036854775808" $ Right typeNumeric
>    ,scalExpr "1.6" $ Right typeNumeric
>    ,scalExpr "'test'" $ Right UnknownType
>    ,scalExpr "null" $ Right UnknownType
>    ]
>   ,Group "other simple scalexprs"
>     [scalExpr "'1'::int" $ Right typeInt
>     ,scalExpr "date '2000-01-01'" $ Right $ ScalarType "date"
>     ,scalExpr "interval '90' day" $ Right $ ScalarType "interval"
>     ,scalExpr "?" $ Right UnknownType
>     ]
>   ,Group "function application"
>     [scalExpr "length('test')" $ Right typeInt
>     ,scalExpr "-5" $ Right typeInt
>     ]
>   ,Group "function application like"
>     [scalExpr "extract(year from date '2000-01-01')" $ Right typeInt
>     ,scalExpr "extract(year from 3)" $ Left [NoMatchingOperator "extract" [typeInt]]
>     ]
>   ]
>   where
>     scalExpr = TCScalExpr defaultTemplate1Catalog emptyEnvironment
>                           defaultTypeCheckFlags
