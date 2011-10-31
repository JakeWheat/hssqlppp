> module Database.HsSqlPpp.Tests.TypeChecking.ScalarExprs
>     (scalarExprs) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types

> scalarExprs :: Item
> scalarExprs =
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
>     ]
>   ]

