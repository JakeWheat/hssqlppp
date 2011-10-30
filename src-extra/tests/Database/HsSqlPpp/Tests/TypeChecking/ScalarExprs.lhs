> module Database.HsSqlPpp.Tests.TypeChecking.ScalarExprs
>     (scalarExprs) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types

> scalarExprs :: Item
> scalarExprs =
>   Group "scalarExprs"
>   [Group "simple literals"
>    [ScalExpr "true" $ Right $ typeBool
>    ,ScalExpr "false" $ Right $ typeBool]
>   ]
