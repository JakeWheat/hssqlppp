
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.PrecisionAndNullable
>     (precisionAndNullable) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Internals.AstInternal
> import Database.HsSqlPpp.Internals.TypeChecking.Environment

> precisionAndNullable :: Item
> precisionAndNullable =
>   Group "scalarExprsPrecisionAndNullable"
>   [Group "identifiers"
>    [ScalarExprExtra anEnv  "an"  (Right anType)
>    ,ScalarExprExtra aEnv   "a"   (Right aType)
>    ,ScalarExprExtra cnEnv  "cn"  (Right cnType)
>    ,ScalarExprExtra cEnv   "c"   (Right cType)
>    ,ScalarExprExtra vnEnv  "vn"  (Right vnType)
>    ,ScalarExprExtra vEnv   "v"   (Right vType)
>    ,ScalarExprExtra dnEnv  "dn"  (Right dnType)
>    ,ScalarExprExtra dEnv   "d"   (Right dType)
>    --,ScalarExprExtra vConcatEnv vConcatExpr (Right vConcatType)
>    ,ScalarExprExtra vEqEnv vEqExpr (Right vEqType)
>    ]
>   ]
>   where
>     anType = TypeExtra typeInt Nothing Nothing True
>     anEnv = selListEnv [("an", anType)]
>     aType = TypeExtra typeInt Nothing Nothing False
>     aEnv = selListEnv [("a", aType)]
>     cnType = TypeExtra typeChar (Just 4) Nothing True
>     cnEnv = selListEnv [("cn", cnType)]
>     cType = TypeExtra typeChar (Just 3) Nothing False
>     cEnv = selListEnv [("c", cType)]
>     vnType = TypeExtra typeVarChar (Just 7) Nothing True
>     vnEnv = selListEnv [("vn", vnType)]
>     vType = TypeExtra typeVarChar (Just 6) Nothing False
>     vEnv = selListEnv [("v", vType)]
>     dnType = TypeExtra typeNumeric (Just 10) (Just 2) True
>     dnEnv = selListEnv [("dn", dnType)]
>     dType = TypeExtra typeNumeric (Just 9) (Just 3) False
>     dEnv = selListEnv [("d", dType)]
>     vConcatType = TypeExtra typeVarChar (Just 13) Nothing True
>     vConcatExpr = "v||vn"
>     vConcatEnv = selListEnv [("vn", vnType),("v", vType)]
>     vEqType = TypeExtra typeBool Nothing Nothing True
>     vEqExpr = "v=vn"
>     vEqEnv = selListEnv [("vn", vnType),("v", vType)]
>     --
>     selListEnv env = either (const brokeEnvironment) id $ envSelectListEnvironment env

