
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.PrecisionAndNullable
>     (precisionAndNullable) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Internals.AstInternal

> precisionAndNullable :: Item
> precisionAndNullable =
>   Group "scalarExprsPrecisionAndNullable"
>   [Group "identifiers"
>    [ScalarExprExtra [an]  "an"  (Right anType)
>    ,ScalarExprExtra [a]   "a"   (Right aType)
>    ,ScalarExprExtra [cn]  "cn"  (Right cnType)
>    ,ScalarExprExtra [c]   "c"   (Right cType)
>    ,ScalarExprExtra [vn]  "vn"  (Right vnType)
>    ,ScalarExprExtra [v]   "v"   (Right vType)
>    ,ScalarExprExtra [dn]  "dn"  (Right dnType)
>    ,ScalarExprExtra [d]   "d"   (Right dType)
>    ]
>   ]
>   where
>     anType = TypeExtra typeInt Nothing Nothing True
>     an = ("an", anType)
>     aType = TypeExtra typeInt Nothing Nothing False
>     a = ("a", aType)
>     cnType = TypeExtra typeChar (Just 4) Nothing True
>     cn = ("cn", cnType)
>     cType = TypeExtra typeChar (Just 3) Nothing False
>     c = ("c", cType)
>     vnType = TypeExtra typeVarChar (Just 7) Nothing True
>     vn = ("vn", vnType)
>     vType = TypeExtra typeVarChar (Just 6) Nothing False
>     v = ("v", vType)
>     dnType = TypeExtra typeNumeric (Just 10) (Just 2) True
>     dn = ("dn", dnType)
>     dType = TypeExtra typeNumeric (Just 9) (Just 3) False
>     d = ("d", dType)
