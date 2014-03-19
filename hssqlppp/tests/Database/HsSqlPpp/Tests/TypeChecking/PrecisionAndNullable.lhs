
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.PrecisionAndNullable
>     (precisionAndNullable) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Internals.AstInternal
> import Database.HsSqlPpp.Internals.TypeChecking.Environment

> precisionAndNullable :: Item
> precisionAndNullable =
>   Group "scalarExprsPrecisionAndNullable"
>   [Group "identifiers"
>    [ScalarExprExtra cat1 anEnv  "an"  (Right anType)
>    ,ScalarExprExtra cat1 aEnv   "a"   (Right aType)
>    ,ScalarExprExtra cat1 cnEnv  "cn"  (Right cnType)
>    ,ScalarExprExtra cat1 cEnv   "c"   (Right cType)
>    ,ScalarExprExtra cat1 vnEnv  "vn"  (Right vnType)
>    ,ScalarExprExtra cat1 vEnv   "v"   (Right vType)
>    ,ScalarExprExtra cat1 dnEnv  "dn"  (Right dnType)
>    ,ScalarExprExtra cat1 dEnv   "d"   (Right dType)
>    --,ScalarExprExtra vConcatEnv vConcatExpr (Right vConcatType)
>    ,ScalarExprExtra cat1 vEqEnv vEqExpr (Right vEqType)
>    ,ScalarExprExtra cat2 a2Env "isnull(an,a)" (Right aType)
>    ,ScalarExprExtra cat2 anEnv "isnull(an,an)" (Right anType)
>    ,ScalarExprExtra cat1 aEnv "a is null" (Right isNType)
>    ,ScalarExprExtra cat1 aEnv "a is not null" (Right isNType)
>    ,ScalarExprExtra cat1 anEnv "an is null" (Right isNType)
>    ,ScalarExprExtra cat1 anEnv "an is not null" (Right isNType)
>    ,ScalarExprExtra cat1 coalEnv "coalesce(an,dn,a)" (Right coalType)
>    -- gives incompartible types
>    --,ScalarExprExtra cat1 case1Env "case an when v then a when c then an end" (Right case1Type)
>    ,ScalarExprExtra cat2 case1Env "case vn when v then a when c then an end" (Right case1Type)
>    ,ScalarExprExtra cat1 case2Env "case when an is null then a when v is null then an else dn end" (Right case2Type)
>    ]
>   ]
>   where
>     cat1 = defaultTemplate1Catalog
>     cat2 = defaultTSQLCatalog
>     anType = TypeExtra typeInt Nothing Nothing True
>     anEnv = selListEnv [("an", anType)]
>     aType = TypeExtra typeInt Nothing Nothing False
>     aEnv = selListEnv [("a", aType)]
>     a2Env = selListEnv [("a", aType),("an", anType)]
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
>     isNType = TypeExtra typeBool Nothing Nothing False
>     coalEnv = selListEnv [("a", aType),("an", anType),("dn", dnType)]
>     coalType = TypeExtra typeNumeric (Just 10) (Just 2) False
>     case1Env = selListEnv [("vn", vnType),("a", aType),("an", anType),("c", cType),("v", vType)]
>     case1Type = anType
>     case2Env = selListEnv [("a", aType),("an", anType),("dn", dnType),("v", vType)]
>     case2Type = dnType
>     --
>     selListEnv env = either (const brokeEnvironment) id $ envSelectListEnvironment env

