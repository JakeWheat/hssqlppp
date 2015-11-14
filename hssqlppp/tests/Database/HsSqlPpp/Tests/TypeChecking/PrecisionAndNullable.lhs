
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.PrecisionAndNullable
>     (precisionAndNullable) where

> --import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Tests.TestTypes
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Dialect
> --import Database.HsSqlPpp.Internals.AstInternal
> --import Database.HsSqlPpp.Internals.TypeChecking.Environment
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> --import qualified Data.Text.Lazy as L
> --import Database.HsSqlPpp.Internals.TypesInternal hiding (mkTypeExtra,mkTypeExtraNN)

> precisionAndNullable :: Item
> precisionAndNullable =
>   Group "PrecisionAndNullable" $
>   [Group "ScalarExprs"
>    [seePg anEnv  "an"  (Right anType)
>    ,seePg aEnv   "a"   (Right aType)
>    ,seePg cnEnv  "cn"  (Right cnType)
>    ,seePg cEnv   "c"   (Right cType)
>    ,seePg vnEnv  "vn"  (Right vnType)
>    ,seePg vEnv   "v"   (Right vType)
>    ,seePg dnEnv  "dn"  (Right dnType)
>    ,seePg dEnv   "d"   (Right dType)
>    ,seePg vConcatEnv vConcatExpr (Right vConcatType)
>    ,seePg vConcatEnv "v||'test12'" (Right $ TypeExtra (ScalarType "text") (Just 12) Nothing False)
>    ,seePg vEqEnv vEqExpr (Right vEqType)
>    ,seeTs a2Env "isnull(an,a)" (Right aType)
>    ,seeTs anEnv "isnull(an,an)" (Right anType)
>    ,seePg aEnv "a is null" (Right isNType)
>    ,seePg aEnv "a is not null" (Right isNType)
>    ,seePg anEnv "an is null" (Right isNType)
>    ,seePg anEnv "an is not null" (Right isNType)
>    ,seePg coalEnv "coalesce(an,dn,a)" (Right coalType)
>    -- gives incompatible types
>    --,seePg case1Env "case an when v then a when c then an end" (Right case1Type)
>    ,seeTs case1Env "case vn when v then a when c then an end" (Right case1Type)
>    ,seePg case2Env "case when an is null then a when v is null then an else dn end" (Right case2Type)
>    ,seeTs (selListEnv []) "dateadd(year,1,'1997/01/01')" (Right $ mkTypeExtraNN $ ScalarType "timestamp")
>    ,seeTs vEnv   "len(v)"   (Right aType)
>    ]
>   ]
>   ++ [Group "PrecisionAndNullabletcQueryExpr"
>       [tcQueryExpr
>         [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                             ,("b", CatNameExtra "varchar" (Just 7) Nothing False)]]
>         "select a,b from t"
>         $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                 ,("b", TypeExtra typeVarChar (Just 7) Nothing False)]
>
>
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", CatNameExtra "numeric" (Just 6) (Just 2) False)
>                                      ,("b", CatNameExtra "numeric" (Just 10) (Just 3) False)]]
>        "select nullif(a,b) as ni from t"
>        $ Right $ CompositeType [("ni", TypeExtra typeNumeric (Just 6) (Just 2) True)]
>
>
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", CatNameExtra "float" (Just 10) (Just 2) False)
>                                      ,("b", CatNameExtra "varchar" (Just 12) Nothing True)]]
>        "select * from t"
>        $ Right $ CompositeType [("a", TypeExtra typeFloat8 (Just 10) (Just 2) False)
>                                ,("b", TypeExtra typeVarChar (Just 12) Nothing True)]
>       ,tsqlQueryExpr
>                   [CatCreateTable ("public","t1")  [("a", CatNameExtra "int4" Nothing Nothing True)
>                                         ,("b", CatNameExtra "char" (Just 5) Nothing False)]
>                   ,CatCreateTable ("public","t2")  [("c", CatNameExtra "float" (Just 10) (Just 2) False)
>                                         ,("d", CatNameExtra "varchar" (Just 6) Nothing False)]]
>        "select * from t1 union all select * from t2"
>        $ Right $ CompositeType [("a", TypeExtra typeFloat8 (Just 10) (Just 2) True)
>                                ,("b", TypeExtra typeVarChar (Just 6) Nothing False)]

>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", CatNameExtra "varchar" (Just 13) Nothing False)
>                                      ,("c", CatNameExtra "varchar" (Just 15) Nothing True)]]
>        "select case when a is null then b else c end as cs from t u"
>        $ Right $ CompositeType [("cs", TypeExtra typeVarChar (Just 15) Nothing True)]
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", mkCatNameExtra "text")]]
>        "select * from t u(c,d)"
>        $ Right $ CompositeType [("c", mkTypeExtra typeInt)
>                                ,("d", mkTypeExtra $ ScalarType "text")]
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", mkCatNameExtra "text")]]
>        "select u.a,u.b from t u"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                ,("b", mkTypeExtra $ ScalarType "text")]

>
>
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", mkCatNameExtra "text")]]
>        "select count(*) from t"
>        $ Right $ CompositeType [("count", mkTypeExtraNN typeBigInt)]


>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")]
>                  ,CatCreateTable ("public","u") [("a", mkCatNameExtra "int4")]]
>        "select * from t union select * from u"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt)]

>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")]
>                  ,CatCreateTable ("public","u") [("b", mkCatNameExtra "int4")]]
>        "select * from t union select * from u"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt)]
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")]]
>        "select a,count(*) over () as r from t"
>        $ Right $ CompositeType  [("a", mkTypeExtra typeInt),
>                                 ("r", mkTypeExtraNN typeBigInt)]
>       -- postponed until we decide about implicit casts from numeric to string types
>       --,tsqlQueryExpr [CatCreateTable ("public","t") [("a", CatNameExtra "int4" Nothing Nothing False)]]
>       -- "select a, lower(a) as l from t"
>       -- $ Right $ CompositeType  [("a", TypeExtra (ScalarType "int4") Nothing Nothing False),
>       --                          ("l", TypeExtra (ScalarType "text") Nothing Nothing False)]
>       ,tsqlQueryExpr [CatCreateTable ("public","t") [("d", CatNameExtra "date" Nothing Nothing False)]]
>        "select d from t where d > dateadd(year,1,'1997-01-01')"
>        $ Right $ CompositeType  [("d", mkTypeExtraNN typeDate)]
>       ,tsqlQueryExpr [CatCreateTable ("public","t") [("a", CatNameExtra "int4" Nothing Nothing False)],
>                       CatCreateTable ("public","tt") [("v", CatNameExtra "varchar" (Just 6) Nothing False)]]
>        "select t.a from t inner join tt on t.a=tt.v"
>        $ Right $ CompositeType  [("a", mkTypeExtraNN typeInt)]
>       ]
>     ]
>   where
>     --cat1 = diDefaultCatalog postgresDialect
>     --cat2 = diDefaultCatalog sqlServerDialect
>     anType = TypeExtra typeInt Nothing Nothing True
>     anEnv = selListEnv [("an", anType)]
>     aType = mkTypeExtraNN typeInt
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
>     vConcatType = TypeExtra (ScalarType "text") (Just 13) Nothing True
>     vConcatExpr = "v||vn"
>     vConcatEnv = selListEnv [("vn", vnType),("v", vType)]
>     vEqType = TypeExtra typeBool Nothing Nothing True
>     vEqExpr = "v=vn"
>     vEqEnv = selListEnv [("vn", vnType),("v", vType)]
>     isNType = mkTypeExtraNN typeBool
>     coalEnv = selListEnv [("a", aType),("an", anType),("dn", dnType)]
>     coalType = TypeExtra typeNumeric (Just 10) (Just 2) False
>     case1Env = selListEnv [("vn", vnType),("a", aType),("an", anType),("c", cType),("v", vType)]
>     case1Type = anType
>     case2Env = selListEnv [("a", aType),("an", anType),("dn", dnType),("v", vType)]
>     case2Type = dnType
>     --
>     selListEnv env = either (const brokeEnvironment) id $ envSelectListEnvironment env
>     tcQueryExpr cus =
>         let cat = makeCatalog postgresDialect cus
>         in TCQueryExpr cat defaultTypeCheckFlags {tcfDialect = postgresDialect}
>     tsqlQueryExpr cus =
>         let cat = makeCatalog sqlServerDialect cus
>         in TCQueryExpr cat defaultTypeCheckFlags {tcfDialect = sqlServerDialect}
>     seePg = ScalarExprExtra postgresDialect (diDefaultCatalog postgresDialect)
>     seeTs = ScalarExprExtra sqlServerDialect (diDefaultCatalog sqlServerDialect)
>     typeInt = ScalarType "int4"
>     typeBigInt = ScalarType "int8"
>     typeVarChar = ScalarType "varchar"
>     typeNumeric = ScalarType "numeric"
>     typeFloat8 = ScalarType "float8"
>     typeDate = ScalarType "date"
>     typeChar = ScalarType "char"
>     typeBool = ScalarType "bool"
