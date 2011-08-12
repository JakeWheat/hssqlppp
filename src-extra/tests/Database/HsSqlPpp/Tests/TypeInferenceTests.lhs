
> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.TypeInferenceTests (typeInferenceTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> --import Data.List
> import Data.Generics.Uniplate.Data

> --import Debug.Trace
>
> --import Database.HsSqlPpp.Utils.Here
> --import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Quote
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Pretty

> data Item = Expr ScalarExpr [Maybe Type]
>           | Group String [Item]

> typeInferenceTests :: Test.Framework.Test
> typeInferenceTests = itemToTft typeInferenceTestData
>
> typeInferenceTestData :: Item
> typeInferenceTestData =
>   Group "typeInferenceTests" [
>     e [sqlExpr| 1 = '2'|] [Just typeBool
>                            ,Just typeInt
>                            ,Just typeInt]
>    ,e [sqlExpr| (3,4) = ('2','3')|] [Just typeBool
>                                      ,Just $ AnonymousRecordType [typeInt,typeInt]
>                                      ,Just typeInt
>                                      ,Just typeInt
>                                      ,Just $ AnonymousRecordType [typeInt,typeInt]
>                                      ,Just typeInt
>                                      ,Just typeInt]
>    ,e [sqlExpr| ('2','3') = (3,4)|] [Just typeBool
>                                      ,Just $ AnonymousRecordType [typeInt,typeInt]
>                                      ,Just typeInt
>                                      ,Just typeInt
>                                      ,Just $ AnonymousRecordType [typeInt,typeInt]
>                                      ,Just typeInt
>                                      ,Just typeInt]
>    ,e [sqlExpr| ('2',3) = (3,'4')|] [Just typeBool
>                                      ,Just $ AnonymousRecordType [typeInt,typeInt]
>                                      ,Just typeInt
>                                      ,Just typeInt
>                                      ,Just $ AnonymousRecordType [typeInt,typeInt]
>                                      ,Just typeInt
>                                      ,Just typeInt]
>   ]
>   where
>     e = Expr

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Group s is) = testGroup s $ map itemToTft is

> itemToTft (Expr e t) = testCase (printScalarExpr e) $
>     let a = typeCheckScalarExpr defaultTemplate1Catalog e
>         t1 = [infType el | el <- universeBi a]
>     in assertEqual "" t t1
