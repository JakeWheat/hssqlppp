
> module Database.HsSqlPpp.Tests.TypeChecking.Update
>     (tcUpdateTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types

> tcUpdateTestData :: Item
> tcUpdateTestData = -- FIXME : update broken
>   Group "update" $ drop 1000 [
>       s "update nope set a = 1;"
>         $ Left [UnrecognisedRelation "nope"
>                ,UnrecognisedIdentifier "a"]
>      ,s "update pg_attrdef set adsrc = '' where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,s "update pg_attrdef set (adbin,adsrc) = ('a','b','c');"
>         $ Left [NoMatchingOperator "="
>                 [AnonymousRecordType [ScalarType "text"
>                                      ,ScalarType "text"]
>                 ,AnonymousRecordType [UnknownType
>                                      ,UnknownType
>                                      ,UnknownType]]]
>      ,s "update pg_attrdef set (adrelid,adsrc) = (true,'b');"
>         $ Left [NoMatchingOperator "="
>                 [AnonymousRecordType [ScalarType "oid"
>                                      ,ScalarType "text"]
>                 ,AnonymousRecordType [ScalarType "bool"
>                                      ,UnknownType]]]
>      ,s "update pg_attrdef set (shmadrelid,adsrc) = ('a','b');"
>         $ Left [UnrecognisedIdentifier "shmadrelid"]
>      ,s "update pg_attrdef set adsrc='';"
>         $ Right [Just ([], [] {-UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]-})]
>      ,s "update pg_attrdef set adsrc='' where 1=2;"
>         $ Right [Just ([], [] {-UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]-})]
>           -- FIXME?
>       -- TODO: actually, pg doesn't support this so need to generate error instead
>      {-,s "update pg_attrdef set (adbin,adsrc) = ((select 'a','b'));"
>         $ Right [Just ([], [] {-UpdateInfo "pg_attrdef" [("adbin",ScalarType "text"),("adsrc",ScalarType "text")]-})]-}
>      --check where ids
>      ,s "update pg_attrdef set adsrc='' where adsrc='';"
>         $ Right [Just ([], [] {-UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]-})]
>      ,s "update pg_attrdef set adnum = adnum + 1;"
>         $ Right [Just ([], [] {-UpdateInfo "pg_attrdef" [("adnum",ScalarType "int2")]-})]
>      ]

>  where
>    --e = Expr
>    s = StmtType
>    --c = CatStmtType
>    --d = Ddl

