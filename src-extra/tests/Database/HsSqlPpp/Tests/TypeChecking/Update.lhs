
> module Database.HsSqlPpp.Tests.TypeChecking.Update
>     (tcUpdateTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types

> tcUpdateTestData :: Item
> tcUpdateTestData =
>   Group "update" [
>       s "update nope set a = 1;"
>         $ Left [UnrecognisedRelation "nope"]
>      ,s "update pg_attrdef set adsrc = '' where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,s "update pg_attrdef set (adbin,adsrc) = ('a','b','c');"
>         $ Left [IncompatibleTypes
>                 (AnonymousRecordType [ScalarType "pg_node_tree"
>                                      ,ScalarType "text"])
>                 (AnonymousRecordType [UnknownType
>                                      ,UnknownType
>                                      ,UnknownType])]
>      ,s "update pg_attrdef set (adrelid,adsrc) = (true,'b');"
>         $ Left [IncompatibleTypes
>                 (AnonymousRecordType [ScalarType "oid"
>                                      ,ScalarType "text"])
>                 (AnonymousRecordType [ScalarType "bool"
>                                      ,UnknownType])]
>      ,s "update pg_attrdef set (shmadrelid,adsrc) = ('a','b');"
>         $ Left [UnrecognisedIdentifier "shmadrelid"]
>      ,s "update pg_attrdef set adsrc='';"
>         $ Right [Just ([], [])]
>      ,s "update pg_attrdef set adsrc='' where 1=2;"
>         $ Right [Just ([], [])]
>           -- FIXME?
>       -- TODO: actually, pg doesn't support this so need to generate error instead
>      {-,s "update pg_attrdef set (adbin,adsrc) = ((select 'a','b'));"
>         $ Right [Just ([], [])]-}
>      --check where ids
>      ,s "update pg_attrdef set adsrc='' where adsrc='';"
>         $ Right [Just ([], [])]
>      ,s "update pg_attrdef set adnum = adnum + 1;"
>         $ Right [Just ([], [])]
>      ]

>  where
>    s = StmtType

