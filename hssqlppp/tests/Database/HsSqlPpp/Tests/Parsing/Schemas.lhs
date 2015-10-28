> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.Schemas (schemas) where

> import qualified Data.Text as T

> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> schemas :: Item
> schemas =
>   Group "schema" [
>      Group "ddl - schemas" [
>       s "create schema test;"
>       [CreateSchema ea
>        (Nmc "test")
>        Nothing]
>      ,s "create schema test authorization owner;"
>       [CreateSchema ea
>        (Nmc "test")
>        (Just $ name "owner")
>      ]
>     ,Group "drop schema" [ 
>       s "drop schema test;"
>       [DropSchema ea (Nmc "test") Restrict] 
>      ,s "drop schema test restrict;"
>       [DropSchema ea (Nmc "test") Restrict]
>      ,s "drop schema test cascade;"
>       [DropSchema ea (Nmc "test") Cascade]
>      ]

>     ,Group "alter schema" [ 
>       s "alter schema test rename to test2;"
>       [AlterSchema ea (Nmc "test") $ AlterSchemaName ea (Nmc "test2")]
>       ,s "alter schema test owner to new_owner;"
>       [AlterSchema ea (Nmc "test") $ AlterSchemaOwner ea (name "new_owner")]
>      ]
>     ]
>     ,Group "ddl - schema-explicit tables" [
>       s "create table s.test (\n\
>        \  fielda text,\n\
>        \  fieldb int\n\
>        \);"
>       [CreateTable ea
>        (nameWithSchema "s" "test")
>        [att "fielda" "text"
>        ,att "fieldb" "int"
>        ]
>        []
>        Nothing]
>      ,s "alter table s.a rename to b;"
>       [AlterTable ea (nameWithSchema "s" "a") $ RenameTable ea (name "b")]
>      ,s "drop table s.t;"
>       [DropSomething ea Table Require [nameWithSchema "s" "t"] Restrict]
>     ]
>     ,Group "ddl - schema-explicit views" [
>       s "create view s1.v1 as\n\
>        \select a,b from s2.t;"
>       [CreateView ea
>       (nameWithSchema "s1" "v1") Nothing
>       (makeSelect
>        {selSelectList = sl [si $ ei "a"
>                            ,si $ ei "b"]
>        ,selTref = [trefWithSchema "s2" "t"]})]
>      ,s "alter view s1.v1 as\n\
>         \select a,b from s2.t;"
>        [AlterView ea
>        (nameWithSchema "s1" "v1") Nothing
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"
>                             ,si $ ei "b"]
>         ,selTref = [trefWithSchema "s2" "t"]})]
>      ,s "drop view s.v;"
>       [DropSomething ea View Require [nameWithSchema "s" "v"] Restrict]
>    ]
>     ,Group "dml - schemas" [
>       q "select a from s.t" stbl{selTref = [trefWithSchema "s" "t"]}
>      ,q "select a from s1.t1 natural inner join t2"
>       stbl {selTref = [naturalInnerJoin (trefWithSchema "s1" "t1") (tref "t2") ]}
>    ]
>   ]
>  where
>    s = Stmt
>    stbl = makeSelect
>           {selSelectList = sl [si $ ei "a"]
>           ,selTref = []}
>    q = QueryExpr
>    nameWithSchema sc n = Name ea $ [Nmc $ T.unpack sc, Nmc $ T.unpack n]
>    trefWithSchema sc t = Tref ea (nameWithSchema sc t)
