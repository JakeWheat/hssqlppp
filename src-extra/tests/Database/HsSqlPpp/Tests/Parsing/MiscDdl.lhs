
> module Database.HsSqlPpp.Tests.Parsing.MiscDdl (miscDdlParsingTestData) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> miscDdlParsingTestData:: Item
> miscDdlParsingTestData =
>   Group "misc ddl" [
>     Group "misc create" [
>       s "create view v1 as\n\
>         \select a,b from t;"
>       [CreateView ea
>        (dqi "v1") Nothing
>        (selectFrom [selI "a", selI "b"] (Tref ea (i "t") (NoAlias ea)))]
>      ,s "create view v1(c,d) as\n\
>         \select a,b from t;"
>       [CreateView ea
>        (dqi "v1") (Just [Nmc "c",Nmc "d"])
>        (selectFrom [selI "a", selI "b"] (Tref ea (i "t") (NoAlias ea)))]
>      ,s "create domain td as text check (value in ('t1', 't2'));"
>       [CreateDomain ea (dqi "td") (SimpleTypeName ea "text") ""
>        (Just (InPredicate ea (Identifier ea "value") True
>               (InList ea [stringQ "t1" ,stringQ "t2"])))]
>      ,s "create type tp1 as (\n\
>         \  f1 text,\n\
>         \  f2 text\n\
>         \);"
>       [CreateType ea (dqi "tp1") [TypeAttDef ea (Nmc "f1") (SimpleTypeName ea "text")
>                                  ,TypeAttDef ea (Nmc "f2") (SimpleTypeName ea "text")]]
>
>      ,s "create sequence s start with 5 increment by 4 no maxvalue no minvalue cache 1;"
>         [CreateSequence ea (dqi "s") 4 1 ((2::Integer) ^ (63::Integer) - 1) 5 1]
>
>      ,s "alter sequence s owned by a.b;"
>         [AlterSequence ea (dqi "s") $ qi "a" "b"]
>
>      ,s "create trigger tr\n\
>          \after insert or delete on tb\n\
>          \for each statement\n\
>          \execute procedure fb();"
>         [CreateTrigger ea (Nmc "tr") TriggerAfter [TInsert,TDelete] (dqi "tb") EachStatement "fb" []]
>      ]
>
>     ,Group "drops" [
>       s "drop domain t;"
>       [DropSomething ea Domain Require [dqi "t"] Restrict]
>      ,s "drop domain if exists t,u cascade;"
>       [DropSomething ea Domain IfExists [dqi "t", dqi "u"] Cascade]
>      ,s "drop domain t restrict;"
>       [DropSomething ea Domain Require [dqi "t"] Restrict]
>
>      ,s "drop type t;"
>       [DropSomething ea Type Require [dqi "t"] Restrict]
>      ,s "drop table t;"
>       [DropSomething ea Table Require [dqi "t"] Restrict]
>      ,s "drop view t;"
>       [DropSomething ea View Require [dqi "t"] Restrict]
>      ]]
>  where
>    s = Stmt
