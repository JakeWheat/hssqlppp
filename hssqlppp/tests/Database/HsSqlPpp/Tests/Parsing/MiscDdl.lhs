
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.MiscDdl (miscDdl) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> miscDdl:: Item
> miscDdl =
>   Group "misc ddl" [
>     Group "misc create" [
>       s "create database dbname;"
>       [CreateDatabase ea (name "dbname")]
>      ,s "create view v1 as\n\
>         \select a,b from t;"
>       [CreateView ea
>        (name "v1") Nothing
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"
>                             ,si $ ei "b"]
>         ,selTref = [tref "t"]})]
>      ,s "create view v1(c,d) as\n\
>         \select a,b from t;"
>       [CreateView ea
>        (name "v1") (Just [Nmc "c",Nmc "d"])
>        (makeSelect
>         {selSelectList = sl [si $ ei "a"
>                             ,si $ ei "b"]
>         ,selTref = [tref "t"]})]
>      ,s "create domain td as text check (value in ('t1', 't2'));"
>       [CreateDomain ea (name "td") (st "text") ""
>        (Just (InPredicate ea (ei "value") True
>               (InList ea [stringQ "t1" ,stringQ "t2"])))]
>      ,s "create type tp1 as (\n\
>         \  f1 text,\n\
>         \  f2 text\n\
>         \);"
>       [CreateType ea (name "tp1") [TypeAttDef ea (Nmc "f1") (st "text")
>                                  ,TypeAttDef ea (Nmc "f2") (st "text")]]
>
>      ,s "create sequence s start with 5 increment by 4 no maxvalue no minvalue cache 1;"
>         [CreateSequence ea (name "s") 4 1 ((2::Integer) ^ (63::Integer) - 1) 5 1]
>
>      ,s "alter sequence s owned by a.b;"
>         [AlterSequence ea (name "s") $ qn "a" "b"]
>
>      ,s "create trigger tr\n\
>          \after insert or delete on tb\n\
>          \for each statement\n\
>          \execute procedure fb();"
>         [CreateTrigger ea (Nmc "tr") TriggerAfter [TInsert,TDelete]
>          (name "tb") EachStatement (name "fb") []]
>      ]
>
>     ,Group "drops" [
>       s "drop domain t;"
>       [DropSomething ea Domain Require [name "t"] Restrict]
>      ,s "drop domain if exists t,u cascade;"
>       [DropSomething ea Domain IfExists [name "t", name "u"] Cascade]
>      ,s "drop domain t restrict;"
>       [DropSomething ea Domain Require [name "t"] Restrict]
>
>      ,s "drop type t;"
>       [DropSomething ea Type Require [name "t"] Restrict]
>      ,s "drop table t;"
>       [DropSomething ea Table Require [name "t"] Restrict]
>      ,s "drop view t;"
>       [DropSomething ea View Require [name "t"] Restrict]
>      ,s "drop database dbname;"
>       [DropSomething ea Database Require [name "dbname"] Restrict]
>      ]]
>  where
>    s = Stmt
