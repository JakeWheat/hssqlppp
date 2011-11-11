
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.CreateTable (createTableParsingTestData) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> createTableParsingTestData:: Item
> createTableParsingTestData =
>   Group "create table" [
>      Group "simple tables" [
>       s "create table test (\n\
>         \  fielda text,\n\
>         \  fieldb int\n\
>         \);"
>       [CreateTable ea
>        (dqi "test")
>        [att "fielda" "text"
>        ,att "fieldb" "int"
>        ]
>        []]
>      ,s "create table tbl (\n\
>         \  fld boolean default false);"
>       [CreateTable ea (dqi "tbl")
>        [AttributeDef ea (Nmc "fld") (st "boolean")
>                          (Just lFalse) []][]]
>
>      ,s "create table tbl as select 1;"
>       [CreateTableAs ea (dqi "tbl")
>        (makeSelect
>         {selSelectList = sl [si $ num "1"]})]
>
>      ,s "alter table a alter column b set default 1;"
>       [AlterTable ea (dqi "a") [AlterColumnDefault ea (Nmc "b") (num "1")]]
>
>      ,s "alter table a add constraint unique(b);"
>       [AlterTable ea (dqi "a") [AddConstraint ea (UniqueConstraint ea "" [Nmc "b"])]]
>      ]
>     ,Group "constraints" [
>       Group "nulls" [
>       s "create table t1 (\n\
>         \ a text null\n\
>         \);"
>         [CreateTable ea (dqi "t1") [AttributeDef ea (Nmc "a") (st "text")
>                            Nothing [NullConstraint ea ""]]
>          []]
>      ,s "create table t1 (\n\
>         \ a text not null\n\
>         \);"
>         [CreateTable ea (dqi "t1") [AttributeDef ea (Nmc "a") (st "text")
>                                     Nothing [NotNullConstraint ea ""]]
>          []]
>      ]
>
>      ,Group "unique" [
>       s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ unique (x,y)\n\
>         \);"
>         [CreateTable ea (dqi "t1") [att "x" "int"
>                                    ,att "y" "int"]
>          [UniqueConstraint ea "" [Nmc "x",Nmc "y"]]]

test arbitrary ordering

>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ unique (x),\n\
>         \ y int\n\
>         \);"
>         [CreateTable ea (dqi "t1") [att "x" "int"
>                                    ,att "y" "int"]
>          [UniqueConstraint ea "" [Nmc "x"]]]

unique row

>      ,s "create table t1 (\n\
>         \ x int unique\n\
>         \);"
>         [CreateTable ea (dqi "t1")
>          [AttributeDef ea (Nmc "x") (st "int") Nothing
>           [RowUniqueConstraint ea ""]][]]
>
>      ,s "create table t1 (\n\
>         \ x int unique not null\n\
>         \);"
>         [CreateTable ea (dqi "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                                     [RowUniqueConstraint ea ""
>                                     ,NotNullConstraint ea ""]][]]

quick sanity check

>      ,s "create table t1 (\n\
>         \ x int not null unique\n\
>         \);"
>         [CreateTable ea (dqi "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                                     [NotNullConstraint ea ""
>                                     ,RowUniqueConstraint ea ""]][]]
>      ]
>
>      ,Group "primary key" [
>       s "create table t1 (\n\
>         \ x int primary key\n\
>         \);"
>         [CreateTable ea (dqi "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowPrimaryKeyConstraint ea ""]][]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ primary key (x,y)\n\
>         \);"
>         [CreateTable ea (dqi "t1") [att "x" "int"
>                           ,att "y" "int"]
>          [PrimaryKeyConstraint ea "" [Nmc "x", Nmc "y"]]]
>      ]
>
>      ,Group "check" [
>       s "create table t (\n\
>         \f text check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable ea (dqi "t")
>          [AttributeDef ea (Nmc "f") (st "text") Nothing
>           [RowCheckConstraint ea "" (InPredicate ea
>                                   (Identifier ea "f") True
>                                   (InList ea [stringQ "a", stringQ "b"]))]] []]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ check (x>y)\n\
>         \);"
>         [CreateTable ea (dqi "t1") [att "x" "int"
>                                    ,att "y" "int"]
>          [CheckConstraint ea "" (binop ">" (ei "x") (ei "y"))]]
>      ]
>
>      ,Group "misc" [
>       s "create table t (\n\
>         \f text not null unique check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable ea (dqi "t")
>          [AttributeDef ea (Nmc "f") (st "text") Nothing
>           [NotNullConstraint ea ""
>            ,RowUniqueConstraint ea ""
>            ,RowCheckConstraint ea "" (InPredicate ea
>                                    (Identifier ea "f") True
>                                    (InList ea [stringQ "a"
>                                               ,stringQ "b"]))]] []]
>      ]

>      ,Group "references" [
>       s "create table t1 (\n\
>         \ x int references t2\n\
>         \);"
>         [CreateTable ea (dqi "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (dqi "t2") Nothing
>                             Restrict Restrict]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2(y)\n\
>         \);"
>         [CreateTable ea (dqi "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (dqi "t2") (Just $ Nmc "y")
>                             Restrict Restrict]][]]
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2\n\
>         \);"
>         [CreateTable ea (dqi "t1") [att "x" "int"
>                                    ,att "y" "int"]
>          [ReferenceConstraint ea "" [Nmc "x", Nmc "y"] (dqi "t2") []
>           Restrict Restrict]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2(z,w)\n\
>         \);"
>         [CreateTable ea (dqi "t1") [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint ea "" [Nmc "x", Nmc "y"] (dqi "t2") [Nmc "z", Nmc "w"]
>           Restrict Restrict]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on delete cascade\n\
>         \);"
>         [CreateTable ea (dqi "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (dqi "t2") Nothing
>                             Cascade Restrict]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on update cascade\n\
>         \);"
>         [CreateTable ea (dqi "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (dqi "t2") Nothing
>                             Restrict Cascade]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on delete cascade on update cascade\n\
>         \);"
>         [CreateTable ea (dqi "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                                     [RowReferenceConstraint ea "" (dqi "t2") Nothing
>                                      Cascade Cascade]][]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2 on update cascade on delete cascade\n\
>         \);"
>         [CreateTable ea (dqi "t1") [att "x" "int"
>                                    ,att "y" "int"]
>          [ReferenceConstraint ea "" [Nmc "x", Nmc "y"] (dqi "t2") []
>           Cascade Cascade]]
>
>      ]
>      ]
>      ]

>  where
>    s = Stmt
