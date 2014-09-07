
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.CreateTable (createTable) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> createTable:: Item
> createTable =
>   Group "create table" [
>      Group "simple tables" [
>       s "create table test (\n\
>         \  fielda text,\n\
>         \  fieldb int\n\
>         \);"
>       [CreateTable ea
>        (name "test")
>        [att "fielda" "text"
>        ,att "fieldb" "int"
>        ]
>        []]
>      ,s "create table tbl (\n\
>         \  fld boolean default false);"
>       [CreateTable ea (name "tbl")
>        [AttributeDef ea (Nmc "fld") (st "boolean")
>                          (Just lFalse) []][]]
>
>      ,s "create table tbl as select 1;"
>       [CreateTableAs ea (name "tbl")
>        (makeSelect
>         {selSelectList = sl [si $ num "1"]})]
>
>      ,s "alter table a rename to b;"
>       [AlterTable ea (name "a") $ RenameTable ea (name "b")]
>      ,s "alter table a rename column b to c;"
>       [AlterTable ea (name "a") $ RenameColumn ea (Nmc "b") (Nmc "c")]
>
>      ,s "alter table a add column b int;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AddColumn ea $ att "b" "int"]]
>
>      ,s "alter table a drop column b;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [DropColumn ea (Nmc "b")]]
>
>      ,s "alter table a alter column b set data type int;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ SetDataType ea (st "int")]]
>
>      ,s "alter table a alter column b set data type int;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ SetDataType ea (st "int")]]
>
>      ,s "alter table a alter column b set default 1;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ SetDefault ea (num "1")]]
>      ,s "alter table a alter column b drop default;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ DropDefault ea]]
>
>      ,s "alter table a alter column b set not null;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ SetNotNull ea]]
>      ,s "alter table a alter column b drop not null;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ DropNotNull ea]]
>
>      ,s "alter table a add column b int,drop column c;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AddColumn ea $ att "b" "int"
>                                                        ,DropColumn ea (Nmc "c")]]
>
>      ,s "alter table a drop column b;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [DropColumn ea (Nmc "b")]]
>      ,s "alter table a add constraint unique(b);"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AddConstraint ea (UniqueConstraint ea "" [Nmc "b"])]]
>      ]
>     ,Group "constraints" [
>       Group "nulls" [
>       s "create table t1 (\n\
>         \ a text null\n\
>         \);"
>         [CreateTable ea (name "t1") [AttributeDef ea (Nmc "a") (st "text")
>                            Nothing [NullConstraint ea ""]]
>          []]
>      ,s "create table t1 (\n\
>         \ a text not null\n\
>         \);"
>         [CreateTable ea (name "t1") [AttributeDef ea (Nmc "a") (st "text")
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
>         [CreateTable ea (name "t1") [att "x" "int"
>                                    ,att "y" "int"]
>          [UniqueConstraint ea "" [Nmc "x",Nmc "y"]]]

test arbitrary ordering

>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ unique (x),\n\
>         \ y int\n\
>         \);"
>         [CreateTable ea (name "t1") [att "x" "int"
>                                    ,att "y" "int"]
>          [UniqueConstraint ea "" [Nmc "x"]]]

unique row

>      ,s "create table t1 (\n\
>         \ x int unique\n\
>         \);"
>         [CreateTable ea (name "t1")
>          [AttributeDef ea (Nmc "x") (st "int") Nothing
>           [RowUniqueConstraint ea ""]][]]
>
>      ,s "create table t1 (\n\
>         \ x int unique not null\n\
>         \);"
>         [CreateTable ea (name "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                                     [RowUniqueConstraint ea ""
>                                     ,NotNullConstraint ea ""]][]]

quick sanity check

>      ,s "create table t1 (\n\
>         \ x int not null unique\n\
>         \);"
>         [CreateTable ea (name "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                                     [NotNullConstraint ea ""
>                                     ,RowUniqueConstraint ea ""]][]]
>      ]
>
>      ,Group "primary key" [
>       s "create table t1 (\n\
>         \ x int primary key\n\
>         \);"
>         [CreateTable ea (name "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowPrimaryKeyConstraint ea ""]][]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ primary key (x,y)\n\
>         \);"
>         [CreateTable ea (name "t1") [att "x" "int"
>                           ,att "y" "int"]
>          [PrimaryKeyConstraint ea "" [Nmc "x", Nmc "y"]]]
>      ]
>
>      ,Group "check" [
>       s "create table t (\n\
>         \f text check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable ea (name "t")
>          [AttributeDef ea (Nmc "f") (st "text") Nothing
>           [RowCheckConstraint ea "" (InPredicate ea
>                                   (ei "f") True
>                                   (InList ea [stringQ "a", stringQ "b"]))]] []]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ check (x>y)\n\
>         \);"
>         [CreateTable ea (name "t1") [att "x" "int"
>                                    ,att "y" "int"]
>          [CheckConstraint ea "" (binop ">" (ei "x") (ei "y"))]]
>      ]
>
>      ,Group "misc" [
>       s "create table t (\n\
>         \f text not null unique check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable ea (name "t")
>          [AttributeDef ea (Nmc "f") (st "text") Nothing
>           [NotNullConstraint ea ""
>            ,RowUniqueConstraint ea ""
>            ,RowCheckConstraint ea "" (InPredicate ea
>                                    (ei "f") True
>                                    (InList ea [stringQ "a"
>                                               ,stringQ "b"]))]] []]
>      ]

>      ,Group "references" [
>       s "create table t1 (\n\
>         \ x int references t2\n\
>         \);"
>         [CreateTable ea (name "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (name "t2") Nothing
>                             Restrict Restrict]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2(y)\n\
>         \);"
>         [CreateTable ea (name "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (name "t2") (Just $ Nmc "y")
>                             Restrict Restrict]][]]
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2\n\
>         \);"
>         [CreateTable ea (name "t1") [att "x" "int"
>                                    ,att "y" "int"]
>          [ReferenceConstraint ea "" [Nmc "x", Nmc "y"] (name "t2") []
>           Restrict Restrict]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2(z,w)\n\
>         \);"
>         [CreateTable ea (name "t1") [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint ea "" [Nmc "x", Nmc "y"] (name "t2") [Nmc "z", Nmc "w"]
>           Restrict Restrict]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on delete cascade\n\
>         \);"
>         [CreateTable ea (name "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (name "t2") Nothing
>                             Cascade Restrict]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on update cascade\n\
>         \);"
>         [CreateTable ea (name "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (name "t2") Nothing
>                             Restrict Cascade]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on delete cascade on update cascade\n\
>         \);"
>         [CreateTable ea (name "t1") [AttributeDef ea (Nmc "x") (st "int") Nothing
>                                     [RowReferenceConstraint ea "" (name "t2") Nothing
>                                      Cascade Cascade]][]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2 on update cascade on delete cascade\n\
>         \);"
>         [CreateTable ea (name "t1") [att "x" "int"
>                                    ,att "y" "int"]
>          [ReferenceConstraint ea "" [Nmc "x", Nmc "y"] (name "t2") []
>           Cascade Cascade]]
>
>      ]
>      ]
>      ]

>  where
>    s = Stmt
