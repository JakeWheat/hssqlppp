
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
>        "test"
>        [att "fielda" "text"
>        ,att "fieldb" "int"
>        ]
>        []]
>      ,s "create table tbl (\n\
>         \  fld boolean default false);"
>       [CreateTable ea "tbl" [AttributeDef ea "fld" (SimpleTypeName ea "boolean")
>                           (Just $ BooleanLit ea False) []][]]
>
>      ,s "create table tbl as select 1;"
>       [CreateTableAs ea "tbl"
>        (selectE (SelectList ea [SelExp ea (NumberLit ea "1")]))]
>
>      ,s "alter table a alter column b set default 1;"
>       [AlterTable ea "a" [AlterColumnDefault ea "b" (NumberLit ea "1")]]
>
>      ,s "alter table a add constraint unique(b);"
>       [AlterTable ea "a" [AddConstraint ea (UniqueConstraint ea "" [Name "b"])]]
>      ]
>     ,Group "constraints" [
>       Group "nulls" [
>       s "create table t1 (\n\
>         \ a text null\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "a" (SimpleTypeName ea "text")
>                            Nothing [NullConstraint ea ""]]
>          []]
>      ,s "create table t1 (\n\
>         \ a text not null\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "a" (SimpleTypeName ea "text")
>                            Nothing [NotNullConstraint ea ""]]
>          []]
>      ]
>
>      ,Group "unique" [
>       s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ unique (x,y)\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [UniqueConstraint ea "" [Name "x",Name "y"]]]

test arbitrary ordering

>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ unique (x),\n\
>         \ y int\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [UniqueConstraint ea "" [Name "x"]]]

unique row

>      ,s "create table t1 (\n\
>         \ x int unique\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowUniqueConstraint ea ""]][]]
>
>      ,s "create table t1 (\n\
>         \ x int unique not null\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowUniqueConstraint ea ""
>                            ,NotNullConstraint ea ""]][]]

quick sanity check

>      ,s "create table t1 (\n\
>         \ x int not null unique\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [NotNullConstraint ea ""
>                            ,RowUniqueConstraint ea ""]][]]
>      ]
>
>      ,Group "primary key" [
>       s "create table t1 (\n\
>         \ x int primary key\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowPrimaryKeyConstraint ea ""]][]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ primary key (x,y)\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [PrimaryKeyConstraint ea "" ["x", "y"]]]
>      ]
>
>      ,Group "check" [
>       s "create table t (\n\
>         \f text check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable ea "t"
>          [AttributeDef ea "f" (SimpleTypeName ea "text") Nothing
>           [RowCheckConstraint ea "" (InPredicate ea
>                                   (Identifier ea "f") True
>                                   (InList ea [stringQ "a", stringQ "b"]))]] []]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ check (x>y)\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [CheckConstraint ea "" (FunCall ea ">" [Identifier ea "x", Identifier ea "y"])]]
>      ]
>
>      ,Group "misc" [
>       s "create table t (\n\
>         \f text not null unique check (f in('a', 'b'))\n\
>         \);"
>         [CreateTable ea "t"
>          [AttributeDef ea "f" (SimpleTypeName ea "text") Nothing
>           [NotNullConstraint ea ""
>            ,RowUniqueConstraint ea ""
>            ,RowCheckConstraint ea "" (InPredicate ea
>                                    (Identifier ea "f") True
>                                    (InList ea [stringQ "a"
>                                            ,stringQ "b"]))]] []]
>      ]

>      ,Group "references" [
>       s "create table t1 (\n\
>         \ x int references t2\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowReferenceConstraint ea "" "t2" Nothing
>                             Restrict Restrict]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2(y)\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowReferenceConstraint ea "" "t2" (Just "y")
>                             Restrict Restrict]][]]
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint ea "" ["x", "y"] "t2" []
>           Restrict Restrict]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2(z,w)\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint ea "" ["x", "y"] "t2" ["z", "w"]
>           Restrict Restrict]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on delete cascade\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowReferenceConstraint ea "" "t2" Nothing
>                             Cascade Restrict]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on update cascade\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowReferenceConstraint ea "" "t2" Nothing
>                             Restrict Cascade]][]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on delete cascade on update cascade\n\
>         \);"
>         [CreateTable ea "t1" [AttributeDef ea "x" (SimpleTypeName ea "int") Nothing
>                            [RowReferenceConstraint ea "" "t2" Nothing
>                             Cascade Cascade]][]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2 on update cascade on delete cascade\n\
>         \);"
>         [CreateTable ea "t1" [att "x" "int"
>                           ,att "y" "int"]
>          [ReferenceConstraint ea "" ["x", "y"] "t2" []
>           Cascade Cascade]]
>
>      ]
>      ]
>      ]

>  where
>    s = Stmt
