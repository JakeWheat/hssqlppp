
> module Database.HsSqlPpp.Tests.TypeChecking.Creates
>     (tcCreateTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> import Database.HsSqlPpp.Catalog

> tcCreateTestData :: Item
> tcCreateTestData =
>   Group "creates" [
>       d "create table t1 (\n\
>         \   a int,\n\
>         \   b text\n\
>         \);"
>         [CatCreateTable "t1" [("a",ScalarType "int4")
>                               ,("b",ScalarType "text")]
>                               [("tableoid", ScalarType "oid")
>                               ,("cmax", ScalarType "cid")
>                               ,("xmax", ScalarType "xid")
>                               ,("cmin", ScalarType "cid")
>                               ,("xmin", ScalarType "xid")
>                               ,("ctid", ScalarType "tid")]]
>      ,d "create type t1 as (\n\
>         \   a int,\n\
>         \   b text\n\
>         \);"
>         [CatCreateComposite "t1" [("a",ScalarType "int4")
>                                   ,("b",ScalarType "text")]]
>
>      ,d "create domain t1 as text;"
>         [CatCreateDomain (DomainType "t1") (ScalarType "text")]

>      --fixme: check in domain constraint
>      {-,d "create domain t1 as text check (value in ('a', 'b'));\n\
>         \select 'text'::t1;"
>         [CatCreateDomain (DomainType "t1") (ScalarType "text")]-}
>
>
>      ,d "create view v1 as select * from pg_attrdef;"
>         [CatCreateView "v1" [("adrelid",ScalarType "oid")
>                              ,("adnum",ScalarType "int2")
>                              ,("adbin",ScalarType "pg_node_tree")
>                              ,("adsrc",ScalarType "text")]]
>
>      ,d "create function t1(text) returns text as $$\n\
>         \null;\n\
>         \$$ language sql stable;"
>         [CatCreateFunction FunName "t1" [ScalarType "text"]
>                             (ScalarType "text") False]
>      ,d "create language plpgsql;"
>         [CatCreateFunction FunName "plpgsql_call_handler" [] (Pseudo LanguageHandler) False
>         ,CatCreateFunction FunName "plpgsql_validator" [ScalarType "oid"] (Pseudo Void) False]
>      ]
>
>  where
>    d = Ddl

