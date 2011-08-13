
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> module Database.HsSqlPpp.Tests.TypeChecking.Triggers
>     (tcTriggersTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> --import Database.HsSqlPpp.Types
>
> --import Database.HsSqlPpp.Catalog

--------------------------------------------------------------------------------


> tcTriggersTestData :: Item
> tcTriggersTestData = --fixme
>   Group "triggers" [
>       {-d [$here|
>          create table t1 (
>            a int,
>            b int,
>            c text
>          );
>          create table t2 (
>            a int,
>            b int,
>            c int
>          );
>          create function trig_fn() returns trigger as $a$
>          begin
>            if (NEW.a > 100) or (OLD.b < 5) then
>                raise exception 'no good';
>            end if;
>            return NEW;
>          end;
>          $a$ language plpgsql volatile;
>          create trigger trig
>            after update on t1
>            for each row
>            execute procedure check_relvar_update();
>          create trigger trig
>            after update on t2
>            for each row
>            execute procedure check_relvar_update();
>          |]
>       [CatCreateTable "t1" [("a",ScalarType "int4")
>                            ,("b",ScalarType "int4")
>                            ,("c",ScalarType "text")]
>                            [("tableoid",ScalarType "oid")
>                            ,("cmax",ScalarType "cid")
>                            ,("xmax",ScalarType "xid")
>                            ,("cmin",ScalarType "cid")
>                            ,("xmin",ScalarType "xid")
>                            ,("ctid",ScalarType "tid")]
>       ,CatCreateTable "t2" [("a",ScalarType "int4")
>                            ,("b",ScalarType "int4")
>                            ,("c",ScalarType "int4")]
>                            [("tableoid",ScalarType "oid")
>                            ,("cmax",ScalarType "cid")
>                            ,("xmax",ScalarType "xid")
>                            ,("cmin",ScalarType "cid")
>                            ,("xmin",ScalarType "xid")
>                            ,("ctid",ScalarType "tid")]
>       ,CatCreateFunction FunName "trig_fn" [] (Pseudo Trigger) False]-}
>      {-,s [$here|
>          create table t1 (
>            a int,
>            b int,
>            c text
>          );
>          create function trig_fn() returns trigger as $a$
>          begin
>            if NEW.d = 'bad value' then
>              return NEW;
>                raise exception 'no good';
>            end if;
>          end;
>          $a$ language plpgsql volatile;
>          create trigger trig
>            after update on t1
>            for each row
>            execute procedure check_relvar_update();
>          |]
>         $ Left []-}
>      ]

>  where
>    --e = Expr
>    --s = StmtType
>    --c = CatStmtType
>    --d = Ddl

