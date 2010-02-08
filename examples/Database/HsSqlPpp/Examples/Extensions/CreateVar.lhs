Copyright 2010 Jake Wheat

Extension to remove the boilerplate from adding tables with a single
attribute and single row, a bit like a global variable in the
database. Unfinished, needs the extended constraint system to be working

> {-# LANGUAGE ViewPatterns, QuasiQuotes #-}
>
> module Database.HsSqlPpp.Examples.Extensions.CreateVar
>     where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote
> import Database.HsSqlPpp.Annotation
>
> createVarExample :: ExtensionTest
> createVarExample = ExtensionTest
>   "CreateVar"
>   createVar
>   [$sqlStmts| select create_var('varname', 'vartype'); |]
>   [$sqlStmts|
>
>   create table varname_table (
>     varname vartype
>   );
>
>   create function get_varname() returns vartype as $a$
>     select * from varname_table;
>   $a$ language sql stable;
>
>   -- haven't needed this
>   /*drop function if exists varname_table_constraint_trigger_operator();
>   create function varname_table_constraint_trigger_operator() returns trigger as $a$
>   begin
>     null;
>   end;
>   $a$ language plpgsql;*/
>
>   -- todo once constraint extension is written
>   /*create function check_con_varname_table_varname_key() returns boolean as $a$
>   begin
>     return true;
>   end;
>   $a$ language plpgsql stable;
>
>   create function check_con_varname_table_01_tuple() returns boolean as $a$
>   begin
>     return true;
>   end;
>   $a$ language plpgsql stable;
>   drop function if exists varname_table_constraint_trigger_operator();
>   create function varname_table_constraint_trigger_operator() returns trigger as $a$
>   begin
>     null;
>   end;
>   $a$ language plpgsql;*/
>   |]
>
> createVar :: Data a => a -> a
> createVar =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView _ "create_var" [StringLit _ _ varname
>                                                    ,StringLit _ _ typename]):tl
>             -> let tablename = varname ++ "_table"
>                    fnname = "get_" ++ varname
>                in [$sqlStmts|
>
>   create table $(tablename) (
>    $(varname) $(typename)
>   );
>
>   create function $(fnname)() returns $(typename) as $a$
>     select * from $(tablename);
>   $a$ language sql stable;
>
>                    |] ++ tl
>         x1 -> x1
>