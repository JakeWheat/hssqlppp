

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.FunctionsDdl (functionsDdlParsingTestData) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> functionsDdlParsingTestData:: Item
> functionsDdlParsingTestData =
>    Group "functionsddl" [
>      Group "basics" [
>       s "create function t1(text) returns text as $$\n\
>         \select a from t1 where b = $1;\n\
>         \$$ language sql stable;"
>       [CreateFunction ea (dqi "t1") [ParamDefTp ea $ SimpleTypeName ea $ name "text"]
>        (SimpleTypeName ea $ name "text") NoReplace Sql
>        (SqlFnBody ea
>         [QueryStatement ea $ selectFromWhere [SelExp ea (Identifier ea "a")] (Tref ea (i "t1") (NoAlias ea))
>          (App ea (name "=")
>           [Identifier ea "b", PositionalArg ea 1])])
>        Stable]
>      ,s "create function fn() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>       [CreateFunction ea (dqi "fn") [] (SimpleTypeName ea $ name "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [VarDef ea (Nmc "a") (SimpleTypeName ea $ name "int") Nothing
>                                            ,VarDef ea (Nmc "b") (SimpleTypeName ea $ name "text") Nothing]
>                           [NullStatement ea]))
>        Volatile]
>      ,s "create function fn() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>       [CreateFunction ea (dqi "fn") [] (SimpleTypeName ea $ name "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing [VarDef ea (Nmc "a") (SimpleTypeName ea $ name "int") Nothing
>                           ,VarDef ea (Nmc "b") (SimpleTypeName ea $ name "text") Nothing]
>         [NullStatement ea]))
>        Volatile]
>      ,s "create function fn(a text[]) returns int[] as $$\n\
>         \declare\n\
>         \  b xtype[] := '{}';\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql immutable;"
>       [CreateFunction ea (dqi "fn")
>        [ParamDef ea (Nmc "a") $ ArrayTypeName ea $ SimpleTypeName ea $ name "text"]
>        (ArrayTypeName ea $ SimpleTypeName ea $ name "int") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [VarDef ea (Nmc "b") (ArrayTypeName ea $ SimpleTypeName ea $ name "xtype") (Just $ stringQ "{}")]
>          [NullStatement ea]))
>        Immutable]
>      ,s "create function fn() returns void as '\n\
>         \declare\n\
>         \  a int := 3;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction ea (dqi "fn") [] (SimpleTypeName ea $ name "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [VarDef ea (Nmc "a") (SimpleTypeName ea $ name "int") (Just $ NumberLit ea "3")]
>          [NullStatement ea]))
>        Stable]
>      ,s "create function fn(int) returns void as '\n\
>         \declare\n\
>         \  a alias for $1;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction ea (dqi "fn")
>        [ParamDefTp ea $ SimpleTypeName ea $ name "int"]
>        (SimpleTypeName ea $ name "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [ParamAlias ea (Nmc "a") 1]
>          [NullStatement ea]))
>        Stable]
>      ,s "create function fn(b int) returns void as '\n\
>         \declare\n\
>         \  a alias for b;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction ea (dqi "fn")
>        [ParamDef ea (Nmc "b") $ SimpleTypeName ea $ name "int"]
>        (SimpleTypeName ea $ name "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [VarAlias ea (Nmc "a") (dqi "b")]
>          [NullStatement ea]))
>        Stable]
>      ,s "create function fn() returns setof int as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea (dqi "fn") []
>        (SetOfTypeName ea $ SimpleTypeName ea $ name "int") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "create function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea (dqi "fn") []
>        (SimpleTypeName ea $ name "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "create or replace function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea (dqi "fn") []
>        (SimpleTypeName ea $ name "void") Replace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "drop function test(text);"
>       [DropFunction ea Require [(dqi "test",[SimpleTypeName ea $ name "text"])] Restrict]
>      ,s "drop function test(int,int);"
>       [DropFunction ea Require [(dqi "test",[SimpleTypeName ea $ name "int"
>                                             ,SimpleTypeName ea $ name "int"])] Restrict]
>      ,s "drop function if exists a(),test(text) cascade;"
>       [DropFunction ea IfExists [(dqi "a",[])
>                                 ,(dqi "test",[SimpleTypeName ea $ name "text"])] Cascade]
>     ]]
>  where
>    s = Stmt
