

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
>       [CreateFunction ea (dqi "t1") [ParamDefTp ea $ st "text"]
>        (st "text") NoReplace Sql
>        (SqlFnBody ea
>         [QueryStatement ea $
>          makeSelect
>          {selSelectList = sl [si $ ei "a"]
>          ,selTref = [tref "t1"]
>          ,selWhere = Just $ binop "=" (ei "b") (PositionalArg ea 1)}])
>        Stable]
>      ,s "create function fn() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>       [CreateFunction ea (dqi "fn") [] (st "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [varDef "a" (st "int")
>                                            ,varDef "b" (st "text")]
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
>       [CreateFunction ea (dqi "fn") [] (st "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing [varDef "a" (st "int")
>                           ,varDef "b" (st "text")]
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
>        [paramDef "a" $ at "text"]
>        (at "int") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [varDefv "b" (at "xtype") (stringQ "{}")]
>          [NullStatement ea]))
>        Immutable]
>      ,s "create function fn() returns void as '\n\
>         \declare\n\
>         \  a int := 3;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction ea (dqi "fn") [] (st "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [varDefv "a" (st "int") (num "3")]
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
>        [ParamDefTp ea $ st "int"]
>        (st "void") NoReplace Plpgsql
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
>        [ParamDef ea (Nmc "b") $ st "int"]
>        (st "void") NoReplace Plpgsql
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
>        (SetOfTypeName ea $ st "int") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "create function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea (dqi "fn") []
>        (st "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "create or replace function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea (dqi "fn") []
>        (st "void") Replace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "drop function test(text);"
>       [DropFunction ea Require [(dqi "test",[st "text"])] Restrict]
>      ,s "drop function test(int,int);"
>       [DropFunction ea Require [(dqi "test",[st "int"
>                                             ,st "int"])] Restrict]
>      ,s "drop function if exists a(),test(text) cascade;"
>       [DropFunction ea IfExists [(dqi "a",[])
>                                 ,(dqi "test",[st "text"])] Cascade]
>     ]]
>  where
>    s = Stmt
