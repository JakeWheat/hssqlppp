

> module Database.HsSqlPpp.Tests.Parsing.FunctionsDdl (functionsDdlParsingTestData) where
>
> --import Test.HUnit
> --import Test.Framework
> --import Test.Framework.Providers.HUnit
> --import Data.Generics
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Annotation
> --import Database.HsSqlPpp.Parser
> --import Database.HsSqlPpp.Pretty

> import Database.HsSqlPpp.Tests.Parsing.Utils

> functionsDdlParsingTestData:: Item
> functionsDdlParsingTestData =
>    Group "functionsddl" [
>      Group "basics" [
>       s "create function t1(text) returns text as $$\n\
>         \select a from t1 where b = $1;\n\
>         \$$ language sql stable;"
>       [CreateFunction ea "t1" [ParamDefTp ea $ SimpleTypeName ea "text"]
>        (SimpleTypeName ea "text") NoReplace Sql
>        (SqlFnBody ea
>         [QueryStatement ea $ selectFromWhere [SelExp ea (Identifier ea "a")] (Tref ea (i "t1") (NoAlias ea))
>          (FunCall ea "="
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
>       [CreateFunction ea "fn" [] (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [VarDef ea "a" (SimpleTypeName ea "int") Nothing
>                                            ,VarDef ea "b" (SimpleTypeName ea "text") Nothing]
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
>       [CreateFunction ea "fn" [] (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing [VarDef ea "a" (SimpleTypeName ea "int") Nothing
>                           ,VarDef ea "b" (SimpleTypeName ea "text") Nothing]
>         [NullStatement ea]))
>        Volatile]
>      ,s "create function fn(a text[]) returns int[] as $$\n\
>         \declare\n\
>         \  b xtype[] := '{}';\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql immutable;"
>       [CreateFunction ea "fn"
>        [ParamDef ea "a" $ ArrayTypeName ea $ SimpleTypeName ea "text"]
>        (ArrayTypeName ea $ SimpleTypeName ea "int") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [VarDef ea "b" (ArrayTypeName ea $ SimpleTypeName ea "xtype") (Just $ stringQ "{}")]
>          [NullStatement ea]))
>        Immutable]
>      ,s "create function fn() returns void as '\n\
>         \declare\n\
>         \  a int := 3;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction ea "fn" [] (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [VarDef ea "a" (SimpleTypeName ea "int") (Just $ NumberLit ea "3")]
>          [NullStatement ea]))
>        Stable]
>      ,s "create function fn(int) returns void as '\n\
>         \declare\n\
>         \  a alias for $1;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction ea "fn"
>        [ParamDefTp ea $ SimpleTypeName ea "int"]
>        (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [ParamAlias ea "a" 1]
>          [NullStatement ea]))
>        Stable]
>      ,s "create function fn(b int) returns void as '\n\
>         \declare\n\
>         \  a alias for b;\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \' language plpgsql stable;"
>       [CreateFunction ea "fn"
>        [ParamDef ea "b" $ SimpleTypeName ea "int"]
>        (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea
>         (Block ea Nothing
>          [VarAlias ea "a" "b"]
>          [NullStatement ea]))
>        Stable]
>      ,s "create function fn() returns setof int as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea "fn" []
>        (SetOfTypeName ea $ SimpleTypeName ea "int") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "create function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea "fn" []
>        (SimpleTypeName ea "void") NoReplace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "create or replace function fn() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql stable;"
>       [CreateFunction ea "fn" []
>        (SimpleTypeName ea "void") Replace Plpgsql
>        (PlpgsqlFnBody ea (Block ea Nothing [] [NullStatement ea]))
>        Stable]
>      ,s "drop function test(text);"
>       [DropFunction ea Require [("test",[SimpleTypeName ea "text"])] Restrict]
>      ,s "drop function test(int,int);"
>       [DropFunction ea Require [("test",[SimpleTypeName ea "int"
>                                         ,SimpleTypeName ea "int"])] Restrict]
>      ,s "drop function if exists a(),test(text) cascade;"
>       [DropFunction ea IfExists [("a",[])
>                           ,("test",[SimpleTypeName ea "text"])] Cascade]
>     ]]
>  where
>    --e = Expr
>    s = Stmt
>    --f = PgSqlStmt