Copyright 2009 Jake Wheat

Set of tests for the extensions

> {-# LANGUAGE RankNTypes,FlexibleContexts #-}

> module Database.HsSqlPpp.Tests.ExtensionTests (extensionTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char
> import Control.Monad.Error
> --import Debug.Trace

> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Extensions.ChaosExtensions
> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.PrettyPrinter.PrettyPrinter

> extensionTests :: Test.Framework.Test
> extensionTests =
>   testGroup "extensionTests" (mapCheckExtension [
>     t rewriteCreateVars
>       "select create_var('varname','vartype');"
>       "create table varname_table (\n\
>       \  varname vartype);\n\
>       \create function get_varname() returns vartype as $a$\n\
>       \  select * from varname_table;\n\
>       \$a$ language sql stable;"
>    ,t addReadonlyTriggers
>       "select set_relvar_type('stuff','readonly');"
>       "create function check_stuff_d_readonly () returns trigger as $a$\n\
>       \begin\n\
>       \  if (not (false)) then\n\
>       \    raise exception 'delete on base_relvar_metadata violates transition constraint base_relvar_metadata_d_readonly';\n\
>       \  end if;\n\
>       \return null;\n\
>       \end;\n\
>       \$a$ language plpgsql volatile;\n\
>       \create function check_stuff_i_readonly () returns trigger as $a$\n\
>       \begin\n\
>       \  if (not (false)) then\n\
>       \       raise exception 'delete on base_relvar_metadata violates transition constraint base_relvar_metadata_d_readonly';\n\
>       \  end if;\n\
>       \  return null;\n\
>       \end;\n\
>       \$a$ language plpgsql volatile;\n\
>       \create function check_stuff_u_readonly () returns trigger as $a$\n\
>       \begin\n\
>       \  if (not (false)) then\n\
>       \       raise exception 'delete on base_relvar_metadata violates transition constraint base_relvar_metadata_d_readonly';\n\
>       \  end if;\n\
>       \  return null;\n\
>       \end;\n\
>       \$a$ language plpgsql volatile;"
>    ])
>   where
>     t a b c = (a,b,c)
>     mapCheckExtension = map (\(a,b,c) ->  checkExtension a b c)
>     checkExtension :: (StatementList -> StatementList) -> String -> String -> Test.Framework.Test
>     checkExtension f stxt ttxt = testCase ("check " ++ stxt) $
>       case (do
>             sast <- parseSql stxt
>             let esast = f sast
>             --trace (printSql esast) $ return ()
>             tast <- parseSql ttxt
>             return (tast,esast)) of
>         Left e -> assertFailure $ show e
>         Right (ts,es) -> assertEqual "" (stripAnnotations ts) (stripAnnotations es)
