
Currently, just some notes.

Extension to implement a module system:

initial syntax is new_module - which adds the module and then adds
everything following to that module until a new new_module is
hit. Just use . separated names, no explicit hierarchy for now.

then, add public export lists, get these in a catalog

then add import lists (can only import whole modules, no qualification
for now - no namespace control so names still have to be unique), get
these in a catalog

then, add a check to run on a type-checked ast, which makes sure only
access is consistent with export and import lists

then, add facility to explicitly list what is used from which import

put default catalog in module

work on namespacing and qualification?

match haskell style of one module per file in right folder location?

enforce directed graph of module dependencies

use some idea of interface files for modules?

rough idea from dylan: create multiple export lists for each module
with names so you can import the appropriate export you want to use,
have this optional so if you only use one export list you don't have
to name it, and if you import without specifying the export list name,
you get the first one in the file by default. This way, e.g., we can
export stuff for the other sql modules to use, but not for clients
connecting to the database, and simulate a friend-type relationship
between modules. Maybe also add an implicit export which exports
everything?

take a bit further, e.g. for the '6nf' thing, could create a namespace
control which says, this table is accessible only by the following
views, then create rules on the views, so we have physical
implementation independence and the table with the nulls is completely
hidden, without having to create a separate module.

> {-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Extensions.Modules
>     (modules
>     ,modulesExample) where
>
> import Data.Generics.Uniplate.Data
> import Control.Monad.State
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.SqlQuote
> import Database.HsSqlPpp.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Extensions.AstUtils
>
> modulesExample :: ExtensionTest
> modulesExample =
>   ExtensionTest
>     "modules"
>     modules
>     [$sqlStmts| select module('Chaos.Server.Metadata');
>      create table t1 (
>        a text
>      );
>      select 2;
>      |]
>     [$sqlStmts|
>      create table modules (
>        module_name text,
>        module_order serial
>      );
>      create table all_module_objects (
>        object_name text,
>        object_type text,
>        module_name text
>      );
>      insert into modules (module_name) values ('Chaos.Server.Metadata');
>      create table t1 (
>        a text
>      );
>      insert into all_module_objects (object_name,object_type,module_name)
>             values ('t1','table','Chaos.Server.Metadata');
>      select 2;
>      |]
>
> modules :: [Statement] -> [Statement]
> modules st =
>     (replaceSourcePos (head st) [$sqlStmts|
>      create table modules (
>       module_name text,
>       module_order serial
>     );
>     create table all_module_objects (
>       object_name text,
>       object_type text,
>       module_name text
>     ); |])
>     ++ reverse (((\f -> evalState (transformBiM f (reverse st)) "no_module") $ \x ->
>             case x of
>                s@[$sqlStmt| select module($s(modname)); |] : tl
>                    -> do
>                       put modname
>                       return $ replaceSourcePos1 s [$sqlStmt|
>                                 insert into modules (module_name)
>                                 values ($s(modname));|] : tl
>                s@(CreateTable _ n _ _) : tl -> insertIt s tl n "table"
>                s@(CreateView _ n _) : tl -> insertIt s tl n "view"
>                s@(CreateType _ n _) : tl -> insertIt s tl n "type"
>                s@(CreateFunction _ n _ _ _ _ _ _) : tl -> insertIt s tl n "function"
>                s@(CreateTrigger _ n _ _ _ _ _ _) : tl -> insertIt s tl n "trigger"
>                s@(CreateDomain _ n _ _ _ ) : tl -> insertIt s tl n "domain"
>                x1 -> return x1))
>     where
>       insertIt s tl nm ty= do
>          m <- get
>          return $ replaceSourcePos1 s ([$sqlStmt|
>                    insert into all_module_objects (object_name,object_type,module_name)
>                    values ($s(nm),$s(ty), $s(m));|]) : s : tl
