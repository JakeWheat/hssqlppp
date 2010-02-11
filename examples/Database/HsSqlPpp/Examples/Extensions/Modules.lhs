Copyright 2010 Jake Wheat

Currently, just some notes.

Extension to implement a module system:

~~~~

create table modules (
  module_name text,
  module_parent_name text,
  module_order serial
);

create table all_module_objects (
  object_name text,
  object_type text,
  module_name text
);
~~~~

initial syntax is new_module - which adds the module and then adds
everything following to that module until a new new_module is
hit. Just use . separated names, no explicit heirarchy for now.

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

> {-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Examples.Extensions.Modules
>     (modules
>     ,modulesExample) where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.SqlQuote
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
>
> modulesExample :: ExtensionTest
> modulesExample =
>   ExtensionTest
>     "modules"
>     modules
>     [$sqlStmts| select module('Chaos.Server.Metadata'); |]
>     []
>
> modules :: Data a => a -> a
> modules =
>     transformBi $ \x ->
>       case x of
>         [$sqlStmt| select module($s(modname)); |] : tl
>             -> tl
>         x1 -> x1
>
