Copyright 2009 Jake Wheat

This is the public api to the environment data type, it just
forwards the public part of EnvironmentInternal, which is the
module used by the type checking code.

> {- | This module contains the environment data types and a helper functions.
>    You almost certainly will never have to do anything with Environments apart
>    from read them from a database to supply to the annotation function. If you
>    only need to type check against the default template1 catalog, then you
>    don't even need to do this - is you use the annotation function with
>    no Environment parameter, it uses the default template1 catalog.
>
>    The environment data type serves the following purposes:
>
>  * Contains all the catalog information needed to type check against
>     an existing database.
>
>  * A copy of the catalog information from a default template1
>    database is included - 'defaultTemplate1Environment', at some
>    point this might be used to allow typechecking sql code against
>    this catalog without having an available PostGreSQL install.
>
>  * It is used internally to keep track of updates to the catalog
>     whilst running an annotation process (e.g. so that a select can
>     type check against a create table given in the same source). It
>     is also used to track other identifier types, such as attribute
>     references in select expressions, and argument and variable
>     types inside create function statements.
>
>  You can see what kind of stuff is contained in the Environment type
>  by looking at the 'EnvironmentUpdate' type.
> -}

> module Database.HsSqlPpp.TypeChecking.Environment
>     (
>      -- * Data types
>      Environment
>      -- ** Updates
>     ,EnvironmentUpdate(..)
>      -- ** bits and pieces
>     ,QualifiedIDs
>     ,CastContext(..)
>     ,CompositeFlavour(..)
>     ,CompositeDef
>     ,FunctionPrototype
>     ,DomainDefinition
>     ,FunFlav(..)
>      -- * 'Environment' values
>     ,emptyEnvironment
>     ,defaultEnvironment
>     ,defaultTemplate1Environment
>      -- * Functions
>     ,readEnvironmentFromDatabase
>     ,updateEnvironment
>     --,destructEnvironment
>      -- * operator utils
>     ,OperatorType(..)
>     ,getOperatorType
>     ,isOperatorName
>     ) where

> import Database.HsSqlPpp.TypeChecking.EnvironmentInternal
> import Database.HsSqlPpp.TypeChecking.EnvironmentReader
> import Database.HsSqlPpp.TypeChecking.DefaultTemplate1Environment
