Copyright 2009 Jake Wheat

This is the public api to the environment data type, it just
forwards the public part of EnvironmentInternal, which is the
module used by the type checking code.

> {- | This module contains the environment data types and a helper functions,
>  It serves the following purposes:
>
>  * contains all the catalog information needed to type check against
>     an existing database
>
>  * a copy of the catalog information from a default template1
>    database is included - 'defaultTemplate1Environment', at some
>    point this might be used to allow typechecking sql code against
>    this catalog without having an available PostGreSQL install.
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
