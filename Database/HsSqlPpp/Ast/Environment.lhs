Copyright 2009 Jake Wheat

This is the public api to the environment data type, it just forwards
the public part of EnvironmentInternal, which is the module used by
the type checking code.

> {- | This module contains the database catalog data types and helper functions.
>
>  The environment data type (which should really be called catalog)
>  serves the following purposes:
>
>  * Contains all the catalog information needed to type check against
>     an existing database.
>
>  * A copy of the catalog information from a default template1
>    database is included - 'defaultTemplate1Environment'.
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
>
> -}

> module Database.HsSqlPpp.Ast.Environment
>     (
>      -- * Data types
>      Environment
>      -- ** Updates
>     ,EnvironmentUpdate(..)
>     ,ppEnvUpdate
>      -- ** bits and pieces
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
>     ,deconstructEnvironment
>      -- * operator utils
>     ,OperatorType(..)
>     ,getOperatorType
>     ,isOperatorName
>     ) where

> import Database.HsSqlPpp.AstInternals.Environment.EnvironmentInternal
> import Database.HsSqlPpp.AstInternals.Environment.EnvironmentReader
> import Database.HsSqlPpp.AstInternals.Environment.DefaultTemplate1Environment
