
This is the public module for the type checking functionality.

> {- | Contains functions for typechecking sql asts.
> -}
> module Database.HsSqlPpp.TypeCheck
>     (
>      -- * typechecking/ annotation functions
>      typeCheckStatements
>     ,typeCheckQueryExpr
>     ,typeCheckScalarExpr
>     ,TypeCheckFlags(..)
>     ,Dialect(..)
>     ,ansiDialect
>     ,defaultTypeCheckFlags
>     ,emptyEnvironment
>     ) where
>
> import Database.HsSqlPpp.Internals.AstInternal
> import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Dialects.Ansi
> import Database.HsSqlPpp.Internals.TypeChecking.Environment (emptyEnvironment)
> --import Database.HsSqlPpp.Internals.TypeChecking.Utils
> --import Database.HsSqlPpp.Internals.AstAnnotation

> -- | reasonable defaults for type checking, doesn't add anything
> -- optional
> defaultTypeCheckFlags :: TypeCheckFlags
> defaultTypeCheckFlags =
>     TypeCheckFlags
>     {tcfAddQualifiers = False
>     ,tcfAddFullTablerefAliases = False
>     ,tcfAddSelectItemAliases = False
>     ,tcfExpandStars = False
>     ,tcfDialect = ansiDialect}
