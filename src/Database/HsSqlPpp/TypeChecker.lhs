
This is the public module for the type checking functionality.

> {- | Contains functions for typechecking sql asts.
> -}
> module Database.HsSqlPpp.TypeChecker
>     (
>      -- * typechecking/ annotation functions
>      typeCheckStatements
>     ,typeCheckParameterizedStatement
>     ,typeCheckQueryExpr
>     ,typeCheckScalarExpr
>     ,TypeCheckingFlags(..)
>     ,defaultTypeCheckingFlags
>     --,addExplicitCasts
>     ) where
>
> import Database.HsSqlPpp.Internals.AstInternal
> --import Database.HsSqlPpp.Internals.TypeChecking.Utils
> --import Database.HsSqlPpp.Internals.AstAnnotation
