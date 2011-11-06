
> module Database.HsSqlPpp.Tests.TypeChecking.ImplicitCasts
>     (impCasts) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> --import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.TypeChecker



> impCasts :: Item
> impCasts =
>   Group "impCasts" [
>     e "'1' + 2" "'1' :: int4 + 2"
>   ]
>   where
>     e = ImpCastsScalar
