

> module Database.HsSqlPpp.Tests.TypeChecking.Utils where

> import Database.HsSqlPpp.Types

> mkTypeExtra :: Type -> TypeExtra
> mkTypeExtra t = TypeExtra t Nothing Nothing True

> mkTypeExtraNN :: Type -> TypeExtra
> mkTypeExtraNN t = TypeExtra t Nothing Nothing False
