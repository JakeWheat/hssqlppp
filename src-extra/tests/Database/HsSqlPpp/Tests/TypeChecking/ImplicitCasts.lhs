
> module Database.HsSqlPpp.Tests.TypeChecking.ImplicitCasts
>     (impCasts) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> --import Database.HsSqlPpp.Types
> --import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker



> impCasts :: Item
> impCasts =
>   Group "impCasts"
>   [e "'1' + 2" "'1' :: int4 + 2"
>   ,e "1.5 :: numeric between 1.1 and 2"
>      "1.5 :: numeric between 1.1 and 2 :: numeric"
>   ,e "'aa'::text = 'bb'"
>      "'aa'::text = 'bb'::text"
>   -- check for bad cast being inserted into wheres
>   ,s "select * from t where 1 = 1;"
>      "select * from t where 1 = 1;"
>   ]
>   where
>     e = ImpCastsScalar
>     s = ImpCastsQuery
