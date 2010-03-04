Copyright 2009 Jake Wheat

This file contains a bunch of small low level utilities to help with
type checking.

> module Database.HsSqlPpp.AstInternals.TypeChecking.ErrorUtils
>     where
>
> import Database.HsSqlPpp.AstInternals.TypeType

> type E a = Either [TypeError] a
> type Et = E Type
>
> lmt :: Maybe Type -> Et
> lmt = maybe (Left []) Right
>
> tes :: Et -> [TypeError]
> tes = either id (const [])
>
> etmt :: Et -> Maybe Type
> etmt = either (const Nothing) Just
>
