
This file contains a bunch of small low level utilities to help with
type checking.

> module Database.HsSqlPpp.AstInternals.TypeChecking.ErrorUtils
>     where
>
> import Database.HsSqlPpp.AstInternals.TypeType
> import Debug.Trace

> type E a = Either [TypeError] a
> type Et = E Type
>
> lmt :: Maybe a -> E a
> lmt = maybe (Left []) Right
>
> tes :: E a -> [TypeError]
> tes = either id (const [])
>
> etmt :: E a -> Maybe a
> etmt = either (const Nothing) Just
>

> liftList :: [(a,Maybe b)] -> Maybe [(a,b)]
> liftList is = sequence $ flip map is $ \(a,b) -> case b of
>                                                    Just b' -> Just (a,b')
>                                                    Nothing -> Nothing

> showIt :: Show a => String -> a -> a
> showIt m a = trace (m ++ " " ++ show a) $ a
