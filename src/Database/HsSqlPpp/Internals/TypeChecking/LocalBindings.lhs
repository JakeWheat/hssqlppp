
Forwarder for the interface to LocalBindingsInternal used by the rest
of the system.

> module Database.HsSqlPpp.Internals.TypeChecking.LocalBindings
>     (
>      LocalBindingsUpdate(..)
>     ,LocalBindings
>     ,emptyBindings
>     ,lbUpdate
>     ,lbLookupID
>     ,lbLookupIDInType
>     ,ppLocalBindings
>     ,createLocalBindings
>     ,getUnqualifiedBindings
>     ,joinBindings
>     ,lookupLocalBinding
>     ) where

> --import Control.Monad
>
> import Database.HsSqlPpp.Internals.TypeChecking.LocalBindingsInternal
> --import Database.HsSqlPpp.Internals.TypeType
> --import Database.HsSqlPpp.Internals.TypeChecking.ErrorUtils


> {-data LocalEnv =
>         TrefEnv String [(String,Type)] [(String,Type)]
>       | FunTrefEnv String Type
>       | CompFunTrefEnv String [(String, Type)]
>       | JoinTrefEnv [String] LocalEnv LocalEnv
>       | EmptyEnv
>       | TableAliasEnv String LocalEnv
>       | FullAliasEnv String [String] LocalEnv
>       | CorrelatedEnv LocalEnv LocalEnv
>         deriving Show


> lookupID :: LocalEnv -> Maybe String -> String -> E (Maybe Type)
> lookupID (TrefEnv _t _pus _pvs) Nothing _i = Right Nothing
> lookupID (TrefEnv t pus pvs) (Just q) i
>    | t == q =
>      Right $ msum [lookup i pus, lookup i pvs]
>    | otherwise = Right Nothing

> lookupID (FunTrefEnv _f _) Nothing _i = Right Nothing
> lookupID (FunTrefEnv f t) (Just q) i | q == f && i == f = Right $ Just t
>                                      | otherwise = Right Nothing

> lookupID (JoinTrefEnv js e0 e1) Nothing _ = Right Nothing
> lookupID (JoinTrefEnv js e0 e1) (Just q) i
>     -- if this is a join id, just lookup in e0
>    | i `elem` js
>    , t@(Right (Just _)) <- lookupe0 = t
>    -- otherwise require to be in either e0 or e1 but not both
>    | t@(Right (Just _)) <- lookupe0
>    , Nothing <- lookupe1 = t
>    | Nothing <- lookupe0
>    , t@(Right (Just _)) <- lookupe1 = t
>    | Right (Just _) <- lookupe0
>    , Right (Just _) <- lookupe1 = Left $ AmbiguousIdentifier i
>    | Nothing <- lookupe0
>    , Nothing <- lookupe1 = Left $ UnrecognisedIdentifier i
>    where
>      lookupe0 = lookupID e0 (Just q) i
>      lookupe1 = lookupID e1 (Just q) i-}
