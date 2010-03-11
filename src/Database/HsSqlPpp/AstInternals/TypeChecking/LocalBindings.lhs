Copyright 2010 Jake Wheat

Forwarder for the interface to LocalBindingsInternal used by the rest
of the system.

> module Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindings
>     (
>      LocalBindingsUpdate(..)
>     ,LocalBindings
>     ,emptyBindings
>     ,lbUpdate
>     ,lbExpandStar
>     ,lbLookupID
>     ,lbUpdateDot
>     ,ppLocalBindings
>     ) where
>
> import Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindingsInternal
