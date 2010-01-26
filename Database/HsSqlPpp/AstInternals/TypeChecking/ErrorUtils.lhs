Copyright 2009 Jake Wheat

This file contains a bunch of small low level utilities to help with
type checking.

> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.AstInternals.TypeChecking.ErrorUtils
>     (
>      dependsOn
>     ,dependsOnRTpe
>     ,dependsOnT
>     ,tpeToT
>     ,liftErrors
>     ,getErrors
>     ) where

> import Database.HsSqlPpp.AstInternals.TypeType

================================================================================

= type checking utils

== checkErrors

if we find a typecheckfailed in the list, then propagate that, else
use the final argument.

> dependsOn :: [Type] -> t -> t -> t
> dependsOn ts bad ok =
>   if any (==TypeCheckFailed) ts
>     then bad
>     else ok

> dependsOnRTpe :: [Type] -> Either a Type -> Either a Type
> dependsOnRTpe ts = dependsOn ts (Right TypeCheckFailed)

> dependsOnT :: [Type] -> Type -> Type
> dependsOnT ts = dependsOn ts TypeCheckFailed

convert an 'either [typeerror] type' to a type

> tpeToT :: Either [TypeError] Type -> Type
> tpeToT tpe = case tpe of
>                   Left _ -> TypeCheckFailed
>                   Right t -> t

> liftErrors :: [TypeError] -> Either [TypeError] ()
> liftErrors es = if null es
>                   then Right ()
>                   else Left es

extract errors from an either, gives empty list if right

> getErrors :: Either [TypeError] Type -> [TypeError]
> getErrors = either id (const [])

