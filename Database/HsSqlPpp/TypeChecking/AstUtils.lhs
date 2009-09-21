Copyright 2009 Jake Wheat

This file contains a bunch of small low level utilities to help with
type checking.

> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.TypeChecking.AstUtils
>     (
>      checkTypes
>     ,chainTypeCheckFailed
>     ,errorToTypeFail
>     ,errorToTypeFailF
>     ,checkErrorList
>     ,getErrors
>     ) where

> import Data.List

> import Database.HsSqlPpp.TypeChecking.TypeType
> import Database.HsSqlPpp.TypeChecking.EnvironmentInternal

================================================================================

= type checking utils

== checkErrors

if we find a typecheckfailed in the list, then propagate that, else
use the final argument.

> checkTypes :: [Type] -> Either [TypeError] Type -> Either [TypeError] Type
> checkTypes (TypeCheckFailed:_) _ = Right TypeCheckFailed
> checkTypes (_:ts) r = checkTypes ts r
> checkTypes [] r = r

small variant, not sure if both are needed

> chainTypeCheckFailed :: [Type] -> Either a Type -> Either a Type
> chainTypeCheckFailed a b =
>   if any (==TypeCheckFailed) a
>     then Right TypeCheckFailed
>     else b

convert an 'either [typeerror] type' to a type

> errorToTypeFail :: Either [TypeError] Type -> Type
> errorToTypeFail tpe = case tpe of
>                         Left _ -> TypeCheckFailed
>                         Right t -> t

convert an 'either [typeerror] x' to a type, using an x->type function

> errorToTypeFailF :: (t -> Type) -> Either [TypeError] t -> Type
> errorToTypeFailF f tpe = case tpe of
>                                   Left _ -> TypeCheckFailed
>                                   Right t -> f t

used to pass a regular type on iff the list of errors is null

> checkErrorList :: [TypeError] -> Type -> Either [TypeError] Type
> checkErrorList es t = if null es
>                         then Right t
>                         else Left es

extract errors from an either, gives empty list if right

> getErrors :: Either [TypeError] Type -> [TypeError]
> getErrors e = either id (const []) e

