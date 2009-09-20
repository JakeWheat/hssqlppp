Copyright 2009 Jake Wheat

This file contains some generic utility stuff

> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.Utils where

> import Data.Maybe
> import Data.List
> import Data.Either
> import Control.Arrow
> import Control.Monad.Error

> errorWhen :: (Error a) =>
>            Bool -> a -> Either a ()
> errorWhen cond e = do
>     when (cond) $ Left e

> liftME :: a -> Maybe b -> Either a b
> liftME d m = case m of
>                Nothing -> Left d
>                Just b -> Right b

> both :: (a->b) -> (a,a) -> (b,b)
> both fn = (fn *** fn)


> replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
> replace _ _ [] = []
> replace old new xs@(y:ys) =
>   case stripPrefix old xs of
>     Nothing -> y : replace old new ys
>     Just ys' -> new ++ replace old new ys'

