Copyright 2009 Jake Wheat

This file contains some generic utility stuff

> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.Utils where

> import Data.Maybe
> import Data.List
> import Data.Either
> import Control.Arrow
> import Control.Monad.Error
> import Control.Applicative

> errorWhen :: (Error a) => Bool -> a -> Either a ()
> errorWhen cond = when cond . Left

> returnWhen :: (Monad m) => Bool -> a -> m a -> m a
> returnWhen c t t1 = if c then return t else t1

> liftME :: a -> Maybe b -> Either a b
> liftME d m = case m of
>                Nothing -> Left d
>                Just b -> Right b

> both :: (a->b) -> (a,a) -> (b,b)
> both fn = fn *** fn

> (<:>) :: (Applicative f) =>
>          f a -> f [a] -> f [a]
> (<:>) a b = (:) <$> a <*> b

> eitherToMaybe :: Either a b -> Maybe b
> eitherToMaybe (Left _) = Nothing
> eitherToMaybe (Right b) = Just b

> fromRight :: b -> Either a b -> b
> fromRight b (Left _) = b
> fromRight _ (Right r) = r

> fromLeft :: a -> Either a b -> a
> fromLeft _ (Left l) = l
> fromLeft a (Right _) = a

> mapEither :: (a->c) -> (b->d) -> Either a b -> Either c d
> mapEither l _ (Left a) = Left $ l a
> mapEither _ r (Right b) = Right $ r b

> mapRight :: (b -> c) -> Either a b -> Either a c
> mapRight r e = mapEither id r e

> mapLeft :: (a -> c) -> Either a b -> Either c b
> mapLeft l e = mapEither l id e

> isRight :: Either a b -> Bool
> isRight (Right _) = True
> isRight (Left _) = False

> leftToEmpty :: (r -> [a]) -> Either l r -> [a]
> leftToEmpty f e = either (const []) f e

> replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
> replace _ _ [] = []
> replace old new xs@(y:ys) =
>   case stripPrefix old xs of
>     Nothing -> y : replace old new ys
>     Just ys' -> new ++ replace old new ys'


> split :: Char -> String -> [String]
> split _ ""                =  []
> split c s                 =  let (l, s') = break (== c) s
>                            in  l : case s' of
>                                            [] -> []
>                                            (_:s'') -> split c s''

> liftThrows :: (MonadError t m) => Either t a -> m a
> liftThrows (Left err) = throwError err
> liftThrows (Right val) = return val
