
This file contains some generic utility stuff

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE FlexibleContexts #-}
> module Database.HsSqlPpp.Internals.Utils where
>
> import Control.Arrow
> import Control.Applicative
> import Data.List
> import Data.Char

> infixl 9 |>
> (|>) :: (a -> b) -> (b -> c) -> a -> c
> (|>) = flip (.)
>
> both :: (a->b) -> (a,a) -> (b,b)
> both fn = fn *** fn
>
> (<:>) :: (Applicative f) =>
>          f a -> f [a] -> f [a]
> (<:>) a b = (:) <$> a <*> b

> firstM :: Functor f => (t -> f a) -> (t, t1) -> f (a, t1)
> firstM f (a,b) = (,b) <$> f a

> secondM :: Functor f => (t -> f a) -> (t1, t) -> f (t1, a)
> secondM f (a,b) = (a,) <$> f b

> replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
> replace _ _ [] = []
> replace old new xs@(y:ys) =
>   case stripPrefix old xs of
>     Nothing -> y : replace old new ys
>     Just ys' -> new ++ replace old new ys'
>
> split :: Char -> String -> [String]
> split _ ""                =  []
> split c s                 =  let (l, s') = break (== c) s
>                            in  l : case s' of
>                                            [] -> []
>                                            (_:s'') -> split c s''
>
> npartition :: Eq b => (a -> b) -> [a] -> [(b,[a])]
> npartition keyf =
>   np []
>   where
>     np = foldl (\acc p -> insertWith (++) (keyf p) [p] acc)
>
> insertWith :: Eq k => (a -> a -> a) -> k -> a -> [(k,a)] -> [(k,a)]
> insertWith ac k v m =
>     case lookup k m of
>       Nothing -> m ++ [(k,v)]
>       Just v' -> let nv = ac v' v
>                  in map (\p@(k1,_) -> if k1 == k
>                                       then (k1,nv)
>                                       else p) m

This should preserve order, so in the result, the keys (k in
[(k,[a],[b])]) are ordered by their first appearance in as, then bs,
and the values are ordered the matches in the same order as they
appear in the two lists ([a] and [b] in [(k,[a],[b])])

> joinLists :: Eq k => (a -> k) -> (b -> k)
>              -> [a] -> [b] -> [(k,[a],[b])]
> joinLists ka kb as bs =
>     let -- arrange the two lists by key
>         kasps = npartition ka as
>         kbsps = npartition kb bs
>         -- get the list of keys
>         ks = nub $ map fst kasps ++ map fst kbsps
>         -- put together the two lists by key
>     in flip map ks $ \k ->
>         (k, getem k kasps, getem k kbsps)
>     where
>       getem :: Eq k => k -> [(k,[a])] -> [a]
>       getem k = concatMap snd . filter ((==k) . fst)

> trim :: String -> String
> trim = f . f
>    where f = reverse . dropWhile isSpace
