
code to resolve the qualifier of an identifier and expand stars and
some helpers to get ids for a table, and help with aliases

this should eventually expand to take over the local identifier
bindings code

> module Database.HsSqlPpp.Internals.TypeChecking.IDEnv
>     (IDEnv(..)
>     ,emptyIDEnv
>     ,qualifyID
>     ,expandStar) where

> import Data.List
> --import Debug.Trace
> import Control.Monad
> import Data.Maybe

> data IDEnv =

table name, public ids, private ids aliases already folded in
used for tref and subtref

>                TrefIDEnv String [String] [String]

>              | FunTrefIDEnv String
>              | CompFunTrefIDEnv String [String]

first element is the alias, used for joins

>              | JoinTrefIDEnv [String] -- join ids for natural/using equijoins
>                   IDEnv IDEnv
>              | EmptyIDEnv String
>              | TableAliasIDEnv String IDEnv
>              | FullAliasIDEnv String [String] IDEnv
>              | CorrelatedIDEnv IDEnv IDEnv -- normal env, extra correlated env
>                deriving Show

> emptyIDEnv :: String -> IDEnv
> emptyIDEnv = EmptyIDEnv

> qualifyID :: IDEnv -> String -> Maybe (String,String)
> qualifyID idenv s =
>   {-(if s == "c"
>    then showit ("qualifyID " ++ show idenv ++ " " ++ show s ++ ": ")
>    else id) $ -} qualifyID' idenv s
> qualifyID' :: IDEnv -> String -> Maybe (String,String)
> qualifyID' (CorrelatedIDEnv ids cids) i =
>    msum [qualifyID ids i, qualifyID cids i]
> qualifyID' (TrefIDEnv s pus pvs) i = if i `elem` pus || i `elem` pvs
>                                   then Just (s,i)
>                                   else Nothing
> qualifyID' (FunTrefIDEnv f) i = if f == i
>                                 then Just (f,i)
>                                 else Nothing
> qualifyID' (CompFunTrefIDEnv f cs) i = case () of
>                                       _ | i `elem` cs -> Just (f,i)
>                                         | i == f -> Nothing --slighly hacky, this should be left alone
>                                         | otherwise -> Nothing
> qualifyID' (JoinTrefIDEnv js t0 t1) i =
>   case (qualifyID t0 i,qualifyID t1 i) of
>     (Just q, _) | i `elem` js -> Just q
>     (Just q, Nothing) -> Just q
>     (Nothing, Just q) -> Just q
>     _ -> Nothing

> qualifyID' (EmptyIDEnv _) _ = Nothing -- error $ "qualify: " ++ show x ++ " " ++ show y

private ids. When can a private column be referenced without a
qualifying name?
a straight tref env
in a join when the private column is joined on? FIXME
in an aliased trefenv

> -- special cases for aliased private ids

> qualifyID' (TableAliasIDEnv t (TrefIDEnv _ _ pvs)) i | i `elem` pvs = Just (t,i)
> qualifyID' (FullAliasIDEnv t _ (TrefIDEnv _ _ pvs)) i | i `elem` pvs = Just (t,i)

special case for aliased funtrefs

> qualifyID' (TableAliasIDEnv t (FunTrefIDEnv _)) i =
>   if t == i
>   then Just (t,t)
>   else Nothing


> qualifyID' (FullAliasIDEnv t cs _) i | i `elem` cs = Just (t,i)
>                                   | otherwise = Nothing

> qualifyID' (TableAliasIDEnv t ids) i =
>   fmap (\x -> (t,snd x)) $ qualifyID ids i

> --showit :: Show a => String -> a -> a
> --showit a t = trace (a ++ show t ++ "\n\n") t

> expandStar :: IDEnv -> Maybe String -> Maybe [(String,String)]
> expandStar i s = {-showit ("expandStar: " ++ show i ++ "\n" ++ show s ++ "\n\n")
>                  $ showit "esresult: " $ -} expandStar' i s

> expandStar' :: IDEnv -> Maybe String -> Maybe [(String,String)]
> expandStar' (CorrelatedIDEnv ids _) i = expandStar ids i
> expandStar' (TrefIDEnv s pus _) Nothing = Just $ zip (repeat s) pus
> expandStar' (TrefIDEnv s pus _) (Just s1) | s == s1 = Just $ zip (repeat s) pus
>                                         | otherwise = Nothing
> expandStar' (CompFunTrefIDEnv f cs) Nothing = Just $ zip (repeat f) cs
> expandStar' (CompFunTrefIDEnv f cs) (Just f1) | f == f1 = Just $ zip (repeat f) cs
>                                               | otherwise = Nothing
> expandStar' (FunTrefIDEnv f) Nothing = Just [(f,f)]
> expandStar' (FunTrefIDEnv f) (Just f1) | f == f1 = Just [(f,f)]
>                                        | otherwise = Nothing

special case for a table alias on a non table valued funtref

> expandStar' (TableAliasIDEnv t (FunTrefIDEnv _)) Nothing = Just [(t,t)]
> expandStar' (TableAliasIDEnv t (FunTrefIDEnv _)) (Just f1)
>     | t == f1 = Just [(t,t)]
>     | otherwise = Nothing



> expandStar' (JoinTrefIDEnv js t0 t1) Nothing =
>   let isJ = (`elem` js) . snd
>       (jis,t0is) = partition isJ $ fromMaybe [] (expandStar t0 Nothing)
>              -- remove join columns from second list
>       t1is = filter (not . isJ) $ fromMaybe [] (expandStar t1 Nothing)
>   in case jis ++ t0is ++ t1is of -- check for duplicates?
>     [] -> Nothing
>     x -> Just x

expand actual alias, assume that there is no overlap so don't have to eliminate js

> expandStar' (JoinTrefIDEnv _js t0 t1) i =
>   let t0is = fromMaybe [] (expandStar t0 i)
>       t1is = fromMaybe [] (expandStar t1 i)
>   in case t0is ++ t1is of -- check for duplicates?
>     [] -> Nothing
>     x -> Just x

> expandStar' (TableAliasIDEnv t ids) i
>     | maybe True (==t) i = fmap (map (\x -> (t,snd x))) $ expandStar ids Nothing
>     | otherwise = Nothing
> expandStar' (FullAliasIDEnv t cs _) i
>     | maybe True (==t) i = Just $ map (t,) cs
>     | otherwise = Nothing

> expandStar' (EmptyIDEnv _) _ = Nothing
