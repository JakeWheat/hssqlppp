
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

> data IDEnv =

table name, public ids, private ids aliases already folded in
used for tref and subtref

>                TrefEnv String [String] [String]


a funtref which returns either a scalar or a composite, with/out
setof aliases already folded in, the bool says whether the function
returns a composite or not. For composites g() returning g(x) - select
g from g() gives select g from g(), for non composites, it gives
select g.g from g().

>              | FunTrefEnv String String
>              | CompFunTrefEnv String [String]

first element is the alias, used for joins

>              | JoinTrefEnv [String] -- join ids for natural/using equijoins
>                   (Maybe (String, Maybe [String])) -- alias for the whole join
>                   IDEnv IDEnv
>              | EmptyIDEnv String
>              | TableAliasEnv String IDEnv
>              | FullAliasEnv String [String] IDEnv
>                deriving Show

> emptyIDEnv :: String -> IDEnv
> emptyIDEnv = EmptyIDEnv

> qualifyID :: IDEnv -> String -> Maybe (String,String)
> qualifyID (TrefEnv s pus pvs) i = if i `elem` pus || i `elem` pvs
>                                   then Just (s,i)
>                                   else Nothing
> qualifyID (FunTrefEnv f c) i = if c == i
>                                then Just (f,i)
>                                else Nothing
> qualifyID (CompFunTrefEnv f cs) i = case () of
>                                       _ | i == f -> Nothing
>                                         | i `elem` cs -> Just (f,i)
>                                         | otherwise -> Nothing
> qualifyID (JoinTrefEnv js Nothing t0 t1) i =
>   case (qualifyID t0 i,qualifyID t1 i) of
>     (Just q, _) | i `elem` js -> Just q
>     (Just q, Nothing) -> Just q
>     (Nothing, Just q) -> Just q
>     _ -> Nothing
> qualifyID (JoinTrefEnv js (Just (t,Nothing)) t0 t1) i =
>   case (qualifyID t0 i,qualifyID t1 i) of
>     (Just (_q,i'), _) | i `elem` js -> Just (t,i')
>     (Just (_q,i'), Nothing) -> Just (t,i')
>     (Nothing, Just (_q,i')) -> Just (t,i')
>     _ -> Nothing
> qualifyID (JoinTrefEnv _ (Just (t,Just cs)) _ _) i =
>   if i `elem` cs then Just  (t,i) else Nothing

> qualifyID (EmptyIDEnv _) _ = Nothing -- error $ "qualify: " ++ show x ++ " " ++ show y

private ids. When can a private column be referenced without a
qualifying name?
a straight tref env
in a join when the private column is joined on? FIXME
in an aliased trefenv

> -- special cases for aliased private ids

> qualifyID (TableAliasEnv t (TrefEnv _ _ pvs)) i | i `elem` pvs = Just (t,i)
> qualifyID (FullAliasEnv t _ (TrefEnv _ _ pvs)) i | i `elem` pvs = Just (t,i)

special case for aliased funtrefs

> qualifyID (TableAliasEnv t (FunTrefEnv _ _)) i =
>   if t == i
>   then Just (t,t)
>   else Nothing


> qualifyID (FullAliasEnv t cs _) i | i `elem` cs = Just (t,i)
>                                   | otherwise = Nothing

> qualifyID (TableAliasEnv t ids) i =
>   fmap (\x -> (t,snd x)) $ qualifyID ids i

> --showit :: Show a => String -> a -> a
> --showit a t = trace (a ++ show t ++ "\n\n") t

> expandStar :: IDEnv -> (Maybe String) -> Maybe [(String,String)]
> expandStar i s = {-showit ("expandStar: " ++ show i ++ "\n" ++ show s ++ "\n\n")
>                  $ showit "esresult: " $ -} expandStar' i s

> expandStar' :: IDEnv -> (Maybe String) -> Maybe [(String,String)]
> expandStar' (TrefEnv s pus _) Nothing = Just $ zip (repeat s) pus
> expandStar' (TrefEnv s pus _) (Just s1) | s == s1 = Just $ zip (repeat s) pus
>                                         | otherwise = Nothing
> expandStar' (CompFunTrefEnv f _cs) Nothing = Just [(f,f)]
> expandStar' (CompFunTrefEnv f cs) (Just f1) | f == f1 = Just $ zip (repeat f) cs
> expandStar' (FunTrefEnv f c) Nothing = Just [(f,c)]
> expandStar' (FunTrefEnv f c) (Just f1) | f == f1 = Just [(f,c)]
>                                        | otherwise = Nothing

special case for a table alias on a non table valued funtref

> expandStar' (TableAliasEnv t (FunTrefEnv _ _)) Nothing = Just [(t,t)]
> expandStar' (TableAliasEnv t (FunTrefEnv _ _)) (Just f1)
>     | t == f1 = Just [(t,t)]
>     | otherwise = Nothing



> expandStar' (JoinTrefEnv js Nothing t0 t1) Nothing =
>   let isJ = (`elem` js) . snd
>       (jis,t0is) = partition isJ $ maybe [] id (expandStar t0 Nothing)
>              -- remove join columns from second list
>       t1is = filter (not . isJ) $ maybe [] id (expandStar t1 Nothing)
>   in case jis ++ t0is ++ t1is of -- check for duplicates?
>     [] -> Nothing
>     x -> Just x

expand actual alias, assume that there is no overlap so don't have to eliminate js

> expandStar' (JoinTrefEnv _js Nothing t0 t1) i =
>   let t0is = maybe [] id (expandStar t0 i)
>       t1is = maybe [] id (expandStar t1 i)
>   in case t0is ++ t1is of -- check for duplicates?
>     [] -> Nothing
>     x -> Just x


> expandStar' (JoinTrefEnv js (Just (t,Nothing)) t0 t1) i =
>   let isJ = (`elem` js) . snd
>       (jis,t0is) = partition isJ $ maybe [] id (expandStar t0 i)
>              -- remove join columns from second list
>       t1is = filter (not . isJ) $ maybe [] id (expandStar t1 i)
>   in case map (\j -> (t,snd j)) $ jis ++ t0is ++ t1is of -- check for duplicates?
>     [] -> Nothing
>     x -> Just x

> expandStar' (JoinTrefEnv _ (Just (t,Just cs)) _ _) i
>   | maybe True (==t) i = Just $ map (t,) cs
>   | otherwise = Nothing


> expandStar' (TableAliasEnv t ids) i
>     | maybe True (==t) i = fmap (map (\x -> (t,snd x))) $ expandStar ids Nothing
>     | otherwise = Nothing
> expandStar' (FullAliasEnv t cs _) i
>     | maybe True (==t) i = Just $ map (t,) cs
>     | otherwise = Nothing

> expandStar' (EmptyIDEnv _) _ = Nothing



> {-
> apply alias:
> cases:
> 1) just a qualifier -> override the qualifier for all the ids
> 2) a qualifier with too many column names: error
> 3) a qualifier with not enough column names: rename the first n columns
>    keep the others the same. The private columns keep their names but take the new qualifier
> FIXME: add tests for not enough and for private in this case
> 4) correct number of cols, as shown
> -}
> {-applyAlias :: TableAlias -> IDEnv -> (IDEnv,TableAlias)
> applyAlias (NoAlias a) (TrefEnv tn pus pvs) =
>   (TrefEnv tn pus pvs,FullAlias a tn pus)
> applyAlias (TableAlias a t) (TrefEnv _tn pus pvs) =
>   (TrefEnv t pus pvs,FullAlias a t pus)
> applyAlias (FullAlias a t cs) (TrefEnv _tn _pus pvs) =
>   (TrefEnv t cs pvs,FullAlias a t cs)
> applyAlias (NoAlias a) (FunTrefEnv tn fn) =
>   (FunTrefEnv tn fn,FullAlias a tn [fn])
> applyAlias (TableAlias a t) (FunTrefEnv _ fn) =
>   (FunTrefEnv t fn,FullAlias a t [fn])
> applyAlias (FullAlias a tn [cn]) (FunTrefEnv _tn _fn) =
>   (FunTrefEnv tn cn,FullAlias a tn [cn])

> applyAlias a x@(EmptyIDEnv _) = (x,a)


> applyAlias (NoAlias a) (JoinTrefEnv tn t0 t1) =
>   (JoinTrefEnv tn t0 t1, NoAlias a)
> applyAlias (TableAlias a t) (JoinTrefEnv _tn t0 t1) =
>   (JoinTrefEnv t pus pvs,FullAlias a t pus)
> applyAlias (FullAlias a t cs) (TrefEnv _tn _pus pvs) =
>   (TrefEnv t cs pvs,FullAlias a t cs)-}

> {-

> get alias: want to return the fullest alias possible at each stage
> if all the columns have the same qualifier, then this is a full alias
> if they don't, then has to be no alias

> -}
