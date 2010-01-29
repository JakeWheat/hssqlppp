Copyright 2010 Jake Wheat

This module contains the code to manage local identifier bindings
during the type checking process. This is used for e.g. looking up the
types of parameter and variable references in plpgsql functions, and
for looking up the types of identifiers in select expressions.

Some notes on lookups
all lookups are case insensitive - todo: change correlation names and
ids to lower case in the lbupdate function
start by searching the head of the lookup update list and working down
the code here handles resolving the types of join columns when they
are not the same, and the update routine returns error if the join columns are not compatible
the code here handles expanding record types so that the components can be looked up




> {-# OPTIONS_HADDOCK hide  #-}

> module Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindings
>     (
>      LocalBindingsUpdate(..)
>     ,LocalBindings
>     ,emptyBindings
>     ,lbUpdate
>     ,lbExpandStar
>     ,lbLookupID
>     ) where

> import Control.Monad
> --import Debug.Trace
> --import Data.List
> import Data.Maybe

> import Database.HsSqlPpp.AstInternals.TypeType
> --import Database.HsSqlPpp.Utils
> --import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal

> data LocalBindings = LocalBindings [LocalBindingsUpdate]

> emptyBindings :: LocalBindings
> emptyBindings = LocalBindings []

> data LocalBindingsUpdate = LBQualifiedIds {
>                              source :: String
>                             ,correlationName :: String
>                             ,lbids :: [(String,Type)]
>                             ,internalIds :: [(String,Type)]
>                             }
>                          | LBUnqualifiedIds {
>                              source :: String
>                             ,lbids :: [(String,Type)]
>                             ,internalIds :: [(String,Type)]
>                             }
>                          | LBJoinIds {
>                              source1 :: String
>                             ,correlationName1 :: String
>                             ,lbids1 :: [(String,Type)]
>                             ,internalIds1 :: [(String,Type)]
>                             ,source2 :: String
>                             ,correlationName2 :: String
>                             ,lbids2 :: [(String,Type)]
>                             ,internalIds2 :: [(String,Type)]
>                             ,joinIds :: [String]
>                             }
>                            deriving Show

> lbUpdate :: LocalBindingsUpdate -> LocalBindings -> LocalBindings
> lbUpdate lbu (LocalBindings lb) = LocalBindings (lbu : lb)

> lbExpandStar :: LocalBindings
>              -> String -- correlation name
>              -> Either [TypeError] [(String,String,String,Type)] -- either error or [source,(corr,name,type)]
> lbExpandStar (LocalBindings l) cor =
>   es l
>   where
>     es :: [LocalBindingsUpdate] -> Either [TypeError] [(String,String,String,Type)]
>     es (LBQualifiedIds src cor1 ids _ :lbus) = if cor == cor1 || cor == ""
>                                                then mapEm src cor1 ids
>                                                else es lbus
>     es (LBUnqualifiedIds src ids _ : lbus) = if cor == ""
>                                              then mapEm src "" ids
>                                              else es lbus
>     es (u@(LBJoinIds _ c1 _ _ _ c2 _ _ _) : lbus) = if cor `elem` ["",c1,c2]
>                                                  then Right $ fromJust $ lookup cor $ getStarIds u
>                                                  else es lbus
>     es [] = Left [UnrecognisedCorrelationName cor]
>     mapEm :: String -> String -> [(String,Type)] -> Either [TypeError] [(String,String,String,Type)]
>     mapEm src c = Right . map (\(a,b) -> (src,c,a,b))

> lbLookupID :: LocalBindings
>            -> String -- correlation name
>            -> String -- identifier name
>            -> Either [TypeError] (String,String,String,Type) -- type error or source, corr, type
> lbLookupID (LocalBindings lb) cor i =
>   lk lb
>   where
>     lk (lbu:lbus) = case findID cor i lbu of
>                                           Nothing -> lk lbus
>                                           Just t -> t
>     lk [] = Left [UnrecognisedIdentifier (if cor == "" then i else cor ++ "." ++ i)]

> findID :: String
>        -> String
>        -> LocalBindingsUpdate
>        -> Maybe (Either [TypeError] (String,String,String,Type))
> findID cor i (LBQualifiedIds src cor1 ids intIds) =
>     if cor `elem` ["", cor1]
>     then case (msum [lookup i ids
>                     ,lookup i intIds]) of
>            Just ty -> Just $ Right (src,cor1,i,ty)
>            Nothing -> if cor == ""
>                       then Nothing
>                       else Just $ Left [UnrecognisedIdentifier (if cor == "" then i else cor ++ "." ++ i)]
>     else Nothing

> findID cor i (LBUnqualifiedIds src ids intIds) =
>   if cor == ""
>   then flip fmap (msum [lookup i ids
>                        ,lookup i intIds])
>          $ \ty -> Right (src,"",i,ty)
>   else Nothing
> findID cor i u@(LBJoinIds _ _ _ _ _ _ _ _ _) =
>     lookup (cor,i) $ getJoinIdMap u

expand out the ids for a join

so we have a full list to lookup single ids whether qualified or not,
and return possibly a ambiguous id error

> getJoinIdMap :: LocalBindingsUpdate
>              -> [((String,String), Either [TypeError] (String,String,String,Type))]
> getJoinIdMap (LBJoinIds s1 c1 ids1 iids1 s2 c2 ids2 iids2 jids) =
>     concat [
>       -- all non join table 1 ids unqualified
>       map (\(i,t) -> (("", i), Right (s1,c1,i,t))) $ notJids ids1 ++ iids1
>       -- all non join table 2 ids unqualified
>      ,map (\(i,t) -> (("", i), Right (s2,c2,i,t))) $ notJids ids2 ++ iids2
>       -- all join ids unqualified, linked to source 1
>      ,map (\(i,t) -> (("", i), Right (s1,c1,i,t))) jidts
>       -- all non join table 1 ids qualified
>      ,map (\(i,t) -> ((c1, i), Right (s1,c1,i,t))) $ notJids ids1 ++ iids1
>       -- all non join table 2 ids qualified
>      ,map (\(i,t) -> ((c2, i), Right (s2,c2,i,t))) $ notJids ids2 ++ iids2
>       -- all join ids qualified with c1
>      ,map (\(i,t) -> ((c1, i), Right (s1,c1,i,t))) jidts
>       -- all join ids qualified with c2
>      ,map (\(i,t) -> ((c2, i), Right (s2,c2,i,t))) jidts
>     ]
>   where
>     notJids = filter (\(n,_) -> n `notElem` jids)
>     jidts = map (\n -> (n, fromJust $ lookup n ids1)) jids

> getJoinIdMap x = error $ "internal error: getJoinIdMap called on " ++ show x

> getStarIds :: LocalBindingsUpdate
>              -> [(String, [(String,String,String,Type)])]
> getStarIds (LBJoinIds s1 c1 ids1 iids1 s2 c2 ids2 iids2 jids) =
>     -- uncorrelated
>     [("", concat [
>             -- all join ids qualified with c1
>             map (\(i,t) -> (s1,c1,i,t)) jidts
>             -- all non join table 1 ids unqualified
>            ,map (\(i,t) -> (s1,c1,i,t)) $ notJids ids1
>             -- all non join table 2 ids unqualified
>            ,map (\(i,t) -> (s2,c2,i,t)) $ notJids ids2
>             ])
>      -- c1
>     ,(c1, concat [
>             -- all join ids qualified with c1
>             map (\(i,t) -> (s1,c1,i,t)) jidts
>             -- all non join table 1 ids unqualified
>            ,map (\(i,t) -> (s1,c1,i,t)) $ notJids ids1
>               ])
>     -- c2
>     ,(c2, concat [
>             -- all join ids qualified with c2
>             map (\(i,t) -> (s2,c2,i,t)) jidts
>             -- all non join table 2 ids unqualified
>            ,map (\(i,t) -> (s2,c2,i,t)) $ notJids ids2
>     ])]


>      {- 
>       -- all non join table 1 ids unqualified
>       map (\(i,t) -> (("", i), Right (s1,c1,i,t))) $ notJids ids1 ++ iids1
>       -- all non join table 2 ids unqualified
>      ,map (\(i,t) -> (("", i), Right (s2,c2,i,t))) $ notJids ids2 ++ iids2
>       -- all join ids unqualified, linked to source 1
>      ,map (\(i,t) -> (("", i), Right (s1,c1,i,t))) jidts
>       -- all non join table 1 ids qualified
>      ,map (\(i,t) -> ((c1, i), Right (s1,c1,i,t))) $ notJids ids1 ++ iids1
>       -- all non join table 2 ids qualified
>      ,map (\(i,t) -> ((c2, i), Right (s2,c2,i,t))) $ notJids ids2 ++ iids2
>       -- all join ids qualified with c1
>      ,map (\(i,t) -> ((c1, i), Right (s1,c1,i,t))) jidts
>       -- all join ids qualified with c2
>      ,map (\(i,t) -> ((c2, i), Right (s2,c2,i,t))) jidts
>     ]-}
>   where
>     notJids = filter (\(n,_) -> n `notElem` jids)
>     jidts = map (\n -> (n, fromJust $ lookup n ids1)) jids

> getStarIds x = error $ "internal error: getJoinIdMap called on " ++ show x
