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
> import Data.List
> --import Debug.Trace
> import Data.Char

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

> lbUpdate :: LocalBindingsUpdate -> LocalBindings -> LocalBindings
> lbUpdate lbu (LocalBindings lb) = LocalBindings (lbu : lb)

> lbExpandStar :: LocalBindings
>              -> String -- correlation name
>              -> Either [TypeError] [(String,String,String,Type)] -- either error or [source,(corr,name,type)]
> lbExpandStar = undefined

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
> findID cor i (LBJoinIds _ _ _ _ _ _ _ _ _)  = undefined

