Copyright 2010 Jake Wheat

This module contains the code to manage local identifier bindings
during the type checking process.

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
> import Debug.Trace
> import Data.Char

> import Database.HsSqlPpp.AstInternals.TypeType
> import Database.HsSqlPpp.Utils
> import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal

> data LocalBindings = LocalBindings

> emptyBindings :: LocalBindings
> emptyBindings = undefined

> data LocalBindingsUpdate = LBQualifiedIds {
>                              source :: String
>                             ,correlationName :: String
>                             ,ids :: [(String,Type)]
>                             ,internalIds :: [(String,Type)]
>                             }
>                          | LBUnqualifiedIds {
>                              source :: String
>                             ,ids :: [(String,Type)]
>                             ,internalIds :: [(String,Type)]
>                             }
>                          | LBJoinIds {
>                              source1 :: String
>                             ,correlationName1 :: String
>                             ,ids1 :: [(String,Type)]
>                             ,internalIds1 :: [(String,Type)]
>                             ,source2 :: String
>                             ,correlationName2 :: String
>                             ,ids2 :: [(String,Type)]
>                             ,internalIds2 :: [(String,Type)]
>                             ,joinIds :: [String]
>                             }

> lbUpdate :: LocalBindings -> LocalBindingsUpdate -> LocalBindings
> lbUpdate = undefined

> lbExpandStar :: LocalBindings
>              -> String -- correlation name
>              -> Either [TypeError] [(String,(String,Type))] -- either error or [source,(name,type)]
> lbExpandStar = undefined

> lbLookupID :: LocalBindings
>            -> String -- correlation name
>            -> String -- identifier name
>            -> Either [TypeError] (String,Type) -- type error or source, type
> lbLookupID = undefined
