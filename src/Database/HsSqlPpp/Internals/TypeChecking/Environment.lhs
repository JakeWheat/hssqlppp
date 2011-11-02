

This module represents part of the bound names environment used in the
type checker. It doesn't cover the stuff that is contained in the
catalog (so it is slightly misnamed), but focuses only on identifiers
introduced by things like tablerefs, sub selects, plpgsql parameters
and variables, etc.

> {-# LANGUAGE DeriveDataTypeable #-}
> module Database.HsSqlPpp.Internals.TypeChecking.Environment
>     (-- * abstract environment value
>      Environment
>      -- * environment create and update functions
>     ,emptyEnvironment
>     ,envCreateTrefEnvironment
>      -- * environment query functions
>     ,envLookupIdentifier
>     ,envExpandStar
>     ) where

> import Data.Data
> import Data.Char

> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal

---------------------------------

> -- | Represent an environment using an abstracted version of the syntax
> -- which produced the environment. This structure has all the catalog
> -- queries resolved. No attempt is made to combine environment parts from
> -- different sources, they are just stacked together, the logic for
> -- working with combined environments is in the query functions below
> data Environment =
>                  -- | represents an empty environment, makes e.g. joining
>                  -- the environments for a list of trefs in a select list
>                  -- more straightforward
>                    EmptyEnvironment
>                  -- | represents the bindings introduced by a tableref:
>                  -- the name, the public fields, the private fields
>                  | SimpleTref String [(String,Type)] [(String,Type)]

>                    deriving (Data,Typeable,Show,Eq)



---------------------------------------------------

Create/ update functions, these are shortcuts to create environment variables,
the main purpose is to encapsulate looking up information in the
catalog and combining environment values with updates

> emptyEnvironment :: Environment
> emptyEnvironment = EmptyEnvironment

> envCreateTrefEnvironment :: Catalog -> [NameComponent] -> Either [TypeError] Environment
> envCreateTrefEnvironment cat tbnm = do
>   (nm,pub,prv) <- catLookupTableAndAttrs cat tbnm
>   return $ SimpleTref nm pub prv


-------------------------------------------------------


The main hard work is done in the query functions: so the idea is that
the update functions create environment values which contain the
context free contributions of each part of the ast to the current
environment, and these query functions do all the work of resolving
implicit correlation names, ambigous identifiers, etc.

> nnm :: [NameComponent] -> String
> nnm [] = error "Env: empty name component"
> nnm ns = case last ns of
>            Nmc n -> map toLower n
>            QNmc n -> n

> envLookupIdentifier :: [NameComponent] -> Environment -> Either [TypeError] Type
> envLookupIdentifier nmc (SimpleTref nm pub prv) =
>   let n = nnm nmc
>   in case lookup n pub of
>        Just t -> return t
>        Nothing -> Left [UnrecognisedIdentifier n]

> envExpandStar :: Maybe NameComponent -> Environment -> Either [TypeError] [(String,Type)]
> envExpandStar nmc (SimpleTref nm pub prv)
>   | case nmc of
>              Nothing -> True
>              Just x -> nnm [x] == nm = Right pub
>   | otherwise = case nmc of
>                    Nothing -> Left [BadStarExpand]
>                    Just n -> Left [UnrecognisedCorrelationName $ nnm [n]]
