

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
>     ,isEmptyEnv
>     ,envCreateTrefEnvironment
>     ,createJoinTrefEnvironment
>     ,envSelectListEnvironment
>     ,createCorrelatedSubqueryEnvironment
>      -- * environment query functions
>     ,envLookupIdentifier
>     ,envExpandStar
>     ) where

> import Data.Data
> --import Data.Char
> import Data.Maybe
> import Control.Monad
> import Control.Arrow
> import Data.List
> --import Debug.Trace

> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Internals.TypeChecking.TypeConversion
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
>                  | JoinTref [(String,Type)] -- join ids
>                             Environment Environment
>                  | SelectListEnv [(String,Type)]
>                    -- | correlated subquery environment
>                  | CSQEnv Environment -- outerenv
>                           Environment -- main env
>                    deriving (Data,Typeable,Show,Eq)



---------------------------------------------------

Create/ update functions, these are shortcuts to create environment variables,
the main purpose is to encapsulate looking up information in the
catalog and combining environment values with updates

> emptyEnvironment :: Environment
> emptyEnvironment = EmptyEnvironment

> isEmptyEnv :: Environment -> Bool
> isEmptyEnv EmptyEnvironment = True
> isEmptyEnv _ = False

> envCreateTrefEnvironment :: Catalog -> [NameComponent] -> Either [TypeError] Environment
> envCreateTrefEnvironment cat tbnm = do
>   (nm,pub,prv) <- catLookupTableAndAttrs cat tbnm
>   return $ SimpleTref nm pub prv

> envSelectListEnvironment :: [(String,Type)] -> Either [TypeError] Environment
> envSelectListEnvironment cols = do
>   return $ SelectListEnv cols


> -- | create an environment as two envs joined together
> createJoinTrefEnvironment :: Catalog
>                           -> Environment
>                           -> Environment
>                           -> Maybe [NameComponent] -- join ids: empty if cross join
>                                                    -- nothing for natural join
>                           -> Either [TypeError] Environment
> createJoinTrefEnvironment cat tref0 tref1 jsc = do
>   -- todo: handle natural join case
>   (jids::[String]) <- case jsc of
>             Nothing -> do
>                        j0 <- fmap (map (snd . fst)) $ envExpandStar Nothing tref0
>                        j1 <- fmap (map (snd . fst)) $ envExpandStar Nothing tref1
>                        return $ j0 `intersect` j1
>             Just x -> return $ map ncStr x

>  --         maybe (error "natural join ids") (map (nnm . (:[]))) jsc

>   jts <- forM jids $ \i -> do
>            (_,t0) <- envLookupIdentifier [QNmc i] tref0
>            (_,t1) <- envLookupIdentifier [QNmc i] tref1
>            fmap (i,) $ resolveResultSetType cat [t0,t1]
>   -- todo: check type compatibility
>   return $ JoinTref jts tref0 tref1

> createCorrelatedSubqueryEnvironment :: Environment -> Environment -> Environment
> createCorrelatedSubqueryEnvironment cenv env =
>   CSQEnv cenv env



-------------------------------------------------------


The main hard work is done in the query functions: so the idea is that
the update functions create environment values which contain the
context free contributions of each part of the ast to the current
environment, and these query functions do all the work of resolving
implicit correlation names, ambigous identifiers, etc.

> nnm :: [NameComponent] -> String
> nnm [] = error "Env: empty name component"
> nnm ns = ncStr $ last ns

-----------------------------------------------------

problem with these query functions:
all the functions return errors

but they want to recurse, when recursing want to differentiate between
error and no answer

solution: have public wrapper which returns error
and internal recursive functions which can also return Nothing and the
wrapper converts nothing into the appropriate error

> envLookupIdentifier :: [NameComponent] -> Environment
>                     -> Either [TypeError] ((String,String), Type)
> envLookupIdentifier nmc EmptyEnvironment = Left [UnrecognisedIdentifier $ nnm nmc]

> envLookupIdentifier [q,i] t@(SimpleTref nm _ _)
>   | ncStr q == nm = envLookupIdentifier [i] t
>   | otherwise = Left [UnrecognisedCorrelationName $ ncStr q]

> envLookupIdentifier [i] (SimpleTref nm pub prv) =
>       let n = ncStr i
>       in case (lookup n pub,lookup n prv) of
>              (Just _, Just _) -> Left [AmbiguousIdentifier n]
>              (Just t,_) -> return ((nm,n),t)
>              (_,Just t) -> return ((nm,n),t)
>              (Nothing,Nothing) -> Left [UnrecognisedIdentifier n]

> envLookupIdentifier i (SimpleTref {}) =
>   Left [UnrecognisedIdentifier $ show i]

> envLookupIdentifier nmc (SelectListEnv cols) =
>     -- todo: this isn't right
>   let n = nnm nmc
>   in case lookup n cols of
>        Just t -> return (("",n),t)
>        Nothing -> Left [UnrecognisedIdentifier n]

> envLookupIdentifier nmc (CSQEnv cenv env) =
>   case (envLookupIdentifier nmc cenv
>        ,envLookupIdentifier nmc env) of
>      (r@(Right _), _) -> r
>      (_,r@(Right _)) -> r
>      (r,_) -> r


> envLookupIdentifier nmc (JoinTref jids env0 env1) =
>   let n = nnm nmc
>   in case (lookup n jids
>           ,envLookupIdentifier nmc env0
>           ,envLookupIdentifier nmc env1) of
>        -- not sure this is right, errors are ignored when the other
>        -- tref returns something value, hope
>        -- this doesn't hide something
>        (Just _, Right t, _) -> Right t -- by default qualify with the first name
>        (_,Left _, Left _) -> Left [UnrecognisedIdentifier n]
>        (_,Right t, Left _) -> Right t
>        (_,Left _, Right t) -> Right t
>        (_,Right _, Right _) -> Left [AmbiguousIdentifier n]

-------------------------------------------------------

> envExpandStar :: Maybe NameComponent -> Environment -> Either [TypeError] [((String,String),Type)]

> envExpandStar nmc env = {-let r =-} envExpandStar' nmc env
>                         {-in trace ("env expand star: " ++ show nmc ++ " " ++ show r)
>                            r-}

> envExpandStar' :: Maybe NameComponent -> Environment -> Either [TypeError] [((String,String),Type)]

> envExpandStar' _nmc  EmptyEnvironment = Left [BadStarExpand]

> envExpandStar' _nmc (SelectListEnv cols) = Right $ map (first ("",)) cols

> envExpandStar' nmc (CSQEnv _cenv env) = envExpandStar nmc env

> envExpandStar' nmc (SimpleTref nm pub _prv)
>   | {-trace ("expand tref" ++ show nmc ++ " " ++ show nm) $ -}
>     case nmc of
>              Nothing -> True
>              Just x -> nnm [x] == nm = Right $ map (first (nm,)) pub
>   | otherwise = case nmc of
>                    Nothing -> Left [BadStarExpand]
>                    Just n -> Left [UnrecognisedCorrelationName $ nnm [n]]

> envExpandStar' nmc (JoinTref jts env0 env1) = do
>   -- have to get the columns in the right order:
>   -- join columns first (have to get the types of these also - should
>   -- probably do that > -- in createjointrefenv since that is where the
>   -- type compatibility is checked
>   -- then the env0 columns without any join cols
>   -- then the env1 columns without any join cols
>   (s0,s1) <- case (envExpandStar nmc env0, envExpandStar nmc env1) of
>                (Right a, Right b) -> Right (a,b)
>                (Right a, Left _) -> Right (a, [])
>                (Left _, Right a) -> Right ([],a)
>                (Left a, Left _b) -> Left a
>   let (js,t0s) = partition isJ s0
>       (j1s,t1s) = partition isJ s1
>   -- bit hacky, to fix with the wrapper above
>   return $ let a = js ++ t0s
>            in if null a
>               then j1s ++ t1s
>               else a ++ t1s
>   where
>     isJ a = (snd . fst $ a) `elem` map fst jts

(a -> a -> Bool) -> [a] -> [a] -> [a]

filter ((`notElem` (map (snd.fst)fst jts)) . snd . fst) se

