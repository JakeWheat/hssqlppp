

This module represents part of the bound names environment used in the
type checker. It doesn't cover the stuff that is contained in the
catalog (so it is slightly misnamed), but focuses only on identifiers
introduced by things like tablerefs, sub selects, plpgsql parameters
and variables, etc.

> {-# LANGUAGE DeriveDataTypeable,TupleSections,ScopedTypeVariables,OverloadedStrings #-}
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
>     ,createTrefAliasedEnvironment
>     ,brokeEnvironment
>     ,orderByEnvironment
>      -- * environment query functions
>     ,envLookupIdentifier
>     ,envExpandStar
>     ) where

> import Data.Data
> import Data.Char
> --import Data.Maybe
> import Control.Monad
> import Control.Arrow
> import Data.List
> --import Debug.Trace
> --import Text.Groom

> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Internals.TypeChecking.TypeConversion
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal hiding (ncStr)
> import Data.Generics.Uniplate.Data
> import Data.Text (Text)
> import qualified Data.Text as T

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
>                  | SimpleTref Text [(Text,Type)] [(Text,Type)]
>                  -- | environment from joining two tables
>                  | JoinTref [(Text,Type)] -- join ids
>                             Environment Environment
>                  -- | environment from a sub select
>                  | SelectListEnv [(Text,Type)]
>                    -- | correlated subquery environment
>                  | CSQEnv Environment -- outerenv
>                           Environment -- main env
>                    -- | an aliased tref
>                  | TrefAlias Text (Maybe [Text]) Environment
>                  | BrokeEnvironment
>                    -- order by: can use the name of a select list column
>                    -- or anything from the same environment which select
>                    -- list operates on
>                  | OrderByEnvironment Environment Environment
>                    deriving (Data,Typeable,Show,Eq)

---------------------------------------------------

Create/ update functions, these are shortcuts to create environment variables,
the main purpose is to encapsulate looking up information in the
catalog and combining environment values with updates

TODO: remove the create prefixes

> emptyEnvironment :: Environment
> emptyEnvironment = EmptyEnvironment

> isEmptyEnv :: Environment -> Bool
> isEmptyEnv EmptyEnvironment = True
> isEmptyEnv _ = False

> envCreateTrefEnvironment :: Catalog -> [NameComponent] -> Either [TypeError] Environment
> envCreateTrefEnvironment cat tbnm = do
>   (nm,pub,prv) <- catLookupTableAndAttrs cat tbnm
>   return $ SimpleTref nm pub prv

> envSelectListEnvironment :: [(Text,Type)] -> Either [TypeError] Environment
> envSelectListEnvironment cols =
>   return $ SelectListEnv $ map (first $ T.map toLower) cols


> -- | create an environment as two envs joined together
> createJoinTrefEnvironment :: Catalog
>                           -> Environment
>                           -> Environment
>                           -> Maybe [NameComponent] -- join ids: empty if cross join
>                                                    -- nothing for natural join
>                           -> Either [TypeError] Environment
> createJoinTrefEnvironment cat tref0 tref1 jsc = do
>   -- todo: handle natural join case
>   (jids::[Text]) <- case jsc of
>             Nothing -> do
>                        j0 <- fmap (map (snd . fst)) $ envExpandStar Nothing tref0
>                        j1 <- fmap (map (snd . fst)) $ envExpandStar Nothing tref1
>                        return $ j0 `intersect` j1
>             Just x -> return $ map nmcString x

>  --         maybe (error "natural join ids") (map (nnm . (:[]))) jsc

>   jts <- forM jids $ \i -> do
>            (_,t0) <- envLookupIdentifier [QNmc $ T.unpack i] tref0
>            (_,t1) <- envLookupIdentifier [QNmc $ T.unpack i] tref1
>            fmap (i,) $ resolveResultSetType cat [t0,t1]
>   -- todo: check type compatibility
>   return $ JoinTref jts tref0 tref1

> createCorrelatedSubqueryEnvironment :: Environment -> Environment -> Environment
> createCorrelatedSubqueryEnvironment = CSQEnv

> createTrefAliasedEnvironment :: Text -> Maybe [Text] -> Environment -> Environment
> createTrefAliasedEnvironment = TrefAlias

> -- | represents type check failure upstream, don't produce additional
> -- type check errors
> brokeEnvironment :: Environment
> brokeEnvironment = BrokeEnvironment

> isBroken :: Environment -> Bool
> isBroken env = not $ null [() | BrokeEnvironment <- universeBi env]

> orderByEnvironment :: Environment -> Environment -> Environment
> orderByEnvironment = OrderByEnvironment

-------------------------------------------------------


The main hard work is done in the query functions: so the idea is that
the update functions create environment values which contain the
context free contributions of each part of the ast to the current
environment, and these query functions do all the work of resolving
implicit correlation names, ambigous identifiers, etc.

for each environment type, provide two functions which do identifier
lookup and star expansion

> listBindingsTypes :: Environment -> ((Maybe Text,Text) -> [((Text,Text),Type)]
>                                     ,Maybe Text -> [((Text,Text),Type)] -- star expand
>                                     )
> listBindingsTypes EmptyEnvironment = (const [],const [])
> listBindingsTypes BrokeEnvironment = (const [],const [])

> listBindingsTypes (TrefAlias ta Nothing env) =
>   (\(q,n) -> if q `elem` [Nothing, Just ta]
>              then req $ fst (listBindingsTypes env) (Nothing,n)
>              else []
>   ,\q -> if q `elem` [Nothing, Just ta]
>          then req $ snd (listBindingsTypes env) Nothing
>          else [])
>   where
>     req = map (\((_,i),t) -> ((ta,i),t))

> listBindingsTypes (TrefAlias ta (Just cs) env) =
>   (\(q,n) -> --trace ("lookup: " ++ show (q,n)) $
>      if q `elem` [Nothing, Just ta]
>      then    --really hacky, assume the ids come out of the star expansion in same order
>              -- almost certainly wrong some of the time
>              case elemIndex n cs of
>                Just i -> let s :: [((Text, Text), Type)]
>                              s = (snd (listBindingsTypes env) Nothing)
>                          in {-trace ("getit : " ++ show (i,show s))
>                                      $ -}
>                             -- map to change the qualifier name to match
>                             -- this alias not the source tref
>                             map (\((_,j),t) -> ((ta,j),t)) $ take 1 $ drop i s
>                Nothing -> []
>      else []
>   ,\q -> if q `elem` [Nothing, Just ta]
>          then let -- if there are too many aliases for the aliased tref
>                   -- the extras are ignored (not sure if this is correct)
>                   -- if there are not enough, the extras are kept without
>                   -- being renamed (think this is correct)
>                   repColNames = map Just cs ++ repeat Nothing
>                   aliasize :: [((Text, Text), Type)] -> [((Text, Text), Type)]
>                   aliasize =
>                     zipWith (\r ((_,n),t) ->
>                              case r of
>                                Just r' -> ((ta,r'),t)
>                                Nothing -> ((ta,n),t)) repColNames
>               in aliasize $ snd (listBindingsTypes env) Nothing
>          else [])
>   where
>     -- not sure why this is here, code layout is a bit confusing
>     _req = map (\((_,i),t) -> ((ta,i),t))


> listBindingsTypes (SimpleTref nm pus pvs) =
>   (\(q,n) -> let m (n',_) = (q `elem` [Nothing,Just nm])
>                             && n == n'
>              in addQual nm $ filter m $ pus ++ pvs
>   ,\q -> case () of
>            _ | q `elem` [Nothing, Just nm] -> addQual nm pus
>              | otherwise -> [])

> listBindingsTypes (JoinTref jids env0 env1) =
>   (idens,starexp)
>   where

>     idens k = let i0 = is0 k
>                   i1 = is1 k
>               in if not (null i0) && snd k `elem` jnames
>                  then i0
>                  else i0 ++ i1

>     _useResolvedType tr@((q,n),_) = case lookup n jids of
>                                    Just t' -> ((q,n),t')
>                                    Nothing -> tr
>     jnames = map fst jids
>     isJ ((_,n),_) = n `elem` jnames

todo: use useResolvedType

unqualified star:
reorder the ids so that the join columns are first

>     starexp Nothing = let (aj,anj) = partition isJ (st0 Nothing)
>                           bnj = filter (not . isJ) (st1 Nothing)
>                       in aj ++ anj ++ bnj
>     starexp q@(Just _) =
>       let s0 = st0 q
>           s1 = st1 q
>       in case (s0,s1) of
>            -- if we only get ids from one side, then don't
>            -- reorder them (is this right?)
>            (_:_,[]) -> s0
>            ([], _:_) -> s1
>            -- have ids coming from both sides
>            -- no idea how this is supposed to work
>            _ -> let (aj,anj) = partition isJ s0
>                     bnj = filter (not . isJ) s1
>                 in aj ++ anj ++ bnj
>     (is0,st0) = listBindingsTypes env0
>     (is1,st1) = listBindingsTypes env1

selectlistenv: not quite right, but should always have an alias so the
empty qualifier never gets very far

> listBindingsTypes (SelectListEnv is) =
>   (\(_,n) -> addQual "" $ filter ((==n).fst) is
>   ,const $ addQual "" is)

not quite right, see queryexprs.ag

> listBindingsTypes (OrderByEnvironment sl tr) =
>   (\i ->
>      -- hack: return the tref first so that
>      -- a qualifier can be added. This
>      -- is probably more wrong than the other
>      -- way round
>      case (fst (listBindingsTypes tr) i
>           ,fst (listBindingsTypes sl) i) of
>        ([],x) -> x
>        (y,_) -> y
>   ,const [])


csq just uses standard shadowing for iden lookup
for star expand, the outer env is ignored

> listBindingsTypes (CSQEnv outerenv env) =
>   (\k -> case (fst (listBindingsTypes env) k
>               ,fst (listBindingsTypes outerenv) k) of
>            (x,_) | not (null x) -> x
>            (_, x) | not (null x)  -> x
>            _ -> []
>   ,snd $ listBindingsTypes env)


> addQual :: Text -> [(Text,Type)] -> [((Text,Text),Type)]
> addQual q = map (\(n,t) -> ((q,n),t))


-------------------------------------------------------

use listBindingsTypes to implement expandstar and lookupid

> envExpandStar :: Maybe NameComponent -> Environment -> Either [TypeError] [((Text,Text),Type)]

> envExpandStar {-nmc env-} = {-let r =-} envExpandStar2 {-nmc env-}
>                         {-in trace ("env expand star: " ++ show nmc ++ " " ++ show r)
>                            r-}

> envExpandStar2 :: Maybe NameComponent -> Environment -> Either [TypeError] [((Text,Text),Type)]
> envExpandStar2 nmc env =
>   if isBroken env
>   then Left []
>   else
>     let st = snd (listBindingsTypes env) $ fmap nmcString nmc
>     in if null st
>        then case nmc of
>               Just x -> Left [UnrecognisedCorrelationName $ nmcString x]
>               Nothing -> Left [BadStarExpand]
>        else Right st

> nmcString :: NameComponent -> Text
> nmcString (QNmc n) = T.pack n
> nmcString (Nmc n) = T.pack $ map toLower n
> -- todo: don't use error
> nmcString (AntiNameComponent _) = error "tried to get ncstr of antinamecomponent"

> envLookupIdentifier :: [NameComponent] -> Environment
>                      -> Either [TypeError] ((Text,Text), Type)
> envLookupIdentifier nmc env = --trace ("lookup: " ++ show nmc  ++ "\n" ++ groom env) $
>   if isBroken env
>   then Left []
>   else do
>     k <- case nmc of
>                [a,b] -> Right (Just $ nmcString a, nmcString b)
>                [b] -> Right (Nothing, nmcString b)
>                _ -> Left [InternalError "too many nmc components in envlookupiden"]
>     case (fst $ listBindingsTypes env) k of
>       [] -> Left [UnrecognisedIdentifier $ nmcString $ last nmc]
>       [x] -> Right $ keepCasehack x
>       _ -> Left [AmbiguousIdentifier $ nmcString $ last nmc]
>   where
>     keepCasehack ((na,nb),t) =
>       case nmc of
>         [a,b] -> let x = ((keepcase a na,keepcase b nb),t)
>                  in {-(if True -- map toLower nb == "ou_id"
>                      then trace ("\n\n*********************\n\nlookup: " ++ show x ++ "\n\n********************************\n\n" ++ groom env ++ "\n\n********************************\n\n")
>                      else id)-} x
>         [b] -> ((na,keepcase b nb),t)
>         _ -> error "too many nmc components in envlookupiden(2)"
>     keepcase orig new = -- sanity check: make sure the nmcs are equal
>                         if T.map toLower new == nmcString orig
>                         then noLower orig
>                         else new
>     noLower (QNmc n) = T.pack n
>     noLower (Nmc n) = T.pack n


--------------------------
adding for plpgsql notes:

additional envs
* parameter in function
* declaration in function block
* implicit integer loop var in for loop
* set explicit record type in for loop/ assignment to record type
* for constraints in create table, create domain

Write tests to quickly check each bit of code which uses these using
the full typechecking:
update: sets, where, returning
select: tref -> select list, where, group by, order by
join: out to tref, into on expression
implicit variable in for loop
record type in for loop
record type in assignment
record type in select into
delete where and returning
block declarations
constraints in create table, create domain
parameters in function body
statementlist: pass on record updates?
insert: columns?, returning
