Copyright 2010 Jake Wheat

This module contains the code to manage local identifier bindings
during the type checking process. This is used for e.g. looking up the
types of parameter and variable references in plpgsql functions, and
for looking up the types of identifiers in select expressions.

This module exposes the internals of the localbindings datatype for
testing.

Some notes on lookups
all lookups are case insensitive
start by searching the head of the lookup update list and working down
the code here handles resolving the types of join columns when they
are not the same, and the update routine returns error if the join columns are not compatible
the code here handles expanding record types so that the components can be looked up

The local bindings is arranged as a stack. To append to this stack,
you use the LocalBindingsUpdate type. This is designed to be as easyas
possible for clients to use, so as much logic as possible is pushed
into the innards of this module, in particular most of the logic for
working with joins is in here.

The basic idea of the stack is at each level, there is a list of
qualified and unqualified names and types, to look up individual
ids. Some of the lookups map to ambiguous identifier errors. Also at
each level is a list of star expansions, one for each correlation name
in scope, and one for an unqualified star.


> {-# OPTIONS_HADDOCK hide  #-}

> module Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindingsInternal
>     (
>      LocalBindingsUpdate(..)
>     ,LocalBindings(..)
>     ,Source
>     ,FullId
>     ,SimpleId
>     ,IDLookup
>     ,StarLookup
>     ,LocalBindingsLookup(..)
>     ,emptyBindings
>     ,lbUpdate
>     ,lbExpandStar
>     ,lbExpandStar1
>     ,lbLookupID
>     ,lbLookupID1
>     ,ppLocalBindings
>     ,ppLbls
>     ) where

> import Control.Monad
> --import Debug.Trace
> import Data.List
> import Data.Maybe
> import Data.Char

> import Database.HsSqlPpp.AstInternals.TypeType
> import Database.HsSqlPpp.Utils
> import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal

> type E a = Either [TypeError] a

The data type to represent a set of local bindings in scope. The list
of updates used to create the local bindings is saved for debugging/
information.

> data LocalBindings = LocalBindings [LocalBindingsUpdate]
>                                    [LocalBindingsLookup]
>                      deriving Show

Each layer of the local bindings stack is
a map from (correlation name, id name) to source,correlation name, id
name, type tuple, or a type error, used e.g. to represent ambigious
ids, etc.;
and a map from correlation name to a list of these tuples to handle
star expansions.

Missing correlation names are represented by an empty string for the
correlation name.

> type Source = String

> type FullId = (Source,String,String,Type) -- source,correlationname,name,type
>                                           -- correlation name is
>                                           -- there so we can recover
>                                           -- it when the id is
>                                           -- accessed without a
>                                           -- correlation name
> type SimpleId = (String,Type)
> type IDLookup = ((String,String), E FullId)
> type StarLookup = (String, E [FullId])

> data LocalBindingsLookup = LocalBindingsLookup
>                                [IDLookup]
>                                [StarLookup] --stars
>                            deriving (Eq,Show)

This is the local bindings update that users of this module use.

> data LocalBindingsUpdate = LBIds {
>                              source :: Source
>                             ,correlationName :: String
>                             ,lbids :: [SimpleId]
>                             ,internalIds :: [SimpleId]
>                             }
>                          | LBJoinIds {
>                              jtref1 :: LocalBindingsUpdate
>                             ,jtref2 :: LocalBindingsUpdate
>                             ,joinIds :: Either () [String] -- left () represents natural join
>                                                            -- right [] represents no join ids
>                             ,alias :: String
>                            }
>                          | LBParallel { -- for having two or more sets of ids on the same
>                                         -- level, used to catch some ambiguous id errors
>                              plbu1 :: LocalBindingsUpdate
>                             ,plbu2 :: LocalBindingsUpdate
>                            }
>                            deriving Show



> emptyBindings :: LocalBindings
> emptyBindings = LocalBindings [] []

================================================================================

> ppLocalBindings :: LocalBindings -> String
> ppLocalBindings (LocalBindings lbus lbls) =
>   "LocalBindings\n" ++ doList show lbus ++ doList ppLbls lbls

> ppLbls :: LocalBindingsLookup -> String
> ppLbls (LocalBindingsLookup is ss) =
>       "LocalBindingsLookup\n" ++ doList show is ++ doList show ss

> doList :: (a -> String) -> [a] -> String
> doList m l = "[\n" ++ intercalate "\n," (map m l) ++ "\n]\n"


[((String,String)
                                 ,Either [TypeError] FullId)] --ids
                                [(String, Either [TypeError] [FullId])] --stars

================================================================================

wrapper for the proper lookupid function, this is for backwards
compatibility with the old lookup code

> lbLookupID :: LocalBindings
>            -> String -- identifier name
>            -> E Type
> lbLookupID lb ci = let (cor,i) = splitIdentifier ci
>                    in fmap extractType $ lbLookupID1 lb cor i
>                    where
>                      extractType (_,_,_,t) = t
>                      splitIdentifier s = let (a,b) = span (/= '.') s
>                                          in if b == ""
>                                             then ("", a)
>                                             else (a,tail b)

> lbLookupID1 :: LocalBindings
>            -> String -- correlation name
>            -> String -- identifier name
>            -> E FullId -- type error or source, corr, type
> lbLookupID1 (LocalBindings _ lkps) cor' i' =
>   lkId lkps
>   where
>     cor = mtl cor'
>     i = mtl i'
>     lkId ((LocalBindingsLookup idmap _):ls) =
>       case lookup (cor,i) idmap of
>         Just s -> s
>         Nothing -> lkId ls
>     lkId [] = Left [UnrecognisedIdentifier (if cor == "" then i' else cor ++ "." ++ i')]

================================================================================

wrapped for the proper expand star routine, for compatibility with the
old implementation of local bindings

> lbExpandStar :: LocalBindings -> String -> E [SimpleId]
> lbExpandStar lb cor =
>   fmap stripAll $ lbExpandStar1 lb cor
>   where
>     strip (_,_,n,t) = (n,t)
>     stripAll = map strip

> lbExpandStar1 :: LocalBindings -> String -> E [FullId]
> lbExpandStar1 (LocalBindings _ lkps) cor' =
>   exSt lkps
>   where
>     cor = mtl cor'
>     exSt ((LocalBindingsLookup _ lst):ls) =
>         case lookup cor lst of
>           Just s -> s
>           Nothing -> exSt ls
>     exSt [] = Left [UnrecognisedCorrelationName cor]

================================================================================

This is where constructing the local bindings lookup stacks is done

> lbUpdate :: Catalog -> LocalBindings -> LocalBindingsUpdate -> E LocalBindings
> lbUpdate cat (LocalBindings lbus lkps) lbu' =
>    mapRight (\l -> LocalBindings (lbu':lbus) (l:lkps)) $ makeStack cat lbu
>    where
>      lbu = lowerise lbu'
>      -- make correlation names and id names case insensitive
>      -- by making them all lowercase
>      lowerise (LBIds src cor ids iids) =
>        LBIds src (mtl cor) (mtll ids) (mtll iids)
>      lowerise (LBJoinIds t1 t2 ji a) =
>        LBJoinIds (lowerise t1) (lowerise t2) (fmap mtll1 ji) (mtl a)
>      lowerise (LBParallel lbu1 lbu2) =
>        LBParallel (lowerise lbu1) (lowerise lbu2)
>      mtll = map (\(n,t) -> (mtl n, t))
>      mtll1 = map (\l -> mtl l)

> makeStack :: Catalog -> LocalBindingsUpdate -> E LocalBindingsLookup

> makeStack _ (LBIds src cor ids iids) =
>   Right $ LocalBindingsLookup doIds doStar
>   where
>     doIds :: [((String,String)
>               ,E FullId)]
>     doIds = map (makeLookup cor) (map addDetails ids ++ map addDetails iids)
>             -- add unqualified if cor isn't empty string
>             ++ map (makeLookup "")
>                    (case cor of
>                             "" -> []
>                             _ -> map addDetails ids ++ map addDetails iids)
>             where
>               makeLookup c1 (s,_,n,t)= ((c1,n), Right (s,cor,n,t))
>     doStar :: [(String, E [FullId])]
>     doStar = [(cor,Right $ map addDetails ids)] ++
>              case cor of
>                       "" -> []
>                       _ -> [("",Right $ map addDetails ids)]
>     addDetails (n,t) = (src,cor,n,t)

> makeStack cat (LBJoinIds t1 t2 jns a) = do
>   --first get the stacks from t1 and t2
>   --combine the elements of these filtering out the join ids
>   --
>   (LocalBindingsLookup i1 s1) <- makeStack cat t1
>   (LocalBindingsLookup i2 s2) <- makeStack cat t2
>   let isJid ((_,n),_) =  n `elem` jns'
>       removeJids = filter (not . isJid)
>       i1' = removeJids i1
>       i2' = removeJids i2
>       jids :: [IDLookup]
>       jids = flip map jns' $ \n -> fromJust $ find (\((_,n1),_) -> n == n1) i1
>       --jids = map ((fmap makeLookup) . (\n -> fromJust $ lookup ("",n) i1)) jns'
>   return $ LocalBindingsLookup ((combineAddAmbiguousErrors i1' i2') ++ jids) (s1 ++ s2)
>   where
>     jns' = case jns of
>              Left () -> []
>              Right x -> x

> makeStack cat (LBParallel u1 u2) = do
>   -- get the two stacks,
>   -- for any keys that appear in both respective lookups, replace with ambigious error
>   -- and concatenate the lot
>   (LocalBindingsLookup i1 s1) <- makeStack cat u1
>   (LocalBindingsLookup i2 s2) <- makeStack cat u2
>   return $ LocalBindingsLookup (combineAddAmbiguousErrors i1 i2) (s1 ++ s2)

> combineAddAmbiguousErrors :: [IDLookup] -> [IDLookup] -> [IDLookup]
> combineAddAmbiguousErrors i1 i2 =
>   let commonIds = intersect (map fst i1) (map fst i2)
>       removeCommonIds = filter (\a -> fst a `notElem` commonIds)
>       fi1 = removeCommonIds i1
>       fi2 = removeCommonIds i2
>       errors = map (\(c,n) -> ((c,n),Left [AmbiguousIdentifier (c ++ "." ++ n)])) commonIds
>   in fi1 ++ fi2 ++ errors

===============================================================================

> mtl :: String -> String
> mtl = map toLower

================================================================================







> {-lbExpandStar :: LocalBindings -> String -> Either [TypeError] [SimpleId]
> lbExpandStar lb c = fmap (\l -> map (\(_,_,n,t) -> (n,t)) l) $ lbExpandStar1 lb $ mtl c

> lbExpandStar1 :: LocalBindings
>              -> String -- correlation name
>              -> Either [TypeError] [FullId] -- either error or [source,(corr,name,type)]
> lbExpandStar1 (LocalBindings l) cor' =
>   es l
>   where
>     cor = mtl cor'
>     es :: [LocalBindingsUpdate] -> Either [TypeError] [FullId]
>     es (LBQualifiedIds src cor1 ids _ :lbus) = if cor == cor1 || cor == ""
>                                                then mapEm src cor1 ids
>                                                else es lbus
>     es (LBUnqualifiedIds src ids _ : lbus) = if cor == ""
>                                              then mapEm src "" ids
>                                              else es lbus
>     es (u@(LBJoinIds lbu1 lbu2 _) : lbus) =
>        let ids = getStarIds u
>        in case lookup cor ids of
>             Just x -> x
>             Nothing -> es lbus
>        {-if cor `elem` ("": map (\(LBQualifiedIds _ c _ _) -> c) (flattenLibUpdates [lbu1,lbu2]))
>        then Right $ fromJust $ lookup cor $
>        else es lbus-}
>     es [] = Left [UnrecognisedCorrelationName cor]
>     mapEm :: String -> String -> [SimpleId] -> Either [TypeError] [FullId]
>     mapEm src c = Right . map (\(a,b) -> (src,c,a,b))

> lbLookupID :: LocalBindings
>            -> String -- identifier name
>            -> Either [TypeError] Type
> lbLookupID lb ci = let (cor,i) = splitIdentifier $ mtl ci
>                    in fmap (\(_,_,_,t) -> t) $ lbLookupID1 lb cor i
>                    where
>                      splitIdentifier s = let (a,b) = span (/= '.') s
>                                          in if b == ""
>                                             then ("", a)
>                                             else (a,tail b)


> lbLookupID1 :: LocalBindings
>            -> String -- correlation name
>            -> String -- identifier name
>            -> Either [TypeError] FullId -- type error or source, corr, type
> lbLookupID1 (LocalBindings lb) cor' i' =
>   lk lb
>   where
>     cor = mtl cor'
>     i = mtl i'
>     lk (lbu:lbus) = case findID cor i lbu of
>                                           Nothing -> lk lbus
>                                           Just t -> t
>     lk [] = Left [UnrecognisedIdentifier (if cor == "" then i else cor ++ "." ++ i)]

> findID :: String
>        -> String
>        -> LocalBindingsUpdate
>        -> Maybe (Either [TypeError] FullId)
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
> findID cor i u@(LBJoinIds _ _ _) =
>     lookup (cor,i) $ getJoinIdMap u

expand out the ids for a join

so we have a full list to lookup single ids whether qualified or not,
and return possibly a ambiguous id error

> getJoinIdMap :: LocalBindingsUpdate
>              -> [((String,String), Either [TypeError] FullId)]
> getJoinIdMap (LBJoinIds lbu1 lbu2 jids) =
>     concat [
>       -- all non join ids unqualified
>       flip concatMap trefs $ \(LBQualifiedIds s c ids iids) ->
>         map (\(i,t) -> (("", i), Right (s,c,i,t))) $ notJids ids ++ iids
>       -- all join ids unqualified, linked to first jtref
>      ,let (LBQualifiedIds s c _ _) = head trefs
>       in map (\(i,t) -> (("", i), Right (s,c,i,t))) jidts
>       -- all non join ids qualified
>      ,flip concatMap trefs $ \(LBQualifiedIds s c ids iids) ->
>         map (\(i,t) -> ((c, i), Right (s,c,i,t))) $ notJids ids ++ iids
>       -- all join ids qualified by each table qualifier
>      ,let qs = map (\(LBQualifiedIds s c _ _) -> (s,c)) trefs
>       in flip concatMap qs (\(s,c) -> map (\(i,t) -> ((c, i), Right (s,c,i,t))) jidts)
>     ]
>   where
>     --trefs = flattenLibUpdates [lbu1,lbu2]
>     notJids = filter (\(n,_) -> n `notElem` jids)
>     jidts = let (LBQualifiedIds _ _ ids iids) = head trefs
>                 aids = ids ++ iids
>             in map (\n -> (n, fromJust $ lookup n aids)) jids

> getJoinIdMap x = error $ "internal error: getJoinIdMap called on " ++ show x

> getStarIds :: LocalBindingsUpdate
>              -> [(String, [FullId])]
> getStarIds (LBJoinIds lbu1 lbu2 jids) =
>   -- uncorrelated
>   ("", concat [
>             -- all join ids qualified with c1
>             let (LBQualifiedIds s c _ _) = head trefs
>             in map (\(i,t) -> (s,c,i,t)) jidts
>             -- all non join tref ids unqualified
>            ,flip concatMap trefs (\(LBQualifiedIds s c ids _) ->
>                                   map (\(i,t) -> (s,c,i,t)) $ notJids ids)
>             ]) :
>    -- correlated
>   flip map trefs (\(LBQualifiedIds s c ids _) ->
>         (c, concat [
>             -- all join ids qualified with c
>             map (\(i,t) -> (s,c,i,t)) jidts
>             -- all non join ids
>            ,map (\(i,t) -> (s,c,i,t)) $ notJids ids
>               ]))
>   where
>     --trefs = flattenLibUpdates [lbu1,lbu2]
>     notJids = filter (\(n,_) -> n `notElem` jids)
>     jidts = let (LBQualifiedIds _ _ ids iids) = head trefs
>                 aids = ids ++ iids
>             in map (\n -> (n, fromJust $ lookup n aids)) jids

> getStarIds x = error $ "internal error: getJoinIdMap called on " ++ show x

> {-flattenLibUpdates :: [LocalBindingsUpdate] -> [LocalBindingsUpdate]
> flattenLibUpdates (x:xs) = f x ++ flattenLibUpdates xs
>                          where
>                            f l@(LBQualifiedIds _ _ _ _) = [l]
>                            f (LBJoinIds lbu1 lbu2 _) = flattenLibUpdates [lbu1,lbu2]
>                            f _ = undefined
> flattenLibUpdates [] = []-}-}
