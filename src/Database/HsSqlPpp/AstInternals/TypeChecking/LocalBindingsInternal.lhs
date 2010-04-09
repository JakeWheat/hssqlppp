Copyright 2010 Jake Wheat

This module contains the code to manage local identifier bindings
during the type checking process. This is used for e.g. looking up the
types of parameter and variable references in plpgsql functions, and
for looking up the types of identifiers in select expressions.

This module exposes the internals of the localbindings datatype for
testing.

The lookups to support are a single identifier, or to give a star
expansion.


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

> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindingsInternal
>     (
>      LocalBindingsUpdate(..)
>     ,LocalBindings(..)
>     ,Source
>     ,FullId
>     ,SimpleId
>     ,IDLookup
>     ,StarExpand
>     ,LocalBindingsLookup(..)
>     ,emptyBindings
>     ,lbUpdate
>     ,lbExpandStar
>     ,lbLookupID
>     ,lbUpdateDot
>     ,ppLocalBindings
>     ,ppLbls
>     ) where
>
> --import Control.Monad as M
> import Control.Applicative
> --import Debug.Trace
> import Data.List
> import Data.Maybe
> import Data.Char
> --import Data.Either
> --import qualified Data.Map as M
>
> import Database.HsSqlPpp.AstInternals.TypeType
> --import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
> --import Database.HsSqlPpp.AstInternals.TypeChecking.TypeConversion
> import Database.HsSqlPpp.AstInternals.TypeChecking.ErrorUtils
>

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
>
> type FullId = (Source,[String],Type) -- source,fully qualified name components,type
> type SimpleId = (String,Type)
> type IDLookup = (String, E FullId)
> type StarExpand = E [FullId] --the order of the [FullId] part is important
>
> data LocalBindingsLookup = LocalBindingsLookup
>                                [IDLookup]
>                                StarExpand
>                            deriving (Eq,Show)

This is the local bindings update that users of this module use.

> data LocalBindingsUpdate = LBIds {source :: Source
>                                  ,correlationName :: Maybe String
>                                  ,lbids :: [SimpleId]}
>                          | LBTref {source :: Source
>                                   ,talias :: String
>                                   ,lbids :: [SimpleId]
>                                   ,lbsysids :: [SimpleId]}
>                          | LBJoinTref {source :: Source
>                                       ,jtref1 :: LocalBindingsUpdate
>                                       ,jtref2 :: LocalBindingsUpdate
>                                       ,joinIds :: Either () [String] -- left () represents natural join
>                                                            -- right [] represents no join ids
>                                       ,jalias :: Maybe String}
>                            deriving Show
>
> emptyBindings :: LocalBindings
> emptyBindings = LocalBindings [] []

================================================================================

> ppLocalBindings :: LocalBindings -> String
> ppLocalBindings (LocalBindings lbus lbls) =
>   "LocalBindings\n" ++ doList show lbus ++ doList ppLbls lbls
>
> ppLbls :: LocalBindingsLookup -> String
> ppLbls (LocalBindingsLookup is ss) =
>       "LocalBindingsLookup\n" ++ doList show is ++ show ss
>
> doList :: (a -> String) -> [a] -> String
> doList m l = "[\n" ++ intercalate "\n," (map m l) ++ "\n]\n"

================================================================================

> lbUpdate :: Catalog -> LocalBindingsUpdate -> LocalBindings -> E LocalBindings
> lbUpdate cat u1 (LocalBindings us s) = do
>   (ids,se) <- updateStuff cat u1
>   return $ LocalBindings (u1 : us) (LocalBindingsLookup ids se : s)
>

> updateStuff :: Catalog -> LocalBindingsUpdate -> E ([IDLookup],StarExpand)
> updateStuff _ (LBIds src cn ids) =
>     return (fids, Left [BadStarExpand])
>     where
>       fids = maybe fids1 (: fids1) $ mc <$> cn
>       fids1 = map (\(n,t) -> (n, Right (src,maybe [n] (: [n]) cn, t))) ids
>       mc c = (c, Right (src, [c], CompositeType ids))
> updateStuff _ (LBTref src al ids sids) =
>     return (fids,(Right pids))
>     where
>       fids = mc : fids1
>       fids1 = map (\(n,t) -> (n, Right (src,[al,n],t))) $ ids ++ sids
>       mc = (al, Right (src, [al], CompositeType $ ids ++ sids))
>       pids = map (\(n,t) -> (src,[al,n],t)) ids
> updateStuff cat (LBJoinTref _src u1 u2 jids _al) = do
>   (ids1,se1') <- updateStuff cat u1
>   (ids2,se2') <- updateStuff cat u2
>   se1 <- se1'
>   se2 <- se2'
>   let jnames = case jids of
>                          Right ns -> ns
>                          Left () -> intersect (map fst ids1) (map fst ids2)
>   -- make the lookups
>   let jids1 :: [(String,E FullId)]
>       jids1 = flip map jnames $ \i -> (i,fromJust $ lookup i ids1)
>       rj :: [(String,E FullId)] -> [(String,E FullId)]
>       rj = filter $ \e -> fst e `notElem` jnames
>       ids :: [(String,E FullId)]
>       ids = jids1 ++ rj ids1 ++ rj ids2
>   --make the star expansion
>       rj1 :: [FullId] -> [FullId]
>       rj1 = filter $ (\(_,n,_t) -> last n `notElem` jnames)
>       se = map snd jids1 ++ map return (rj1 se1) ++ map return (rj1 se2)
>   return (ids, sequence se)


================================================================================

> lbExpandStar :: LocalBindings -> E [FullId]
> lbExpandStar (LocalBindings _ ((LocalBindingsLookup _ x) : _)) = x
> lbExpandStar _ = Left [BadStarExpand]

================================================================================

> lbLookupID :: LocalBindings -> String -> E FullId
> lbLookupID (LocalBindings _ lbl) i =
>     let ls = map getLbIdl lbl
>     in lkp1 ls $ mtl i
>     where
>       lkp1 :: [[IDLookup]] -> String -> E FullId
>       lkp1 (l:ls) i1 = case lookup i1 l of
>                          Nothing -> lkp1 ls i1
>                          Just x -> x
>       lkp1 [] _ = Left [UnrecognisedIdentifier i]
>       getLbIdl (LocalBindingsLookup x _) = x

================================================================================

> lbUpdateDot :: Catalog -> String -> LocalBindings -> E LocalBindings
> lbUpdateDot cat i lb = do
>     (_,_,c) <- lbLookupID lb i
>     f <- lmt $ expandComposite cat c
>     lbUpdate cat (LBIds "dot qual" Nothing f) emptyBindings

> expandComposite :: Catalog -> Type -> Maybe [(String,Type)]
> expandComposite cat (SetOfType t) = expandComposite cat t
> expandComposite cat (PgRecord (Just t)) = expandComposite cat t
> expandComposite _ (CompositeType fs) = Just fs
> expandComposite cat (NamedCompositeType n) = etmt $ catCompositeAttrs cat [] n
> expandComposite _ _ = Nothing


================================================================================

> mtl :: String -> String
> mtl = map toLower






































wrapper for the proper lookupid function, this is for backwards
compatibility with the old lookup code

> {-lbLookupID :: LocalBindings
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
>
> lbLookupID1 :: LocalBindings
>            -> String -- correlation name
>            -> String -- identifier name
>            -> E FullId -- type error or source, corr, type
> lbLookupID1 (LocalBindings _ lkps) cor' i' =
>   --trace ("lookup: " ++ showID cor' i'
>   --       ++ "in " ++ concatMap ppLbls lkps) $
>   -- hack for triggers
>   case cor of
>     "new" | isTrigRec -> Right ("", "new", i, UnknownType)
>     "old" | isTrigRec -> Right ("", "old", i, UnknownType)
>     _ -> lkId lkps
>   where
>     isTrigRec = case lbLookupID1 (LocalBindings undefined lkps) "" cor of
>                   Right (_,_,_,Pseudo TriggerRecord) -> True
>                   _ -> False
>     cor = mtl cor'
>     i = mtl i'
>     lkId ((LocalBindingsLookup idmap _):ls) =
>       case lookup (cor,i) idmap of
>         Just s -> s
>         Nothing -> if cor /= "" && any ((==cor) . fst . fst) idmap
>                    then Left [UnrecognisedIdentifier $ showID cor i']
>                    else lkId ls
>     lkId [] = if cor' == "" --todo: need to throw unrecognised identifier, if the correlation name isn't "", a id isn't found, and there are other ids with that correlation name
>               then Left [UnrecognisedIdentifier $ showID cor i']
>               else Left [UnrecognisedCorrelationName cor']

================================================================================

wrapped for the proper expand star routine, for compatibility with the
old implementation of local bindings

> lbExpandStar :: LocalBindings -> String -> E [SimpleId]
> lbExpandStar lb cor =
>   fmap stripAll $ lbExpandStar1 lb cor
>   where
>     strip (_,_,n,t) = (n,t)
>     stripAll = map strip
>
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

> lbUpdate :: Catalog -> LocalBindingsUpdate -> LocalBindings -> E LocalBindings
> lbUpdate cat lbu' (LocalBindings lbus lkps) = do
>    lbl <- makeStack cat lbu
>    lbl1 <- expandComposites cat lbl
>    --trace ("update: " ++ ppLbls lbl1) $ return ()
>    return $ LocalBindings (lbu':lbus) (lbl1:lkps)
>    where
>      lbu = lowerise lbu'
>      -- make correlation names and id names case insensitive
>      -- by making them all lowercase
>      lowerise (LBIds src ids) =
>        LBIds src (mtll ids)
>      lowerise (LBJoinIds t1 t2 ji a) =
>        LBJoinIds (lowerise t1) (lowerise t2) (fmap mtll1 ji) (mtl a)
>      lowerise (LBParallel lbu1 lbu2) =
>        LBParallel (lowerise lbu1) (lowerise lbu2)
>      mtll = map (\(n,t) -> (mtl n, t))
>      mtll1 = map (\l -> mtl l)
>
> makeStack :: Catalog -> LocalBindingsUpdate -> E LocalBindingsLookup
> makeStack _ (LBIds src ids) =
>   Right $ LocalBindingsLookup doIds doStar
>   where
>     doIds :: [((String,String)
>               ,E FullId)]
>     doIds = -- add unqualified if cor isn't empty string
>             map (makeLookup "")
>                    (case cor of
>                             "" -> []
>                             _ -> map addDetails ids ++ map addDetails iids)
>             ++ map (makeLookup cor) (map addDetails ids ++ map addDetails iids)
>             where
>               makeLookup c1 (s,_,n,t)= ((c1,n), Right (s,cor,n,t))
>     doStar :: [(String, E [FullId])]
>     doStar = case cor of
>                       "" -> []
>                       _ -> [("",Right $ map addDetails ids)]
>              ++ [(cor,Right $ map addDetails ids)]
>     addDetails (n,t) = (src,cor,n,t)
>
> makeStack cat (LBJoinIds t1 t2 jns a) = do
>   --first get the stacks from t1 and t2
>   --combine the elements of these filtering out the join ids
>   --
>   (LocalBindingsLookup i1 s1) <- makeStack cat t1
>   (LocalBindingsLookup i2 s2) <- makeStack cat t2
>   -- get the names and types of the join columns
>   let jns' = case jns of
>              Left () -> -- natural join, so we have to work out the names
>                         -- by looking at the common attributes
>                         -- we do this by getting the star expansion
>                         -- with no correlation name, and then finding
>                         -- the ids which appear in both lists
>                         -- (so this ignores internal ids)
>                  let ic1 :: [FullId]
>                      ic1 = fromRight [] $ maybe (Right []) id $ lookup "" s1
>                      ic2 = fromRight [] $ maybe (Right []) id $ lookup "" s2
>                      third (_,_,n,_) = n
>                      ii1 :: [String]
>                      ii1 = map third ic1
>                      ii2 = map third ic2
>                  in intersect ii1 ii2
>              Right x -> x
>       -- first prepare for the id lookups
>       -- remove the join ids from the id lookups
>       isJid ((_,n),_) = (n `elem` jns')
>       removeJids = filter (not . isJid)
>       i1' = removeJids i1
>       i2' = removeJids i2
>   jids <- M.sequence $ joinIDTypes i1 i2 jns'
>   {-trace ("joinids: " ++ show jids
>          ++ "\ni1 " ++ show i1
>          ++ "\ni2 " ++ show i2
>          ++ "\ni1' " ++ show i1'
>          ++ "\ni2' " ++ show i2'
>         ) $ return ()-}
>   let jidsLk :: [IDLookup]
>       jidsLk = if null i1 || null i2
>                then [] --error?
>                else let (_,Right (sc1,c1,_,_)) = head i1
>                         (_,Right (sc2,c2,_,_)) = head i2
>                     in flip concatMap jids $ \(n,t) -> [(("",n), Right (sc1,c1,n,t))
>                                                        ,((c1,n), Right (sc1,c1,n,t))
>                                                        ,((c2,n), Right (sc2,c2,n,t))
>                                                        ]
>       --jidsF :: [FullId]
>       --jidsF = rights $ map snd jidsLk
>       newIdLookups = (jidsLk ++ (combineAddAmbiguousErrors i1' i2'))
>       -- now do the star expansions
>       -- for each correlation name, remove any ids which match a join id
>       -- then prepend the join ids to that list
>       se = combineStarExpansions s1 s2 -- don't know if this is quite right
>       removeJids1 :: StarLookup -> StarLookup
>       removeJids1 (k,ids) = (k, fmap (filter (\(_,_,n,_) -> n `notElem` jns')) ids)
>       prependJids :: StarLookup -> StarLookup
>       prependJids (c, lkps) = case lkps of
>                                 Left _ -> (c,lkps)
>                                 Right r | null r -> (c,lkps)
>                                 Right r -> let (s,c1,_,_) = head r
>                                                ids = map (\(n,t) -> (s,c1,n,t)) jids
>                                            in (c, fmap (ids++) lkps)
>       newStarExpansion = map (prependJids . removeJids1) se
>       -- if we have an alias then we just want unqualified ids, then
>       -- the same ids with a t3 alias for both ids and star expansion
>       -- with all the correlation names replaced with the alias
>   if a == ""
>     then return $ LocalBindingsLookup newIdLookups newStarExpansion
>     else return $ LocalBindingsLookup (aliasIds newIdLookups) (aliasExps newStarExpansion)
>   where
>     aliasIds :: [IDLookup] -> [IDLookup]
>     aliasIds lkps = let trimmed = filter (\((c,_),_) -> c == "") lkps
>                         aliased = map (\(c,i) -> (c, fmap replaceCName i)) trimmed
>                     in aliased ++ map (\((_,n),i) -> ((a,n),i)) aliased
>     aliasExps :: [StarLookup] -> [StarLookup]
>     aliasExps lkps = let is = fromMaybe (error "localbindingsinternal.makestack : fromJust") $
>                               lookup "" lkps
>                          aliased = fmap (map replaceCName) is
>                      in [("",aliased), (a, aliased)]
>     replaceCName (s,_,n,t) = (s,a,n,t)
>     joinIDTypes :: [IDLookup] -> [IDLookup] -> [String] -> [E (String,Type)]
>     joinIDTypes i1 i2 = map (joinIDType i1 i2)
>     joinIDType :: [IDLookup] -> [IDLookup] -> String -> E (String,Type)
>     joinIDType i1 i2 s = do
>       (_,_,_,ty1) <- fromMaybe (Left [MissingJoinAttribute]) $
>                      lookup ("",s) i1
>       (_,_,_,ty2) <- fromMaybe (Left [MissingJoinAttribute]) $
>                      lookup ("",s) i2
>       ty <- resolveResultSetType cat [ty1,ty2]
>       return (s,ty)
>
> makeStack cat (LBParallel u1 u2) = do
>   -- get the two stacks,
>   -- for any keys that appear in both respective lookups, replace with ambigious error
>   -- and concatenate the lot
>   (LocalBindingsLookup i1 s1) <- makeStack cat u1
>   (LocalBindingsLookup i2 s2) <- makeStack cat u2
>   return $ LocalBindingsLookup (combineAddAmbiguousErrors i1 i2) $ combineStarExpansions s1 s2
>
> combineStarExpansions :: [StarLookup] -> [StarLookup] -> [StarLookup]
> combineStarExpansions s1 s2 =
>   let p :: [(String, [(String, E [FullId])])]
>       p = npartition fst (s1 ++ s2)
>   in flip map p $ \(a,b) -> (a,concat <$> M.sequence (map snd b))
>
> combineAddAmbiguousErrors :: [IDLookup] -> [IDLookup] -> [IDLookup]
> combineAddAmbiguousErrors i1 i2 =
>   let commonIds = intersect (map fst i1) (map fst i2)
>       removeCommonIds = filter (\a -> fst a `notElem` commonIds)
>       fi1 = removeCommonIds i1
>       fi2 = removeCommonIds i2
>       errors = map (\(c,n) -> ((c,n),Left [AmbiguousIdentifier $ showID c n])) commonIds
>   in fi1 ++ fi2 ++ errors

===============================================================================

> mtl :: String -> String
> mtl = map toLower
>
> showID :: String -> String -> String
> showID cor i = if cor == "" then i else cor ++ "." ++ i

================================================================================

expand composites

slightly dodgy - run through all the unqualified ids in the idlookups, and if any
have a composite type, add each element of that composite under the
correlation name of the idlookup itself, and add a star expansion for
that name also. This pretends that using a correlation name, composite
name and id name as a three part id isn't possible

> expandComposites :: Catalog -> LocalBindingsLookup -> E LocalBindingsLookup
> expandComposites cat (LocalBindingsLookup idlkp stlkp) = do
>   let unqids = filter (\((a,_),_) -> a == "") idlkp
>       strip = map snd unqids
>       getComposites = filter (\(_,_,_,t) -> isCt t) $ rights strip
>   comps <- mapM compExp getComposites
>   let sts = map toStarLookup comps
>   Right (LocalBindingsLookup (idlkp ++ (concat comps)) (stlkp ++ sts))
>   where
>     isCt (SetOfType t) = isCompositeType t
>     isCt t = isCompositeType t
>     getCompFields :: Type -> E [(String,Type)]
>     getCompFields (SetOfType t) = getCompFields t
>     getCompFields (PgRecord Nothing) = Right []
>     getCompFields (PgRecord (Just t)) = getCompFields t
>     getCompFields (CompositeType f) = return f
>     getCompFields (NamedCompositeType s) = catCompositePublicAttrs cat [] s
>     getCompFields (AnonymousRecordType _) = Right [] -- ??
>     getCompFields _ = Right []
>     compExp :: FullId -> E [IDLookup]
>     compExp (s,_,n,t) = do
>       f <- getCompFields t
>       return $ flip map f $ \(n1,t1) -> ((n,n1),Right (s,n,n1,t1))
>     toStarLookup :: [IDLookup] -> StarLookup
>     toStarLookup ids =
>       let fids::[FullId]
>           fids = rights $ map snd ids
>           (_,c,_,_) = if null fids then (undefined,"ERROR",undefined,undefined) else head fids
>       in (c,Right fids)-}
