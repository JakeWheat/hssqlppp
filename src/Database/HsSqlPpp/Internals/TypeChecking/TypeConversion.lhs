

This file contains the functions for resolving types and
function/operator resolution (which is seriously crazy). See the pg
manual chapter 10:

http://www.postgresql.org/docs/8.4/interactive/typeconv.html

This code is really spaghettified.

findCallMatch - pass in a name and a list of arguments, and it returns
the matching function. (pg manual 10.2,10.3)

resolveResultSetType - pass in a set of types, and it tries to find
the common type they can all be cast to. (pg manual 10.5)

checkAssignmentValid - pass in source type and target type, returns
                typelist[] if ok, otherwise error, pg manual 10.4
                Value Storage

I wrote this when I was still struggling with haskell basics so it is
probably the worst bit of code in the codebase (there are a few
other contenders for this accolade). A rewrite is planned, but it
seems to do the job reasonably well at the moment so keeps getting put
off.


> {-# LANGUAGE PatternGuards,NondecreasingIndentation #-}
> module Database.HsSqlPpp.Internals.TypeChecking.TypeConversion (
>                        findCallMatch
>                       ,resolveResultSetType
>                       ,checkAssignmentValid
>                       ,checkAssignmentsValid
>                       ) where
>
> import Data.Maybe
> import Data.List
> import Data.Either
> --import Debug.Trace
> import Data.Char
>
> import Database.HsSqlPpp.Internals.TypeType
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Utils.Utils

 > traceIt :: Show a => String -> a -> a
 > traceIt s t = trace (s ++ ": " ++ show t) t

= findCallMatch

~~~~

findCallMatch - partially implements the type conversion rules for
finding an operator or function match given a name and list of
arguments with partial or fully specified types

TODO:, qualifiers
namespaces
function style casts not in catalog
variadic args
default args
domains -> base type
what about aggregates and window functions?

Algo:

cands = all fns with matching names
        and same number of args

if exact match on types in this list, use it
  (if binary operator being matched, and one arg is typed and one is
  unknown, also match an operator by assuming the unknown is the same
  as the typed arg)

best match part:

filter cands with args which don't exactly match input args, and input
args cannot be converted by an implicit cast. unknowns count as
matching anything
if one left: use it

filter for preferred types:

for each cand, count each arg at each position which needs conversion,
and the cand type is a preferred type at that position.
if there are cands with count>0, keep only cands with the max count,
if one return it
if there are no cands with count>0, keep them all

check unknowns:
if any input args are unknown, and any cand accepts string at that
position, fix that arg's category as string, otherwise if all cands
accept same category at that position, fix that input args as that
category.
if we still have unknowns, then fail

discard cands which don't match the new input arg/category list

for each categorised input arg, if any cand accepts preferred type at
that position, get rid of cands which don't accept preferred type at
that position

if one left: use
else fail

polymorphic matching:
want to create a set of matches to insert into the cast pairs list
so:

find all matches on name, num args and have polymorphic parameters

for each one, check the polymorphic categories - eliminate fns that
have params in wrong category - array, non array, enum.
work out the base types for the polymorphic args at each spot based on
the args passed - so each arg is unchanged except arrays which have
the array part stripped off

now we have a list of types to match against the polymorphic params,
use resolveResultSetType to see if we can produce a match, if so,
create a new prototype which is the same as the polymorphic function
but with this matching arg swapped in, work out the casts and add it
into cand cast pairs, after exact match has been run.


findCallMatch is a bit of a mess

todos:

rewrite this to try to make it a bit clearer

find some way to draw a data flow diagram of the code easily

add a logging facility so the function can explain what has happened
at each state, so you can provide a detailed explanation e.g. if the
code can't find an operator match to see what it has tried to match
against.

~~~~

> type ProtArgCast = (FunctionPrototype, [ArgCastFlavour])

>       {-
>         between, greatest and least are treated as syntactic sugar so we
>         delegate the function lookups to the <=/>= operators.
>         the row comparison should be more general than this, since it supports
>         any operator satisfying some properties
> -}
> findCallMatch :: Catalog -> String -> [Type] ->  Either [TypeError] FunctionPrototype
> findCallMatch cat fnName' argsType =
>     --trace ("typecheckfncall " ++ fnName' ++ show argsType) $
>     --dependsOnRTpe argsType $
>       case fnName of
>               "count" -> -- not quite sure how this is suppose to work,
>                          -- the counts in the pg catalog accept either
>                          -- no args, or one arg of type any, but you can call
>                          -- count with multiple arguments?
>                          return ("count", argsType, typeBigInt, False)
>               "!between" | as@[_,_,_] <- argsType -> do
>                     -- not sure if this is correct - use the result set resolution
>                     -- to make the argument types compatible
>                     -- then just check there is a >=, <= returning a pair
>                     -- of somethings which can be anded
>                     t <- resolveResultSetType cat as
>                     f1 <- lookupReturnType ">=" [t,t]
>                     f2 <- lookupReturnType "<=" [t,t]
>                     _ <- lookupFn "!and" [f1,f2]
>                     return ("!between", [t,t,t], typeBool, False)
>               "greatest" -> do
>                     (_,a,t,x) <- lookupFn fnName argsType
>                     _ <- lookupFn ">=" [t,t]
>                     return ("greatest",a,t,x)
>               "least" -> do
>                     (_,a,t,x) <- lookupFn fnName argsType
>                     _ <- lookupFn "<=" [t,t]
>                     return ("greatest",a,t,x)
>               "!rowctor" -> return ("!rowCtor", argsType, AnonymousRecordType argsType, False)
>                     -- special case the row comparison ops
>                     -- this needs to be fixed: we want to match
>                     -- any implicit casts to functions on composite types
>                     -- first, then we can use the anonymous record type on
>                     -- any composite
>               "." | [_,b] <- argsType -> Right (".", argsType, b, False)
>               _ | fnName `elem` ["=", "<>", "<=", ">=", "<", ">"]
>                          && length argsType == 2
>                          && all isCompositeOrSetOfCompositeType argsType,
>                          Just a1 <- matchCompTypes argsType ->
>                          -- && compositesCompatible cat (head argsType) (head $ tail argsType) ->
>                              return (fnName, a1, typeBool, False)
>               --checked for all special cases, so run general case now
>               s -> lookupFn s argsType
>     where
>       lookupReturnType :: String -> [Type] -> Either [TypeError] Type
>       lookupReturnType s1 args = fmap (\(_,_,r,_) -> r) $ lookupFn s1 args
>       lookupFn :: String -> [Type] -> Either [TypeError] FunctionPrototype
>       lookupFn s1 = findCallMatch1 cat
>                              (if s1 == "u-" then "-" else s1)
>       fnName = map toLower fnName'
>       -- help the type inference for rowCtors. pretty unfinished. If we compare
>       -- two compatible row constructors, then replace any unknown types with the
>       -- pair type
>       matchCompTypes :: [Type] -> Maybe [Type]
>       matchCompTypes [a@(AnonymousRecordType as),b@(AnonymousRecordType bs)] =
>         if not (compositesCompatible cat a b)
>         then Nothing
>         else let (nt1,nt2) = unzip $ map (\(t,t1) -> case (t,t1) of
>                                          (UnknownType, u) -> (u,u)
>                                          (u, UnknownType) -> (u,u)
>                                          _ -> (t,t1)) $ zip as bs
>              in Just [AnonymousRecordType nt1,AnonymousRecordType nt2]
>       matchCompTypes [a,b] =
>         if not (compositesCompatible cat a b)
>         then Nothing
>         else Just [a,b]
>       matchCompTypes _ = Nothing


>
> findCallMatch1 :: Catalog -> String -> [Type] ->  Either [TypeError] FunctionPrototype
> findCallMatch1 cat f inArgs =
>     returnIfOnne [
>        exactMatch
>       ,binOp1UnknownMatch
>       ,polymorpicExactMatches
>       ,reachable
>       ,mostExactMatches
>       ,filteredForPreferred
>       ,unknownMatchesByCat]
>       [NoMatchingOperator f inArgs]
>     where
>       -- basic lists which roughly mirror algo
>       -- get the possibly matching candidates
>       initialCandList :: [FunctionPrototype]
>       initialCandList = filter (\(_,candArgs,_,_) ->
>                                   length candArgs == length inArgs) $
>                                map expandVariadic $ catLookupFns cat f
>
>       expandVariadic fp@(fn,a,r,v) =
>         if v
>           then case last a of
>                  ArrayType t -> (fn, na,r,v)
>                                 where na = init a ++ replicate (length inArgs - length a + 1) t
>                  _ -> fp --should be error
>           else fp
>
>       -- record what casts are needed for each candidate
>       castPairs :: [[ArgCastFlavour]]
>       castPairs = map (listCastPairs . getFnArgs) initialCandList
>
>       candCastPairs :: [ProtArgCast]
>       candCastPairs = {-traceIt "candCastPairs" $-} zip initialCandList castPairs
>
>       -- see if we have an exact match
>       exactMatch :: [ProtArgCast]
>       exactMatch = filterCandCastPairs (all (==ExactMatch)) candCastPairs
>
>       -- implement the one known, one unknown resolution for binary operators
>       binOp1UnknownMatch :: [ProtArgCast]
>       binOp1UnknownMatch = getBinOp1UnknownMatch candCastPairs
>
>       --collect possible polymorphic matches
>       polymorphicMatches :: [ProtArgCast]
>       polymorphicMatches = filterPolymorphics candCastPairs
>
>       polymorpicExactMatches :: [ProtArgCast]
>       polymorpicExactMatches = filterCandCastPairs (all (==ExactMatch)) polymorphicMatches
>
>       -- eliminate candidates for which the inargs cannot be casted to
>       reachable :: [ProtArgCast]
>       reachable = mergePolys (filterCandCastPairs (none (==CannotCast)) candCastPairs)
>                     polymorphicMatches
>
>       mostExactMatches :: [ProtArgCast]
>       mostExactMatches =
>         let inArgsBase = map (replaceWithBase cat) inArgs
>             exactCounts :: [Int]
>             exactCounts =
>               map ((length
>                       . filter (\(a1,a2) -> a1==replaceWithBase cat a2)
>                       . zip inArgsBase)
>                 . (\((_,a,_,_),_) -> a)) reachable
>             pairs = zip reachable exactCounts
>             maxm = maximum exactCounts
>         in case () of
>              _ | null reachable -> []
>                | maxm > 0 -> map fst $ filter (\(_,b) -> b == maxm) pairs
>                | otherwise -> []
>
>       -- keep the cands with the most casts to preferred types
>       preferredTypesCounts = countPreferredTypeCasts reachable
>       keepCounts = maximum preferredTypesCounts
>       itemCountPairs :: [(ProtArgCast,Int)]
>       itemCountPairs = zip reachable preferredTypesCounts
>       filteredForPreferred :: [ProtArgCast]
>       filteredForPreferred = map fst $ filter (\(_,i) -> i == keepCounts) itemCountPairs
>
>       -- collect the inArg type categories to do unknown inArg resolution
>       argCats :: [Either () String]
>       argCats = getCastCategoriesForUnknowns filteredForPreferred
>       unknownMatchesByCat :: [ProtArgCast]
>       unknownMatchesByCat = getCandCatMatches filteredForPreferred argCats
>
>       -------------
>
>       listCastPairs :: [Type] -> [ArgCastFlavour]
>       listCastPairs l = listCastPairs' inArgs l
>                         where
>                           listCastPairs' :: [Type] -> [Type] -> [ArgCastFlavour]
>                           listCastPairs' (ia:ias) (ca:cas) =
>                               (case () of
>                                  _ | ia == ca -> ExactMatch
>                                    | castableFromTo cat ImplicitCastContext ia ca ->
>                                        if forceRight (catPreferredType cat ca)
>                                          then ImplicitToPreferred
>                                          else ImplicitToNonPreferred
>                                    | otherwise -> CannotCast
>                               ) : listCastPairs' ias cas
>                           listCastPairs' [] [] = []
>                           listCastPairs' _ _ = error "internal error: mismatched num args in implicit cast algorithm"
>
>
>       getBinOp1UnknownMatch :: [ProtArgCast] -> [ProtArgCast]
>       getBinOp1UnknownMatch cands =
>           if not (isOperatorName f &&
>                   length inArgs == 2 &&
>                   count (==UnknownType) inArgs == 1)
>             then []
>             else let newInArgs =
>                          replicate 2 (if head inArgs == UnknownType
>                                         then inArgs !! 1
>                                         else head inArgs)
>                  in filter (\((_,a,_,_),_) -> a == newInArgs) cands
>
>       filterPolymorphics :: [ProtArgCast] -> [ProtArgCast]
>       filterPolymorphics cl =
>           let ms :: [ProtArgCast]
>               ms = filter canMatch polys
>               polyTypes :: [Maybe Type]
>               polyTypes = map resolvePolyType ms
>               polyTypePairs :: [(Maybe Type, ProtArgCast)]
>               polyTypePairs = zip polyTypes ms
>               keepPolyTypePairs :: [(Type, ProtArgCast)]
>               keepPolyTypePairs =
>                 mapMaybe (\(t,p) -> case t of
>                                            Nothing -> Nothing
>                                            Just t' -> Just (t',p))
>                               polyTypePairs
>               finalRows = map (\(t,p) -> instantiatePolyType p t)
>                               keepPolyTypePairs
>               --create the new cast lists
>               cps :: [[ArgCastFlavour]]
>               cps = map (listCastPairs . getFnArgs . fst) finalRows
>           in zip (map fst finalRows) cps
>           where
>             polys :: [ProtArgCast]
>             polys = filter (\((_,a,_,_),_) -> any (`elem`
>                                               -- bit hacky
>                                              [Pseudo Any
>                                              ,Pseudo AnyArray
>                                              ,Pseudo AnyElement
>                                              ,Pseudo AnyEnum
>                                              ,Pseudo AnyNonArray]) a) cl
>             canMatch :: ProtArgCast -> Bool
>             canMatch pac =
>                let ((_,fnArgs,_,_),_) = pac
>                in canMatch' inArgs fnArgs
>                where
>                  canMatch' [] [] = True
>                  canMatch' (ia:ias) (pa:pas) =
>                    case pa of
>                      Pseudo Any -> nextMatch
>                      Pseudo AnyArray -> isArrayType ia && nextMatch
>                      Pseudo AnyElement -> nextMatch
>                      Pseudo AnyEnum -> False
>                      Pseudo AnyNonArray -> if isArrayType ia
>                                              then False
>                                              else nextMatch
>                      _ -> True
>                    where
>                      nextMatch = canMatch' ias pas
>                  canMatch' _ _ = error "internal error: mismatched lists in canMatch'"
>             resolvePolyType :: ProtArgCast -> Maybe Type
>             resolvePolyType ((_,fnArgs,_,_),_) =
>                 {-trace ("\nresolving " ++ show fnArgs ++ " against " ++ show inArgs ++ "\n") $-}
>                 let argPairs = zip inArgs fnArgs
>                     typeList :: [Type]
>                     typeList = catMaybes $ flip map argPairs
>                                  $ \(ia,fa) -> case fa of
>                                                   Pseudo Any -> if isArrayType ia
>                                                                 then eitherToMaybe $ unwrapArray ia
>                                                                 else Just ia
>                                                   Pseudo AnyArray -> eitherToMaybe $ unwrapArray ia
>                                                   Pseudo AnyElement -> if isArrayType ia
>                                                                        then eitherToMaybe $ unwrapArray ia
>                                                                        else Just ia
>                                                   Pseudo AnyEnum -> Nothing
>                                                   Pseudo AnyNonArray -> Just ia
>                                                   _ -> Nothing
>                 in {-trace ("\nresolve types: " ++ show typeList ++ "\n") $-}
>                    case resolveResultSetType cat typeList of
>                      Left _ -> Nothing
>                      Right t -> Just t
>             instantiatePolyType :: ProtArgCast -> Type -> ProtArgCast
>             instantiatePolyType pac t =
>               let ((fn,a,r,v),_) = pac
>                   instArgs = swapPolys t a
>                   p1 = (fn, instArgs, swapPoly t r,v)
>               in let x = (p1,listCastPairs instArgs)
>                  in {-trace ("\nfixed:" ++ show x ++ "\n")-} x
>               where
>                 swapPolys :: Type -> [Type] -> [Type]
>                 swapPolys = map . swapPoly
>                 swapPoly :: Type -> Type -> Type
>                 swapPoly pit at =
>                   case at of
>                     Pseudo Any -> if isArrayType at
>                                     then ArrayType pit
>                                     else pit
>                     Pseudo AnyArray -> ArrayType pit
>                     Pseudo AnyElement -> if isArrayType at
>                                            then ArrayType pit
>                                            else pit
>                     Pseudo AnyEnum -> pit
>                     Pseudo AnyNonArray -> pit
>                     SetOfType (Pseudo AnyElement) -> if isArrayType at
>                                                      then SetOfType (ArrayType pit)
>                                                      else SetOfType pit
>                     _ -> at
>       --merge in the instantiated poly functions, with a twist:
>       -- if we already have the exact same set of args in the non poly list
>       -- as a poly, then don't include that poly
>       mergePolys :: [ProtArgCast] -> [ProtArgCast] -> [ProtArgCast]
>       mergePolys orig polys =
>           let origArgs = map (\((_,a,_,_),_) -> a) orig
>               filteredPolys = filter (\((_,a,_,_),_) -> a `notElem` origArgs) polys
>           in orig ++ filteredPolys
>
>       countPreferredTypeCasts :: [ProtArgCast] -> [Int]
>       countPreferredTypeCasts =
>           map (\(_,cp) -> count (==ImplicitToPreferred) cp)
>
>       -- Left () is used for inArgs which aren't unknown,
>       --                      and for unknowns which we don't have a
>       --                      unique category
>       -- Right s -> s is the single letter category at
>       --                           that position
>       getCastCategoriesForUnknowns :: [ProtArgCast] -> [Either () String]
>       getCastCategoriesForUnknowns cands =
>           filterArgN 0
>           where
>             candArgLists :: [[Type]]
>             candArgLists = map (\((_,a,_,_), _) -> a) cands
>             filterArgN :: Int -> [Either () String]
>             filterArgN n =
>                 if n == length inArgs
>                   then []
>                   else let targType = inArgs !! n
>                        in ((if targType /= UnknownType
>                               then Left ()
>                               else getCandsCatAt n) : filterArgN (n+1))
>                 where
>                   getCandsCatAt :: Int -> Either () String
>                   getCandsCatAt n' =
>                       let typesAtN = map (!!n') candArgLists
>                           catsAtN = map (forceRight . catTypeCategory cat) typesAtN
>                       in case () of
>                            --if any are string choose string
>                            _ | any (== "S") catsAtN -> Right "S"
>                              -- if all are same cat choose that
>                              | all (== head catsAtN) catsAtN -> Right $ head catsAtN
>                              -- otherwise no match, this will be
>                              -- picked up as complete failure to match
>                              -- later on
>                              | otherwise -> Left ()
>
>       getCandCatMatches :: [ProtArgCast] -> [Either () String] -> [ProtArgCast]
>       getCandCatMatches candsA cats = getMatches candsA 0
>          where
>            getMatches :: [ProtArgCast] -> Int -> [ProtArgCast]
>            getMatches cands n =
>                case () of
>                  _ | n == length inArgs -> cands
>                    | (inArgs !! n) /= UnknownType -> getMatches cands (n + 1)
>                    | otherwise ->
>                        let catMatches :: [ProtArgCast]
>                            catMatches = filter (\c -> Right (getCatForArgN n c) ==
>                                                      (cats !! n)) cands
>                            prefMatches :: [ProtArgCast]
>                            prefMatches = filter (forceRight . catPreferredType cat .
>                                                    getTypeForArgN n) catMatches
>                            keepMatches :: [ProtArgCast]
>                            keepMatches = if length prefMatches > 0
>                                            then prefMatches
>                                            else catMatches
>                        in getMatches keepMatches (n + 1)
>            getTypeForArgN :: Int -> ProtArgCast -> Type
>            getTypeForArgN n ((_,a,_,_),_) = a !! n
>            getCatForArgN :: Int -> ProtArgCast -> String
>            getCatForArgN n = forceRight . catTypeCategory cat . getTypeForArgN n
>
>       -- utils
>       -- filter a candidate/cast flavours pair by a predicate on each
>       -- individual cast flavour
>       filterCandCastPairs :: ([ArgCastFlavour] -> Bool)
>                           -> [ProtArgCast]
>                           -> [ProtArgCast]
>       filterCandCastPairs predi = filter (\(_,cp) -> predi cp)
>
>       getFnArgs :: FunctionPrototype -> [Type]
>       getFnArgs (_,a,_,_) = a
>       returnIfOnne [] e = Left e
>       returnIfOnne (l:ls) e = if length l == 1
>                               then Right $ getHeadFn l
>                               else returnIfOnne ls e
>
>       getHeadFn :: [ProtArgCast] -> FunctionPrototype
>       getHeadFn l =  let ((hdFn, _):_) = l
>                      in hdFn
>       none p = not . any p
>       count p = length . filter p
>
> data ArgCastFlavour = ExactMatch
>                     | CannotCast
>                     | ImplicitToPreferred
>                     | ImplicitToNonPreferred
>                       deriving (Eq,Show)
>

~~~~
resolveResultSetType -
partially implement the typing of results sets where the types aren't
all the same and not unknown
used in union,except,intersect columns, case, array ctor, values, greatest and least

algo:
if all inputs are same and not unknown -> that type
replace domains with base types
if all inputs are unknown then text
if the non unknown types aren't all in same category then fail
choose first input type that is a preferred type if there is one
choose last non unknown type that has implicit casts from all preceding inputs
check all can convert to selected type else fail

code is not as much of a mess as findCallMatch
~~~~

> resolveResultSetType :: Catalog -> [Type] -> Either [TypeError] Type
> resolveResultSetType cat inArgs = do
>   errorWhen (null inArgs) [TypelessEmptyArray]
>   returnWhen allSameType (head inArgs) $ do
>   returnWhen allSameBaseType (head inArgsBase) $ do
>   --returnWhen allUnknown (UnknownType) $ do
>   errorWhen (not allSameCat) [IncompatibleTypeSet inArgs]
>   returnWhen (isJust targetType &&
>               allConvertibleToFrom (fromMaybe (error "TypeConversion.resolveresultsettype 1: fromJust") targetType) inArgs)
>               (fromMaybe (error "TypeConversion.resolveresultsettype 2: fromJust") targetType) $ do
>   Left [IncompatibleTypeSet inArgs]
>   where
>      allSameType = all (== head inArgs) inArgs -- &&
>                      --head inArgs /= UnknownType
>      allSameBaseType = all (== head inArgsBase) inArgsBase &&
>                      head inArgsBase /= UnknownType
>      inArgsBase = map (replaceWithBase cat) inArgs
>      --allUnknown = all (==UnknownType) inArgsBase
>      allSameCat = let firstCat = catTypeCategory cat (head knownTypes)
>                   in all (\t -> catTypeCategory cat t == firstCat)
>                          knownTypes
>      targetType = case catMaybes [firstPreferred, lastAllConvertibleTo] of
>                     [] -> Nothing
>                     (x:_) -> Just x
>      firstPreferred = find (forceRight . catPreferredType cat) knownTypes
>      lastAllConvertibleTo = firstAllConvertibleTo (reverse knownTypes)
>      firstAllConvertibleTo (x:xs) = if allConvertibleToFrom x xs
>                                       then Just x
>                                       else firstAllConvertibleTo xs
>      firstAllConvertibleTo [] = Nothing
>      knownTypes = filter (/=UnknownType) inArgsBase
>      allConvertibleToFrom = all . flip (castableFromTo cat ImplicitCastContext)

todo:
cast empty array, where else can an empty array work?

================================================================================

= checkAssignmentValue

> checkAssignmentValid :: Catalog -> Type -> Type -> Either [TypeError] ()
> checkAssignmentValid cat from to =
>     if castableFromTo cat AssignmentCastContext from to
>     then Right ()
>     else Left [IncompatibleTypes to from]
>
> compositesCompatible :: Catalog -> Type -> Type -> Bool
> compositesCompatible cat =
>     castableFromTo cat ImplicitCastContext

> checkAssignmentsValid :: Catalog -> [Type] -> [Type] -> Either [TypeError] ()
> checkAssignmentsValid cat from to = do
>     -- special case for assigning to composite
>     let f = case to of
>                    [t] | isCompositeType t -> [AnonymousRecordType from]
>                    _ -> from
>     errorWhen (length f /= length to)
>               [WrongNumberOfColumns]
>     let ls = concat $ lefts $ zipWith (checkAssignmentValid cat) f to
>     errorWhen (not (null ls)) ls


================================================================================

= castable function

wrapper around the catalog to add a bunch of extra valid casts

> castableFromTo :: Catalog -> CastContext -> Type -> Type -> Bool
> castableFromTo cat cc from to =
>   --trace ("check cast " ++ show from ++ "->" ++ show to) $
>   -- put this here to avoid having to write it everywhere else
>   from == to
>   -- unknown can be implicitly cast to anything (is this completely true?)
>   || from == UnknownType
>   --hack?
>   || to == UnknownType
>   -- check base types of domains
>   || ((isDomainType from || isDomainType to)
>       && castableFromTo cat cc (replaceWithBase cat from)
>                                (replaceWithBase cat to))
>   -- check the casts listed in the catalog
>   || forceRight (catCast cat cc from to)
>   -- implicitcast => assignment cast
>   || (cc == AssignmentCastContext
>       && forceRight (catCast cat ImplicitCastContext from to))
>   -- can assign composite to record
>   || (cc == AssignmentCastContext
>       && isCompOrSetoOfComp from
>       && case to of
>            PgRecord _ -> True
>            _ -> False)
>   -- check unboxing: wrapped single attribute
>   || recurseTransFrom (unboxedSingleType from)
>   || recurseTransTo (unboxedSingleType to)
>   -- check unboxing: wrapped composite
>   || recurseTransFrom (unboxedSetOfType from)
>   || recurseTransTo (unboxedSetOfType to)
>   -- check composites compatible by comparing attribute types
>   || case (getCompositeTypes from
>           ,getCompositeTypes to) of
>        -- zip almost does the right thing here, needs a bit of tweaking
>        (Just ft, Just tt) | length ft == length tt -> all (uncurry $ castableFromTo cat cc) $ zip ft tt
>        _ -> False
>   where
>
>     getCompositeTypes (NamedCompositeType n) =
>         Just $ map snd $ fromRight [] $ catCompositePublicAttrs cat [] n
>     getCompositeTypes (CompositeType t) = Just $ map snd t
>     getCompositeTypes (AnonymousRecordType t) = Just t
>     getCompositeTypes (PgRecord Nothing) = Nothing
>     getCompositeTypes (PgRecord (Just t)) = getCompositeTypes t
>     getCompositeTypes _ = Nothing
>
>     isCompOrSetoOfComp (SetOfType c) = isCompositeType c
>     isCompOrSetoOfComp c = isCompositeType c
>
>     unboxedSingleType (SetOfType (CompositeType [(_,t)])) = Just t
>     unboxedSingleType (PgRecord (Just t)) = unboxedSingleType t
>     unboxedSingleType _ = Nothing
>
>     unboxedSetOfType (SetOfType a) = Just a
>     unboxedSetOfType (PgRecord (Just t)) = unboxedSetOfType t
>     unboxedSetOfType _ = Nothing
>
>     recurseTransFrom = maybe False (flip (castableFromTo cat cc) to)
>     recurseTransTo = maybe False (castableFromTo cat cc from)
>
> replaceWithBase :: Catalog -> Type -> Type
> replaceWithBase cat t@(DomainType _) = forceRight $ catDomainBaseType cat t
> replaceWithBase _ t = t
