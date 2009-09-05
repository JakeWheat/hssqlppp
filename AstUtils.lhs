Copyright 2009 Jake Wheat

This file collects a bunch of utility functions used in type checking,
etc., asts.

> module AstUtils where

> import Data.Maybe
> import Data.List

> import TypeType
> import FnTypes
> import PGTypes


> data OperatorType = BinaryOp | PrefixOp | PostfixOp
>                   deriving (Eq,Show)

for now, assume that all the overloaded operators that have the
same name are all either binary, prefix or postfix, otherwise the
getoperatortype would need the types of the arguments to determine
the operator type, and the parser would have to be a lot cleverer

> getOperatorType :: String -> OperatorType
> getOperatorType s = case () of
>                       _ | any (\(x,_,_) -> x == s) binaryOperatorTypes ->
>                             BinaryOp
>                         | any (\(x,_,_) -> x == s ||
>                                            (x=="-" && s=="u-"))
>                               prefixOperatorTypes ->
>                             PrefixOp
>                         | any (\(x,_,_) -> x == s) postfixOperatorTypes ->
>                             PostfixOp
>                         | otherwise ->
>                             error $ "don't know flavour of operator " ++ s

================================================================================
Error reporting

> data Message = Error MySourcePos MessageStuff
>              | Warning MySourcePos MessageStuff
>              | Notice MySourcePos MessageStuff
>                deriving (Eq)
>
> data MessageStuff = ContinueNotInLoop
>                   | CustomMessage String
>                     deriving (Eq,Show)
>
> instance Show Message where
>    show m = showMessage m
>
> showMessage :: Message -> [Char]
> showMessage m = case m of
>                   Error sp s -> showit "Error" sp s
>                   Warning sp s -> showit "Warning" sp s
>                   Notice sp s -> showit "Notice" sp s
>                 where
>                   showit lev (fn,l,c) s = lev ++ "\n" ++ fn ++ ":"
>                                           ++ show l ++ ":" ++ show c ++ ":\n"
>                                           ++ show s ++ "\n"
>

================================================================================

= type checking utils

== propagateunknownerror

shortcut which should end up being used in every bit of type checking
to make sure once we have an bad typed node, the type error propagates
up the node tree nicely, instead of creating a cascade of type
errors. Currently also used to propagate unknowntype in the same way.

if the first argument is unknown or type error, pass it on otherwise
use the second argument


> propagateUnknownError :: Type -> Type -> Type
> propagateUnknownError t t1 =
>     case t of
>       a@(TypeError _ _) -> a
>       UnknownType -> UnknownType
>       TypeList l -> doTypeList l
>       _ -> t1
>     where
>       -- run through the type list, if there are any eorors, collect
>       -- them all into a list
>       -- otherwise, if there are any unknowns, then the type is
>       -- unknown
>       -- otherwise, keep the list the same
>       doTypeList ts =
>           let unks = filter (\u -> case u of
>                                      UnknownType -> True
>                                      _ -> False) ts
>               errs = filter (\u -> case u of
>                                      TypeError _ _ -> True
>                                      _ -> False) ts
>           in case () of
>                _ | length errs > 0 -> case () of
>                                         _ | length errs == 1 -> head errs
>                                           | otherwise -> TypeList errs
>                  | length unks > 0 -> UnknownType
>                  | otherwise -> t1

================================================================================

= mini type checking dsl

create a little dsl to do declarative type checking for TypeList
types. We can pass in:
* the list of types,
* a declarative constraint on this list, and
* a method for determining the resultant type.

Not sure all these constraint variations will be needed. or if this
is going to last as the type checking is made more
comprehensive. Currently used for type checking keyword operators, and
in case expressions, maybe some other places.

> data ArgsCheck
>     -- check any number of args, all have the same type
>     = AllSameType Type
>     -- check any one or more args, all have the same type
>     | AllSameType1 Type
>     -- check any one or more args, all have the same type as eachother
>     | AllSameType1Any
>     | AllSameTypeAny
>     -- check all same type, exact number of args
>     | AllSameTypeNum Type Int
>     -- check all same type as each other, exact number of args
>     | AllSameTypeNumAny Int
>     -- check type list matches given list
>     | ExactList TypeList
>     -- check type list passes predicate list respectively
>     | ExactPredList ArgCheckList
>     -- check all types pass single predicate, exact number
>     | AllSameTypePredNum ArgCheck Int

> type TypeList = [Type]
> type ArgCheckList = [ArgCheck]

> data ArgCheck = ArgCheck TypePred TypePredError

> type TypePred = (Type -> Bool)
> type TypePredError = (Type -> TypeErrorInfo)

> exactType :: Type -> ArgCheck
> exactType t = ArgCheck (t==) (WrongType t)

> checkPredList :: MySourcePos -> [ArgCheck] -> [Type] -> [Type]
> checkPredList sp achks ats =
>     if length achks /= length ats
>       then [TypeError sp
>             (WrongNumArgs
>              (length achks)
>              (length ats))]
>       else checkArg 0 [] achks ats
>     where
>       checkArg :: Int -> [Type] -> [ArgCheck] -> [Type] -> [Type]
>       checkArg n acc ((ArgCheck chk err):chks) (t:ts) =
>           if chk t
>             then checkArg (n+1) acc chks ts
>             else checkArg (n+1) (acc ++ [TypeError sp $ err t]) chks ts
>       checkArg _ acc [] [] = acc
>       checkArg _ _ _ _ = error "internal error: pattern match failure in checkArg"

> data RetType
>     -- always returns fixed type
>     = ConstRetType Type
>     -- returns same type as argument n
>     | RetTypeAsArgN Int
>     -- use generic fn on arg list to produce return type
>     | RetTypeFun RetTypeFunner


> type RetTypeFunner = ([Type] -> Type)

the actual dsl engine:

> checkTypes :: MySourcePos -> Type -> ArgsCheck -> RetType -> Type
> checkTypes sp tl@(TypeList l) argC retT =
>     --1: check tl for errors or unknowns
>     --2: check the args against the constraints,
>     --  filter this for unknown or errors
>     --  (it returns Just error, or Nothing if ok)
>     --3: get the return type, and check that for unknowns or errors
>     --4: success, return the result type
>     --todo: where can implicit casts be used in this type checking?
>     let c = case checkArgs of
>               Just t -> t
>               Nothing -> getRetType
>     in pe tl $ pe c c
>     where
>       getRetType =
>           case retT of
>             ConstRetType t -> t
>             RetTypeAsArgN n -> l !! n
>             RetTypeFun f -> f l
>       checkArgs =
>           case argC of
>             AllSameType t -> checkArgListMatches t l
>             AllSameType1 t | length l == 0 ->
>                                 Just $ te NeedOneOrMoreArgs
>                            | otherwise -> checkArgListMatches t l
>             AllSameTypeNum t n | length l /= n ->
>                                     Just $ te $ WrongNumArgs n (length l)
>                                | otherwise -> checkArgListMatches t l
>             AllSameTypeNumAny n | length l /= n ->
>                                     Just $ te $ WrongNumArgs n (length l)
>                                 | otherwise -> checkArgListMatches (head l) l
>             AllSameTypeAny -> checkArgListMatches (head l) l
>             AllSameType1Any | length l == 0 ->
>                                 Just $ te NeedOneOrMoreArgs
>                             | otherwise -> checkArgListMatches (head l) l
>             ExactList ts | ts == l -> Nothing
>                          | canImplicitCast l ts -> Nothing
>                          | otherwise ->
>                               Just $ te $ WrongTypeList ts l
>             ExactPredList chks -> case checkPredList sp chks l of
>                                     x | length x == 0 -> Nothing
>                                       | otherwise -> Just $ TypeList x
>             AllSameTypePredNum p n -> case checkPredList sp
>                                              (replicate n p)
>                                              l of
>                                         x | length x == 0 -> Nothing
>                                           | otherwise -> Just $ TypeList x
>       checkArgListMatches tc tcs = if all (==tc) tcs
>                                      then Nothing
>                                      else Just $ te (WrongTypes tc tcs)
>       te = TypeError sp
>       pe = propagateUnknownError
>       canImplicitCast (f:fs) (t:ts) =
>           {-trace (show (f:fs) ++ show (t:ts)) $-}
>           (f == t || implicitlyCastableFromTo f t) && canImplicitCast fs ts
>       canImplicitCast [] [] = True
>       canImplicitCast _ _ = False

> checkTypes _ x _ _ = error $ "can't check types of non type list: " ++ show x


> checkTypeExists :: MySourcePos -> Type -> Type
> checkTypeExists sp t =
>     if t `elem` defaultTypeNames
>       then t
>       else TypeError sp (UnknownTypeError t)


aliases to protect client code if/when the ast canonical names are
changed

> typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4,
>   typeFloat8,typeVarChar,typeChar,typeBool :: Type
> typeSmallInt = ScalarType "int2"
> typeBigInt = ScalarType "int8"
> typeInt = ScalarType "int4"
> typeNumeric = ScalarType "numeric"
> typeFloat4 = ScalarType "float4"
> typeFloat8 = ScalarType "float8"
> typeVarChar = ScalarType "varchar"
> typeChar = ScalarType "char"
> typeBool = ScalarType "bool"

> canonicalizeType :: Type -> Type
> canonicalizeType t =
>     case t of
>       ScalarType s -> cName s
>       ArrayType a -> ArrayType $ canonicalizeType a
>       SetOfType a -> SetOfType $ canonicalizeType a
>       t1@_ -> t1
>     where
>       cName s = case () of
>                   _ | s `elem` smallIntNames -> typeSmallInt
>                     | s `elem` intNames -> typeInt
>                     | s `elem` bigIntNames -> typeBigInt
>                     | s `elem` numericNames -> typeNumeric
>                     | s `elem` float4Names -> typeFloat4
>                     | s `elem` float8Names -> typeFloat8
>                     | s `elem` varcharNames -> typeVarChar
>                     | s `elem` charNames -> typeChar
>                     | s `elem` boolNames -> typeBool
>                     | otherwise -> ScalarType s
>       smallIntNames = ["int2", "smallint"]
>       intNames = ["int4", "integer", "int"]
>       bigIntNames = ["int8", "bigint"]
>       numericNames = ["numeric", "decimal"]
>       float4Names = ["real", "float4"]
>       float8Names = ["double precision", "float"]
>       varcharNames = ["character varying", "varchar"]
>       charNames = ["character", "char"]
>       boolNames = ["boolean", "bool"]

================================================================================

= function and operator tables
these hold the types of functions and operators for lookup and checking
they live in fntypes.hs

> allOpsAndFns :: [FunctionPrototype]
> allOpsAndFns = binaryOperatorTypes
>                ++ prefixOperatorTypes
>                ++ postfixOperatorTypes
>                ++ functionTypes

> keywordBinaryOperatorTypes :: [(KeywordOperator,[Type],Type)]
> keywordBinaryOperatorTypes = [
>  (And, [typeBool, typeBool], typeBool),
>  (Or, [typeBool, typeBool], typeBool),
>  (Like, [ScalarType "text", ScalarType "text"], typeBool)]
> keywordUnaryOperatorTypes :: [(KeywordOperator,[Type],Type)]
> keywordUnaryOperatorTypes = [
>  (Not, [typeBool], typeBool),
>  (IsNull, [Pseudo Any], typeBool),
>  (IsNotNull, [Pseudo Any], typeBool)]

> allKeywordOps :: [(KeywordOperator, [Type], Type)]
> allKeywordOps = keywordBinaryOperatorTypes ++ keywordUnaryOperatorTypes


this utility fn is used to check that the operator and function
prototypes only contain types listed in the defaultTypeNames list

> checkFunctionTypes :: [(String, [Type])]
> checkFunctionTypes =
>     catMaybes $ map (\(f,a,r) ->
>                      let ts = (r:a)
>                          badts = filter (not . knownType) ts
>                      in if length badts == 0
>                           then Nothing
>                           else Just (f, badts)
>                     ) allOpsAndFns
>     where
>       knownType :: Type -> Bool
>       knownType l = case l of
>                       Pseudo _ -> True
>                       t@(ScalarType _) -> t `elem` defaultTypeNames
>                       ArrayType t -> knownType t
>                       SetOfType t -> knownType t
>                       _ -> error "internal error: unknown type in function proto"

todo:
polymorphic functions
row ctor implicitly and explicitly cast to a composite type
cast empty array

findCallMatch - partially implements the type conversion rules for
finding an operator or function match given a name and list of
arguments with partial or fully specified types

TODO:
namespaces
function style casts not in catalog
variadic args
default args
domains -> base type

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


findCallMatch is a bit of a mess

> type FunctionPrototype = (String,[Type],Type)
> type ProtArgCast = (FunctionPrototype, [ArgCastFlavour])

> findCallMatch :: MySourcePos -> String -> [Type] ->  Either Type FunctionPrototype
> findCallMatch sp f inArgs =
>     case () of
>          _ | length exactMatch == 1 -> Right $ getHeadFn exactMatch
>            | length binOp1UnknownMatch == 1 -> Right $ getHeadFn binOp1UnknownMatch
>            | length reachable == 1 -> Right $ getHeadFn reachable
>            | length filteredForPreferred == 1 -> Right $ getHeadFn filteredForPreferred
>            | length unknownMatchesByCat == 1 ->  Right $ getHeadFn unknownMatchesByCat
>            | otherwise -> {-trace ("exact matches " ++ show exactMatch) $
>                           trace ("\nreachable " ++ show reachable) $
>                           trace ("\nfilteredforpreferred " ++ show filteredForPreferred) $
>                           trace ("\nargCats " ++ show argCats) $
>                           trace ("\nunknownMatchesByCat " ++ show unknownMatchesByCat) -}
>                           Left $ TypeError sp (NoMatchingOperator f inArgs)
>     where
>       -- basic lists which roughly mirror algo
>       -- get the possibly matching candidates
>       icl = initialCandList
>
>       -- record what casts are needed for each candidate
>       castPairs :: [[ArgCastFlavour]]
>       castPairs = map listCastPairs $ map getFnArgs icl
>
>       candCastPairs :: [ProtArgCast]
>       candCastPairs = zip icl castPairs
>
>       -- see if we have an exact match
>       exactMatch :: [ProtArgCast]
>       exactMatch = getExactMatch candCastPairs
>
>       -- implement the one known, one unknown resolution for binary operators
>       binOp1UnknownMatch :: [ProtArgCast]
>       binOp1UnknownMatch = getBinOp1UnknownMatch candCastPairs
>
>       -- eliminate candidates for which the inargs cannot be casted to
>       reachable :: [ProtArgCast]
>       reachable = filterCandCastPairs (none (==CannotCast)) candCastPairs
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
>       initialCandList :: [FunctionPrototype]
>       initialCandList = filter (\(candf,candArgs,_) ->
>                                   (candf,length candArgs) == (f,length inArgs))
>                           allOpsAndFns
>
>       listCastPairs :: [Type] -> [ArgCastFlavour]
>       listCastPairs l = listCastPairs' inArgs l
>                         where
>                           listCastPairs' :: [Type] -> [Type] -> [ArgCastFlavour]
>                           listCastPairs' (ia:ias) (ca:cas) =
>                               (case () of
>                                  _ | ia == ca -> ExactMatch
>                                    | implicitlyCastableFromTo ia ca ->
>                                        case isPreferredType ca of
>                                          True -> ImplicitToPreferred
>                                          False -> ImplicitToNonPreferred
>                                    | otherwise -> CannotCast
>                               ) : listCastPairs' ias cas
>                           listCastPairs' [] [] = []
>                           listCastPairs' _ _ = error "internal error: mismatched num args in implicit cast algorithm"
>
>       getExactMatch :: [ProtArgCast] -> [ProtArgCast]
>       getExactMatch ccp  = filterCandCastPairs (all (==ExactMatch)) ccp
>
>       getBinOp1UnknownMatch :: [ProtArgCast] -> [ProtArgCast]
>       getBinOp1UnknownMatch cands =
>           if not (isOperator f &&
>                   length inArgs == 2 &&
>                   (count (==UnknownStringLit) inArgs) == 1)
>             then []
>             else let newInArgs =
>                          take 2 $ repeat (if head inArgs == UnknownStringLit
>                                             then inArgs !! 1
>                                             else head inArgs)
>                  in filter (\((_,a,_),_) -> a == newInArgs) cands
>
>       countPreferredTypeCasts :: [ProtArgCast] -> [Int]
>       countPreferredTypeCasts l =
>           map (\(_,cp) -> count (==ImplicitToPreferred) cp) l
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
>             candArgLists = map (\((_,a,_), _) -> a) cands
>             filterArgN :: Int -> [Either () String]
>             filterArgN n =
>                 if n == length inArgs
>                   then []
>                   else let targType = inArgs !! n
>                        in ((if targType /= UnknownStringLit
>                               then Left ()
>                               else getCandsCatAt n) : filterArgN (n+1))
>                 where
>                   getCandsCatAt :: Int -> Either () String
>                   getCandsCatAt n' =
>                       let typesAtN = map (!!n') candArgLists
>                           catsAtN = map getTypeCategory typesAtN
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
>                    | (inArgs !! n) /= UnknownStringLit -> getMatches cands (n + 1)
>                    | otherwise ->
>                        let catMatches :: [ProtArgCast]
>                            catMatches = filter (\c -> Right (getCatForArgN n c) ==
>                                                      (cats !! n)) cands
>                            prefMatches :: [ProtArgCast]
>                            prefMatches = filter (\c -> isPreferredType
>                                                        (getTypeForArgN n c)) catMatches
>                            keepMatches :: [ProtArgCast]
>                            keepMatches = if length prefMatches > 0
>                                            then prefMatches
>                                            else catMatches
>                        in getMatches keepMatches (n + 1)
>            getTypeForArgN :: Int -> ProtArgCast -> Type
>            getTypeForArgN n ((_,a,_),_) = a !! n
>            getCatForArgN :: Int -> ProtArgCast -> String
>            getCatForArgN n c = getTypeCategory (getTypeForArgN n c)
>
>       -- utils
>       -- filter a candidate/cast flavours pair by a predicate on each
>       -- individual cast flavour
>       filterCandCastPairs :: ([ArgCastFlavour] -> Bool)
>                           -> [ProtArgCast]
>                           -> [ProtArgCast]
>       filterCandCastPairs predi ccp = filter (\(_,cp) -> predi cp) ccp
>
>       getFnArgs :: FunctionPrototype -> [Type]
>       getFnArgs (_,a,_) = a
>
>       getHeadFn :: [ProtArgCast] -> FunctionPrototype
>       getHeadFn l =  let ((hdFn, _):_) = l
>                      in hdFn
>
>       none p l = not (any p l)
>       count p l = length $ filter p l
>
> data ArgCastFlavour = ExactMatch
>                     | CannotCast
>                     | ImplicitToPreferred
>                     | ImplicitToNonPreferred
>                       deriving (Eq,Show)
>
> isOperator :: String -> Bool
> isOperator s = any (`elem` "+-*/<>=~!@#%^&|`?") s
>
> isPreferredType :: Type -> Bool
> isPreferredType t = case find (\(t1,_,_)-> t1==t) typeCategories of
>                       Nothing -> error $ "internal error, couldn't find type category information: " ++ show t
>                       Just (_,_,p) -> p
>
> getTypeCategory :: Type -> String
> getTypeCategory t = case find (\(t1,_,_)-> t1==t) typeCategories of
>                       Nothing -> error $ "internal error, couldn't find type category information: " ++ show t
>                       Just (_,c,_) -> c
> 
> 
> implicitlyCastableFromTo :: Type -> Type -> Bool
> implicitlyCastableFromTo from to = from == UnknownStringLit ||
>                                      any (==(from,to,ImplicitCastContext)) castTable
> 


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


> resolveResultSetType :: MySourcePos -> [Type] -> Type
> resolveResultSetType sp inArgs = propagateUnknownError (TypeList inArgs) $
>     case () of
>       _ | length inArgs == 0 -> TypeError sp TypelessEmptyArray
>         | allSameType -> head inArgs
>         --todo: do domains
>         | allUnknown -> ScalarType "text"
>         | not allSameCat -> TypeError sp (IncompatibleTypes inArgs)
>         | isJust targetType &&
>           allConvertibleToFrom
>             (fromJust targetType)
>             inArgs -> fromJust targetType
>         | otherwise -> TypeError sp (IncompatibleTypes inArgs)
>    where
>      allSameType = all (== head inArgs) inArgs && head inArgs /= UnknownStringLit
>      allUnknown = all (==UnknownStringLit) inArgs
>      allSameCat = let firstCat = getTypeCategory (head knownTypes)
>                   in all (\t -> getTypeCategory t == firstCat) knownTypes
>      targetType = case catMaybes [firstPreferred, lastAllConvertibleTo] of
>                     [] -> Nothing
>                     (x:_) -> Just x
>      firstPreferred = find isPreferredType knownTypes
>      lastAllConvertibleTo = firstAllConvertibleTo (reverse knownTypes)
>      firstAllConvertibleTo (x:xs) = if allConvertibleToFrom x xs
>                                       then Just x
>                                       else firstAllConvertibleTo xs
>      firstAllConvertibleTo [] = Nothing
>      matchOrImplicitToFrom t t1 = t == t1 || implicitlyCastableFromTo t1 t
>      knownTypes = filter (/=UnknownStringLit) inArgs
>      allConvertibleToFrom t ts = all (matchOrImplicitToFrom t) ts
