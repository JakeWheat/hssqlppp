

TODO: most of this code will move to the internals type conversion.
rewrite the code to be nice and literate explaining everything

> {-# LANGUAGE OverloadedStrings,LambdaCase,MultiWayIf,PatternGuards #-}
> module Database.HsSqlPpp.Internals.TypeChecking.TypeConversion.TypeConversion2
>        (matchApp,LitArg(..)) where

TODO: explicit imports

> import Control.Monad
> --import Control.Applicative
> --import Control.Arrow

> import Data.Text ()
> import qualified Data.Text as T
> import Data.Maybe
> import Data.List
> import Data.Either
> import Data.Char
> import Control.Arrow

> import Text.Show.Pretty
> import Debug.Trace

> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> --import Database.HsSqlPpp.Internals.Catalog.DefaultTemplate1Catalog
> --import Database.HsSqlPpp.Utils.Utils

> -- import Database.HsSqlPpp.Internals.TypeChecking.OldTypeConversion
> import Database.HsSqlPpp.Internals.Dialect
> -- import qualified Database.HsSqlPpp.Internals.TypeChecking.SqlTypeConversion as TSQL


three kinds of type conversion where we might have to insert implicit
casts:

function overload resolution
result set type resolution
is assignment valid

The arg types take a LitArg since we want to say the result type of e.g.
substring(x from 2 for 3) as varchar(3), but if we get
substring(x from a for b) then we don't know the precision from the
for part (but it can't be bigger than the x)

It also supports stuff like odbc convert whose return type depends on
the identifier in the second parameter.

for the special cases for null and precision, have a map of functions
from the function name to the special case transform function which
modifies the return type. Then can try to isolate the special cases a
little bit -> probably need some other special cases, add hooks and
maps of functions for these too

> data LitArg = NumLitArg String
>             | StringLitArg String
>             | NullLitArg
>             | BooleanLitArg Bool
>               deriving (Show,Eq)
>               -- todo: add other literals if needed


> type MyFunType = (CatName, [Type], Type, Bool)

match app matches a function + argument types, and determines (using a
bunch of hacks and special cases) the precision, scale and nullability
of the result type also.

> matchApp :: Dialect
>          -> Catalog
>          -> [NameComponent]
>          -> [(TypeExtra, Maybe LitArg)]
>          -> Either [TypeError] ([TypeExtra],TypeExtra)
> matchApp d cat appName argTypes = do
>      x <- findMatchingApp d cat appName
>                    (map (first teType) argTypes)
>      return $ fixNP x
>   where

todo: fix nulls and precision

precision and scale:

default is to match the precision and scale of any matching input args
or to choose the default for that type

for nulls: default is to assume function produces nullable if any
input args are nullable, and doesn't produce nullable otherwise

>     fixNP :: MyFunType -> ([TypeExtra],TypeExtra)
>     fixNP (_,ts,r,_) =
>         let anyInputsNull = isJust $ find (teNullable . fst) argTypes
>             -- copy nullability of input types
>             carryNulls = zipWith (\(a,_) b -> lt b $ teNullable a)
>                          argTypes ts
>         in (carryNulls, lt r anyInputsNull)
>     lt ty n = TypeExtra ty Nothing Nothing n


find matching app is the code which matches a function prototype to a
list of input argument types, dealing with implicit casts and
overloaded functions. It is based on the algorithm in postgresql.

> findMatchingApp :: Dialect
>                 -> Catalog
>                 -> [NameComponent]
>                 -> [(Type, Maybe LitArg)]
>                 -> Either [TypeError] (CatName,[Type],Type,Bool)
> findMatchingApp _d cat appName argTypes =
>     (\case
>         -- represents a short cut error
>         Left (Left e) -> Left e
>         -- represents a short cut valid result
>         Left (Right r) -> Right r
>         -- represents a normal result at the end
>         Right r -> Right r) $ do
>     let

1. get all the candidates - matching by name


todo: variadic stuff, default stuff
todo: if there are multiple matches with different schemas
     only keep the matches in the first schema in the search path

todo: deal with case and with schemas

todo: polymorphic

>         nameMatches :: [MyFunType]
>         nameMatches = catLookupFns cat appName'
>         -- create a map from arg types to prototype
>         nameMatchMap :: [([Type], MyFunType)]
>         nameMatchMap = map (\v@(_,as,_,_) -> (as,v)) nameMatches
>         -- the raw input types to match against
>         rawArgTypes = map fst argTypes

2. if there is one candidate with the exact args - choose it

>         exactMatches :: [MyFunType]
>         exactMatches = map snd $ flip filter nameMatchMap
>                                       ((==rawArgTypes) . fst)

if this is a binary operator, and one of the types is unknown and the
other is known, and there is onne exact match if the unknown is made
to match the known type, then select it

>         oneKnown = case rawArgTypes of
>                        [UnknownType,UnknownType] -> Nothing
>                        [UnknownType,t] -> Just t
>                        [t,UnknownType] -> Just t
>                        _ -> Nothing
>         binaryOpKnownUnknownMatches :: [MyFunType]
>         binaryOpKnownUnknownMatches =
>             case () of
>                 _ | isOperatorName appName'
>                   , Just t <- oneKnown
>                   -> map snd $ flip filter nameMatchMap
>                                       ((==[t,t]) . fst)
>                   | otherwise -> []

2.5 check for type conversion function?: function name is name of type
    only has one argument, this type is unknown or castable to target

>         typeConversionMatch :: [MyFunType]
>         typeConversionMatch = case (appName', rawArgTypes) of
>             _ -> [] -- todo

3. discard candidates which cannot be reached by implicit casts
   convert domains to base types
   keep only candidates which have the most exact matches
     if one: use it
     if none: still consider them all

>         reachableViaImplicitCasts :: [MyFunType]
>         reachableViaImplicitCasts =
>             let candReachableViaImplicitCasts as =
>                     length rawArgTypes == length as
>                     && and (zipWith canImplicitCastOrSame rawArgTypes as)
>             in map snd $ flip filter nameMatchMap
>                (candReachableViaImplicitCasts . fst)

4. keep candidates which accept the most preferred types where type
   conversion is needed: if one, use it
   else continue with all

>         acceptsMostPreferredTypes :: [MyFunType]
>         acceptsMostPreferredTypes =
>             let preferredTypeCounts :: [(Int,MyFunType)]
>                 preferredTypeCounts = flip map reachableViaImplicitCasts
>                     $ \v@(_,as,_,_) ->
>                            (length $ filter id $
>                             zipWith isCastToPreferred rawArgTypes as
>                            ,v)
>                 maxCount = maximum $ map fst preferredTypeCounts
>             in map snd $ filter ((==maxCount) .fst) preferredTypeCounts

Keep all candidates if none accept preferred types. If only one
candidate remains, use it; else continue to the next step.

>         acceptsMostPreferredNextStep :: [MyFunType]
>         acceptsMostPreferredNextStep =
>             if null acceptsMostPreferredTypes
>             then reachableViaImplicitCasts
>             else acceptsMostPreferredTypes

5. if any input types are unknown:
for each unknown

a) select string cat for each position if any cands accept string in
that position

b) if all the cands accept the same category for that position, choose
it, otherwise fail

discard any candidates not matching the selected category (this can
only happen if string was chosen for an unknown)

for each unknown, if any cands accept the preferred type in that
position, drop all the ones which don't accept that type in that
position
if one left: use it
else: keep all for next


transpose the arguments
calculate the category for each argument:
  will be just string if any strings
  will be just cat if all the same cat
  otherwise nothing

This doesn't take into account which positions are unknown in the
input arg list yet.

>         transposedCandidateArgs = transpose $ map (\(_,x,_,_) -> x)
>                                   acceptsMostPreferredNextStep

>         argumentCategories :: [Maybe T.Text]
>         argumentCategories =
>             let resolveTypeSetCat ts =
>                     case () of
>                      _ | any (==UnknownType) ts -> Just "unk"
>                        | Right (c:cs) <- mapM (catTypeCategory cat) ts
>                        , all (==c) cs -> Just c
>                        | otherwise -> Nothing
>             in map resolveTypeSetCat transposedCandidateArgs

then: zip with the unknown positions
  if we get any pairs of unknown + nothing for the cat:
  fail with ambiguous error

>         chooseCat :: Type -> Maybe T.Text
>                   -> Either [TypeError] (Maybe T.Text)
>         chooseCat a x = case (a,x) of
>              (UnknownType,Nothing) ->Left [AmbiguousOperator appName' rawArgTypes]
>              (UnknownType,Just y) -> Right (Just y)
>              (_,_) -> Right Nothing
>     let _argumentCategoriesNeeded :: Either [TypeError] [Maybe T.Text]
>         _argumentCategoriesNeeded =
>             zipWithM chooseCat rawArgTypes argumentCategories

TODO: filter the cands by argument categories needed
keep the left, if we need to use bestPreferredMatches then the left
escapes

>         {-matchesPreferredCategories =
>             let candMatches (_,ts,_,_) cn =
>                     catTypeCategory-}

then:
  convert the list of just cat to a list of preferred types
  do another transpose of the remaining functions and
  zip with the preferred type
  keep the preferred type as just if any cands match this preferred
   type, otherwise set to nothing

>     let myIsPreferred ty = either (const False) (const True)
>                            $ catPreferredType cat ty
>         choosePreferredType :: [Type] -> Maybe Type
>         choosePreferredType ts =
>             let pts = filter myIsPreferred ts
>             in case pts of
>                    (t:_) -> Just t
>                    [] -> Nothing

>         preferredTypes :: [Maybe Type]
>         preferredTypes = map choosePreferredType transposedCandidateArgs


now have a list of just/nothings with justs for the types which have
to match

filter the cands using this just list
now have the final best prefered match list and can
  either return one if there is one
  return ambiguous if there is more than one ?is this possible
  or return ambiguous with the previous list if none get through this
    filter

>         matchesPreferred (_,ts,_,_) =
>             let f _ Nothing = True
>                 f t (Just pt) = t == pt
>             in and $ zipWith f ts preferredTypes
>         bestPreferredMatches :: [MyFunType]
>         bestPreferredMatches =
>             filter matchesPreferred acceptsMostPreferredNextStep

>         bestPreferredMatchesNextStep :: [MyFunType]
>         bestPreferredMatchesNextStep =
>             if null bestPreferredMatches
>             then acceptsMostPreferredTypes
>             else bestPreferredMatches

6. if there are unknown and known, and all the knowns are the same,
assume the unknowns to be this type. If there is one match, use it

>         hasUnknown = isJust $ find (==UnknownType) rawArgTypes
>         allKnownsType :: Maybe Type
>         allKnownsType =
>             let allNonUnknowns = filter (/=UnknownType) rawArgTypes
>             in case allNonUnknowns of
>                    [] -> Nothing
>                    (x:xs) | all (==x) xs -> Just x
>                           | otherwise -> Nothing
>         allUnknownsMatchAllKnowns :: [MyFunType]
>         allUnknownsMatchAllKnowns =
>             [c | c@(_,as,_,_) <- bestPreferredMatchesNextStep
>             , hasUnknown
>             , t <- maybeToList allKnownsType
>             , let tys = map (\x -> case x of
>                              UnknownType -> t
>                              _ -> x) rawArgTypes
>             , as == tys
>             ]

>     let showl l = show (length l) ++ "\n" ++ ppShow l
>     let _showProcess =
>           "Name matches: " ++ showl nameMatches
>           ++ "\n\nRaw arg types: " ++ ppShow rawArgTypes
>           ++ "\n\nexactMatches: " ++ showl exactMatches
>           ++ "\n\nbinaryOpKnownUnknownMatches: " ++ showl binaryOpKnownUnknownMatches
>           ++ "\n\ntypeConversionMatches: " ++ showl typeConversionMatch
>           ++ "\n\nreachableViaImplicitCasts: " ++ showl reachableViaImplicitCasts
>           -- ++ "\n\npreferredTypeCounts: " ++ showl preferredTypeCounts
>           ++ "\n\nacceptsMostPreferredTypes: " ++ showl acceptsMostPreferredTypes
>           ++ "\n\nbestPreferredMatches: " ++ showl bestPreferredMatches
>           ++ "\n\nallUnknownsMatchAllKnowns: " ++ showl allUnknownsMatchAllKnowns

2. exact matches
2.1 binary operator known/unknown special case
2.5 typeConversion matches
4. candidate which accepts most preferred types
5. candidate matching preferred types
6. unknowns match knowns

>     let zeroOrOne x = case x of
>               [] -> Right ()
>               [a] -> Left (Right a)
>               _ -> Left (Left [AmbiguousOperator appName' rawArgTypes])
>         oneOrContinue x = case x of
>               [a] -> Left (Right a)
>               _ -> Right ()
>     trace (_showProcess) $ do
>         zeroOrOne exactMatches
>         zeroOrOne typeConversionMatch
>         oneOrContinue binaryOpKnownUnknownMatches
>         oneOrContinue reachableViaImplicitCasts
>         oneOrContinue acceptsMostPreferredTypes
>         oneOrContinue bestPreferredMatches
>         oneOrContinue allUnknownsMatchAllKnowns

otherwise fail

TODO:

When the matching errors, what are the possibilities in user
understandable terms?

1. no functions with that name match
(could return functions with a similar name + show types)

2. have functions which match the name, but the number of args is
wrong:

list the functions + show types
should this show similarly named functions? (what about highlighting
ones with matching arg types?)

3. have functions which have the right # args, but aren't reachable
via implicit casts

could list all the functions, does it make sense to highlight the ones
which can be reached by explicit casts (or are there basically
explicit casts for nearly all pairs of types?)
should this show similarly named functions?
should this show name matches with the wrong number of args

4. I think the only other one that matters is that there are functions
which match via implicit casts, but the system cannot pick a particlar
one
should it also list the other possibilities as above?

Fix the error to contain this information.

>         Left $ Left [NoMatchingOperator appName' rawArgTypes]


>   where
>     -- don't use last
>     -- check for empty list
>     appName' = case last appName of
>                Nmc n -> T.pack $ map toLower n
>                QNmc n -> T.pack n
>                AntiNameComponent _ -> -- todo: use left instead of error
>                  error "tried to find function matching an antinamecomponent"
>     canImplicitCastOrSame :: Type -> Type -> Bool
>     canImplicitCastOrSame from to =
>         from == to || isRight (catCast cat ImplicitCastContext from to)
>     -- check if casting type 'from' to type 'to' is casting
>     -- to the 'from' type's prefered type in the 'from' type's
>     -- category
>     isCastToPreferred :: Type -> Type -> Bool
>     isCastToPreferred from to = maybe False (const True) $ do
>         when (from == to) Nothing
>         when (isLeft (catPreferredType cat to)) Nothing
>         t1 <- either (const Nothing) Just
>               $ catTypeCategory cat from
>         t2 <- either (const Nothing) Just
>               $ catTypeCategory cat to
>         if t1 == t2
>           then Just ()
>           else Nothing

what are all the special cases currently:
special cases for precision
special cases for result null
in OldTypeConverion.findCallMatch:
   between, not between, greatest, least
   rowctor
   .
   comparisons for composite/set types
more stuff in TypeConversion.matchApp
  sql server date stuff
  decode
  something to do with datetimes?
  string precisions?:
    ||, substring, replace
  some nullability special cases?
  jesus, more bullshit, no idea what it is all for
    no tests and documentation as per usual
SqlTypeConversions: special case for implicit casts from text types to
  numeric (see if can handle in rule system)
ScalarExprs.ag
  needs implicit cast
  implicit cast type
  check the types (e.g. cast syntax should use this typeconversion
   machinery)
  tcAppLike: more mssql date shit

  getmaybeintsfromliterals: also suggests only need to support int
    literals in the matchapp function here

on the way out:

we have to add implicit casts for possible nullability and precision
adjustments and work out the result type precision and nullability

dealing with nullable: assume functions are strict, and never return
null if none of the inputs are null. Every function which doesn't work
this way will be special cased here.

precision
precision and scale apply to the following types:
array-style types (will we need 2d arrays?)
numeric
strings
byte arrays

for numeric, the precision and scale for a result are always -1 which
represents the text equivalent for numeric: unlimited scale and
precision.

the precision on a float is fake, and represents a weird short hand to
one of two fixed types, float4 and float8

the precision of char and varchar is the sum of all the precisions of
these in the input by default, lots of special cases here. Also have
literals to deal with.


> --resolveResultSetType :: Catalog -> [TypeExtra] -> Either [TypeError] TypeExtra
> --resolveResultSetType cat tys = undefined


> --checkAssignmentValid :: Catalog -> Type -> Type -> Either [TypeError] ()
> --checkAssignmentValid cat from to = undefined


resolve result set types:
numeric
text
domains
time
precision
nullability

used for union, intersect, except, case, array, values, greatest,
least, join keys


If all inputs are of the same type, and it is not unknown, resolve as
that type.

If any input is of a domain type, treat it as being of the domain's
base type for all subsequent steps. [1]

If all inputs are of type unknown, resolve as type text (the preferred
type of the string category). Otherwise, unknown inputs are ignored.

If the non-unknown inputs are not all of the same type category, fail.

Choose the first non-unknown input type which is a preferred type in
that category, if there is one.

Otherwise, choose the last non-unknown input type that allows all the
preceding non-unknown inputs to be implicitly converted to it. (There
always is such a type, since at least the first type in the list must
satisfy this condition.)

Convert all inputs to the selected type. Fail if there is not a
conversion from a given input to the selected type.



checkassignmentvalid:





also:
text encoding, char set + collations, what else?


TODO: get the list of all the hacks sqream does at the
typechecking sql layer and move it here
