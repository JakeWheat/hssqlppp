

This file contains the functions for resolving types and
function/operator resolution (which is seriously crazy). See the pg
manual chapter 10:

http://www.postgresql.org/docs/8.4/interactive/typeconv.html

sql server todo: want to match sql server implicit cast rules better
when doing tsql type checking. Follows a completely different approach, possible info here:
http://msdn.microsoft.com/en-us/library/ms187928.aspx
http://msdn.microsoft.com/en-us/library/ms190309.aspx
linked from here:
http://blogs.msdn.com/b/craigfr/archive/2010/01/20/more-on-implicit-conversions.aspx


TODO: rewrite
Wrappers to zap:

tcAppLike
matchApp
matchAppExtra
resolveResultSetType
resolveResultSetTypeExtra
findcallmatch
resolveresultsettype
checkassignments

We should have one function for each of the resolvers (3 just like
postgres) all the nullability, precision, dialect and special hacks
should all be in one place.

> {-# LANGUAGE OverloadedStrings, TupleSections, MultiWayIf,FlexibleInstances,ScopedTypeVariables #-}
> module Database.HsSqlPpp.Internals.TypeChecking.TypeConversion.TypeConversion
>     (matchApp
>     ,matchAppExtra
>     ,tcAppLike
>     ,resolveResultSetType
>     ,resolveResultSetTypeExtra
>     ,checkAssignmentValid
>     ,MatchAppLiteralList -- (..)
>     ) where
>
> import Data.Maybe
> import Data.List
> import Data.Either
> import Data.Char
>
> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> import Database.HsSqlPpp.Internals.Utils
> import Control.Monad
> import Control.Applicative
> import Control.Arrow

> import Database.HsSqlPpp.Internals.TypeChecking.TypeConversion.OldTypeConversion
> import Database.HsSqlPpp.Internals.Dialect
> import qualified Database.HsSqlPpp.Internals.TypeChecking.TypeConversion.SqlTypeConversion as TSQL
> import Data.Text ()
> import qualified Data.Text as T
> import Text.Printf
> --import Debug.Trace

******************************************************************

matchApp: takes the function name and argument types, and returns the
matching operator/function

This needs a lot more tests

> type MatchAppLiteralList = [Maybe Int]

> matchApp :: Dialect
>          -> Catalog
>          -> [NameComponent]
>          -> [Type]
>          -> Either [TypeError] ([Type],Type)
> matchApp d cat nmcs =
>     ambiguityResolver $ matchApp' d nmcs
>   where
>     -- matchApp' a b c | trace (show (a,b,c)) False = undefined
>     -- hack in support for sql server datediff function
>     -- need to think of a better way to handle this when
>     -- have a better idea of all the weird syntax used in
>     -- tsql
>     -- this is a todo since currently the sql server dialect uses
>     -- postgresql type names, and this is about to be fixed
>     matchApp' (Dialect {diSyntaxFlavour = SqlServer})
>                   [Nmc dd] [_
>                                  ,ScalarType "date"
>                                  ,ScalarType "date"]
>       | map toLower dd == "datediff" =
>       -- check there are 3 args
>       -- first is identifier from list
>       -- other two are date types
>       Right ([ScalarType "int4",ScalarType "date",ScalarType "date"], ScalarType "int4")
>     matchApp' (Dialect {diSyntaxFlavour = SqlServer}) [Nmc dd] [_,ScalarType "date"]
>       | map toLower dd == "datepart" =
>       Right ([ScalarType "int4",ScalarType "date"], ScalarType "int4")

>     matchApp' (Dialect {diSyntaxFlavour = SqlServer}) [Nmc dd] [_,ScalarType "timestamp"]
>       | map toLower dd == "datepart" =
>       Right ([ScalarType "int4",(ScalarType "timestamp")], ScalarType "int4")


>     matchApp' (Dialect {diSyntaxFlavour = SqlServer}) [Nmc dd] [_,_,ScalarType "date"]
>       | map toLower dd == "dateadd" =
>       Right ([ScalarType "int4",ScalarType "int4",ScalarType "date"], ScalarType "date")

>     matchApp' (Dialect {diSyntaxFlavour = SqlServer}) [Nmc dd] [_,_,ScalarType "timestamp"]
>       | map toLower dd == "dateadd" =
>       Right ([ScalarType "int4",ScalarType "int4",ScalarType "timestamp"], ScalarType "timestamp")

double hack: support oracle decode when in tsql mode:

>     matchApp' (Dialect {diSyntaxFlavour = SqlServer}) [Nmc dd] as
>       | map toLower dd == "decode" =

decode is just syntax for simple case statement:
demand at least 3 arguments
get the type of the first argument: this is the test target

>       case as of
>         (tt:as'@(_:_:_)) -> do

for each pair of arguments following: check the first
one can be compared to the test target

collect all the second types
if there is a single trailing argument this is the else

>             let checkBranches [] acc = return $ reverse acc
>                 checkBranches [els] acc = return $ reverse (els:acc)
>                 checkBranches (w:t:xs) acc = do
>                   _ <- matchApp' d [Nmc "="] [tt,w]
>                   checkBranches xs (t:acc)
>             sndTypes <- checkBranches as' []

check the seconds types + the else for type compatilibility
return this type
todo: add the implicit casting where needed

>             (as,) <$> resolveResultSetType cat sndTypes


>         _ -> Left [NoMatchingOperator (T.pack dd) as]



>     matchApp' d' nmcs' pts = {-trace ("matchapp: " ++ show (d,nmcs,pts)) $ -} do
>       (_,ps,r,_) <- case d' of
>                       (Dialect {diSyntaxFlavour = SqlServer}) -> TSQL.findCallMatch d' cat nm pts
>                       _ -> findCallMatch d' cat nm pts
>       return (ps,r)
>       where
>         nm = case last nmcs' of
>                Nmc n -> T.pack $ map toLower n
>                QNmc n -> T.pack n
>                AntiNameComponent _ -> -- todo: use left instead of error
>                  error "tried to find function matching an antinamecomponent"

hack to support literal arguments to overloaded functions
  currently, the problem arises only for date/datetime arguments
  the solution reflects this
for long argument lists with several literals, there can be a lot of variants generated, but
  this shouldn't slow down the execution because lazyness composes well with the functions used

> ambiguityResolver:: ([Type] -> Either [TypeError] ([Type],Type))
>                     -> [Type] -> Either [TypeError] ([Type],Type)
> ambiguityResolver f ts =  let rs :: [Either [TypeError] ([Type], Type)]
>                               rs = map f $ expandList variants ts
>                           -- this is needed in order to preserve the original error
>                           --  in case when all the attempts fail
>                           in case rights rs of
>                                  x:_ -> Right x
>                                  [] -> head rs
>   where
>     variants t = case t of
>         UnknownType -> [t, ScalarType "timestamp"]
>         _ -> [t]
>     -- similar to handling of superaggregates
>     -- ToDo: move to a general library
>     expandList:: (a -> [a]) -> [a] -> [[a]]
>     expandList f' = foldr (liftM2 (:) . f') [[]]

------------- precision and nullability of function application --------------

uses matchApp for inferring basic types

> matchAppExtra :: Dialect
>                 -> Catalog
>                 -> [NameComponent]
>                 -> MatchAppLiteralList
>                 -> [TypeExtra]
>                 -> Either [TypeError] ([TypeExtra],TypeExtra)
> matchAppExtra dialect cat nmcs lits tes = {-trace ("mae" ++ show (nmcs,tes)) $ -} do
>     (ts',t') <- matchApp dialect cat nmcs $ map teType tes
>     tes' <- joinArgsExtra appName tes $ zipWith addArgExtra tes ts'
>     return (tes', addResultExtra appName tes' t' lits)
>   where
>     addArgExtra te t = te {teType = t}
>     appName = case nmcs of
>       [Nmc dd] -> map toLower dd
>       _ -> ""

precision and nullability of the result

> addResultExtra:: String -> [TypeExtra] -> Type -> MatchAppLiteralList -> TypeExtra
> addResultExtra appName tes t lits = checkPrecisionRelevance . checkResultPrecisionClass tesr
>                                 $ TypeExtra t jp js jn
>   where
>     jp = if
>       | appName == "||" -> Just $ sum $ mapMaybe tePrecision tesr
>       | appName == "substring" ->
>            -- Substring is an interesting case. If we have both the
>            -- start and length as literals, we can figure out the resulting precision
>            -- Otherwise, treat as before with joinPrecision
>            let totalLen = joinPrecision $ map tePrecision tesr
>            in case (map teType tesr) of
>              [ScalarType "nvarchar"] -> totalLen
>              _ ->
>                  case lits of
>                    (Nothing:(Just startPos):(Just len):_) -> do
>                       totalLen' <- totalLen
>                       return $ if startPos + len > totalLen' then totalLen' - startPos else len
>                    _ -> totalLen

>         -- precision of the result is unknown
>       | appName `elem` ["replace"] -- is actually known for 2-argument "replace"
>         -> Nothing
>       | otherwise
>         -> joinPrecision $ map tePrecision tesr
>     js = joinScale $ map teScale tesr
>     jn = if
>       | appName `elem`
>           ( ["isnotnull","isdate","isnumeric"]
>               -- standard "is null" expression
>             ++ ["isnull" | length tes == 1]
>               -- currently, aggregate functions are handled as scalar functions
>             ++ ["count","count_big"])
>         -> False
>       | appName `elem`
>           ( ["coalesce","greatest","least"]
>               -- 2-argument function "isnull" of SqlServer
>               -- ImplicitCastToDo: isnull has quite complex cast rules,
>               --    not really reflected here and in the cast of arguments
>             ++ ["isnull" | length tes == 2]
>               -- nullability of corresponding SqlServer function "charindex"
>               --  may or may not differ, depending on database compatibility level
>               -- I implement the level above 70, so it goes to the default case
>             ++ ["strpos","position"])
>         -> all teNullable tesr
>         -- can produce null independently on the nullability of the arguments
>         -- ImplicitCastToDo: check again: doesn't it depend on the presence of "else" part
>       | appName `elem` ["case","decode","nullif","replicate"]
>         -> True
>         -- the default
>       | appName `elem` ["substr","substring"] -> any teNullable tes
>       | otherwise -> joinNullability $ map teNullable tesr
>     -- arguments that participate in the inference of the result type
>     tesr = case appName of
>       "decode" -> caseResultTypes tes
>           -- only the first argument influences precision and nullability
>       _ | appName `elem` ["nullif","substr","substring","left","right","ltrim","rtrim","replicate","translate","like","notlike","rlike"]
>           -> take 1 tes
>           -- the first two arguments influence
>         | appName `elem` ["charindex"]
>           -> take 2 tes
>           -- the first argument doesn't influence
>         | appName `elem` ["datepart","datediff"]
>           -> drop 1 tes
>           -- the first two arguments don't influence
>         | appName `elem` ["dateadd"]
>           -> drop 2 tes
>       -- the default case
>         | otherwise -> tes
>     -- tail is safe here because matchApp did all the checks
>     caseResultTypes tes' = caseResultTypes' (tail tes') []
>       where
>         caseResultTypes' [] acc = acc
>         caseResultTypes' [els] acc = els:acc
>         caseResultTypes' (_:t':xs) acc = caseResultTypes' xs (t':acc)

------------- cast of arguments --------------

Bring relevant arguments of a function to common precision and nullability.
The meaning of "relevant" is complicated:
  - the list of arguments is split into partitions;
  - precision and nullability is partition-wise joined;
  - the results are broadcast back to the arguments that constituted partitions.
The algorithm follows this outline.
Examples:
  - for binary operators, like "=", both arguments constitute a single partition;
  - for some functions, like "substr", each argument belongs to its own partition;
  - arguments of "case" and "decode" form two partitions which, except for the analyzed value
    and the 'else' argument, are intervened with one another.
Actually, partitions for precision and partitions for nullability can be different.
Example:
  "||": both arguments must be brought to common nullability, but remain with same precision.
Additionaly:
  Before splitting onto partitions, check each argument for:
    - precision class of the original (before matchApp) argument;
    - precision relevance.
  After splitting onto partitions, check each precision partition:
    - all arguments must have same precision class (return an error if they don't).

What the is this function doing? What is it for?

> joinArgsExtra:: String -> [TypeExtra] -> [TypeExtra] -> Either [TypeError] [TypeExtra]
> joinArgsExtra "!odbc-left" _t0 t1 = Right t1
> joinArgsExtra "!odbc-timestampdiff" _t0 t1 = Right t1
> joinArgsExtra "!odbc-timestampadd" _t0 t1 = Right t1

> joinArgsExtra appName tes0 tes1
>     = liftM (uncurry $ zipWith3 combine tes) $ uncurry (liftM2 (,))
>       $ (joinDim joinPrec partitionPrec &&& joinDim joinNull partitionNull) tes
>   where
>     -- checks and adjustments before partitioning
>     tes = map checkPrecisionRelevance
>           $ zipWith adjust tes0
>           $ zipWith checkPrecisionClass tes0 tes1
>       where
>         adjust te0 te1 = te1{tePrecision = head $ adjustStringCastPrec (teType te1) [te0]}
>     -- checks after partitioning
>     checkPartition ptes = if length (nub ppcs) > 1
>         then Left [InternalError $ printf "implicit cast: arguments of '%s' that belong to same partition are of different precision classes: %s -> %s" appName (show ptes) (show ppcs)]
>         else return ptes
>       where
>         ppcs = map (precisionClass . teType) ptes
>     -- the algorithm for a single partitioning dimension
>     joinDim:: ([TypeExtra] -> [TypeExtra])
>               -> ([TypeExtra] -> Either [TypeError]
>                                         ([[TypeExtra]] -> [TypeExtra], [[TypeExtra]]))
>               -> [TypeExtra] -> Either [TypeError] [TypeExtra]
>     joinDim join' partitionArgs'
>         = liftM (uncurry ($) . second (map join')) . partitionArgs'
>     -- combine results for precision and nullability
>     combine te tePrec teNull = te {
>       tePrecision = tePrecision tePrec,
>       teScale = teScale tePrec,
>       teNullable = teNullable teNull
>     }
>     -- joins of precision and nullability partitions
>     joinPrec tes' = map promote tes'
>       where
>         promote (TypeExtra t p s n) = TypeExtra t (p' `mplus` p) (s' `mplus` s) n
>         p' = joinPrecision $ map tePrecision tes
>         s' = joinScale $ map teScale tes
>     joinNull tes' = map promote tes'
>       where
>         promote (TypeExtra t p s n) = TypeExtra t p s (n' || n)
>         n' = joinNullability $ map teNullable tes
>     -- the partitioning functions return partitions paired with a function
>     --    that puts them back into their places
>     -- because, in many cases, partitions for precision and for nullability are the same,
>     --    the partitioning code for such cases is factored out
>     partitionArgs:: [a] -> ([[a]] -> [a], [[a]])
>     partitionArgs as = case () of
>             -- functions whose arguments are independent
>             --  instead of splitting into partitions, just return the original list
>         _ | appName `elem`
>               ( ["datepart","dateadd"]
>                 ++ ["substr","substring","left","right","ltrim","rtrim"]
>                 ++ ["replicate","like","notlike","rlike"]
>                 ++ ["strpos","position","replace"]
>                     -- Oracle joins the datatypes (needed for the comparison)
>                 ++ ["nullif"]
>                 -- SQream specific regex functions
>                 ++ ["regexp_substr","regexp_count","regexp_instr"]
>                 ++ ["string_agg"]
>               )
>             -> (const as, [])
>             -- first argument is special, the rest are processed together
>           | appName `elem` ["datediff"]
>             -> (concat, pairToList $ splitAt 1 as)
>           | appName `elem` ["decode"]
>             ->  let (ws,ts) = decomposeDecodeTail (tail as) ([],[])
>                 in (composeDecodePartitions, [head as :ws, ts])
>             -- the default is to return a single partition
>           | otherwise -> (concat, [as])
>     partitionPrec as = secondM (mapM checkPartition) $ case () of
>             -- independent arguments
>         _ | appName `elem` ["||","concat","translate","charindex"]
>             -> (const as, [])
>             -- single partition
>           | appName `elem`
>               ( ["coalesce","greatest","least"]
>                     -- ImplicitCastToDo: think how to handle this properly
>                 ++ ["isnull" | length as == 2]
>               )
>             -> (concat, [as])
>           | otherwise -> partitionArgs as
>     partitionNull as = return $ case () of
>         _ | appName `elem`
>               ( ["coalesce","greatest","least"]
>                 ++ ["isnull" | length as == 2]
>               )
>             -> (const as, [])
>           | appName `elem` ["charindex"]
>             -> (concat, pairToList $ splitAt 2 as)
>           | otherwise -> partitionArgs as
>     -- utility
>     pairToList (x,y) = [x,y]
>     decomposeDecodeTail [] acc = (reverse***reverse) acc
>     decomposeDecodeTail [els] acc = (reverse.(els:)***reverse) acc
>     decomposeDecodeTail (w:t:xs) acc = decomposeDecodeTail xs $ ((w:)***(t:)) acc
>     composeDecodePartitions [t:ts,ws] = t : concat (transpose [ts,ws])
>     -- redundant
>     composeDecodePartitions xs = concat xs

------------- precision class --------------

This is a small library for checking whether inference of precision does make sense.
It is used both in inference of precision of arguments and result of a function.

It is theoretically possible that types belong to different precision classes,
    but inference of precision still makes sense (consider, for instance,
    conversion between string and decimal).
  Such cases must be handled specially.

> data PrecisionClass = String | Number | FlexiblePrecisionClass
>       deriving (Eq,Show)
>
> precisionClass:: Type -> Maybe PrecisionClass
> precisionClass (ScalarType tn)
>   | tn `elem` ["text","varchar","nvarchar","char"] = Just String
>   | tn `elem` ["int1","int2","int4","int8","float4","float8","numeric"] = Just Number
>   | otherwise = Nothing
> precisionClass UnknownType = Just FlexiblePrecisionClass
> precisionClass _ = Nothing

Do original and new type have compatible precision classes?
Note: this function is not commutative.

> infix 4 .~>.
> (.~>.):: TypeExtra -> TypeExtra -> Bool
> t0 .~>. t = Just FlexiblePrecisionClass `elem` [pc0,pc] || pc0 == pc
>   where
>     [pc0,pc] = map (precisionClass . teType) [t0,t]

retreat to default when original and new type are incompatible

> checkPrecisionClass:: TypeExtra -> TypeExtra -> TypeExtra
> checkPrecisionClass t0 t = if t0 .~>. t then t else t{tePrecision = Nothing, teScale = Nothing}

check precision class of result against precision classes of arguments

> checkResultPrecisionClass:: [TypeExtra] -> TypeExtra -> TypeExtra
> checkResultPrecisionClass tes t
>   = if and $ map (.~>. t) tes then t else t{tePrecision = Nothing, teScale = Nothing}

check whether precision/scale is relevant for a type (consider "round").

> checkPrecisionRelevance:: TypeExtra -> TypeExtra
> checkPrecisionRelevance te = if
>   | Just String <- pc
>     -> te{teScale = Nothing}
>   | Just FlexiblePrecisionClass <- pc
>     -> te
>   | ScalarType tn <- t, tn == "numeric"
>     -> te
>   | otherwise
>     -> te{tePrecision = Nothing, teScale = Nothing}
>   where
>     t = teType te
>     pc = precisionClass t


The purpose of the tcapplike wrapper is to partially fix up the
keyword 'enums' in some of the ms style functions (like
datepart). This is a horrible hack and will be fixed.

> tcAppLike :: Dialect -> Catalog -> [NameComponent]
>            -> [Maybe Int] -> [Maybe TypeExtra]
>            -> Either [TypeError] ([TypeExtra],TypeExtra)

> tcAppLike d cat anm@[Nmc dd] lits [_,a0,a1]
>     | map toLower dd == "datediff" = do
>   -- dodgy hack for datediff
>   tys <- mapM (maybe (Left []) Right) [a0,a1]
>   typeInt <- ScalarType <$> maybe (Left []) Right
>              (ansiTypeNameToDialect d "int")
>   let --Name _ ns = anm
>   (ats,rt) <- matchAppExtra d cat anm lits (mkTypeExtraNN typeInt : tys)
>   return (ats,rt)
> tcAppLike d cat anm@[Nmc dd] lits [_,a0]
>     | map toLower dd == "datepart" = do
>   tys <- mapM (maybe (Left []) Right) [a0]
>   typeInt <- ScalarType <$> maybe (Left []) Right
>              (ansiTypeNameToDialect d "int")
>   (ats,rt) <- matchAppExtra d cat anm lits (mkTypeExtraNN typeInt : tys)
>   return (ats,rt)
> tcAppLike d cat anm@[Nmc dd] lits [_,a0,a1]
>     | map toLower dd == "dateadd" = do
>   tys <- mapM (maybe (Left []) Right) [a0,a1]
>   typeInt <- ScalarType <$> maybe (Left []) Right
>              (ansiTypeNameToDialect d "int")
>   (ats,rt) <- matchAppExtra d cat anm lits (mkTypeExtraNN typeInt : tys)
>   return (ats,rt)

> --tcAppLike d cat anm@(Name _ [Nmc dd]) a b
> --    |  trace ("xz: " ++ show (a,b)) False = undefined

> tcAppLike d cat anm@[Nmc dd] _ ts
>     | map toLower dd == "!odbc-convert" = do
>                  (tys :: [TypeExtra]) <- mapM (maybe (Left []) Right) ts
>                  --let Name _ ns = anm
>                  (ats,rt) <- matchAppExtra d cat anm [] tys
>                  return (ats,rt)

> tcAppLike d cat anm@[Nmc dd] _ [_,a0,a1]
>     | map toLower dd `elem` ["!odbc-timestampadd","!odbc-timestampdiff"] = do
>   tys <- mapM (maybe (Left []) Right) [a0,a1]
>   --let Name _ ns = anm
>   typeInt <- ScalarType <$> maybe (Left []) Right
>              (ansiTypeNameToDialect d "int")
>   (ats,rt) <- matchAppExtra d cat anm [] (mkTypeExtraNN typeInt : tys)
>   return (ats,rt)

> tcAppLike d cat anm lits teArgs = do
>   -- get the types of the arguments
>   -- then lookup in TypeConversion.matchAppExtra
>   tys <- mapM (maybe (Left []) Right) teArgs
>   (ats,rt) <- matchAppExtra d cat anm lits tys
>   return (ats,rt)


******************************************************************

 findCallMatch :: Catalog -> String -> [Type] ->  Either [TypeError] OperatorPrototype
 findCallMatch cat fnName' argsType =

code interspersed with text cut and pasted from postgresql manual
10.3. Functions

Function Type Resolution

Select the functions to be considered from the pg_proc system
catalog. If a non-schema-qualified function name was used, the
functions considered are those with the matching name and argument
count that are visible in the current search path (see Section
5.7.3). If a qualified function name was given, only functions in the
specified schema are considered.

[HsSqlPpp doesn't support schema stuff yet, so just get a list of all
the functions with a matching name]

>   {-let matchingNames = catGetOpsMatchingName cat nmcs
>       exactMatches = filter (\(_,ts,_,_) -> ts == pts) matchingNames
>   case exactMatches of
>     [(_,tys,rt,_)] -> return (tys, rt)
>     [] -> error $ "no matching fn: " ++ show nmcs
>                   ++ "(" ++ intercalate "," (map show pts) ++ ")"
>     _xs -> error "ambiguous"-}



If the search path finds multiple functions of identical argument
types, only the one appearing earliest in the path is
considered. Functions of different argument types are considered on an
equal footing regardless of search path position.

If a function is declared with a VARIADIC array parameter, and the
call does not use the VARIADIC keyword, then the function is treated
as if the array parameter were replaced by one or more occurrences of
its element type, as needed to match the call. After such expansion
the function might have effective argument types identical to some
non-variadic function. In that case the function appearing earlier in
the search path is used, or if the two functions are in the same
schema, the non-variadic one is preferred.

Functions that have default values for parameters are considered to
match any call that omits zero or more of the defaultable parameter
positions. If more than one such function matches a call, the one
appearing earliest in the search path is used. If there are two or
more such functions in the same schema with identical parameter types
in the non-defaulted positions (which is possible if they have
different sets of defaultable parameters), the system will not be able
to determine which to prefer, and so an "ambiguous function call"
error will result if no better match to the call can be found.

Check for a function accepting exactly the input argument types. If
one exists (there can be only one exact match in the set of functions
considered), use it. (Cases involving unknown will never find a match
at this step.)

If no exact match is found, see if the function call appears to be a
special type conversion request. This happens if the function call has
just one argument and the function name is the same as the (internal)
name of some data type. Furthermore, the function argument must be
either an unknown-type literal, or a type that is binary-coercible to
the named data type, or a type that could be converted to the named
data type by applying that type's I/O functions (that is, the
conversion is either to or from one of the standard string
types). When these conditions are met, the function call is treated as
a form of CAST specification. [1]

Look for the best match.

Discard candidate functions for which the input types do not match and
cannot be converted (using an implicit conversion) to match. unknown
literals are assumed to be convertible to anything for this
purpose. If only one candidate remains, use it; else continue to the
next step.

Run through all candidates and keep those with the most exact matches
on input types. (Domains are considered the same as their base type
for this purpose.) Keep all candidates if none have exact matches. If
only one candidate remains, use it; else continue to the next step.

Run through all candidates and keep those that accept preferred types
(of the input data type's type category) at the most positions where
type conversion will be required. Keep all candidates if none accept
preferred types. If only one candidate remains, use it; else continue
to the next step.

If any input arguments are unknown, check the type categories accepted
at those argument positions by the remaining candidates. At each
position, select the string category if any candidate accepts that
category. (This bias towards string is appropriate since an
unknown-type literal looks like a string.) Otherwise, if all the
remaining candidates accept the same type category, select that
category; otherwise fail because the correct choice cannot be deduced
without more clues. Now discard candidates that do not accept the
selected type category. Furthermore, if any candidate accepts a
preferred type in that category, discard candidates that accept
non-preferred types for that argument.

If only one candidate remains, use it. If no candidate or more than
one candidate remains, then fail.




exact match
binop1unknownmatch
polymorphic matches
reachable
mostexactmatches
filteredforpreferred
unknownmatchesbycat



TODO: do a log monad, which can record the tests and then return this
process along with the resolved function


--------------------------------
todo:

> {-resolveResultSetType :: Catalog -> [Type] -> Either [TypeError] Type
> resolveResultSetType _cat [] = error "resolveResultSetType: empty type set"
> resolveResultSetType _cat (t:ts) =
>   if all (==t) ts
>   then Right t
>   else Left [IncompatibleTypeSet (t:ts)]-}

todo:

assignmentCheck
