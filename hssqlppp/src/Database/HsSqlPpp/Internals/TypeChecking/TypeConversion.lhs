

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

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.TypeChecking.TypeConversion
>     (matchApp
>     ,resolveResultSetType
>     ) where
>
> --import Data.Maybe
> --import Data.List
> --import Data.Either
> --import Debug.Trace
> import Data.Char
>
> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> --import Database.HsSqlPpp.Utils.Utils
> --import Database.HsSqlPpp.Internals.TediousTypeUtils

> import Database.HsSqlPpp.Internals.TypeChecking.OldTypeConversion
> import Database.HsSqlPpp.SqlDialect
> import qualified Database.HsSqlPpp.Internals.TypeChecking.SqlTypeConversion as TSQL
> import Data.Text ()
> import qualified Data.Text as T

------------------------------------------------------------------

matchApp: takes the function name and argument types, and returns the
matching operator/function

This needs a lot more tests

> matchApp :: SQLSyntaxDialect
>          -> Catalog
>          -> [NameComponent]
>          -> [Type]
>          -> Either [TypeError] ([Type],Type)
> -- hack in support for sql server datediff function
> -- need to think of a better way to handle this when
> -- have a better idea of all the weird syntax used in
> -- tsql
> matchApp SQLServerDialect _cat [Nmc dd] [_
>                                         ,ScalarType "date"
>                                         ,ScalarType "date"]
>   | map toLower dd == "datediff" =
>   -- check there are 3 args
>   -- first is identifier from list
>   -- other two are date types
>   Right ([typeInt,typeDate,typeDate], typeInt)
> matchApp SQLServerDialect _cat [Nmc dd] [_,ScalarType "date"]
>   | map toLower dd == "datepart" =
>   Right ([typeInt,typeDate], typeInt)
> matchApp SQLServerDialect _cat [Nmc dd] [_,_,ScalarType "date"]
>   | map toLower dd == "dateadd" =
>   Right ([typeInt,typeInt,typeDate], typeDate)


> matchApp d cat nmcs pts = {-trace ("matchapp: " ++ show (d,nmcs,pts)) $ -} do
>   (_,ps,r,_) <- case d of
>                   SQLServerDialect -> TSQL.findCallMatch cat nm pts
>                   _ -> findCallMatch cat nm pts
>   return (ps,r)
>   where
>     nm = case last nmcs of
>            Nmc n -> T.pack $ map toLower n
>            QNmc n -> T.pack n
>            AntiNameComponent _ -> -- todo: use left instead of error
>              error "tried to find function matching an antinamecomponent"

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
