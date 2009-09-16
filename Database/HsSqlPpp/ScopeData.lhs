Copyright 2009 Jake Wheat

Represents the types, identifiers, etc available. Used internally for
static and changing scopes in the type checker, as well as the input
to the type checking routines if you want to supply different/extra
definitions before type checking something, and you're not getting the
extra definitions from an accessible database.

> module Database.HsSqlPpp.ScopeData
>     (
>      Scope(..)
>      ,QualifiedScope
>      ,emptyScope
>      ,scopeReplaceIds
>      ,scopeLookupID
>      ,scopeExpandStar
>      ,combineScopes
>     ) where

> import Data.List
> import Debug.Trace

> import Database.HsSqlPpp.TypeType

> data Scope = Scope {scopeTypes :: [Type]
>                    ,scopeTypeNames :: [(String, Type)]
>                    ,scopeDomainDefs :: [DomainDefinition]
>                    ,scopeCasts :: [(Type,Type,CastContext)]
>                    ,scopeTypeCategories :: [(Type,String,Bool)]
>                    ,scopePrefixOperators :: [FunctionPrototype]
>                    ,scopePostfixOperators :: [FunctionPrototype]
>                    ,scopeBinaryOperators :: [FunctionPrototype]
>                    ,scopeFunctions :: [FunctionPrototype]
>                    ,scopeAggregates :: [FunctionPrototype]
>                     --this should be done better:
>                    ,scopeAllFns :: [FunctionPrototype]
>                    ,scopeAttrDefs :: [CompositeDef]
>                    ,scopeAttrSystemColumns :: [CompositeDef]
>                    ,scopeIdentifierTypes :: [QualifiedScope]
>                    ,scopeJoinIdentifiers :: [String]}
>            deriving (Eq,Show)

= Attribute identifier scoping

The way this scoping works is we have a list of prefixes/namespaces,
which is generally the table/view name, or the alias given to it, and
then a list of identifiers (with no dots) and their types. When we
look up the type of an identifier, if it has an correlation name we
try to match that against a table name or alias in that list, if it is
not present or not unique then throw an error. Similarly with no
correlation name, we look at all the lists, if the id is not present
or not unique then throw an error.

scopeIdentifierTypes is for expanding *. If we want to access the
common attributes from one of the tables in a using or natural join,
this attribute can be qualified with either of the table names/
aliases. But when we expand the *, we only output these common fields
once, so keep a separate list of these fields used just for expanding
the star. The other twist is that these common fields appear first in
the resultant field list.

System columns: pg also has these - they have names and types like
other attributes, but are not included when expanding stars, so you
only get them when you explicitly ask for them. The main use is using
the oid system column which is heavily used as a target for foreign
key references in the pg catalog.

> type QualifiedScope = (String, ([(String,Type)], [(String,Type)]))

> emptyScope :: Scope
> emptyScope = Scope [] [] [] [] [] [] [] [] [] [] [] [] [] [] []

> scopeReplaceIds :: Scope -> [QualifiedScope] -> [String] -> Scope
> scopeReplaceIds scope ids commonJoinFields =
>     scope { scopeIdentifierTypes = ids
>           ,scopeJoinIdentifiers = commonJoinFields }

> catPair :: ([a],[a]) -> [a]
> catPair = uncurry (++)

> scopeLookupID :: Scope -> MySourcePos -> String -> String -> Type
> scopeLookupID scope sp correlationName iden =
>   if correlationName == ""
>     then let types = concatMap (filter (\ (s, _) -> s == iden))
>                        (map (catPair.snd) $ scopeIdentifierTypes scope)
>          in case length types of
>                 0 -> TypeError sp (UnrecognisedIdentifier iden)
>                 1 -> (snd . head) types
>                 _ -> --see if this identifier is in the join list
>                      if iden `elem` scopeJoinIdentifiers scope
>                        then (snd . head) types
>                        else TypeError sp (AmbiguousIdentifier iden)
>     else case lookup correlationName (scopeIdentifierTypes scope) of
>            Nothing -> TypeError sp $ UnrecognisedCorrelationName correlationName
>            Just s -> case lookup iden (catPair s) of
>                        Nothing -> TypeError sp $ UnrecognisedIdentifier $ correlationName ++ "." ++ iden
>                        Just t -> t

> scopeExpandStar :: Scope -> MySourcePos -> String -> [(String,Type)]
> scopeExpandStar scope sp correlationName =
>     if correlationName == ""
>       then let allFields = concatMap (fst.snd) $ scopeIdentifierTypes scope
>                (commonFields,uncommonFields) =
>                   partition (\(a,_) -> a `elem` scopeJoinIdentifiers scope) allFields
>            in nub commonFields ++ uncommonFields
>       else
>           case lookup correlationName $ scopeIdentifierTypes scope of
>             Nothing -> [("", TypeError sp $ UnrecognisedCorrelationName correlationName)]
>             Just s -> fst s


> combineScopes :: Scope -> Scope -> Scope
> --base, overrides
> combineScopes (Scope bt btn bdod bc btc bpre bpost bbin bf bagg baf bcd basc _ _)
>               (Scope ot otn odod oc otc opre opost obin off oagg oaf ocd oasc oi oji) =
>   Scope (funion ot bt)
>         (funion otn btn)
>         (funion odod bdod)
>         (funion oc bc)
>         (funion otc btc)
>         (funion opre bpre)
>         (funion opost bpost)
>         (funion obin bbin)
>         (funion off bf)
>         (funion oagg bagg)
>         (funion oaf baf)
>         (funion ocd bcd)
>         (funion oasc basc)
>         oi -- overwrites old scopes, might need to be looked at again
>         oji
>   where
>     --without this it runs very slowly - guessing because it creates
>     --a lot of garbage
>     funion a b = case () of
>                    _ | a == [] -> b
>                      | b == [] -> a
>                      | otherwise -> union a b

combine scopes still seems to run slowly, so change it so that it
chains scope lookups along a list of scopes instead of unioning the
individual lists.
