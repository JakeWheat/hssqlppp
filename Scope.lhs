Copyright 2009 Jake Wheat

Represents the types, identifiers, etc available. Used internally for
little scopes in the type checker, as well as the input to the type
checking routines if you want to supply different/extra definitions
before type checking something.

> module Scope where

> import Data.List

> import TypeType

> type FunctionPrototype = (String, [Type], Type)

> data Scope = Scope {scopeTypes :: [Type]
>                    ,scopeCasts :: [(Type,Type,CastContext)]
>                    ,scopeTypeCategories :: [(Type,String,Bool)]
>                    ,scopePrefixOperators :: [FunctionPrototype]
>                    ,scopePostfixOperators :: [FunctionPrototype]
>                    ,scopeBinaryOperators :: [FunctionPrototype]
>                    ,scopeFunctions :: [FunctionPrototype]
>                     --this should be done better:
>                    ,scopeAllFns :: [FunctionPrototype]
>                    ,scopeAttrDefs :: [CompositeDef]
>                    ,scopeIdentifierTypes :: [AliasedScope]
>                    ,scopeJoinIdentifiers :: [String]}
>            deriving (Eq,Show)

the way the scoping works is we have a list of prefixes/namespaces,
which is generally the table/view name, or the alias given to it, and
then a list of unaliased identifiers and their types. When we look
something up, if it has an alias we just look in that list, if it is
not present or not unique then throw an error. Similarly with no
alias, we look at all the lists, if the id is not present or not
unique then throw an error.

The join identifiers is for expanding *. If we want to access the
common attributes from one of the tables in a using or natural join,
this attribute can be quialified with either of the table
names/aliases. But when we expand the *, we only output these common
fields once, so keep a separate list of these fields used just for
expanding the star. The other twist is that these common fields appear
first in the resultant field list.

> type AliasedScope = (String, [(String,Type)])

> emptyScope :: Scope
> emptyScope = Scope [] [] [] [] [] [] [] [] [] [] []

> scopeReplaceIds :: Scope -> [AliasedScope] -> [String] -> Scope
> scopeReplaceIds scope ids commonJoinFields =
>     scope { scopeIdentifierTypes = ids
>           ,scopeJoinIdentifiers = commonJoinFields }

> scopeLookupID :: Scope -> MySourcePos -> String -> String -> Type
> scopeLookupID scope sp alias iden =
>   if alias == ""
>     then let types = concat $ map (filter (\(s,_) -> s == iden)) $
>                      map snd $ scopeIdentifierTypes scope
>          in case length types of
>                 0 -> TypeError sp (UnrecognisedIdentifier iden)
>                 1 -> (snd . head) types
>                 _ -> --see if this identifier is in the join list
>                      if iden `elem` scopeJoinIdentifiers scope
>                        then (snd . head) types
>                        else TypeError sp (AmbiguousIdentifier iden)
>     else case lookup alias (scopeIdentifierTypes scope) of
>            Nothing -> TypeError sp $ UnrecognisedAlias alias
>            Just s -> case lookup iden s of
>                        Nothing -> TypeError sp $ UnrecognisedIdentifier $ alias ++ "." ++ iden
>                        Just t -> t

> scopeExpandStar :: Scope -> MySourcePos -> String -> [(String,Type)]
> scopeExpandStar scope sp alias =
>     if alias == ""
>       then let allFields = concatMap snd $ scopeIdentifierTypes scope
>                (commonFields,uncommonFields) =
>                   partition (\(a,_) -> a `elem` scopeJoinIdentifiers scope) allFields
>            in nub commonFields ++ uncommonFields
>       else
>           case lookup alias (scopeIdentifierTypes scope) of
>             Nothing -> [("", TypeError sp $ UnrecognisedAlias alias)]
>             Just s -> s


> combineScopes :: Scope -> Scope -> Scope
> --base, overrides
> combineScopes (Scope bt bc btc bpre bpost bbin bf baf bcd _ _)
>               (Scope ot oc otc opre opost obin off oaf ocd oi oji) =
>   Scope (funion ot bt)
>         (funion oc bc)
>         (funion otc btc)
>         (funion opre bpre)
>         (funion opost bpost)
>         (funion obin bbin)
>         (funion off bf)
>         (funion oaf baf)
>         (funion ocd bcd)
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
chains scopes instead of unioning the lists and maps.
