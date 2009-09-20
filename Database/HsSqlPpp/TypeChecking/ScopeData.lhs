Copyright 2009 Jake Wheat

Represents the types, identifiers, etc available. Used internally for
static and changing scopes in the type checker, as well as the input
to the type checking routines if you want to supply different/extra
definitions before type checking something, and you're not getting the
extra definitions from an accessible database.

> {-# OPTIONS_HADDOCK hide  #-}

 > {-# LANGUAGE DeriveDataTypeable #-}

> module Database.HsSqlPpp.TypeChecking.ScopeData
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
> import Database.HsSqlPpp.TypeChecking.EnvironmentInternal

 > import Data.Generics
 > import Data.Binary

> import Database.HsSqlPpp.TypeChecking.TypeType

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
>            deriving (Eq,Show {-,Typeable,Data-})


> type QualifiedScope = (String, ([(String,Type)], [(String,Type)]))

> -- | scope containing nothing
> emptyScope :: Scope
> emptyScope = Scope [] [] [] [] [] [] [] [] [] [] [] [] [] [] []

> scopeReplaceIds :: Scope -> [QualifiedScope] -> [String] -> Scope
> scopeReplaceIds scope ids commonJoinFields =
>     scope { scopeIdentifierTypes = ids
>           ,scopeJoinIdentifiers = commonJoinFields }

> catPair :: ([a],[a]) -> [a]
> catPair = uncurry (++)

> scopeLookupID :: Scope -> String -> String -> Either [TypeError] Type
> scopeLookupID scope correlationName iden =
>   if correlationName == ""
>     then let types = concatMap (filter (\ (s, _) -> s == iden))
>                        (map (catPair.snd) $ scopeIdentifierTypes scope)
>          in case length types of
>                 0 -> Left [UnrecognisedIdentifier iden]
>                 1 -> Right $ (snd . head) types
>                 _ -> --see if this identifier is in the join list
>                      if iden `elem` scopeJoinIdentifiers scope
>                        then Right $ (snd . head) types
>                        else Left [AmbiguousIdentifier iden]
>     else case lookup correlationName (scopeIdentifierTypes scope) of
>            Nothing -> Left [UnrecognisedCorrelationName correlationName]
>            Just s -> case lookup iden (catPair s) of
>                        Nothing -> Left [UnrecognisedIdentifier $ correlationName ++ "." ++ iden]
>                        Just t -> Right t

> scopeExpandStar :: Scope -> String -> Either [TypeError] [(String,Type)]
> scopeExpandStar scope correlationName =
>     if correlationName == ""
>       then let allFields = concatMap (fst.snd) $ scopeIdentifierTypes scope
>                (commonFields,uncommonFields) =
>                   partition (\(a,_) -> a `elem` scopeJoinIdentifiers scope) allFields
>            in Right $ nub commonFields ++ uncommonFields
>       else
>           case lookup correlationName $ scopeIdentifierTypes scope of
>             Nothing -> Left [UnrecognisedCorrelationName correlationName]
>             Just s -> Right $ fst s


> -- | combine two scopes, e.g. this can be used to take the default scope and
> -- add a few definitions to it before type checking an ast
> combineScopes :: Scope -- ^ base scope
>               -> Scope -- ^ additional scope - this adds to and overrides items in the base scope
>               -> Scope
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

= binary instance

instructions from here:
http://hackage.haskell.org/packages/archive/binary/0.4.1/doc/html/Data-Binary.html

get the file:
http://darcs.haskell.org/binary/tools/derive/BinaryDerive.hs
run
ghci -fglasgow-exts BinaryDerive.hs
enter:
:a Database.HsSqlPpp.TypeChecking.Scope
then:
BinaryDerive.deriveM (undefined::Database.HsSqlPpp.TypeChecking.Scope.Scope)

paste the code in here:

> {-
> instance Binary Database.HsSqlPpp.TypeChecking.ScopeData.Scope where
>     put (Scope a b c d e f g h i j k l m n o) = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i >> put j >> put k >> put l >> put m >> put n >> put o
>     get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f -> get >>= \g -> get >>= \h -> get >>= \i -> get >>= \j -> get >>= \k -> get >>= \l -> get >>= \m -> get >>= \n -> get >>= \o -> return (Scope a b c d e f g h i j k l m n o)
> -}

================================================================================

= new scope design plan:

rename scope and *scope* to environment, use env as variable name
make more abstract: our api is (keep using the word scope here to avoid confusion)
combinescope goes. replaced with scope update data types, so to build one scope
from another, we take the original scope, and apply a list of scope updates to
it to get a new scope
readscope: name this better -> we are reading a scope from a database. This returns
a list of scope updates, not a scope. we can create a scope from this by applying the
list of updates to emptyscope
the api for client programs consists of:
emptyEnvironment :: Environment
data EnvironmentUpdate = EnvCreateTable ... | EnvCreateFunction ... | , etc.
 - abstract away all the different bits of the internals e.g. type,
   typecategories, typenames in separate lists
readEnvironmentFromDatabase :: String -> IO [EnvironmentUpdate]
updateEnvironment :: Environment -> [EnvironmentUpdate] -> Environment
if needed, can code a destructEnvironment for inspection:
destructEnvironment :: Environment -> [EnvironmentUpdate]
with the propery:
updateEnvironment emptyEnvironment (destructEnvironment env) === env
=== supposed to be equivalent to symbol

api that type checker uses:
environmentReplaceIDs
environmentExpandStar

getCompositeAttributes (name, compositetypes allowed list, empty=all allowed)
gettypecategories for a type
canCast src tgt context
getdomainbasetype
lookupfunctionprotos by name

astAnnotation fns take either
no env -> use default
env -> use this, doesn't combine with default
env update list -> combines with default




= ddl

map from ddl statements to how the scope changes

create table
create table as
create view
create type
add composite type to types,typenames
add to type categories?
add to attr defs, system columns

create function
functions, all functions

create domain
types, typenames, domain defs, casts?, typecategories,

drop - reverses creates
