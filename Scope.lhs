Copyright 2009 Jake Wheat

Represents the types, identifiers, etc available. Used internally for
little scopes in the type checker, as well as the input to the type
checking routines if you want to supply different/extra definitions
before type checking something.

> module Scope where

> import qualified Data.Map as M
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
>                    ,scopeIdentifierTypes :: M.Map String Type}
>            deriving (Eq,Show)

> emptyScope :: Scope
> emptyScope = Scope [] [] [] [] [] [] [] [] [] M.empty

> scopeCombineIds :: Scope -> M.Map String Type -> Scope
> scopeCombineIds s i = combineScopes s (emptyScope {scopeIdentifierTypes = i})

> combineScopes :: Scope -> Scope -> Scope
> --base, overrides
> combineScopes (Scope bt bc btc bpre bpost bbin bf baf bcd bi)
>               (Scope ot oc otc opre opost obin off oaf ocd oi) =
>   Scope (funion ot bt)
>         (funion oc bc)
>         (funion otc btc)
>         (funion opre bpre)
>         (funion opost bpost)
>         (funion obin bbin)
>         (funion off bf)
>         (funion oaf baf)
>         (funion ocd bcd)
>         (M.union oi bi)
>   where
>     --without this it runs very slowly - guessing because it creates
>     --a lot of garbage
>     funion a b = case () of
>                    _ | a == [] -> b
>                      | b == [] -> a
>                      | otherwise -> union a b

combine scopes still seems to run slowly, so change it so that it
chains scopes instead of unioning the lists and maps.