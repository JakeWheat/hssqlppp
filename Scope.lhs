Copyright 2009 Jake Wheat

-- currently used to get the types of identifiers
-- eventually should include things like types, table attributes,
-- etc.,

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
>                    ,scopeAllFns :: [FunctionPrototype]
>                    ,scopeIdentifierTypes :: M.Map String Type}
>            deriving (Eq,Show)

> emptyScope :: Scope
> emptyScope = Scope [] [] [] [] [] [] [] [] M.empty

> scopeCombineIds :: Scope -> M.Map String Type -> Scope
> scopeCombineIds s i = combineScopes s (emptyScope {scopeIdentifierTypes = i})

> combineScopes :: Scope -> Scope -> Scope
> --base, overrides
> combineScopes (Scope bt bc btc bpre bpost bbin bf baf bi)
>               (Scope ot oc otc opre opost obin off oaf oi) =
>   Scope (funion ot bt)
>         (funion oc bc)
>         (funion otc btc)
>         (funion opre bpre)
>         (funion opost bpost)
>         (funion obin bbin)
>         (funion off bf)
>         (funion oaf baf)
>         (M.union oi bi)
>   where
>     --without this it runs very slowly - guessing because it creates
>     --a lot of garbage
>     funion a b = case () of
>                    _ | a == [] -> b
>                      | b == [] -> a
>                      | otherwise -> union a b
