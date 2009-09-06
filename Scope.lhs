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
>   Scope (union ot bt)
>         (union oc bc)
>         (union otc btc)
>         (union opre bpre)
>         (union opost bpost)
>         (union obin bbin)
>         (union off bf)
>         (union oaf baf)
>         (M.union oi bi)
