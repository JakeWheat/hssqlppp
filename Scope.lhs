Copyright 2009 Jake Wheat

-- currently used to get the types of identifiers
-- eventually should include things like types, table attributes,
-- etc.,

> module Scope where

> import qualified Data.Map as M

> import TypeType

> data Scope = Scope { identifierTypes :: M.Map String Type }

> defaultScope,emptyScope :: Scope
> defaultScope = Scope M.empty
> emptyScope = Scope M.empty

> scopeCombineIds :: Scope -> M.Map String Type -> Scope
> scopeCombineIds s i = combineScopes s (emptyScope {identifierTypes = i})

> combineScopes :: Scope -> Scope -> Scope
> --base, overrides
> combineScopes (Scope bi) (Scope oi) = Scope (M.union oi bi)
