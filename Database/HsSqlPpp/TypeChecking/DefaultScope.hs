{-# OPTIONS_HADDOCK hide #-}
{-
Copyright 2009 Jake Wheat

If you change the scope datatype, the DefaultScope.hs file will be
invalid, and you can't use HsSqlSystem to generate a new one since it
depends on DefaultScope. The solution is to copy this file over
DefaultScope. You can then use HsSqlSystem to generate a new default
scope file.

TODO: don't use show to save and load the default scope, use
data.binary to serialize and unserialize it.

-}

module Database.HsSqlPpp.TypeChecking.DefaultScope where

import Database.HsSqlPpp.TypeChecking.TypeType
import Database.HsSqlPpp.TypeChecking.ScopeData


defaultScope :: Scope
defaultScope = emptyScope
