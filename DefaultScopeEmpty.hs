{-
Copyright 2009 Jake Wheat

This file is used in the following scenario:

hack up scope or readscope or something

run ./HsSqlSystem.lhs getscope template1 >DefaultScope.hs

oops - DefaultScope.hs has been blanked too soon (i.e. before ghc has
loaded it as part of running HsSqlSystem), and HsSqlSystem can't run,
and we're stuck without being able to generate a new DefaultScope.

Or//

run ./HsSqlSystem.lhs getscope template1 >DefaultScope1.hs
then mv DefaultScope1.hs DefaultScope.hs

and, oops - DefaultScope hasn't generated properly and can't
compile. Once again we can't run HsSqlSystem.hs to fix it.

If you end up in this position, use
cp DefaultScopeEmpty.hs DefaultScope.hs
and you can then generate a new DefaultScope again using HsSqlPpp.

Finally//

You alter the scope type, and hssqlsystem won't run since the value in
defaultscope no longer matches the scope type, overwrite
DefaultScope.hs with this file and then you can regenerate the
DefaultScope with the new type. Assuming you've updated ScopeReader
also.

Perhaps having the program to generate defaultscope.hs depend on it
being a valid file before it can regenerate it is a bad design...

-}


module DefaultScope where

import TypeType
import Scope


defaultScope :: Scope
defaultScope = emptyScope