#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

TODO: the point of these tests will be to check the line and column
mapping from parsed and pretty printed sql back to the original source
text.

> module DatabaseLoaderTests (databaseLoaderTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import DatabaseLoader

> databaseLoaderTests :: Test.Framework.Test
> databaseLoaderTests = testGroup "databaseLoaderTests" [
>      t "execute: PGRES_FATAL_ERROR: ERROR:  column \"object_name\" of relation \"system_implementation_objects\" does not exist\n\
>        \LINE 1: insert into system_implementation_objects (object_name,objec...\n\
>        \                                                   ^\n"
>        0 1 --0 should be 43

>     ,t "execute: PGRES_FATAL_ERROR: ERROR:  column \"x\" does not exist\n\
>        \LINE 3:   (x,'base_relvar')\n\
>        \           ^\n"
>        0 3


>   ]
>         where
>           t et l c = testCase et $ do
>                           let (rl, rc) = getLineAndColumnFromErrorText et
>                           assertEqual "" (l,c) (rl,rc)
