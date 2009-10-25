#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

Runner for automated tests, just pulls in the tests defs from the
other files.


> import Test.Framework
> import Database.HsSqlPpp.Tests.ParserTests
> --import Database.HsSqlPpp.Tests.DatabaseLoaderTests
> import Database.HsSqlPpp.Tests.AstCheckTests
> import Database.HsSqlPpp.Tests.ExtensionTests

> main :: IO ()
> main =
>   defaultMain [
>     parserTests
>    ,astCheckTests
>    --,databaseLoaderTests
>    ,extensionTests
>    ]
