#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

Runner for automated tests, just pulls in the tests defs from the
other files.


> import Test.Framework
> import Database.HsSqlPpp.ParserTests
> import Database.HsSqlPpp.DatabaseLoaderTests
> import Database.HsSqlPpp.AstCheckTests

> main :: IO ()
> main =
>   defaultMain [
>     parserTests
>    ,astCheckTests
>    ,databaseLoaderTests
>    ]
