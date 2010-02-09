#! /usr/bin/env runghc

Copyright 2010 Jake Wheat

Code to run the example wrappers. Kept seperate from HsSqlSystem since
the template haskell versions won't compile without the example
database first being set up.

> import Database.HsSqlPpp.Examples.Wrappers.ThTupleExample as TT

> main :: IO();
> main = do
>   TT.runTest >>= putStrLn