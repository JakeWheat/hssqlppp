#!/usr/bin/env runghc

> import System

> import ParseWithLexerTest

> main :: IO ()
> main = do
>   args <- getArgs
>   let f = head args
>   putStrLn $ "parsing " ++ show f
>   ts <- readFile f
>   print $ parseSql ts
