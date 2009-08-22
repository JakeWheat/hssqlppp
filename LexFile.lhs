#! /usr/bin/env runhaskell

Copyright 2009 Jake Wheat

> import Lexer
> import System

> main :: IO ()
> main = do
>   args <- getArgs
>   let f = head args
>   putStrLn $ "parsing " ++ show f
>   x <- lexSqlFile f
>   return ()
>   case x of
>        Left er -> print er
>        Right l -> mapM_ print l

