#!/usr/bin/env runghc

> import System

> import Parser
> import PrettyPrinter

> main :: IO ()
> main = do
>   args <- getArgs
>   let f = head args
>   putStrLn $ "parsing " ++ show f
>   x <- parseSqlFile f
>   case x of
>        Left er -> error $ show er
>        Right l -> do
>            putStrLn $ show l
>            putStrLn "END OF AST END OF AST END OF AST END OF AST END OF AST END OF AST"
>            putStrLn $ printSql l
>   return ()
