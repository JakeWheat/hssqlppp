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
>        Left er -> do
>            fs <- readFile f
>            error $ showEr er fs
>        Right l -> do
>            print l
>            putStrLn "END OF AST END OF AST END OF AST END OF AST END OF AST END OF AST"
>            putStrLn $ printSql l
>            --check roundtrip
>            case parseSql (printSql l) of
>              Left er -> do
>                     fs <- readFile f
>                     error $ "roundtrip failed: " ++ showEr er fs
>              Right l' -> if l == l'
>                            then putStrLn "roundtrip ok"
>                            else putStrLn "roundtrip failed: different ast"
>   return ()
