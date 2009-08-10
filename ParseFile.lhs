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
>            --check roundtrip
>            case parseSql (printSql l) of
>              Left er -> error $ "roundtrip failed: " ++ show er
>              Right l' -> if l == l'
>                            then putStrLn "roundtrip ok"
>                            else putStrLn "roundtrip failed: different ast"
>   return ()
