#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

================================================================================

Executable to parse sql from a file, check that it appears to parse ok,
that pretty printing it and parsing that text gives the same parse tree,
and then displays the pretty printed version so you can see how well it's
done (maybe it could interpolate each original statement with its
parsed, pretty printed version so you can more easily check how
authentic the sql is and how much has been silently dropped on the floor

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
