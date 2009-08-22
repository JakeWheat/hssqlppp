#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

================================================================================

Executable to parse sql from a file, check that it appears to parse ok,
that pretty printing it and parsing that text gives the same ast,
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
>   mapM_ parseFile args
>   where
>     parseFile f = do
>       putStrLn $ "parsing " ++ show f
>       x <- parseSqlFile f
>       case x of
>            Left er -> putStrLn $ show er
>            Right l -> do
>                --print l
>                --putStrLn "END OF AST END OF AST END OF AST END OF AST END OF AST END OF AST"
>                putStrLn "parse ok"
>                let pp = printSql l
>                --putStrLn pp
>                --check roundtrip
>                case parseSql pp of
>                  Left er -> do
>                         error $ "roundtrip failed: " ++ show er
>                  Right l' -> if l == l'
>                                then putStrLn "roundtrip ok"
>                                else putStrLn "roundtrip failed: different ast"
>       return ()
