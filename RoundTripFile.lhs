#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

Used to test the parsing and pretty printing round trip. Takes two
arguments, a source filename and a target filename. If the target file
exists, it quits. Parses the source file then pretty prints it to the
target filename.

> import System
> import Control.Monad
> import System.Directory

> import Parser
> import PrettyPrinter


> main :: IO ()
> main = do
>   args <- getArgs
>   when (length args /= 2) $
>          error "Please pass exactly two filenames, source and target."
>   let (source:target:[]) = args
>   targetExists <- doesFileExist target
>   when (targetExists) $
>          error "the target file name exists already, please delete it or choose a new filename"
>   x <- parseSqlFile source
>   case x of
>        Left er -> putStrLn $ show er
>        Right l -> do
>            let pp = printSql l
>            writeFile target pp
