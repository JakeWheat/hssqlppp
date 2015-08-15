

test program to show using the new incremental lexer
The idea is to use getline to get lines of sql on the console

when a complete statement is read, call a function. Want to support
sql statements which cover multiple lines and pasting in multiple
statements at once/ entering multiple statements on one line.

Incremental lexer cancelled.

To reproduce the behaviour, read until get a semi colon.

When is a semi colon not the end of a command?
Inside a string, quoted identifier, comment, copy payload.

Write a custom little incremental scanner. Still have tricky issues
(postgres copy from stdin) and dialect issues (ms quoted ids are [],
myssql are ``, etc.).

> {-# LANGUAGE OverloadedStrings,TupleSections,ScopedTypeVariables #-}

> --import qualified Data.Text.IO as T
> --import qualified Data.Text as T
> --import qualified Data.Text.Lazy as LT
> --import qualified Data.Text.Lazy.IO as LT
> --import Data.Attoparsec.Text
> --import Control.Applicative
> import Data.Char
> --import Debug.Trace
> import System.IO
> --import Data.Ratio
> import Data.List hiding (takeWhile)
> import Prelude hiding (takeWhile)
> --import Control.Monad
> --import System.Environment
> --import Test.Framework
> --import Test.HUnit
> --import Test.Framework.Providers.HUnit
> --import Database.HsSqlPpp.LexicalSyntax
> import Control.Monad
> --import Debug.Trace

TODO: write test cases, pull out the parsing code to do this

> readAnotherStatement :: String -> IO ()
> readAnotherStatement prefix' = do
>     let prefix = if all isSpace prefix'
>                  then ""
>                  else prefix' ++ "\n"
>     --putStr $ "> " ++ prefix
>     when (null prefix) (putStr "> " >> hFlush stdout)
>     line <- getLine
>     --putStrLn $ "got: " ++ line
>     let stmts :: [String]
>         (stmts, leftOver) = parseStatements (prefix ++ line)
>     --putStrLn $ "stuff :" ++ show (stmts, leftOver)
>     when (not (null stmts)) $ do
>         putStr "ST: "
>         putStrLn $ intercalate "\nST: " stmts
>         hFlush stdout
>     --when (not (null leftOver)) $
>     --    putStrLn $ "LEFTOVER: " ++ leftOver
>     readAnotherStatement leftOver
>     --parseStatements stmts curSt xs
>     --   | trace ("stmts: " ++ show (stmts, curSt, xs)) False = undefined


> parseStatements :: String -> ([String],String)
> parseStatements txt = statements [] "" txt
>   where
>     statements stmts curSt (';':xs) =
>         statements (stmts ++ [curSt ++ ";"]) "" xs
>     -- switch to quoted iden
>     statements stmts curSt ('"':xs) =
>         quotedIden stmts (curSt ++ "\"") xs
>     -- switch to string lit
>     statements stmts curSt ('\'':xs) =
>         stringLit stmts (curSt ++ "'") xs
>     -- switch to block comment
>     statements stmts curSt ('/':'*':xs) =
>         blockComment stmts (curSt ++ "/*") xs
>     -- switch to line comment
>     statements stmts curSt ('-':'-':xs) =
>         lineComment stmts (curSt ++ "--") xs
>     -- normal char
>     statements stmts curSt (x:xs) =
>         statements stmts (curSt ++ [x]) xs
>     -- end of current input
>     statements stmts curSt [] = (stmts, curSt)

>     -- quotedIden stmts curSt xs
>     --    | trace ("qi: " ++ show (stmts, curSt, xs)) False = undefined
>     quotedIden stmts curSt ('"':'"':xs) =
>         quotedIden stmts (curSt ++ "\"\"") xs
>     quotedIden stmts curSt ('"':xs) =
>         statements stmts (curSt ++ "\"") xs
>     quotedIden stmts curSt (x:xs) =
>         quotedIden stmts (curSt ++ [x]) xs
>     quotedIden stmts curSt [] = (stmts, curSt)

>     stringLit stmts curSt ('\'':'\'':xs) =
>         stringLit stmts (curSt ++ "''") xs
>     stringLit stmts curSt ('\'':xs) =
>         statements stmts (curSt ++ "'") xs
>     stringLit stmts curSt (x:xs) =
>         stringLit stmts (curSt ++ [x]) xs
>     stringLit stmts curSt [] = (stmts, curSt)

>     blockComment stmts curSt ('*':'/':xs) =
>         statements stmts (curSt ++ "*/") xs
>     blockComment stmts curSt (x:xs) =
>         blockComment stmts (curSt ++ [x]) xs
>     blockComment stmts curSt [] = (stmts, curSt)

>     lineComment stmts curSt ('\n':xs) =
>         statements stmts (curSt ++ "\n") xs
>     lineComment stmts curSt (x:xs) =
>         lineComment stmts (curSt ++ [x]) xs
>     lineComment stmts curSt [] = (stmts, curSt)



> main :: IO ()
> main = readAnotherStatement ""
