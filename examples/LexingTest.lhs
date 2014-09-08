

test program to show using the new incremental lexer
The idea is to use getline to get lines of sql on the console

when a complete statement is read, call a function. Want to support
sql statements which cover multiple lines and pasting in multiple
statements at once/ entering multiple statements on one line.

> {-# LANGUAGE OverloadedStrings,TupleSections,ScopedTypeVariables #-}

> import qualified Data.Text.IO as T
> import qualified Data.Text as T
> import qualified Data.Text.Lazy as LT
> import qualified Data.Text.Lazy.IO as LT
> import Data.Attoparsec.Text
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
> import Database.HsSqlPpp.LexicalSyntax



> type Partial = T.Text -> SResult
> type SResult = Result [(Position,Token)]

> dialect :: SQLSyntaxDialect
> dialect = PostgreSQLDialect --SQLServerDialect

> main :: IO ()
> main = do
>         let readloop :: Maybe Partial -> IO ()
>             readloop mpr = do
>                 putStr $ maybe ("> ") (const ". ") mpr
>                 hFlush stdout
>                 line <- T.getLine
>                 loopText mpr (T.snoc line '\n')
>             loopText :: Maybe Partial -> T.Text -> IO ()
>             loopText mpr txt = do
>                 let y = case mpr of
>                           Nothing -> --trace ("parse '" ++ T.unpack txt ++ "'") $
>                                      parse (sqlStatement ("",1,0)) txt
>                           Just pr -> --trace ("feed '" ++ T.unpack txt ++ "'") $
>                                      feed (Partial pr) txt
>                 case y of
>                   Done leftover st -> do
>                          runStatement st
>                          -- keep processing the partial results
>                          -- in case there are multiple statements
>                          -- todo: checks the leftover to see if
>                          -- it is just whitespace, add to this
>                          -- checking to see if it is just comments
>                          -- and whitespace
>                          if not (T.all isSpace leftover)
>                             then loopText Nothing leftover
>                             else readloop Nothing
>                   Partial pr -> readloop (Just pr)
>                   Fail _leftover ctx err -> do
>                       putStrLn $ err ++ " " ++ show ctx
>                       readloop Nothing
>         readloop Nothing

> runStatement :: [(Position,Token)] -> IO ()
> runStatement s = do --trace "runstatement" $
>                  putStr "Statement: "
>                  LT.putStrLn $ LT.concat (map (prettyToken dialect . snd) s)
>                  putStrLn $ intercalate "\n" $ map show s

> sqlStatement :: Position -> Parser [(Position,Token)]
> sqlStatement p = sqlStatement' p []
> sqlStatement' :: Position -> [(Position,Token)] -> Parser [(Position,Token)]
> sqlStatement' p acc = do
>     -- keep lexing until get a ';' symbol
>     x <- sqlToken dialect p
>     let ts = x:acc
>     if snd x == Symbol ";"
>       then return $ reverse ts
>       else
>           -- dodgy: the work out the position
>           -- by pretty printing the previous token
>           -- and adding it to the current position
>           let pt = prettyToken dialect (snd x)
>               p' = addPosition p pt
>           in sqlStatement' p' ts

-----------------------------------------------


TODO:
support copy from stdin for postgresql (later)
replace the existing lexer in hssqlppp
public api in hssqlppp
