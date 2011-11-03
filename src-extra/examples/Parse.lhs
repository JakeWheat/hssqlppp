
> import System.Environment

> import Database.HsSqlPpp.Parser

> main :: IO ()
> main = do
>   [f] <- getArgs
>   ast <- parseStatementsFromFile defaultParseFlags f
>   print ast
