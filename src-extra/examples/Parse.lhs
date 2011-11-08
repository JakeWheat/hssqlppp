
> import System.Environment

> import Database.HsSqlPpp.Parser

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- readFile f
>   let ast = parseStatements defaultParseFlags f Nothing src
>   print ast
