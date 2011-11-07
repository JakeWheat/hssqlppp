
> import Database.HsSqlPpp.Parsing.Lexer
> import Database.HsSqlPpp.Parser
> import Text.Groom
> import System.Environment

> main :: IO ()
> main = do
>   [s] <- getArgs
>   putStrLn $ groom $ lexSqlText "" s
>   --putStrLn $ groom $ parsePlpgsql "" s
