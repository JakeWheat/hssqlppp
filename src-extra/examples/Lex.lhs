
> import Database.HsSqlPpp.Parsing.Lexer
> --import Database.HsSqlPpp.Parser
> import Text.Groom
> import System.Environment
> import Database.HsSqlPpp.Parsing.SqlDialect

> main :: IO ()
> main = do
>   [s] <- getArgs
>   putStrLn $ groom $ lexSql PostgreSQLDialect "" Nothing s
>   --putStrLn $ groom $ parsePlpgsql "" s
