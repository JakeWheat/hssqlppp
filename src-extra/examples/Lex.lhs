
> import Database.HsSqlPpp.Utility
> import Text.Groom
> import System.Environment
> import Database.HsSqlPpp.SqlDialect

> main :: IO ()
> main = do
>   [s] <- getArgs
>   f <- readFile s
>   putStrLn $ groom $ lexSql PostgreSQLDialect "" Nothing f
>   --putStrLn $ groom $ parsePlpgsql "" s
