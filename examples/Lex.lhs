
> import Database.HsSqlPpp.Utility
> import Text.Groom
> import System.Environment
> import Database.HsSqlPpp.SqlDialect
> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [s] <- getArgs
>   f <- LT.readFile s
>   putStrLn $ groom $ lexSql PostgreSQLDialect "" Nothing f
>   --putStrLn $ groom $ parsePlpgsql "" s
