
> import Database.HsSqlPpp.LexicalSyntax
> import Text.Groom
> import System.Environment
> import qualified Data.Text.IO as T

> main :: IO ()
> main = do
>   [s] <- getArgs
>   f <- T.readFile s
>   putStrLn $ groom $ sqlTokens PostgreSQLDialect ("",1,0) f
>   --putStrLn $ groom $ parsePlpgsql "" s
