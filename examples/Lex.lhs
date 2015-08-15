
> import Database.HsSqlPpp.LexicalSyntax
> import Text.Show.Pretty
> import System.Environment
> import qualified Data.Text.IO as T

> main :: IO ()
> main = do
>   [s] <- getArgs
>   f <- T.readFile s
>   putStrLn $ ppShow $ sqlTokens PostgreSQLDialect "" (Just (1,0)) f
>   --putStrLn $ ppShow $ parsePlpgsql "" s
