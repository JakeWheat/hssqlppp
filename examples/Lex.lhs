
> import Database.HsSqlPpp.Lex
> import Text.Show.Pretty
> import System.Environment
> import qualified Data.Text.Lazy.IO as L
> import Database.HsSqlPpp.Dialect

> main :: IO ()
> main = do
>   [s] <- getArgs
>   f <- L.readFile s
>   putStrLn $ ppShow $ lexTokens postgresDialect "" (Just (1,0)) f
>   --putStrLn $ ppShow $ parsePlpgsql "" s
