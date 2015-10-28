
> import System.Environment

> import Database.HsSqlPpp.Parse

> import qualified Data.Text.Lazy.IO as T
> --import qualified Data.Text.Lazy as T

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- T.readFile f
>   let ast = parseStatements defaultParseFlags f Nothing src
>   print ast
