
> import System.Environment

> --import Language.Haskell.Exts
> --import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Utils.GroomNoAnns
> import Text.Groom

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- readFile f
>   let ast = parsePlpgsql defaultParseFlags {pfDialect=SQLServerDialect}
>                          f Nothing src
>   putStrLn $ either groom groomNoAnns ast
