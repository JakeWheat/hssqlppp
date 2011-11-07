
> import System.Environment

> --import Language.Haskell.Exts
> --import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Utils.GroomNoAnns

> main :: IO ()
> main = do
>   [f] <- getArgs
>   ast <- parseStatementsFromFile defaultParseFlags {pfDialect=SQLServerDialect} f
>   putStrLn $ groomNoAnns ast
