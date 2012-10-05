
> import System.Environment
> import Data.List

> import Text.Groom

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Catalog

> main :: IO ()
> main = do
>   [f] <- getArgs
>   Right ast <- parseStatementsFromFile f
>   let (cat,_) = typeCheckStatements defaultTemplate1Catalog ast
>       cc = deconstructCatalog cat \\ deconstructCatalog defaultTemplate1Catalog
>   putStrLn $ groom cc
