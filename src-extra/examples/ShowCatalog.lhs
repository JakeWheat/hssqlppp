
> import System.Environment
> import Data.List

> import Text.Groom

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast

> main :: IO ()
> main = do
>   [f] <- getArgs
>   Right ast <- parseStatementsFromFile f
>   let (cat,aast) = typeCheckStatements defaultTemplate1Catalog ast
>       cc = deconstructCatalog cat \\ deconstructCatalog defaultTemplate1Catalog
>   putStrLn $ groom aast
>   putStrLn $ groom cc
