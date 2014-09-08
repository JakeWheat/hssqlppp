
> import System.Environment
> import Data.List

> import Text.Groom

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Catalog
> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- LT.readFile f
>   let Right ast = parseStatements defaultParseFlags f Nothing src
>   let (cat,_) = typeCheckStatements defaultTypeCheckingFlags defaultTemplate1Catalog ast
>       cc = deconstructCatalog cat \\ deconstructCatalog defaultTemplate1Catalog
>   putStrLn $ groom cc
