
> import System.Environment
> import Data.List

> import Text.Show.Pretty

> import Database.HsSqlPpp.Parse
> import Database.HsSqlPpp.TypeCheck
> import Database.HsSqlPpp.Catalog
> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- LT.readFile f
>   let Right ast = parseStatements defaultParseFlags f Nothing src
>   let (cat,_) = typeCheckStatements defaultTypeCheckFlags defaultTemplate1Catalog ast
>       cc = deconstructCatalog cat \\ deconstructCatalog defaultTemplate1Catalog
>   putStrLn $ ppShow cc
