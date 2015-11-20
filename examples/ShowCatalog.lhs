
> import System.Environment
> import Data.List

> import Text.Show.Pretty

> import Database.HsSqlPpp.Parse
> import Database.HsSqlPpp.TypeCheck
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.Dialect
> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- LT.readFile f
>   let Right ast = parseStatements defaultParseFlags f Nothing src
>       inCat = diDefaultCatalog ansiDialect
>   let (cat,_) = typeCheckStatements defaultTypeCheckFlags inCat ast
>       cc = deconstructCatalog cat \\ deconstructCatalog inCat
>   putStrLn $ ppShow cc
