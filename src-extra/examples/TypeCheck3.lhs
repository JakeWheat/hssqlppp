
> import Control.Monad
> import Data.Generics.Uniplate.Data
> import System.Environment
> import Data.List

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Utils.GroomUtils
> import Text.Groom
> import Database.HsSqlPpp.Tests.TpchData

> main :: IO ()
> main = do
>   [f] <- getArgs
>   query <- readFile f
>   let ast :: [Statement]
>       ast = either (error . show) id
>             $ parsePlpgsql defaultParseFlags "" Nothing query
>       -- type check the ast
>       aast :: [Statement]
>       aast = snd $ typeCheckStatements defaultTypeCheckingFlags cat ast
>   putStrLn $ groomTypes aast
>   where
>     Right cat = updateCatalog
>                   tpchCatalog
>                   defaultTemplate1Catalog

