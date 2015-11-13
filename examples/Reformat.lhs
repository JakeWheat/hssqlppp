
> import System.Environment

> --import Language.Haskell.Exts
> --import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Parse
> import Database.HsSqlPpp.Dialect
> import Database.HsSqlPpp.Pretty

> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- LT.readFile f
>   let ast = either (error . show) id $
>             parseStatements defaultParseFlags
>                {pfDialect = sqlServerDialect} f Nothing src
>   LT.putStrLn $ prettyStatements defaultPrettyFlags ast
