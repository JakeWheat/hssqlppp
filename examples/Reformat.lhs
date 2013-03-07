
> import System.Environment

> --import Language.Haskell.Exts
> --import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Pretty

> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- LT.readFile f
>   let ast = either (error . show) id $
>             parseStatements defaultParseFlags
>                {pfDialect = SQLServerDialect} f Nothing src
>   LT.putStrLn $ printStatements defaultPPFlags ast
