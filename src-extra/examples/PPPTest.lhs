

Parses some sql, then pretty prints it and parses it again to check
that pretty print . parse == id

> import System.Environment

> import Language.Haskell.Exts
> import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Pretty
> import Control.Monad

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- readFile f
>   let ast = either (error . show) id
>             $ parseStatements defaultParseFlags f Nothing src
>       pp = printStatements defaultPPFlags ast
>       ast2 = parseStatements defaultParseFlags f Nothing pp
>   when (ast /= ast) $ putStrLn "error: pretty print . parse /= id"
