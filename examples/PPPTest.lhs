

Parses some sql, then pretty prints it and parses it again to check
that pretty print . parse == id

> import System.Environment

> --import Language.Haskell.Exts
> --import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.Utility
> import Control.Monad
> import Data.Algorithm.Diff
> import Text.Groom
> import Data.Generics.Uniplate.Data
> import Data.Data
> import Database.HsSqlPpp.Annotation
> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- LT.readFile f
>   let ast = resetAnnotations
>             $ either (error . show) id
>             $ parseStatements defaultParseFlags f Nothing src
>       pp = printStatements defaultPPFlags ast
>       ast2 = resetAnnotations
>              $ either (error . show) id
>              $ parseStatements defaultParseFlags f Nothing pp
>   when (ast /= ast2) $ do
>     putStrLn "error: pretty print . parse /= id"
>     putStrLn $ unlines $ diff (lines $ groom ast) (lines $ groom ast2)


> diff :: [String] -> [String] -> [String]
> diff a b =
>   map ppdfl diffs
>   where
>     diffs = getDiff a b
>     ppdfl (d,t) = case d of
>                     F -> "- " ++ t
>                     S -> "+ " ++ t
>                     B -> t
