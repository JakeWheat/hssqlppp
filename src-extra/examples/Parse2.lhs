
> import System.Environment

> import Language.Haskell.Exts
> import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Parser

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- readFile f
>   let ast = parseStatements defaultParseFlags f Nothing src
>   putStrLn $ either show showNoAnns ast


> showNoAnns :: Show a => a -> String
> showNoAnns s =
>   case parseExp (show s) of
>     ParseOk ast -> prettyPrint (stripA ast)
>     x -> error $ show x
>   where
>     stripA = transformBi $ \x ->
>                case x of
>                  (Paren (RecConstr (UnQual (Ident "Annotation")) _)) ->
>                           Con $ UnQual $ Ident "A"
>                  x' -> x'
