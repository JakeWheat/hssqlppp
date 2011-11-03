
> import System.Environment

> import Language.Haskell.Exts
> import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Parser

> main :: IO ()
> main = do
>   [f] <- getArgs
>   ast <- parseStatementsFromFile defaultParseFlags f
>   putStrLn $ showNoAnns ast


> showNoAnns :: Show a => a -> String
> showNoAnns = p stripA
>   where
>     stripA :: Exp -> Exp
>     stripA = transformBi $ \x ->
>                case x of
>                  (Paren (RecConstr (UnQual (Ident "Annotation")) _)) ->
>                           Con $ UnQual $ Ident "Ann"
>                  x1 -> x1
>     p f s =
>         case parseExp (show s) of
>           ParseOk ast -> prettyPrint (f ast)
>           x -> error $ show x
