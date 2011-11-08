
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
>       -- get a list of scalarexpr and queryexpr nodes with
>       -- no type: indicates an error has occured
>       noTypeSEs :: [ScalarExpr]
>       noTypeSEs = [x | x <- universeBi aast
>                      , atype (getAnnotation x) == Nothing]
>       noTypeQEs :: [QueryExpr]
>       noTypeQEs = [x | x <- universeBi aast
>                      , atype (getAnnotation x) == Nothing]
>       -- get the list of type errors with source positions
>       -- from the typechecked ast
>       tes :: [([TypeError],Maybe (String,Int,Int))]
>       tes = [(e,sp) | a@(Annotation {}) <- universeBi aast
>                     , let e = errs a
>                     , let sp = asrc a
>                     , not (null e)]
>   -- print the type errors in emacs format
>   forM_ tes $ \(es,sp) -> do
>       case sp of
>         Nothing -> putStrLn "unknown source"
>         Just (_fn,l,c) -> putStrLn $ f ++ ":" ++ show l ++ ":" ++ show c ++ ":"
>       mapM_ (putStrLn . show) es
>   -- double check: if there are untyped expr nodes then
>   -- there should be type errors present as well
>   unless (null noTypeSEs && null noTypeQEs)
>     $ putStrLn $ "untyped nodes present"
>   -- print the result types in the unlikely event that we have some
>   putStrLn $ intercalate " " $ map (show . atype . getAnnotation) aast
>   where
>     cat = defaultTemplate1Catalog
