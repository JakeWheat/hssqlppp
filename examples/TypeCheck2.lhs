
> import Control.Monad
> --import Data.Generics.Uniplate.Data
> import System.Environment
> import Data.List

> import Database.HsSqlPpp.Parse
> import Database.HsSqlPpp.TypeCheck
> --import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Dialect
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Syntax
> import Database.HsSqlPpp.Utility
> --import Text.Show.Pretty
> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [f] <- getArgs
>   query <- LT.readFile f
>   let ast :: [Statement]
>       ast = either (error . show) id
>             $ parseProcSQL defaultParseFlags "" Nothing query
>       -- type check the ast
>       aast :: [Statement]
>       aast = snd $ typeCheckStatements defaultTypeCheckFlags cat ast
>       -- get a list of scalarexpr and queryexpr nodes with
>       -- no type: indicates an error has occured
>       -- and get the list of type errors with source positions
>       -- from the typechecked ast
>       tes :: [([TypeError],Maybe (String,Int,Int))]
>       noTypeSEs :: [ScalarExpr]
>       noTypeQEs :: [QueryExpr]
>       (_,tes,noTypeQEs,noTypeSEs) = tcTreeInfo aast
>   -- print the type errors in emacs format
>   putStrLn $ emacsShowErrors tes
>   -- double check: if there are untyped expr nodes then
>   -- there should be type errors present as well
>   unless (null noTypeSEs && null noTypeQEs)
>     $ putStrLn $ "untyped nodes present"
>   -- print the result types in the unlikely event that we have some
>   putStrLn $ intercalate " " $ map (show . anType . getAnnotation) aast
>   where
>     cat = diDefaultCatalog postgresDialect
