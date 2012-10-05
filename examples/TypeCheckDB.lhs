
> {-# LANGUAGE OverloadedStrings #-}
> import System.Environment

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast hiding (ann)

> import Database.HsSqlPpp.Utils.CatalogReader
> import qualified Data.Text.Lazy ()
> main :: IO ()
> main = do
>   [cs] <- getArgs
>   cus <- readCatalogFromDatabase cs
>   let Right cat = updateCatalog cus defaultCatalog
>       query = "select * from t"
>       ast :: QueryExpr
>       Right ast = parseQueryExpr defaultParseFlags "" Nothing query
>       aast :: QueryExpr
>       aast = typeCheckQueryExpr defaultTypeCheckingFlags cat ast
>       ann :: Annotation
>       ann = getAnnotation aast
>       ty :: Maybe Type
>       ty = anType ann
>   print ty
