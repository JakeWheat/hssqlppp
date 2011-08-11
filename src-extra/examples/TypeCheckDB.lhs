
> import System.Environment

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Utils.CatalogReader

> main :: IO ()
> main = do
>   [cs] <- getArgs
>   cus <- readCatalogFromDatabase cs
>   let Right cat = updateCatalog defaultCatalog cus
>       query = "select * from t"
>       ast :: QueryExpr
>       Right ast = parseQueryExpr "" query
>       aast :: QueryExpr
>       aast = typeCheckQueryExpr cat ast
>       ann :: Annotation
>       ann = getAnnotation aast
>       ty :: Maybe Type
>       ty = atype ann
>   print ty
