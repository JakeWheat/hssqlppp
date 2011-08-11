
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast

> main :: IO ()
> main = do
>   let query = "select * from t"
>       ast :: QueryExpr
>       Right ast = parseQueryExpr "" query
>       aast :: QueryExpr
>       aast = typeCheckQueryExpr cat ast
>       ann :: Annotation
>       ann = getAnnotation aast
>       ty :: Maybe Type
>       ty = atype ann
>   print ty
>   where
>     Right cat = updateCatalog defaultTemplate1Catalog
>                   [CatCreateTable "t" [("a", typeInt)
>                                       ,("b", typeInt)
>                                       ] []]
