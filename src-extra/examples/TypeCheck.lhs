
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast hiding (ann)

> main :: IO ()
> main = do
>   let query = "select * from t"
>       ast :: QueryExpr
>       Right ast = parseQueryExpr defaultParseFlags "" Nothing query
>       aast :: QueryExpr
>       aast = typeCheckQueryExpr defaultTypeCheckingFlags cat ast
>       ann :: Annotation
>       ann = getAnnotation aast
>       ty :: Maybe Type
>       ty = anType ann
>   print ty
>   where
>     Right cat = updateCatalog
>                   [CatCreateTable "t" [("a", "int")
>                                       ,("b", "int")
>                                       ]]
>                   defaultTemplate1Catalog
