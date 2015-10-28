
> {-# LANGUAGE OverloadedStrings #-}
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast hiding (ann)
> import Data.Text.Lazy ()

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
>       ty = fmap teType $ anType ann
>   print ty
>   where
>     Right cat = updateCatalog
>                   [CatCreateTable ("public","t") [("a", mkCatNameExtra "int")
>                                                  ,("b", mkCatNameExtra "int")
>                                                  ]]
>                   defaultTemplate1Catalog
