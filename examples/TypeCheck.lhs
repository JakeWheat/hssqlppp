
> {-# LANGUAGE OverloadedStrings #-}
> import Database.HsSqlPpp.Parse
> import Database.HsSqlPpp.TypeCheck
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Dialect
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Syntax hiding (ann)
> import Data.Text.Lazy ()

> main :: IO ()
> main = do
>   let query = "select * from t"
>       ast :: QueryExpr
>       Right ast = parseQueryExpr defaultParseFlags "" Nothing query
>       aast :: QueryExpr
>       aast = typeCheckQueryExpr defaultTypeCheckFlags cat ast
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
>                   (diDefaultCatalog ansiDialect)
