
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Pretty

> data MakeSelect = MakeSelect [String] String

> sqlGen :: MakeSelect -> QueryExpr
> sqlGen (MakeSelect cols tb) =
>   makeSelect
>   {selSelectList = SelectList emptyAnnotation
>                    $ map si cols
>   ,selTref = [Tref emptyAnnotation
>               (Name emptyAnnotation [Nmc tb])]}
>   where
>     si i = SelExp emptyAnnotation
>              (Identifier emptyAnnotation
>              (Name emptyAnnotation [Nmc i]))

> main :: IO ()
> main = do
>   let s = MakeSelect ["a", "b"] "t"
>   putStrLn $ printQueryExpr defaultPPFlags $ sqlGen s

