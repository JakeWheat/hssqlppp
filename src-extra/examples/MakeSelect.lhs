
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Pretty


> data MakeSelect = MakeSelect [String] String

> sqlGen :: MakeSelect -> QueryExpr
> sqlGen (MakeSelect cols tb) =
>   Select emptyAnnotation Dupes
>          sl tr
>          Nothing [] Nothing [] Nothing Nothing
>   where
>     sl = SelectList emptyAnnotation
>                     (map si cols)
>     tr = [Tref emptyAnnotation
>                (SQIdentifier emptyAnnotation [tb])
>                (NoAlias emptyAnnotation)]
>     si i = SelExp emptyAnnotation
>                   (Identifier emptyAnnotation
>                               i)

> main :: IO ()
> main = do
>   let s = MakeSelect ["a", "b"] "t"
>   putStrLn $ printQueryExpr $ sqlGen s

