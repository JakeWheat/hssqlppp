
> import UUAGCHaddocks

> main :: IO ()
> main = do
>   postprocessUuagc ["src/Database/HsSqlPpp/Internals/AstInternal.ag"
>                    ,"src/Database/HsSqlPpp/Internals/Annotation.ag"]
>                    [("Annotation",[]),("QueryExpr", ["Select"])]
>                    "src/Database/HsSqlPpp/Internals/AstInternal.hs"
