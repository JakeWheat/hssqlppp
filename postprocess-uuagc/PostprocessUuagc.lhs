
> import UUAGCHaddocks

> main :: IO ()
> main = do
>   postprocessUuagc ["hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.ag"
>                    ,"hssqlppp/src/Database/HsSqlPpp/Internals/Annotation.ag"]
>                    [("Annotation",[]),("QueryExpr", ["Select"])]
>                    "hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hs"
