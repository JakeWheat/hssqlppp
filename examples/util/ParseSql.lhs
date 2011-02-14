
Example of how to parse sql

compile from project root with something like

ghc -Wall --make -XScopedTypeVariables -XDeriveDataTypeable -XTupleSections  -idevel:src/lib:src/qq:src/postgresql:examples/util examples/util/ParseSql.lhs

> import System.Environment
>
> import Database.HsSqlPpp.Parser
>
> -- nicely formatted show
> import Database.HsSqlPpp.Utils.PPExpr


> main :: IO ()
> main = do
>   argv <- getArgs
>   t <- parseStatementsFromFile (head argv)
>   case t of
>        Left e -> print e
>        Right t1 -> putStrLn $ ppExpr t1
