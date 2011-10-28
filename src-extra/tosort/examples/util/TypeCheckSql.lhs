
Example of parsing and typechecking some sql, displays the type errors
only.


> import System.Environment

> import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.SqlTypes
>
> import Database.HsSqlPpp.Parser
>
> main :: IO ()
> main = do
>   argv <- getArgs
>   t <- parseStatementsFromFile (head argv)
>   case t of
>        Left e -> print e
>        Right t1 -> do
>          let tt :: [Statement]
>              tt = snd $ typeCheckStatements defaultTemplate1Catalog t1
>              errs :: [TypeError]
>              errs = universeBi tt
>          mapM_ print errs
