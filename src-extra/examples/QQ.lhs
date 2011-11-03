
> {-# LANGUAGE QuasiQuotes #-}
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Quote
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Pretty

> test :: Statement
> test = [$sqlStmt|
>
>   create table $(tablename) (
>    $(varname) $(typename)
>   );
>
>         |]
>   where
>     tablename = "my_table"
>     varname = "my_field"
>     typename = "text"

> main :: IO ()
> main = putStrLn $ printStatements defaultPPFlags [test]
