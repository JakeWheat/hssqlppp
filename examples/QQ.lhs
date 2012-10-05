
> {-# LANGUAGE QuasiQuotes #-}
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Quote
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Pretty

> test :: Statement
> test = [$sqlStmt|
>
>   create table $m(tablename) (
>    $m(varname) $m(typename)
>   );
>
>         |]
>   where
>     tablename = [sqlNameComponent|my_table|]
>     varname = [sqlNameComponent|my_field|]
>     typename = [sqlNameComponent|text|]

> main :: IO ()
> main = putStrLn $ printStatements defaultPPFlags [test]
