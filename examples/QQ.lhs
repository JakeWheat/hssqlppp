
> {-# LANGUAGE QuasiQuotes #-}
> import Database.HsSqlPpp.Syntax
> import Database.HsSqlPpp.Quote
> --import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Pretty
> import qualified Data.Text.Lazy.IO as LT

> test :: Statement
> test = [sqlStmt|
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
> main = LT.putStrLn $ prettyStatements defaultPrettyFlags [test]
