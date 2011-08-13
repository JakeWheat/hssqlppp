
Convert qgen output into sql server format

> import Data.Generics.Uniplate.Data

> import System.Environment

> import Database.HsSqlPpp.Parser

> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Pretty
> import Data.Data

> main :: IO ()
> main = do
>   [fn] <- getArgs
>   f <- readFile fn
>   putStrLn $ fixSql f

> fixSql :: String -> String
> fixSql sql =
>   let Right qe = parseStatements "" sql
>       qe' = fixSqlAst qe
>   in printStatements qe'

> fixSqlAst :: Data a => a -> a
> fixSqlAst = fixDate . fixSubstring . fixExtract . fixIntervals

 dateadd(day, -90, ‘1998-12-01’)
Instead of:
 date ‘1998-12-01’ - interval ‘90’ day

> fixIntervals :: Data a => a -> a
> fixIntervals = transformBi $ \x -> case x of
>   FunCall a f [TypedStringLit _ (SimpleTypeName _ "date") d
>                 ,Interval _ v i _]
>     | f `elem` ["+","-"]
>     , Just i' <- lookup i [(IntervalDay,"day")
>                           ,(IntervalMonth,"month")
>                           ,(IntervalYear,"year")]
>     -> FunCall a "dateAdd" [Identifier a i'
>                            ,case f of
>                               "-" -> FunCall a "u-" [NumberLit a v]
>                               _ -> NumberLit a v
>                            ,StringLit a d]
>   x' -> x'

 datepart(year,l_shipdate)
Instead of:
 extract(year from l_shipdate)

> fixExtract :: Data a => a -> a
> fixExtract = transformBi $ \x -> case x of
>   Extract a ExtractYear e -> FunCall a "datepart" [Identifier a "year", e]
>   x' -> x'


 substring(c_phone,1,2)
Instead of:
 substring(c_phone from 1 for 2)

> fixSubstring :: Data a => a -> a
> fixSubstring = transformBi $ \x -> case x of
>   FunCall a "!substring" [e,fr,fo] -> FunCall a "substring" [e,fr,fo]
>   x' -> x'

 ‘1998-12-01’
Instead of:
 date ‘1998-12-01’

> fixDate :: Data a => a -> a
> fixDate = transformBi $ \x -> case x of
>    TypedStringLit a (SimpleTypeName _ "date") d -> StringLit a d
>    x' -> x'