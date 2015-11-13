
Convert qgen output into sql server format

> {-# LANGUAGE QuasiQuotes #-}
> import Data.Generics.Uniplate.Data

> import System.Environment
> import Data.Data

> import Database.HsSqlPpp.Parse
> import Database.HsSqlPpp.Syntax
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.Quote
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Dialect
> import qualified Data.Text.Lazy as LT
> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [fn] <- getArgs
>   f <- LT.readFile fn
>   LT.putStrLn $ fixSql f

> fixSql :: LT.Text -> LT.Text
> fixSql sql =
>   let qe = either (error . show) id
>            -- tpch outputs sql standard syntax
>            -- which matches the default dialect for
>            -- hssqlppp better that the sql server dialect
>            $ parseStatements defaultParseFlags "" Nothing sql
>       qe' = fixSqlAst qe
>   in prettyStatements defaultPrettyFlags {ppDialect = sqlServerDialect} qe'

> fixSqlAst :: Data a => a -> a
> fixSqlAst = fixDate . fixSubstring . fixExtract . fixIntervals

 dateadd(day, -90, ‘1998-12-01’)
Instead of:
 date ‘1998-12-01’ - interval ‘90’ day

> fixIntervals :: Data a => a -> a
> fixIntervals = transformBi $ \x -> case x of
>   [sqlExpr| $e(a) + $e(b) |] | Just (i,v,d) <- dateInfo a b ->
>      [sqlExpr| dateAdd($e(i),$e(v),$e(d))|]
>   [sqlExpr| $e(a) - $e(b) |]| Just (i,v,d) <- dateInfo a b ->
>      [sqlExpr| dateAdd($e(i),-$e(v),$e(d))|]
>   x' -> x'
>   where
>     dateInfo [sqlExpr| $n(date) $s(d)|]
>              (Interval _ v i _)
>              | Just i' <- lookup i [(IntervalDay,[sqlExpr| day |])
>                                    ,(IntervalMonth,[sqlExpr|month|])
>                                    ,(IntervalYear,[sqlExpr|year|])]
>              , Name _ [Nmc "date"] <- date
>              = Just (i',NumberLit emptyAnnotation v,StringLit emptyAnnotation d)
>     dateInfo _ _ = Nothing

 datepart(year,l_shipdate)
Instead of:
 extract(year from l_shipdate)

> fixExtract :: Data a => a -> a
> fixExtract = transformBi $ \x -> case x of
>   [sqlExpr| extract(year from $e(expr) ) |] ->
>       [sqlExpr| datepart(year,$e(expr)) |]
>   x' -> x'


 substring(c_phone,1,2)
Instead of:
 substring(c_phone from 1 for 2)

> fixSubstring :: Data a => a -> a
> fixSubstring = transformBi $ \x -> case x of
>   [sqlExpr| substring($e(i) from $e(a) for $e(b)) |] ->
>       [sqlExpr| substring($e(i),$e(a),$e(b)) |]
>   x' -> x'

 ‘1998-12-01’
Instead of:
 date ‘1998-12-01’

> fixDate :: Data a => a -> a
> fixDate = transformBi $ \x -> case x of
>    TypedStringLit a (SimpleTypeName _  (Name _ [Nmc "date"])) d -> StringLit a d
>    x' -> x'
