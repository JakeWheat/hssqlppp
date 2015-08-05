
> {-# LANGUAGE OverloadedStrings #-}
> --import Control.Monad
> --import Data.Generics.Uniplate.Data
> import System.Environment
> --import Data.List

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.Types
> --import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Utils.GroomUtils
> --import Text.Groom
> import Database.HsSqlPpp.Tests.TpchData
> import qualified Data.Text.Lazy.IO as LT
> import Text.Show.Pretty

> main :: IO ()
> main = do
>   [f] <- getArgs
>   query <- LT.readFile f
>   let ast :: [Statement]
>       ast = either (error . show) id
>             $ parsePlpgsql defaultParseFlags {pfDialect = SQLServerDialect } f Nothing query
>       -- type check the ast
>       aast :: [Statement]
>       aast = snd $ typeCheckStatements defaultTypeCheckingFlags { tcfDialect = SQLServerDialect } cat ast
>   putStrLn $ ppShow aast
>   where
>     cat = either (error . ppShow) id $
>           updateCatalog
>                   catu
>                   defaultTSQLCatalog
>     catu = [CatCreateTable "v008_tot_times"
>             [("work_ticket_no", mkCatNameExtra "int")
>             ,("close_date", mkCatNameExtra "date")
>             ,("equipment_name", mkCatNameExtra "varchar")
>             ,("work_in_central_lab", mkCatNameExtra "tinyint")
>             ,("customer_no", mkCatNameExtra "varchar")]]

create table v008_tot_times (
       work_ticket_no int not null,
       close_date date null,
       equipment_name varchar(20) null,
       work_in_central_lab tinyint null,
       customer_no varchar(10) null
);
