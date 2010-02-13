Copyright 2010 Jake Wheat

Convert the Chaos 2010 example sql to html, and do some stuff with
hssqlppp to it and show the results.

> module Database.HsSqlPpp.DevelTools.DoChaosSql
>     (doChaosSql) where

> --import System.Directory
> --import Control.Monad
> --import Text.Pandoc hiding (Str)
> --import System.Cmd
> import System.FilePath.Find
> --import System.IO
> import System.FilePath
> --import Text.Highlighting.Kate
> --import Debug.Trace
> --import Text.XHtml.Strict hiding (title,src)
> --import Data.DateTime
> --import Text.RegexPR
> --import Debug.Trace
> --import Control.Applicative
> --import qualified Data.List as L
> --import Data.Char
> --import Text.XML.HaXml hiding (find,x)
> --import Text.PrettyPrint (render)
> --import Text.XML.HaXml.Pretty
>
> import Database.HsSqlPpp.DevelTools.PandocUtils
> --import Database.HsSqlPpp.Utils.Utils
> --import Database.HsSqlPpp.Utils.Here
> --import Database.HsSqlPpp.DevelTools.TestFileProcessor

> doChaosSql :: (PandocType
>                -> String
>                -> Input
>                -> String
>                -> IO ())
>            -> IO ()
> doChaosSql pf = do
>   -- create html versions
>   sourceFiles >>= mapM_ convFile
>   -- create short index
>   return ()
>   where
>     sourceFiles = do
>       find always sourceFileP "testfiles/chaos2010sql/"
>     sourceFileP = extension ==? ".sql" ||? extension ==? ".txt"
>     convFile f = do
>       pf (case takeExtension f of
>             ".txt" -> Txt
>             ".sql" -> Sql
>             _ -> error $ "unrecognised extension in dochaosql" ++ f)
>          (snd $splitFileName f)
>          (File f)
>          (f ++ ".html")

