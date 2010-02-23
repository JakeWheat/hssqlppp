Copyright 2010 Jake Wheat

Convert the Chaos 2010 example sql to html, and do some stuff with
hssqlppp to it and show the results.

> {-# LANGUAGE ScopedTypeVariables #-}
> module Database.HsSqlPpp.DevelTools.DoChaosSql
>     (doChaosSql) where
>
> import System.FilePath.Find
> import System.FilePath
> import Control.Monad.Error
>
> import Database.HsSqlPpp.DevelTools.PandocUtils
> import Database.HsSqlPpp.Examples.AnnotateSource2
> import Database.HsSqlPpp.Examples.Extensions.ChaosExtensions
> import Database.HsSqlPpp.Examples.Chaos2010
>
> doChaosSql :: (PandocType
>                -> String
>                -> Input
>                -> String
>                -> IO ())
>            -> IO ()
> doChaosSql pf = do
>   -- create html versions of original source
>   sf <- sourceFiles
>   mapM_ convFile sf
>   -- do annotated source files
>   new <- liftIO (annotateSource2 (Just chaosExtensions) Nothing "template1" chaosFiles)
>   forM_ new (\(f,c) -> pf Txt (snd (splitFileName f) ++ " transformed")
>                           (Str c) (f ++ ".tr.html"))
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
>          (snd $ splitFileName f)
>          (File f)
>          (f ++ ".html")


TODO:

use the new annotate, then we can present the original pristine
source, and the source that has been scribbled all over by hsssqlppp.

add a separate page to summarize the resulant catalog, use the modules
to split this into sections. When the export lists are done, use this
to divide each section into public, private.

add a separate page to list the type errors with links to the source
where they occur (both the original and mangled source)
