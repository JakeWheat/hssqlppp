
ghc -XDeriveDataTypeable -isrc:devel:tests:examples/extensions:examples/dbload:examples/chaos:examples/hssqlppputil -hide-package monads-fd --make devel/MakeWebsite.lhs && devel/MakeWebsite

> import System.FilePath.Find
> import System.FilePath
> import Data.Char
> import System.Directory
> import Control.Monad

> import Text.DocTool.DocTool
> import TestFileProcessor
> import DoChaosSql

> main :: IO ()
> main = do
>   doesDirectoryExist "website" >>=
>     \l -> when l $ removeDirectoryRecursive "website"
>   f <- fileList
>   --mapM_ (\(OutputFile _ _ t _) -> putStrLn t) f
>   docify "website/" f


> fileList :: IO [OutputFile]
> fileList = do
>   wso <- doF "docs/website/" (makeRelative "docs/website/")
>   src <- doF "src/" ("source" </>)
>   ex <- doF "examples/" ("source" </>)
>   tsts <- doF "tests/" ("source" </>)
>   dv <- doF "devel/" ("source" </>)
>   qq <- quasiQuoteTestsTable

>   let tfp = [OutputFile (Text parserTestsTable)
>                         Txt "website/ParserTests.html"
>                         "HsSqlPpp parser examples"
>             ,OutputFile (Text typeCheckTestsTable)
>                         Txt "website/TypeCheckTests.html"
>                         "HsSqlPpp type checking examples"
>             ,OutputFile (Text qq)
>                         Txt "website/QuasiQuoteTests.html"
>                         "HsSqlPpp quasiquotation examples"]
>   trch1 <- getTransformedChaosSql
>   let trch = flip map trch1 $ \(t,f,s) ->
>                       OutputFile (Text s) Txt ("website/source" </> f) t
>   return $ concat [wso,src,ex,tsts,dv,tfp,trch]
>   where
>     doF fl c = find always supportedFileP fl
>                >>= return . map (toOf c)
>     ft f = case map toLower (takeExtension f) of
>              ".sql" -> Sql
>              ".lhs" -> Lhs
>              ".hs" -> Hs
>              ".ag" -> Ag
>              ".txt" -> Txt
>              ".css" -> Css
>              _ -> error $ "don't know extension " ++ f
>     toOf c fn = OutputFile (File fn)
>                          (ft fn)
>                          ("website" </> c fn
>                           ++ case ft fn of
>                                Css -> ""
>                                _ -> ".html")
>                          (takeBaseName fn)

> supportedFileP :: FindClause Bool
> supportedFileP = extension ==? ".hs"
>                  ||? extension ==? ".lhs"
>                     ||? extension ==? ".ag"
>                     ||? extension ==? ".txt"
>                     ||? extension ==? ".sql"
>                     ||? extension ==? ".css"
