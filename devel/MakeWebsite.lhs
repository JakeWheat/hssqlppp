
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
>   f <- fileList
>   docify "hssqlppp/" f


> fileList :: IO [OutputFile]
> fileList = do
>   doesDirectoryExist "hssqlppp" >>=
>     \l -> when l $ removeDirectoryRecursive "hssqlppp"
>   wso <- doF "docs/website/" (makeRelative "docs/website/")
>   src <- doF "src/" ("source" </>)
>   ex <- doF "examples/" ("source" </>)
>   devel <- doF "devel/" ("source" </>)
>   tests <- doF "tests/" ("source" </>)

>   qq <- quasiQuoteTestsTable

>   let tfp = [OutputFile (Text parserTestsTable)
>                         Txt "hssqlppp/ParserTests.html"
>                         "HsSqlPpp parser examples"
>             ,OutputFile (Text typeCheckTestsTable)
>                         Txt "hssqlppp/TypeCheckTests.html"
>                         "HsSqlPpp type checking examples"
>             ,OutputFile (Text qq)
>                         Txt "hssqlppp/QuasiQuoteTests.html"
>                         "HsSqlPpp quasiquotation examples"]
>   trch1 <- getTransformedChaosSql
>            >>= return . map (\(title,fn,txt) ->
>                                  OutputFile (Text txt) Sql ("hssqlppp/source" </> fn) title)
>   return $ trch1 ++ wso ++ tfp ++ src ++ ex ++ devel ++ tests
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
>                          ("hssqlppp" </> c fn
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
