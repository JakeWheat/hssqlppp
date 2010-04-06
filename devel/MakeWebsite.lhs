
> import System.FilePath.Find
> import System.FilePath
> import Data.Char
> import System.Directory
> import Control.Monad

> import Text.DocTool.DocTool

> main :: IO ()
> main = do
>   doesDirectoryExist "website" >>=
>     \l -> when l $ removeDirectoryRecursive "website"
>   f <- fileList
>   docify "website/" f


> fileList :: IO [OutputFile]
> fileList = do
>   wso <- doF "docs/website/" (makeRelative "docs/website/")
>   src <- doF "src/" ("source" </>)
>   ex <- doF "examples/" ("source" </>)
>   tsts <- doF "tests/" ("source" </>)
>   dv <- doF "devel/" ("source" </>)
>   ch <- doF "examplesql/" id
>   return $ wso ++ src ++ ex ++ tsts ++ dv ++ ch
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
