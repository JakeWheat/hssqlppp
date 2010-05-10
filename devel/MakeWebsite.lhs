Make the website,

To compile and run, use something like:

time ghc -threaded -XDeriveDataTypeable -DPOSTGRES -cpp -pgmPcpphs -optP--cpp -idevel:src:examples/chaos:examples/extensions/:examples/util/:tests/ --make devel/MakeWebsite.lhs && time devel/MakeWebsite +RTS -N


> import System.FilePath.Find
> import System.FilePath
> import Data.Char
> import System.Directory
> import Control.Monad
> import System.Environment
> import Data.List hiding (find)
> import Data.Ord

> import Text.DocTool.DocTool
> import TestFileProcessor
> import DoChaosSql

> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     ["sourcelinks"] -> sourceLinks
>     [] -> do
>           f <- fileList
>           docify "hssqlppp/" f
>     x -> error $ "don't know" ++ show x


> fileList :: IO [OutputFile]
> fileList = do
>   doesDirectoryExist "hssqlppp" >>=
>     \l -> when l $ removeDirectoryRecursive "hssqlppp"
>   wso <- doF "docs/website/" (makeRelative "docs/website/")
>   src <- doF "src/" ("source" </>)
>   ex' <- doF "examples/" ("source" </>)
>   let ex = filter (\(OutputFile (File fn) _ _ _) -> fn `notElem`
>                    ["util/Database/HsSqlPpp/Utils/PQ.chs.h"
>                    ,"util/Database/HsSqlPpp/Utils/PQ.hs"]) ex'

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
>                                  OutputFile (Text txt) Txt ("hssqlppp/source" </> fn) title)
>   return $ trch1  ++ wso ++ tfp ++ src ++ ex ++ devel ++ tests
>   where
>     doF fl c = find always supportedFileP fl
>                >>= return . map (toOf c)
>     ft f = case map toLower (takeExtension f) of
>              ".sql" -> Sql
>              ".lhs" -> Lhs
>              ".hs" -> Hs
>              ".chs" -> Hs
>              ".ag" -> Ag
>              ".txt" -> Txt
>              ".c" -> C
>              ".h" -> H
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
>                  ||? extension ==? ".ag"
>                  ||? extension ==? ".txt"
>                  ||? extension ==? ".sql"
>                  ||? extension ==? ".css"
>                  ||? extension ==? ".c"
>                  ||? extension ==? ".h"
>                  ||? extension ==? ".chs"


> sourceLinks :: IO ()
> sourceLinks = do
>   fns <- find always supportedFileP "."
>   mapM_ putStrLn $ sortBy sf fns

> sf :: FilePath -> FilePath -> Ordering
> sf a b = let a1 = splitDirectories a
>              b1 = splitDirectories b
>          in compareLists a1 b1
>          where
>            compareLists [] _ = error "error"
>            compareLists _ [] = error "error"
>            compareLists [c] [d] = compare c d
>            compareLists [c] (d:d1:_) = LT
>            compareLists (c:c1:_) [d] = GT
>            compareLists (c:cs) (d:ds) = case compare c d of
>                                           LT -> LT
>                                           GT -> GT
>                                           EQ -> compareLists cs ds


comparing splitDirectories a b

find -type f | grep -iP  "\.(ag|sql|txt|hs|lhs|c|h|chs)$" | sed -e "s@^./@@" | sed -e "s/.*/[\0](\0.html)/"

find . -type d -exec find {} -type f -maxdepth 1 \;