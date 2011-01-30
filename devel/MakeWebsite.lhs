Make the website,

To compile and run, use something like:

ghc --make -threaded -XScopedTypeVariables -XDeriveDataTypeable -DPOSTGRES -cpp -pgmPcpphs -optP--cpp -idevel:src/lib:src/qq:src/postgresql:examples/chaos:examples/extensions/:examples/util/:tests/ --make devel/DevelTool.lhs && time 

> module MakeWebsite (makeWebsite, sourceLinks) where

> import System.FilePath.Find
> import System.FilePath
> import Data.Char
> import System.Directory
> import Control.Monad
> import System.Environment
> import Data.List hiding (find)

> import Text.DocTool.DocTool
> import TestFileProcessor
> import DoChaosSql

> makeWebsite :: IO ()
> makeWebsite = do
>           f <- fileList
>           docify "hssqlppp/" f


> fileList :: IO [OutputFile]
> fileList = do
>   doesDirectoryExist "hssqlppp" >>=
>     flip when (removeDirectoryRecursive "hssqlppp")
>   wso <- doF "website_source/" (makeRelative "website_source")
>   src' <- doF "src/" ("source" </>)
>   let src = removeMatches ["src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs"] src'
>   ex' <- doF "examples/" ("source" </>)
>   let ex = removeMatches ["util/Database/HsSqlPpp/Utils/PQ.chs.h"
>                          ,"util/Database/HsSqlPpp/Utils/PQ.hs"] ex'

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
>   trch1 <- flip fmap getTransformedChaosSql
>                 $ map $ \(title, fn, txt) ->
>                           OutputFile (Text txt)
>                                      Txt
>                                      ("hssqlppp/source" </> fn)
>                                      title
>   return $ tfp ++ src ++ trch1 ++ wso ++ ex ++ devel ++ tests
>   where
>     removeMatches :: [String] -> [OutputFile] -> [OutputFile]
>     removeMatches bads =
>       filter (\(OutputFile (File fn) _ _ _) -> fn `notElem` bads)
>     doF fl c = fmap (map $ toOf c) $ find always supportedFileP fl
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
>            compareLists [_] (_:_:_) = LT
>            compareLists (_:_:_) [_] = GT
>            compareLists (c:cs) (d:ds) = case compare c d of
>                                           LT -> LT
>                                           GT -> GT
>                                           EQ -> compareLists cs ds


comparing splitDirectories a b

find -type f | grep -iP  "\.(ag|sql|txt|hs|lhs|c|h|chs)$" | sed -e "s@^./@@" | sed -e "s/.*/[\0](\0.html)/"

find . -type d -exec find {} -type f -maxdepth 1 \;
