
missing from the web pages:

generated test files:
  check the background colour and the rendering/style
  add the headers back


source file renders
transformed sql renders

> {-# LANGUAGE TupleSections #-}
> import System.FilePath.Find
> import System.FilePath
> --import Data.Char
> import System.Directory
> --import Control.Monad
> --import System.Environment
> --import Data.List hiding (find)
> import Text.Pandoc
> --import Text.Groom
> import Control.Arrow
> --import Data.DateTime

> --import Text.DocTool.DocTool
> import TestFileProcessor
> --import DoChaosSql
> import Data.Time.Clock
> import Data.Time.Format

> main :: IO ()
> main = do
>     fs <- getSourceFiles
>     mds <- mapM readSourceFile fs
>     qq <- quasiQuoteTestsTable

>     let mdsExtra = [("ParserTests", Pandoc nullMeta parserTestsTable)
>                    ,("TypeCheckTests", Pandoc nullMeta typeCheckTestsTable)
>                    ,("QuasiQuoteTests", Pandoc nullMeta qq)]
>         mds' = mds ++ mdsExtra
>     let v = "0.5.16"
>     t <- getCurrentTime
>     let tm = formatTime defaultTimeLocale "%d/%m/%y %T" t
>         ft = "generated on " ++ tm ++ ", hssqlppp-" ++ v
>         mds'' = map (second $ decoratePandoc v ft) mds'
>         mds''' = map (first outputFilename) mds''
>     mapM_ writef mds'''

> writef :: (FilePath,Pandoc) -> IO ()
> writef (fp,p) = do
>     opt <- getHtmlOpts
>     let p' = writeHtmlString opt p
>     createDirectoryIfMissing True $ dropFileName fp
>     writeFile fp p'

> getHtmlOpts :: IO WriterOptions
> getHtmlOpts = do
>     template <- either (error . show) id
>         `fmap` getDefaultTemplate Nothing "html"
>     return $ def
>         { writerStandalone = True
>         , writerTableOfContents = True
>         , writerTemplate = template
>         , writerVariables = [
>             ("css", "main.css")
>             ]
>         , writerHighlight = True
>         }


> decoratePandoc :: String -> String -> Pandoc -> Pandoc
> decoratePandoc v ft (Pandoc m b) =
>     Pandoc m (header v ++ b ++ wfooter ft)

> getSourceFiles :: IO [FilePath]
> getSourceFiles = find always supportedFileP "website-source"

> readSourceFile :: FilePath -> IO (FilePath,Pandoc)
> readSourceFile f = (f,) `fmap` rm `fmap` readFile f
>   where
>     rm x = let y = readMarkdown def x
>            in case y of
>                 Right z -> z
>                 Left e -> error $ show e

> outputFilename :: FilePath -> FilePath
> outputFilename fp = "build/website/" ++ takeFileName fp ++ ".html"

> --readMarkdownFragment :: String -> [Block]
> --readMarkdownFragment s =
> --    case readMarkdown def s of
> --        Pandoc _ b -> b

> header :: String -> [Block]
> header v = [makeDiv "header" [Plain [Link [Str $ "HsSqlPpp-" ++ v] ("index.html","")]]]

> wfooter :: String -> [Block]
> wfooter ft = [RawBlock (Format "html") "<br /><br /><br />"
>              ,makeDiv "footer" $ [Plain [Str ft]]]

> makeDiv :: String -> [Block] -> Block
> makeDiv cls = Div ("", [cls], [])

> {-makeWebsite :: IO ()
> makeWebsite = do
>           f <- fileList
>           docify "hssqlppp/" f
>           renameFile "hssqlppp/index.txt.html" "hssqlppp/index.html"


> fileList :: IO [OutputFile]
> fileList = do
>   doesDirectoryExist "hssqlppp" >>=
>     flip when (removeDirectoryRecursive "hssqlppp")
>   wso <- doF "website_source/" (makeRelative "website_source")
>   --src' <- doF "src/" ("source" </>)
>   --let src = removeMatches ["src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs"] src'
>   {-ex' <- doF "examples/" ("source" </>)
>   let ex = removeMatches ["util/Database/HsSqlPpp/Utils/PQ.chs.h"
>                          ,"util/Database/HsSqlPpp/Utils/PQ.hs"] ex'

>   devel <- doF "util/" ("source" </>)
>   tests <- doF "tests/" ("source" </>)-}

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
>   {-trch1 <- flip fmap getTransformedChaosSql
>                 $ map $ \(title, fn, txt) ->
>                           OutputFile (Text txt)
>                                      Txt
>                                      ("hssqlppp/source" </> fn)
>                                      title-}
>   return $ tfp {- ++ src -} {-++ trch1-} ++ wso -- ++ ex ++ devel ++ tests
>   where
>     {-removeMatches :: [String] -> [OutputFile] -> [OutputFile]
>     removeMatches bads =
>       filter (\(OutputFile (File fn) _ _ _) -> fn `notElem` bads)-}
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
>                          (takeBaseName fn) -}


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


> {-sourceLinks :: IO ()
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
>                                           EQ -> compareLists cs ds-}


comparing splitDirectories a b

find -type f | grep -iP  "\.(ag|sql|txt|hs|lhs|c|h|chs)$" | sed -e "s@^./@@" | sed -e "s/.*/[\0](\0.html)/"

find . -type d -exec find {} -type f -maxdepth 1 \;
