Copyright 2010 Jake Wheat

This file creates the documentation for the project which is uploaded
to the website.

> {-# LANGUAGE QuasiQuotes #-}
>
> module Database.HsSqlPpp.DevelTools.MakeWebsite
>     (makeWebsite) where
>
> import System.Directory
> import Control.Monad
> import Text.Pandoc hiding (Str)
> import System.Cmd
> import System.FilePath.Find
> import System.IO
> import System.FilePath
> import Text.Highlighting.Kate
> import Debug.Trace
> import Text.XHtml.Strict hiding (title,src)
> import Data.DateTime
>
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.DevelTools.TestFileProcessor
>
> makeWebsite :: IO ()
> makeWebsite = do
>   hSetBuffering stdout NoBuffering
>   doesDirectoryExist "website" >>=
>     \l -> when(l) $ removeDirectoryRecursive "website"
>   createDirectory "website"
>   copyFile "docs/main.css" "website/main.css"
>   let v = "0.3.0-pre"  --- todo: read from cabal
>   let hd = wheader v
>   t <- getCurrentTime
>   let ft = wfooter v (formatDateTime "%D %T" t)
>   let pd1 = htmlize hd ft
>       pf = pd1 Txt
>       plhs = pd1 Lhs
>   pf "HsSqlPpp documentation"
>      (File "docs/index.txt")
>      "index.html"
>   pf "HsSqlPpp examples"
>      (File "docs/examples.txt")
>      "examples.html"
>   plhs "HsSqlPpp parser examples"
>        (Str parserTestsTable)
>        "ParserTests.html"
>   plhs "HsSqlPpp type checking examples"
>        (Str typeCheckTestsTable)
>        "TypeCheckTests.html"
>   qq <- quasiQuoteTestsTable
>   plhs "HsSqlPpp quasiquotation examples"
>        (Str qq)
>        "QuasiQuoteTests.html"
>   doSourceFiles pd1
>   doHaddock
>   return ()

-------------------------------------------------------------------------------

> doSourceFiles :: (PandocType -> String -> Input -> [Char] -> IO ()) -> IO ()
> doSourceFiles pdize = do
>   sf <- sourceFiles
>   let index = concatMap (\s -> let s1 = s ++ ".html"
>                                in "* [" ++ s ++ "](" ++ s1 ++ ")\n") sf
>   pdize Txt "HsSqlPpp source files" (Str index) "pandoc_source/index.html"
>   moveDTCOut
>   mapM_ pandocIt sf
>   moveDTCBack
>   where
>     pandocIt fn = do
>            let target = "pandoc_source/" ++ fn ++ ".html"
>                title = snd $ splitFileName fn
>            pdize (if takeExtension fn `elem` [".lhs", ".lag"]
>                   then Lhs
>                   else Highlight) title (File fn) target
>     sourceFiles = do
>       l <- find always sourceFileP "examples/"
>       l1 <- find always sourceFileP "src/"
>       return $ l ++ l1
>     sourceFileP = extension ==? ".hs" ||? extension ==? ".lhs"
>                     ||? extension ==? ".ag"
>                     ||? extension ==? ".lag"
>

todo: add filenames at top of these pages or// duplicate the title as
the top header for each page

-------------------------------------------------------------------------------

pandoc/highlight wrappers

> data PandocType = Lhs
>                 | Highlight
>                 | Txt
>                   deriving Show
> data Input = Str String
>            | File String
>
>
> -- main utility funtion
> htmlize :: String -> String -> PandocType -> String -> Input -> String -> IO ()
> htmlize hdr ftr pt title src tgt = do
>   putStrLn tgt
>   createDirectoryIfMissing True $ "website/" ++ dropFileName tgt
>   let tgt1 = "website/" ++ tgt
>   src1 <- case src of
>             Str s -> return s
>             File f -> readFile f
>   let filterRelativeLinks = filterLinks (concat $
>                           replicate (length $
>                                      splitDirectories $
>                                      dropFileName tgt)
>                           "../")
>   let src2 = hdr ++ src1 ++ ftr
>   writeFile tgt1 $ filterRelativeLinks $
>                 case pt of
>                            Lhs -> pandocLhs title $ wrapSqlCode src2
>                            Highlight -> highlight hdr ftr title src1
>                            Txt -> pandoc title src2
>
> -- pure wrappers to do various rendering
> pandoc :: String -> String -> String
> pandoc hd = (writeHtmlString wopt) . (readMarkdown defaultParserState)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               ,writerTitlePrefix = hd
>               ,writerTableOfContents = False
>               ,writerHeader = cssLink
>              }
>
> pandocFrag :: String -> String
> pandocFrag = (writeHtmlString wopt) . (readMarkdown defaultParserState)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = False
>              }
>
>
> pandocLhs :: String -> String -> String
> pandocLhs hd = (writeHtmlString wopt) . (readMarkdown ropt)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               ,writerTitlePrefix = hd
>               ,writerTableOfContents = False
>               ,writerHeader = cssLink
>               ,writerLiterateHaskell=True
>              }
>     ropt = defaultParserState {
>             stateLiterateHaskell = True
>            }
>
> -- hack to render some quasiquoted sql using sql highlighting
> wrapSqlCode :: String -> String
> wrapSqlCode = replace
>               "\n\\begin{code}\n"
>               "\n~~~~~{.SqlPostgresql}\n"
>               . replace
>               "\n\\end{code}\n"
>               "\n~~~~~\n"

> filterLinks :: String -> String -> String
> filterLinks path = replace
>                    "\"website/"
>                    ("\"" ++ path)



pandoc won't render haskell source which isn't literate nicely at all,
so run it though highlighting-kate only

> highlight :: String -> String -> String -> String -> String
> highlight hdr ftr title s = do
>   case highlightAs "Haskell" s of
>     Right r -> "<html><head><title>" ++ title ++ "</title>" ++
>                cssLink ++ "</head><body>" ++
>                pandocFrag hdr ++
>                renderHtmlFragment (formatAsXHtml [OptTitleAttributes] "Haskell" r) ++
>                pandocFrag ftr ++
>                "</body></html>"
>     Left err -> trace ("highlight error: " ++ err) s

------------------------------------------------------------------------------

> wheader :: String -> String
> wheader v = [$here|
>
> <div class='header'>[HsSqlPpp-|] ++ v ++
>             [$here|](website/index.html)</div>|] ++ "\n\n"
> wfooter :: String -> String -> String
> wfooter v d = [$here|
>
> <div class='footer'>Copyright 2010 Jake Wheat, generated on |]
>               ++ d ++ ", hssqlppp-" ++ v ++ [$here|</div>|]
>
> cssLink :: String
> cssLink = "<link href=\"website/main.css\" rel='stylesheet' type='text/css'>"

-------------------------------------------------------------------------------

> doHaddock :: IO ()
> doHaddock = do
>   --cos hscolour can't handle the large defaulttemplate1catalog,
>   --just move it out the way temporarily
>   moveDTCOut
>   _ <- rawSystem "cabal" ["configure"]
>   _ <- rawSystem "cabal" ["haddock", "--hyperlink-source"]
>   renameDirectory "dist/doc/html/hssqlppp/" "website/haddock"
>   moveDTCBack

-------------------------------------------------------------------------------

> moveDTCOut :: IO()
> moveDTCOut = do
>   renameFile "src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs"
>              "src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs.moved"
>   copyFile "src/Database/HsSqlPpp/AstInternals/Catalog/ShortDefaultTemplate1Catalog.lhs"
>            "src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs"
>
> moveDTCBack :: IO ()
> moveDTCBack = do
>   renameFile "src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs.moved"
>              "src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs"

