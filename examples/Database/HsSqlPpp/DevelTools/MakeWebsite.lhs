Copyright 2010 Jake Wheat

This file creates the documentation for the project which is uploaded
to the website.

> {-# LANGUAGE QuasiQuotes #-}
>
> module Database.HsSqlPpp.DevelTools.MakeWebsite
>     (makeWebsite) where
>
> import Data.Char
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
> import Database.HsSqlPpp.Tests.ParserTests as PT
> import Database.HsSqlPpp.Tests.TypeCheckTests as TT
> --import Database.HsSqlPpp.Tests.QuasiQuoteTests as QT
>
> makeWebsite :: IO ()
> makeWebsite = do
>   hSetBuffering stdout NoBuffering
>   doesDirectoryExist "website" >>=
>     \l -> when(l) $ removeDirectoryRecursive "website"
>   createDirectory "website"
>   let v = "0.3.0-pre"
>   let hd = wheader v
>   t <- getCurrentTime
>   let ft = wfooter v (formatDateTime "%D %T" t)
>   let pd1 = pandocToFile hd ft
>       pf = pd1 Txt
>       plhs = pd1 Lhs
>   pf "HsSqlPpp documentation"
>      (File "docs/index.txt")
>      "index.html"
>   pf "HsSqlPpp examples"
>      (File "docs/examples.txt")
>      "examples.html"
>   plhs "HsSqlPpp parser examples"
>        (Str $ rowsToHtml parserTestsTable)
>        "ParserTests.html"
>   plhs "HsSqlPpp type checking examples"
>        (Str $ rowsToHtml typeCheckTestsTable)
>        "TypeCheckTests.html"
>   plhs "HsSqlPpp quasiquotation examples"
>        (Str $ rowsToHtml quasiQuoteTestsTable)
>        "QuasiQuoteTests.html"
>   doPandocSource pd1
>   --doHaddock
>   return ()

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
>
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

-------------------------------------------------------------------------------

> doPandocSource :: (PandocType -> String -> Input -> [Char] -> IO ()) -> IO ()
> doPandocSource pdize = do
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

>
> wrapSqlCode :: String -> String
> wrapSqlCode = replace
>               "\n\\begin{code}\n"
>               "\n~~~~~{.SqlPostgresql}\n"
>               . replace
>               "\n\\end{code}\n"
>               "\n~~~~~\n"

------------------------------------------------------------------------------

> wheader :: String -> String
> wheader v = [$here|
>
> <div class='header'>[HsSqlPpp-|] ++ v ++
>             [$here|](http://community.haskell.org/%7EJakeWheat/hssqlppp/index.html)</div>
>
> |]

 > wheader v = "\n\n\n~~~~~~~~{.header}\n\n\n[HsSqlPpp%20" ++ v ++
 >             "](http://community.haskell.org/%7EJakeWheat/hssqlppp/index.html)\n\n\n~~~~\n\n\n\n"

> wfooter :: String -> String -> String
> wfooter v d = [$here|
>
> <div class='footer'>Copyright 2010 Jake Wheat, generated on |]
>               ++ d ++ ", hssqlppp-" ++ v ++ [$here|</div>
>
>               |]

> {- [$here|
>  <div>
>  * [Index](index.html)
>  * [Examples](examples.html)
>  * [Haddock](haddock/index.html)
>  * [Browse source online](pandoc_source/index.html)
>  * [HackageDB page](http://hackage.haskell.org/package/hssqlppp)
>  * [Launchpad/ Repo](http://launchpad.net/hssqlppp)
>  </div>
> |]-}

-------------------------------------------------------------------------------

pandoc won't render haskell source which isn't literate nicely at all,
so run it though highlighting-kate only

> highlight :: String -> String -> String -> String -> String
> highlight hdr ftr title s = do
>   case highlightAs "Haskell" s of
>     Right r -> "<html><head><title>" ++ title ++ "</title>" ++
>                "<style>" ++ bc ++ defaultHighlightingCss ++ "</style>" ++
>                htmlHeader ++ "</head><body>" ++
>                pandocFrag hdr ++
>                renderHtmlFragment (formatAsXHtml [OptTitleAttributes] "Haskell" r) ++
>                pandocFrag ftr ++
>                "</body></html>"
>     Left err -> trace ("highlight error: " ++ err) s
>   where
>    bc = "pre.Haskell {background-color: #f9f9e0;}\n"

-------------------------------------------------------------------------------

pandoc wrappers

> data PandocType = Lhs
>                 | Highlight
>                 | Txt
>                   deriving Show
> data Input = Str String
>            | File String

> pandocToFile :: String -> String -> PandocType -> String -> Input -> String -> IO ()
> pandocToFile hdr ftr pt title src tgt = do
>   putStrLn tgt
>   createDirectoryIfMissing True $ "website/" ++ dropFileName tgt
>   let tgt1 = "website/" ++ tgt
>   src1 <- case src of
>             Str s -> return s
>             File f -> readFile f
>   let src2 = hdr ++ src1 ++ ftr
>   writeFile tgt1 $ case pt of
>                            Lhs -> pandocLhs title $ wrapSqlCode src2
>                            Highlight -> highlight hdr ftr title src1
>                            Txt -> pandoc title src2
>
> pandoc :: String -> String -> String
> pandoc hd = (writeHtmlString wopt) . (readMarkdown defaultParserState)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               ,writerTitlePrefix = hd
>               ,writerTableOfContents = False
>               ,writerHeader = htmlHeader
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
>               ,writerHeader = htmlHeader
>               ,writerLiterateHaskell=True
>              }
>     ropt = defaultParserState {
>             stateLiterateHaskell = True
>            }
>
> htmlHeader :: String
> htmlHeader = [$here|
> <style>
> h1 {
> font-size: x-large;
> display:block;
> background-color: #f9f9f9;
> border-top: thin black solid;
> left: -3ex;
> }
> h2 {
> font-size: large;
> display:block;
> background-color: #f9f9f9;
> border-top: thin black solid;
> left: -1.5ex;
> }
> body {
>    margin-left: 5em;
>    margin-right: 5em;
>    margin-bottom: 5em;
> }
> .header {
>     border: thin black solid;
>     display:inline;
>  }
>  .footer {
>    text-align: center;
>    font-size: small;
>  }
>  pre {
>      padding: 0;
>  }
>  pre.SqlPostgresql {
>      padding: 0.5em;
>      background-color: #f5f9f9;
>  }
>  pre.haskell {
>      padding: 0.5em;
>      background-color: #f9f9e0;
>  }
>  pre.sh {
>      padding: 0.5em;
>      background-color: #f9f9f9;
>  }
> table, tr, td {
>   border-collapse:collapse;
>   cell-padding:0px;
>   cell-spacing:0px;
>   padding:0px
>   spacing:0px
>   margin:0px
>   vertical-align:top;
> }
> td pre {
>    width: 98%;
>    height: 98%;
>   vertical-align:top;
> }
> table {
>       width:98%;
> }
> td {
>    width: 49%;
>   vertical-align:top;
> }
> </style>
> |]

===============================================================================

routines to convert the parser and type check test data into html pages

> data Row = Row [Text] [Text]
>          | HHeader String
>
> data Text = Text String
>           | Sql String
>           | Haskell String
>
> parserTestsTable :: [Row]
> parserTestsTable = mapParserTests PT.parserTestData
>
> typeCheckTestsTable :: [Row]
> typeCheckTestsTable = mapTypeCheckTests TT.typeCheckTestData
>
> quasiQuoteTestsTable :: [Row]
> quasiQuoteTestsTable = [] -- mapQuasiQuoteTestsTests QT.quasiQuoteTestData
>
> mapParserTests :: PT.Item -> [Row]
> mapParserTests (PT.Expr s e) = [Row [Sql s] [Haskell (ppExpr e)]]
> mapParserTests (PT.Stmt s e) = [Row [Sql s] [Haskell (ppExpr e)]]
> mapParserTests (PT.PgSqlStmt s e) = [Row [Sql s] [Haskell (ppExpr e)]]
> mapParserTests (PT.Group n is) = HHeader n : concatMap mapParserTests is

need to use haskell-src-exts for the quasi quote tests since we want
to get the quasi quote source syntax, not the asts it produces at
compile time.

> mapTypeCheckTests :: TT.Item -> [Row]
> mapTypeCheckTests (TT.Group n is) = HHeader n : concatMap mapTypeCheckTests is
> mapTypeCheckTests (TT.Expr s r) = [Row [Sql s] [Haskell (ppExpr r)]]
> mapTypeCheckTests (TT.StmtType s r) = [Row [Sql s] [Haskell (ppExpr r)]]
> mapTypeCheckTests (TT.CatStmtType s c r) = [Row [Haskell (ppExpr c),Sql s] [Haskell (ppExpr r)]]
> mapTypeCheckTests (TT.Ddl s c) = [Row [Sql s] [Haskell (ppExpr c)]]
>
> rowsToHtml :: [Row] -> String
> rowsToHtml rs =
>   "<table>" ++
>   concatMap rowToHtml rs ++
>   "</table>"
>   where
>     rowToHtml (Row a b) =
>         "<tr><td style='width:50%'>" ++ concatMap tToH a ++
>         "</td><td style='width:50%'>"  ++ concatMap tToH b ++
>         "</td></tr>"
>     rowToHtml (HHeader s) =
>         "</table>\n" ++ s ++ "\n" ++ map (const '=') s ++ "\n<table>\n"
>     tToH (Text s) = s
>     tToH (Sql s) = code "SqlPostgresql" s
>     tToH (Haskell s) = code "haskell" s
>     code t s = "\n\n~~~~~~{." ++ t ++ "}\n"
>                ++ trim s
>                ++ "\n~~~~~~\n\n"
>
> trim :: String -> String
> trim = f . f
>    where f = reverse . dropWhile isSpace
