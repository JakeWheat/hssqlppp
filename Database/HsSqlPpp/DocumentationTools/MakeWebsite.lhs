Copyright 2010 Jake Wheat

This file creates the documentation for the project, to be uploaded to
the website.

> {-# LANGUAGE QuasiQuotes #-}

> module Database.HsSqlPpp.DocumentationTools.MakeWebsite
>     (makeWebsite) where

> import Data.Char
> import System.Directory
> import Control.Monad
> import Text.Pandoc
> import System.Cmd
> import System.FilePath.Find
> import System.IO
> import System.FilePath
>
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Tests.ParserTests as PT
> import Database.HsSqlPpp.Tests.TypeCheckTests as TT
>
> makeWebsite :: IO ()
> makeWebsite = do
>   doesDirectoryExist "website" >>=
>     \l -> when(l) $ removeDirectoryRecursive "website"
>   createDirectory "website"
>   pf "README" "website/index.html"
>   pf "docs/examples.txt" "website/examples.html"
>   -- pandocise source code files
>   doPandocSource
>   doHaddock
>   plhs "website/ParserTests.html" $ rowsToHtml parserTestsTable
>   plhs "website/TypeCheckTests.html" $ rowsToHtml typeCheckTestsTable
>   return ()
>   where
>     pf s t = readFile s >>= return . (siteMap ++) >>= return . pandoc >>= writeFile t
>     plhs f s = writeFile f $ pandocLhs (siteMap ++ s)

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

> moveDTCOut :: IO()
> moveDTCOut = do
>   renameFile "Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs"
>              "Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs.moved"
>   copyFile "Database/HsSqlPpp/AstInternals/Catalog/ShortDefaultTemplate1Catalog.lhs"
>            "Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs"

> moveDTCBack :: IO ()
> moveDTCBack = do
>   renameFile "Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs.moved"
>              "Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs"

-------------------------------------------------------------------------------

> doPandocSource :: IO ()
> doPandocSource = do
>   sf <- sourceFiles
>   hSetBuffering stdout NoBuffering
>   moveDTCOut
>   mapM_ pandocIt sf
>   moveDTCBack
>   let index = concatMap (\s -> let s1 = s ++ ".html"
>                                in "* [" ++ s ++ "](" ++ s1 ++ ")\n") sf
>   writeFile "website/pandoc_source/index.html" $ pandoc $ pandocIndex index
>   return ()
>   where
>     pandocIt fn = do
>            putStrLn fn
>            createDirectoryIfMissing True $ "website/pandoc_source/" ++ dropFileName fn
>            let target = "website/pandoc_source/" ++ fn ++ ".html"
>            if takeExtension fn `elem` [".lhs", ".lag"]
>              then plhs fn target
>              else phs fn target
>     sourceFiles = do
>       l <- find always sourceFileP "Database"
>       return $ "HsSqlSystem.lhs" : l
>     sourceFileP = extension ==? ".hs" ||? extension ==? ".lhs"
>                     ||? extension ==? ".ag"
>                     ||? extension ==? ".lag"
>     plhs s t = readFile s >>= return . pandocLhs >>= writeFile t
>     phs s t = readFile s >>= return . pandocHs >>= writeFile t

> pandocIndex :: String -> String
> pandocIndex s = s

-------------------------------------------------------------------------------

> siteMap :: String
> siteMap = [$here|
>  <div>
>  * [Index](index.html)
>  * [Examples](examples.html)
>  * [Haddock](haddock/index.html)
>  * [Browse source online](pandoc_source/)
>  * [HackageDB page](http://hackage.haskell.org/package/hssqlppp)
>  * [Launchpad/ Repo](http://launchpad.net/hssqlppp)
>  </div>
> |]

-------------------------------------------------------------------------------

pandoc wrappers

> pandoc :: String -> String
> pandoc = (writeHtmlString wopt) . (readMarkdown defaultParserState)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               --,writerTitlePrefix = "HsSqlPpp documentation"
>               ,writerTableOfContents = True
>               ,writerHeader = htmlHeader
>              }

> pandocLhs :: String -> String
> pandocLhs = (writeHtmlString wopt) . (readMarkdown ropt)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               --,writerTitlePrefix = "HsSqlPpp documentation"
>               ,writerTableOfContents = True
>               ,writerHeader = htmlHeader
>               ,writerLiterateHaskell=True
>              }
>     ropt = defaultParserState {
>             stateLiterateHaskell = True
>            }

> pandocHs :: String -> String
> pandocHs = (writeHtmlString wopt) . (readMarkdown ropt)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               --,writerTitlePrefix = "HsSqlPpp documentation"
>               ,writerTableOfContents = True
>               ,writerHeader = htmlHeader
>               ,writerLiterateHaskell = False
>              }
>     ropt = defaultParserState {
>             stateLiterateHaskell = False
>            }


data ParserState = ParserState {
stateParseRaw :: Bool
stateParserContext :: ParserContext
stateQuoteContext :: QuoteContext
stateSanitizeHTML :: Bool
stateKeys :: KeyTable
stateNotes :: NoteTable
stateTabStop :: Int
stateStandalone :: Bool
stateTitle :: [Inline]
stateAuthors :: [String]
stateDate :: String
stateStrict :: Bool
stateSmart :: Bool
stateLiterateHaskell :: Bool
stateColumns :: Int
stateHeaderTable :: [HeaderType]
stateIndentedCodeClasses :: [String]
}


> htmlHeader :: String
> htmlHeader = [$here|
> <style>
> pre {
>     border: 1px dotted gray;
>     background-color: #ececec;
>     color: #1111111;
>     padding: 0.5em;
> }
> table, tr, td {
>   border-collapse:collapse;
>   cell-padding:0px;
>   cell-spacing:0px;
>   padding:0px
>   spacing:0px
>   margin:0px
> }
> td pre {
>    width: 98%;
>    height: 98%;
> }
> table {
>       width:98%;
> }
> td {
>    width: 49%;
> }
> </style>
> |]

===============================================================================

routines to convert the parser and type check test data into html pages

> data Row = Row [Text] [Text]
>          | HHeader String

> data Text = Text String
>           | Sql String
>           | Haskell String

> parserTestsTable :: [Row]
> parserTestsTable = mapParserTests PT.parserTestData

> typeCheckTestsTable :: [Row]
> typeCheckTestsTable = mapTypeCheckTests TT.typeCheckTestData

> mapParserTests :: PT.Item -> [Row]
> mapParserTests (PT.Expr s e) = [Row [Sql s] [Haskell (ppExpr e)]]
> mapParserTests (PT.Stmt s e) = [Row [Sql s] [Haskell (ppExpr e)]]
> mapParserTests (PT.PgSqlStmt s e) = [Row [Sql s] [Haskell (ppExpr e)]]
> mapParserTests (PT.Group n is) = HHeader n : concatMap mapParserTests is

> mapTypeCheckTests :: TT.Item -> [Row]
> mapTypeCheckTests (TT.Group n is) = HHeader n : concatMap mapTypeCheckTests is
> mapTypeCheckTests (TT.Expr s r) = [Row [Sql s] [Haskell (ppExpr r)]]
> mapTypeCheckTests (TT.StmtType s r) = [Row [Sql s] [Haskell (ppExpr r)]]
> mapTypeCheckTests (TT.CatStmtType s c r) = [Row [Haskell (ppExpr c),Sql s] [Haskell (ppExpr r)]]
> mapTypeCheckTests (TT.Ddl s c) = [Row [Sql s] [Haskell (ppExpr c)]]

> rowsToHtml :: [Row] -> String
> rowsToHtml rs =
>   "<table>" ++
>   concatMap rowToHtml rs ++
>   "</table>"
>   where
>     rowToHtml (Row a b) =
>         "<tr><td>" ++ concatMap tToH a ++
>         "</td><td>"  ++ concatMap tToH b ++
>         "</td></tr>"
>     rowToHtml (HHeader s) =
>         "</table>\n" ++ s ++ "\n" ++ map (const '=') s ++ "\n<table>\n"
>     tToH (Text s) = s
>     tToH (Sql s) = code "SqlPostgresql" s
>     tToH (Haskell s) = code "haskell" s
>     code t s = "\n\n~~~~~~{." ++ t ++ "}\n"
>                ++ trim s
>                ++ "\n~~~~~~\n\n"

> trim :: String -> String
> trim = f . f
>    where f = reverse . dropWhile isSpace
