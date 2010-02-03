#! /usr/bin/env runhaskell

Copyright 2010 Jake Wheat

This file creates the documentation for the project, to be uploaded to
the website.

do

cabal haddock --hyperlink-source
& copy files across

README -> pandoc -> index.html
the readme file is written in markdown

hssqlsystem -> filter out documentation sections -> pandoc ->
hssqlsystem.html
the idea with hsssqlsystem is to add a markdown chunk
next to each command with some sort of delimiters (all of this outside
the birdfeet),
then to build the docs: filter hsssqlsystem to leave just these
chunks, which gives us a regular markdown file

parsertests.lhs,typechecktests.lhs -> hs src exts -> some sort of uniplate thing -> simple ast
 -> markdown -> pandoc -> html
idea is to get a parse tree of the source code, the transform this into a simple
data structure representing sections, and test data, drop all the
code. then these are put into a table in markdown syntax, the sql and
hs marked with the appropriate stuff to get them syntax highlighting,
then pandoc to html

> {-# LANGUAGE QuasiQuotes #-}

> import Data.Char
> import System.Directory
> import Control.Monad
> import Text.Pandoc
>
> import Database.HsSqlPpp.Utils
> import Database.HsSqlPpp.Here
> import Database.HsSqlPpp.Tests.ParserTests as PT
> import Database.HsSqlPpp.Tests.TypeCheckTests as TT
>
> main :: IO ()
> main = do
>   doesDirectoryExist "website" >>=
>     \l -> when(l) $ removeDirectoryRecursive "website"
>   createDirectory "website"
>   pf "README" "website/index.html"
>   pf "docs/examples.txt" "website/examples.html"
>   -- pandocise source code files
>   -- haddock
>   -- generate doc files from test files
>   plhs "website/ParserTests.html" $ rowsToHtml parserTestsTable
>   plhs "website/TypeCheckTests.html" $ rowsToHtml typeCheckTestsTable
>   -- site map
>   return ()
>   where
>     pf s t = readFile s >>= return . pandoc >>= writeFile t
>     plhs f s = writeFile f $ pandocLhs s
>   {-f <- parseFile "Database/HsSqlPpp/Tests/ParserTests.lhs"
>   case f of
>     ParseOk ast -> do
>            putStrLn $ ppExpr ast
>     x -> error $ show x-}
>   {-clean website
>   mkdir website
>   cp templatefiles website
>   hack to get hscolour to complete
>   cabal haddock --hyperlink-source
>   create alternative source reference:
>      do file index
>      and then use pandoc command below to highlight the source
>   cp dist/doc/html/hssqlppp website/haddock
>   pandoc "README" "index.html"
>   pandoc "HsSqlSystem.lhs" "HsSqlSystem.html"
>   make test files-}

-------------------------------------------------------------------------------

pandoc wrappers

 > pandocLhs =

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
> pandocLhs = (writeHtmlString wopt) . (readMarkdown defaultParserState)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               --,writerTitlePrefix = "HsSqlPpp documentation"
>               ,writerTableOfContents = True
>               ,writerHeader = htmlHeader
>               ,writerLiterateHaskell=True
>              }


markdownToRST :: String -> String
 markdownToRST =
   (writeRST defaultWriterOptions {writerReferenceLinks = True}) .
   readMarkdown defaultParserState
 
 main = getContents >>= putStrLn . markdownToRST

pandoc -H docs/header -s --toc -f markdown+lhs HsSqlSystem.lhs  > HsSqlSystem.lhs.html

data WriterOptions = WriterOptions {
writerStandalone :: Bool
writerTemplate :: String
writerVariables :: [(String, String)]
writerIncludeBefore :: String
writerIncludeAfter :: String
writerTabStop :: Int
writerTableOfContents :: Bool
writerS5 :: Bool
writerXeTeX :: Bool
writerHTMLMathMethod :: HTMLMathMethod
writerIgnoreNotes :: Bool
writerIncremental :: Bool
writerNumberSections :: Bool
writerStrictMarkdown :: Bool
writerReferenceLinks :: Bool
writerWrapText :: Bool
writerLiterateHaskell :: Bool
writerEmailObfuscation :: ObfuscationMethod
writerIdentifierPrefix :: String
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

>{- -- | Documentation command to produce some hssqlppp docs, takes a
> --   pandoc source file and converts to html, can run and insert
> --   commands embedded in the source
> pandoc :: MonadIO m => String -> ErrorT String m String
> pandoc txt = return txt
> {-
>   liftM (writeHtmlString wopt . readMarkdown defaultParserState)
>     (hsTextize txt)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               ,writerTitlePrefix = "HsSqlPpp documentation"
>               ,writerTableOfContents = True
>               ,writerHeader = "<style>\n\
>                               \pre {\n\
>                               \    border: 1px dotted gray;\n\
>                               \    background-color: #ececec;\n\
>                               \    color: #1111111;\n\
>                               \    padding: 0.5em;\n\
>                               \}\n\
>                               \</style>"
>              }-}


writerStandalone :: Bool	Include header and footer
writerHeader :: String	Header for the document
writerTitlePrefix :: String	Prefix for HTML titles
writerTabStop :: Int	Tabstop for conversion btw spaces and tabs
writerTableOfContents :: Bool	Include table of contents
writerS5 :: Bool	We're writing S5
writerHTMLMathMethod :: HTMLMathMethod	How to print math in HTML
writerIgnoreNotes :: Bool	Ignore footnotes (used in making toc)
writerIncremental :: Bool	Incremental S5 lists
writerNumberSections :: Bool	Number sections in LaTeX
writerIncludeBefore :: String	String to include before the body
writerIncludeAfter :: String	String to include after the body
writerStrictMarkdown :: Bool	Use strict markdown syntax
writerReferenceLinks :: Bool	Use reference links in writing markdown, rst
writerWrapText :: Bool	Wrap text to line length
writerLiterateHaskell :: Bool	Write as literate haskell
writerEmailObfuscation :: ObfuscationMethod	How to obfu

>   {-ex <- liftIO $ system ("pandoc -s -f markdown -t html "
>                          ++ src ++ " -o " ++ tgt)
>   case ex of
>     ExitFailure e -> throwError $ AEMisc $ "psql failed with " ++ show e
>     ExitSuccess -> return ()-}

> -}


pandoc -H docs/header -s --toc -f markdown+lhs HsSqlSystem.lhs  > HsSqlSystem.lhs.html

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
