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

> import Data.Char

> -- import Language.Haskell.Exts
> import Database.HsSqlPpp.Utils
> import Database.HsSqlPpp.Tests.ParserTests as PT
> import Database.HsSqlPpp.Tests.TypeCheckTests as TT
>
> main :: IO ()
> main = do
>   --putStrLn $ rowsToHtml parserTestsTable
>   putStrLn $ rowsToHtml typeCheckTestsTable
>   return ()
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

> parserTestsTable :: [Row]
> parserTestsTable = mapParserTests PT.parserTestData

> typeCheckTestsTable :: [Row]
> typeCheckTestsTable = mapTypeCheckTests TT.typeCheckTestData


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
>     rowToHtml (Header s) =
>         "</table>\n" ++ s ++ "\n" ++ map (const '=') s ++ "\n<table>\n"
>     tToH (Text s) = s
>     tToH (Sql s) = code "SqlPostgresql" s
>     tToH (Haskell s) = code "haskell" s
>     code t s = "\n\n~~~~~~{." ++ t ++ "}\n"
>                ++ trim s
>                ++ "\n~~~~~~\n\n"

pandoc -H docs/header -s --toc -f markdown+lhs HsSqlSystem.lhs  > HsSqlSystem.lhs.html

> data Row = Row [Text] [Text]
>          | Header String

> data Text = Text String
>           | Sql String
>           | Haskell String

> mapParserTests :: PT.Item -> [Row]
> mapParserTests (PT.Expr s e) = [Row [Sql s] [Haskell (ppExpr e)]]
> mapParserTests (PT.Stmt s e) = [Row [Sql s] [Haskell (ppExpr e)]]
> mapParserTests (PT.PgSqlStmt s e) = [Row [Sql s] [Haskell (ppExpr e)]]
> mapParserTests (PT.Group n is) = Header n : concatMap mapParserTests is

> mapTypeCheckTests :: TT.Item -> [Row]
> mapTypeCheckTests (TT.Group n is) = Header n : concatMap mapTypeCheckTests is
> mapTypeCheckTests (TT.Expr s r) = [Row [Sql s] [Haskell (ppExpr r)]]
> mapTypeCheckTests (TT.StmtType s r) = [Row [Sql s] [Haskell (ppExpr r)]]
> mapTypeCheckTests (TT.CatStmtType s c r) = [Row [Haskell (ppExpr c),Sql s] [Haskell (ppExpr r)]]
> mapTypeCheckTests (TT.Ddl s c) = [Row [Sql s] [Haskell (ppExpr c)]]


> trim :: String -> String
> trim = f . f
>    where f = reverse . dropWhile isSpace