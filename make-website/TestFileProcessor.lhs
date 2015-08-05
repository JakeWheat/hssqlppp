Code to read some of the test files and convert to html to serve as
examples.

> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
> module TestFileProcessor
>     (parserTestsTable
>     ,typeCheckTestsTable
>     ,quasiQuoteTestsTable) where
>
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Utils.Here
> import Text.Show.Pretty
> import Database.HsSqlPpp.Tests.Parsing.ParserTests as PT
> import Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests as TT
> import Language.Haskell.Exts hiding (String)
> --import qualified Language.Haskell.Exts as Exts
> --import Data.Generics
> import Data.Generics.Uniplate.Data
> import Database.HsSqlPpp.Utils.GroomUtils
> import qualified Data.Text.Lazy as L
> import Text.Pandoc
> import Data.Data
> import Text.Blaze.Renderer.String
> import Text.Highlighting.Kate
> --import Debug.Trace

> data Row = Row [[Text]]
>          | HHeader String
>
> data Text = Text String
>           | Sql String
>           | Haskell String
>
> --parserTestsTable :: String
> --parserTestsTable = parserIntro ++ rowsToHtml (mapParserTests PT.parserTestData)

> parserTestsTable :: [Block]
> parserTestsTable = parserIntro ++ rowsToHtml (mapParserTests PT.parserTestData)

>
> typeCheckTestsTable :: [Block]
> typeCheckTestsTable = typeCheckIntro ++ rowsToHtml (mapTypeCheckTests TT.typeCheckTestData)
>

>
> mapParserTests :: PT.Item -> [Row]
> mapParserTests (PT.Expr s e) = [Row [[Sql $ L.unpack s],[Haskell (groomNoAnns e)]]]
> mapParserTests (PT.QueryExpr s e) = [Row [[Sql $ L.unpack s],[Haskell (groomNoAnns e)]]]
> mapParserTests (PT.Stmt s e) = [Row [[Sql $ L.unpack s],[Haskell (groomNoAnns e)]]]
> mapParserTests (PT.PgSqlStmt s e) = [Row [[Sql $ L.unpack s],[Haskell (groomNoAnns e)]]]
> mapParserTests (PT.Group n is) = HHeader n : concatMap mapParserTests is
> mapParserTests (PT.TSQL s e) = [Row [[Sql $ L.unpack s],[Haskell (groomNoAnns e)]]]
> mapParserTests _ = [] -- Row [[Sql $ L.unpack s],[Haskell (groomNoAnns e)]]]


need to use haskell-src-exts for the quasi quote tests since we want
to get the quasi quote source syntax, not the asts it produces at
compile time.

> mapTypeCheckTests :: TT.Item -> [Row]
> mapTypeCheckTests (TT.Group n is) =
>    HHeader n : concatMap mapTypeCheckTests is
> mapTypeCheckTests (TT.ScalExpr s r) =
>   [Row [[Sql $ L.unpack s],[Haskell (ppShow r)]]]
> mapTypeCheckTests (TT.TCQueryExpr c s r) =
>    [Row [[Haskell (ppShow c),Sql $ L.unpack s],[Haskell (ppShow r)]]]
> mapTypeCheckTests (TT.RewriteQueryExpr fs cus s0 s1) =
>    [Row [[Haskell (ppShow fs)
>          ,Haskell (ppShow cus)]
>         ,[Sql $ L.unpack s0,Text "rewritten to",Sql $ L.unpack s1]]]
> mapTypeCheckTests (TT.ImpCastsScalar _ s0 s1) =
>    [Row [[Sql $ L.unpack s0]
>         ,[Sql $ L.unpack s1]]]

> mapTypeCheckTests _ = []

> {-mapTypeCheckTests (TT.StmtType s r) = [Row [[Sql s],[Haskell (ppShow r)]]]
> mapTypeCheckTests (TT.CatStmtType s c r) = [Row [[Haskell (ppShow c),Sql s],[Haskell (ppShow r)]]]
> mapTypeCheckTests (TT.Ddl s c) = [Row [[Sql s],[Haskell (ppShow c)]]]-}
>
> rowsToHtml :: [Row] -> [Block] -- String
> rowsToHtml rs =
>     let l = getLen rs
>     in if l == 0
>     then []
>     else [Table [] (replicate l AlignDefault)
>              (replicate l 0) [] $ map makeRow rs]
>   where
>     makeRow :: Row -> [TableCell]
>     makeRow (Row cells) = map makeCell cells
>     makeRow (HHeader h) = [] -- [Header Int Attr [Inline]]
>     makeCell :: [Text] -> TableCell
>     makeCell = concatMap mk
>     mk :: Text -> [Block]
>     mk (Text s) = [Para [Str s]]
>     mk (Sql s) = code "SqlPostgresql" s
>     mk (Haskell s) = code "haskell" s
>     code :: String -> String -> [Block]
>     code t s = highlightCode t
>         --readMarkdownBlocks ("~~~~." ++ t ++ "\n"
>         --                    ++ trim s
>         --                    ++ "\n~~~~\n")
>         [Div ("", [t], []) [CodeBlock ("", ["sourceCode"] ,[]) $ trim s]]
>     getLen [] = 0
>     getLen (Row x : _) = length x
>     getLen (_:xs) = getLen xs
>     highlightCode :: Data a => String -> a -> a
>     highlightCode t = transformBi $ \x -> case x of
>         CodeBlock a c ->
>             RawBlock "html" $ renderMarkup
>                 $ formatHtmlBlock defaultFormatOpts $ highlightAs t c

:: String	
Language syntax (e.g. haskell) or extension (e.g. hs).
-> String	
Source code to highlight
-> [SourceLine]	
List of highlighted source lines

>         _ -> x

-------------------------------------------------------------------------------

> quasiQuoteTestsTable :: IO [Block]
> quasiQuoteTestsTable = do
>   ast <- pf "hssqlppp-th/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs"
>   let lets = [l | l@(Let _ _) <- universeBi ast]
>   return $ qqIntro ++ rowsToHtml (map ((\s -> Row [[Haskell s]]) . prettyPrint) lets)

> pf :: String -> IO Module
> pf f = do
>   x <- parseFileWithExts [EnableExtension QuasiQuotes] f
>   case x of
>         ParseOk ast -> return ast
>         e -> error $ show e


> readMarkdownBlocks :: String -> [Block]
> readMarkdownBlocks s = case readMarkdown def s of
>     Right (Pandoc _ b) -> b
>     Left x -> error $ show x

> parserIntro :: [Block]
> parserIntro = readMarkdownBlocks [here|
>
> The parser examples have the sql source on the left, and the ast that the parser produces
> the right, the annotations have been replaced with a placeholder 'A' to make the output a bit more readable.
>
> The source this file is generated from is here:
> [ParserTests.lhs](https://github.com/JakeWheat/hssqlppp/blob/master/src-extra/tests/Database/HsSqlPpp/Tests/ParserTests.lhs)
> |]

> typeCheckIntro :: [Block]
> typeCheckIntro = readMarkdownBlocks [here|
>
> The type checking examples have the sql on the left and the result of type checking
> on the right. Different sections are using different tests:
>
> * parse and type check expression, return either type error or type
> * parse and type check a list of statements, return either type error or the top level statement type annotation
> * update the catalog, then check a list of statements
> * parse and type check some ddl statements, return the list of catalog updates they generate
>
> It's a bit rough at the moment, the clarity will be improved.
>
> The source this file is generated from is here:
> [TypeCheckTests.lhs](https://github.com/JakeWheat/hssqlppp/blob/master/src-extra/tests/Database/HsSqlPpp/Tests/TypeCheckTests.lhs)
> |]

> qqIntro :: [Block]
> qqIntro = readMarkdownBlocks [here|
>
> Pretty rough presentation, each example is a lets, with a pair of sql
> quasiquotes: one with antiquotes, and one with the resultant sql without antiquotes.
>
> The source this file is generated from is here:
> [QuasiQuoteTests.lhs](https://github.com/JakeWheat/hssqlppp/blob/master/src-extra/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs)
> |]

>

