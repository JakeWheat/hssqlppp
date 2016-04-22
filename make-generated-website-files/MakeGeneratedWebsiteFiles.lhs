
Takes the tests and produces asciidoc tables of the tests to give some
not very curated idea about what hssqlppp supports for the
documentation, and a bunch of examples of what the ASTs and types look
like.

TODO:
change the number of cells in a row to be flexible (is this needed)
render the qq tests
add some of the missing information to the output

> {-# LANGUAGE QuasiQuotes #-}

> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Tests.Parsing.ParserTests as PT
> import Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests as TT
> --import Text.Show.Pretty
> import qualified Data.Text.Lazy as L
> import qualified Data.Text as T
> import Database.HsSqlPpp.Utils.GroomUtils
> --import Control.Monad
> import Language.Haskell.Exts hiding (String)
> import Data.Generics.Uniplate.Data

> data Cell = Cell [CellItem]
> data CellItem = Str String | Sql String | Haskell String

> data TableItem = Heading Int String
>                | Row [Cell]

> doc :: Int -> PT.Item -> [TableItem]
> doc n (Group nm is) =
>     let subs = concatMap (doc (n + 1)) is
>     in if not (null subs)
>        then Heading n nm : subs
>        else []

> doc _ (ParseScalarExpr _ sql e) =
>     [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (ParseStmts _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (ParseProcSql _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (ParseQueryExpr _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (Lex _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (TCScalExpr _ _ _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (TCQueryExpr _ _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (TCStatements _ _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (InsertQueryExpr _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (RewriteQueryExpr _ _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (ImpCastsScalar _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (ScalarExprExtra _ _ _ sql e) =
>   [Row [Cell [Sql $ L.unpack sql], Cell [Haskell (groomNoAnns e)]]]
> doc _ (MatchApp {}) = []
> doc _ (Custom {}) = []


> parserTestsTable :: String
> parserTestsTable = render $ doc 1 PT.parserTestData

> typeCheckTestsTable :: String
> typeCheckTestsTable = render $ doc 1 TT.typeCheckTestData

> quasiQuotesTestsTable :: IO String
> quasiQuotesTestsTable = do
>   ast <- pf "hssqlppp-th/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs"
>   let lets = [l | l@(Let _ _) <- universeBi ast]
>   return $ render $ map (\x -> Row [Cell [Haskell (prettyPrint x)]]) lets

> pf :: String -> IO Module
> pf f = do
>   x <- parseFileWithExts [EnableExtension QuasiQuotes] f
>   case x of
>         ParseOk ast -> return ast
>         e -> error $ show e

> render :: [TableItem] -> String
> render = go False
>   where
>     go t (Heading level title : is) =
>         concat
>         [if t then "|===\n" else ""
>         ,-- slight hack
>          if level > 1
>          then
>            "\n" ++ replicate level '=' ++ " " ++ title ++ "\n"
>            else ""
>         ,go False is]
>     go t (Row [] : is) = go t is
>     go t (Row cs : is) =
>       let ncs = length cs
>       in concat
>         [if t then "" else "[cols=\"" ++ show ncs ++ "\"]\n|===\n"
>         ,let rce c = case c of
>                Sql s -> "\n[source,sql]\n----\n" ++ s ++ "\n----\n"
>                Str s -> s
>                Haskell s -> "\n[source,haskell]\n----\n" ++ s ++ "\n----\n"
>              rcell (Cell cis) = concatMap rce cis
>          in concatMap (\x -> "a|" ++ escapePipe x ++ "\n") $ map rcell cs
>         ,go True is]
>     go t [] = if t then "|===\n" else ""
>     escapePipe [] = []
>     escapePipe ('\\':'|':xs) = '\\' : '\\' : '\\' : '|' : escapePipe xs
>     escapePipe ('|':xs) = '\\' : '|' : escapePipe xs
>     escapePipe (x:xs) = x : escapePipe xs

> main :: IO ()
> main = do
>   writeFile "build/website/ParserTests.asciidoc" $
>        asciidocHeader "Parser tests"
>        ++ parserIntro ++ "\n"
>        ++ parserTestsTable
>   writeFile "build/website/TypeCheckTests.asciidoc" $
>        asciidocHeader "Typecheck tests"
>        ++ typeCheckIntro ++ "\n"
>        ++ typeCheckTestsTable
>   qqt <- quasiQuotesTestsTable
>   writeFile "build/website/QuasiQuoteTests.asciidoc" $
>        asciidocHeader "Quasiquote tests"
>        ++ qqIntro ++ "\n"
>        ++ qqt

> asciidocHeader :: String -> String
> asciidocHeader t = "\n:toc:\n\
>                 \:toc-placement: macro\n\
>                 \:sectnums:\n\
>                 \:toclevels: 10\n\
>                 \:sectnumlevels: 10\n\
>                 \:source-highlighter: pygments\n\n\
>                 \= " ++ t ++ "\n\n\
>                 \toc::[]\n"



> parserIntro :: String
> parserIntro = [here|
\begin{code}

== Overview

The parser examples have the sql source on the left, and the ast that the parser produces
the right, the annotations have been replaced with a placeholder 'A' to make the output a bit more readable.

The source this file is generated from is here:
[ParserTests.lhs](https://github.com/JakeWheat/hssqlppp/blob/master/src-extra/tests/Database/HsSqlPpp/Tests/ParserTests.lhs)
\end{code}
> |]

> typeCheckIntro :: String
> typeCheckIntro = [here|
>
> The type checking examples have the sql on the left and the result of type checking
> on the right. Different sections are using different tests:
>
> * parse and type check expression, return either type error or type
> * parse and type check a list of statements, return either type error or the top level statement type annotation
> * update the catalog, then check a list of statements
> * parse and type check some ddl statements, return the list of catalog updates they generate
>
> The source this file is generated from is here:
> [TypeCheckTests.lhs](https://github.com/JakeWheat/hssqlppp/blob/master/src-extra/tests/Database/HsSqlPpp/Tests/TypeCheckTests.lhs)
> |]

> qqIntro :: String
> qqIntro = [here|
>
> Pretty rough presentation, each example is a lets, with a pair of sql
> quasiquotes: one with antiquotes, and one with the resultant sql without antiquotes.
>
> The source this file is generated from is here:
> [QuasiQuoteTests.lhs](https://github.com/JakeWheat/hssqlppp/blob/master/src-extra/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs)
> |]

