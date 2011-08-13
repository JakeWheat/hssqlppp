Code to read some of the test files and convert to html to serve as
examples.

> {-# LANGUAGE QuasiQuotes #-}
> module TestFileProcessor
>     (parserTestsTable
>     ,typeCheckTestsTable
>     ,quasiQuoteTestsTable) where
>
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Utils.Here
> import Text.Groom
> import Database.HsSqlPpp.Tests.Parsing.ParserTests as PT
> import Database.HsSqlPpp.Tests.TypeCheckTests as TT
> import Language.Haskell.Exts hiding (String)
> import qualified Language.Haskell.Exts as Exts
> --import Data.Generics
> import Data.Generics.Uniplate.Data
>
> data Row = Row [[Text]]
>          | HHeader String
>
> data Text = Text String
>           | Sql String
>           | Haskell String
>
> parserTestsTable :: String
> parserTestsTable = parserIntro ++ rowsToHtml (mapParserTests PT.parserTestData)
>
> typeCheckTestsTable :: String
> typeCheckTestsTable = typeCheckIntro ++ rowsToHtml (mapTypeCheckTests TT.typeCheckTestData)
>

>
> mapParserTests :: PT.Item -> [Row]
> mapParserTests (PT.Expr s e) = [Row [[Sql s],[Haskell (ppExprNoAnns e)]]]
> mapParserTests (PT.Stmt s e) = [Row [[Sql s],[Haskell (ppExprNoAnns e)]]]
> mapParserTests (PT.PgSqlStmt s e) = [Row [[Sql s],[Haskell (ppExprNoAnns e)]]]
> mapParserTests (PT.Group n is) = HHeader n : concatMap mapParserTests is
> mapParserTests (PT.MSStmt s e) = [Row [[Sql s],[Haskell (ppExprNoAnns e)]]]

> ppExprNoAnns :: Show a => a -> String
> ppExprNoAnns = p stripA
>   where
>     stripA :: Exp -> Exp
>     stripA = transformBi $ \x ->
>                case x of
>                  (Paren (RecConstr (UnQual (Ident "Annotation")) _)) ->
>                           Con $ UnQual $ Ident "Ann"
>                  x1 -> x1
>     p f s =
>         case Exts.parseExp (show s) of
>           Exts.ParseOk ast -> Exts.prettyPrint (f ast)
>           x -> error $ show x


need to use haskell-src-exts for the quasi quote tests since we want
to get the quasi quote source syntax, not the asts it produces at
compile time.

> mapTypeCheckTests :: TT.Item -> [Row]
> mapTypeCheckTests (TT.Group n is) = HHeader n : concatMap mapTypeCheckTests is
> mapTypeCheckTests (TT.Expr s r) = [Row [[Sql s],[Haskell (groom r)]]]
> mapTypeCheckTests (TT.StmtType s r) = [Row [[Sql s],[Haskell (groom r)]]]
> mapTypeCheckTests (TT.CatStmtType s c r) = [Row [[Haskell (groom c),Sql s],[Haskell (groom r)]]]
> mapTypeCheckTests (TT.Ddl s c) = [Row [[Sql s],[Haskell (groom c)]]]
>
> rowsToHtml :: [Row] -> String
> rowsToHtml rs =
>   "<table>" ++
>   concatMap rowToHtml rs ++
>   "</table>"
>   where
>     rowToHtml (Row rws) =
>         let w = 100 `div` length rws
>             md r = "<td style='width:" ++
>                      show w ++ "%'>" ++ concatMap tToH r ++
>                      "</td>"
>         in "<tr>" ++ concatMap md rws ++ "</tr>"
>     rowToHtml (HHeader s) =
>         "</table>\n" ++ s ++ "\n" ++ map (const '=') s ++ "\n<table>\n"
>     tToH (Text s) = s
>     tToH (Sql s) = code "SqlPostgresql" s
>     tToH (Haskell s) = code "haskell" s
>     code t s = "\n\n~~~~~~{." ++ t ++ "}\n"
>                ++ trim s
>                ++ "\n~~~~~~\n\n"

-------------------------------------------------------------------------------

> quasiQuoteTestsTable :: IO String
> quasiQuoteTestsTable = do
>
>   ast <- pf "src-extra/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs"
>   let lets = [l | l@(Let _ _) <- universeBi ast]
>   --mapM_ (putStrLn . prettyPrint) lets
>   return $ qqIntro ++ rowsToHtml (map ((\s -> Row [[Haskell s]]) . prettyPrint) lets)


> pf :: String -> IO Module
> pf f = do
>   x <- parseFileWithExts [QuasiQuotes] f
>   case x of
>         ParseOk ast -> return ast
>         e -> error $ show e


> parserIntro :: String
> parserIntro = [here|
>
> The parser examples have the sql source on the left, and the ast that the parser produces
> the right, the annotations have been replaced with a placeholder 'Ann' to make the output a bit more readable.
>
> The source this file is generated from is here:
> [ParserTests.lhs](https://github.com/JakeWheat/hssqlppp/blob/master/src-extra/tests/Database/HsSqlPpp/Tests/ParserTests.lhs)
> |]

 [ParserTests.lhs](source/tests/Database/HsSqlPpp/Tests/ParserTests.lhs.html)

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
> It's a bit rough at the moment, the clarity will be improved.
>
> The source this file is generated from is here:
> [TypeCheckTests.lhs](https://github.com/JakeWheat/hssqlppp/blob/master/src-extra/tests/Database/HsSqlPpp/Tests/TypeCheckTests.lhs)
> |]

(source/tests/Database/HsSqlPpp/Tests/TypeCheckTests.lhs.html

> qqIntro :: String
> qqIntro = [here|
>
> Pretty rough presentation, each example is a lets, with a pair of sql
> quasiquotes: one with antiquotes, and one with the resultant sql without antiquotes.
>
> The source this file is generated from is here:
> [QuasiQuoteTests.lhs](https://github.com/JakeWheat/hssqlppp/blob/master/src-extra/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs)
> |]

source/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs.html)

