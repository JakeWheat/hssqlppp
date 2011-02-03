> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.FixUpIdentifiersTests
>     (fixUpIdentifiersTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Generics.Uniplate.Data
> import Control.Monad

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.SqlTypes
> import Database.HsSqlPpp.Utils.PPExpr
> import Database.HsSqlPpp.Tests.TestUtils
> import Database.HsSqlPpp.PrettyPrinter
>
> data Item = Group String [Item]
>           | Item [CatalogUpdate] String String
>
> fixUpIdentifiersTests :: Test.Framework.Test
> fixUpIdentifiersTests = itemToTft fixUpIdentifiersTestsData
>
> fixUpIdentifiersTestsData :: Item
> fixUpIdentifiersTestsData = Group "cantests"
>   [Group "simple"
>     [Item db1 "select a,b from t;" "select t.a, t.b from t;"
>     ,Item db1 "select * from t;" "select t.a, t.b from t;"
>     ,Item db1 "select a,c from t,u;" "select t.a, u.c from t,u;"
>     ,Item db1 "select * from t,u;" "select t.a,t.b,u.c,u.d from t,u;"
>     ,Item db1 "select t.* from t,u;" "select t.a, t.b from t,u;"
>     ,Item db1 "select u.* from t,u;" "select u.c,u.d from t,u;"
>     ]
>   ]

qualifier and column name the same


> db1 :: [CatalogUpdate]
> db1 = [CatCreateTable "t" [("a",typeInt)
>                           ,("b", typeInt)] []
>       ,CatCreateTable "u" [("c",typeInt)
>                           ,("d", typeInt)] []]

------------------------

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Group s is) = testGroup s $ map itemToTft is
> itemToTft (Item eu sql esql) = testCase sql $ do
>   let eAst = case parseStatements "" esql of
>                               Left e -> error $ show e
>                               Right l -> resetAnnotations l
>       ast = case parseStatements "" sql of
>                               Left e -> error $ show e
>                               Right l -> resetAnnotations l
>       cAst = fixUpIdentifiers makeCat ast
>       c2Ast = fixUpIdentifiers makeCat cAst
>   putStrLn $ printStatements cAst ++ "\n" ++ printStatements c2Ast
>   when (eAst /= cAst) $ do
>     putStrLn $ "\nExpected:\n\n" ++ printStatements eAst
>     putStrLn $ "\nGot:\n\n" ++ printStatements cAst ++ "\n\n"
>   assertEqual "" eAst cAst
>   assertEqual "redo" cAst c2Ast
>   where
>     makeCat = case updateCatalog defaultTemplate1Catalog eu of
>                         Left x -> error $ show x
>                         Right e -> e
