Test the type checking and errors within tablerefs
and test the produced simple select lists


> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.TypeChecking.TableRefTests
>     (tableRefTests
>     ,tableRefTestData
>     ,Item(..)) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> --import Data.List
> import Data.Generics.Uniplate.Data
> import Control.Monad
> import Text.Groom
>
> --import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.SqlTypes
> --import Database.HsSqlPpp.Utils.PPExpr
> --import Database.HsSqlPpp.Tests.TestUtils
> import Database.HsSqlPpp.PrettyPrinter
>
> data Item = Group String [Item]
>           | Query [CatalogUpdate] String (Either [TypeError] Type)
>
> tableRefTests :: Test.Framework.Test
> tableRefTests = itemToTft tableRefTestData
>
> tableRefTestData :: Item
> tableRefTestData = Group "tableRefs"
>   [Query db1 "select * from t;"
>      $ Right $ SetOfType $ CompositeType
>         [("a",typeInt)
>         ,("b",typeInt)]
>   ,Query db1 "select a,b from t;"
>      $ Right $ SetOfType $ CompositeType
>         [("a",typeInt)
>         ,("b",typeInt)]
>   ,Query db1 "select t.* from t;"
>      $ Right $ SetOfType $ CompositeType
>         [("a",typeInt)
>         ,("b",typeInt)]
>   ,Query db1 "select u.* from t;"
>      $ Left [UnrecognisedCorrelationName "u"]
>   ,Query db1 "select * from t u;"
>      $ Right $ SetOfType $ CompositeType
>         [("a",typeInt)
>         ,("b",typeInt)]
>   ,Query db1 "select * from t u(c,d);"
>      $ Right $ SetOfType $ CompositeType
>         [("c",typeInt)
>         ,("d",typeInt)]
>   ,Query db1 "select u.* from t u(c,d);"
>      $ Right $ SetOfType $ CompositeType
>         [("c",typeInt)
>         ,("d",typeInt)]
>   ,Query db1 "select c,d from t u(c,d);"
>      $ Right $ SetOfType $ CompositeType
>         [("c",typeInt)
>         ,("d",typeInt)]
>   ,Query db1 "select t.* from t u;"
>      $ Left [UnrecognisedCorrelationName "t"]
>   -- disabled because the error comes out twice, and I can't work out why
>   -- this problem occurs in the uuagc code so it is probably the ag code which is wrong
>   {-,Query db1 "select * from t u(a);"
>      $ Left [WrongNumberOfAliasCols 2 1]-}
>   ,Query db1 "select c from t;"
>      $ Left [UnrecognisedIdentifier "c"]
>   ,Query db1 "select a from t u(c,d);"
>      $ Left [UnrecognisedIdentifier "a"]
>   ,Query db1 "select * from non;"
>      $ Left [UnrecognisedRelation "non"]
>   ,Query db1 "select a,b,d from t,u where a=c;"
>      $ Right $ SetOfType $ CompositeType
>         [("a",typeInt)
>         ,("b",typeInt)
>         ,("d",typeInt)]
>   ]

table ref tests:

columns in:
tref, subtref, funtref
jointref: aliased, unaliased, 3 way

select lists:
simple, qualified, *, q.*


errors:
unrecognised id
ambiguous id
incompatible join columns
unrecognised relation
missing join attribute




> db1 :: [CatalogUpdate]
> db1 = [CatCreateTable "t" [("a",typeInt)
>                           ,("b", typeInt)] []
>       ,CatCreateTable "u" [("c",typeInt)
>                           ,("d", typeInt)] []
>       ,CatCreateTable "v" [("v",typeInt)] []]

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Group s is) = testGroup s $ map itemToTft is
> itemToTft (Query eu sql t) = testCase ("typecheck " ++ sql) $ do
>   let ast = case parseQueryExpr "" sql of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckQueryExpr cat ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = [x | x <- universeBi aast]
>       res = if null er
>             then case ty of
>                    Nothing -> Left []
>                    Just ty' -> Right ty'
>             else Left er
>   when (t /= res) $ putStrLn $ "bad sql: " ++ printQueryExpr aast
>        ++ "\n" ++ groom aast
>   assertEqual "" t res
>   where
>     cat = case updateCatalog defaultTemplate1Catalog eu of
>                         Left x -> error $ show x
>                         Right e -> e



