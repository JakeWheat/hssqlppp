Copyright 2010 Jake Wheat

Tests for the local bindings lookup code, which is a bit convoluted in
places, particularly for joins

> module Database.HsSqlPpp.Tests.LocalBindingsTests (localBindingsTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char
> --import Text.Show.Pretty
> --import Debug.Trace

> import Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindings

> import Database.HsSqlPpp.Ast.SqlTypes
> --import Database.HsSqlPpp.Ast.Annotation
> --import Database.HsSqlPpp.Parsing.Parser
> --import Database.HsSqlPpp.Ast.TypeChecker
> --import Database.HsSqlPpp.Ast.Catalog

> data Item = Group String [Item]
>           | Lookup [([LocalBindingsUpdate]
>                     ,String -- correlation name
>                     ,String -- id name
>                     ,Either [TypeError] (String,String,String,Type))] -- source, corr, type
>           | StarExpand [([LocalBindingsUpdate], String, Either [TypeError] [(String,String,String,Type)])]

> localBindingsTests :: [Test.Framework.Test]
> localBindingsTests = itemToTft testData

> testData :: Item
> testData =
>   Group "local bindings tests" [ Lookup [
>     ([], "", "test", Left [UnrecognisedIdentifier "test"])
>    ,([], "test", "test", Left [UnrecognisedIdentifier "test.test"])
>    ,([LBQualifiedIds "source1"
>                      ""
>                      [("test1", typeInt)
>                      ,("test2", typeBool)]
>                      []]
>       ,"","test1", Right ("source1", "", "test1", typeInt))
>    ,([LBQualifiedIds "source1"
>                      ""
>                      [("test1", typeInt)
>                      ,("test2", typeBool)]
>                      []]
>       ,"","test2", Right ("source1", "", "test2", typeBool))
>    ,([LBQualifiedIds "source1"
>                      ""
>                      [("test1", typeInt)
>                      ,("test2", typeBool)]
>                      []]
>       ,"","test3", Left [UnrecognisedIdentifier "test3"])
>    ,([LBQualifiedIds "source1"
>                      ""
>                      [("test1", typeInt)
>                      ,("test2", typeBool)]
>                      []]
>       ,"test","test1", Left [UnrecognisedIdentifier "test.test1"])
>    ,([LBUnqualifiedIds "source1"
>                      [("test1", typeInt)
>                      ,("test2", typeBool)]
>                      []]
>       ,"","test1", Right ("source1", "", "test1", typeInt))
>    ,([LBUnqualifiedIds "source1"
>                      [("test1", typeInt)
>                      ,("test2", typeBool)]
>                      []]
>       ,"","test2", Right ("source1", "", "test2", typeBool))
>    ,([LBUnqualifiedIds "source1"
>                      [("test1", typeInt)
>                      ,("test2", typeBool)]
>                      []]
>       ,"","test3", Left [UnrecognisedIdentifier "test3"])
>    ,([LBUnqualifiedIds "source1"
>                      [("test1", typeInt)
>                      ,("test2", typeBool)]
>                      []]
>       ,"test","test1", Left [UnrecognisedIdentifier "test.test1"])
>   ]]

LBQualifiedIds {
                              source :: String
                             ,correlationName :: String
                             ,ids :: [(String,Type)]
                             ,internalIds :: [(String,Type)]
                             }
                          | LBUnqualifiedIds {
                              source :: String
                             ,ids :: [(String,Type)]
                             ,internalIds :: [(String,Type)]
                             }
                          | LBJoinIds {
                              source1 :: String
                             ,correlationName1 :: String
                             ,ids1 :: [(String,Type)]
                             ,internalIds1 :: [(String,Type)]
                             ,source2 :: String
                             ,correlationName2 :: String
                             ,ids2 :: [(String,Type)]
                             ,internalIds2 :: [(String,Type)]
                             ,joinIds :: [String]
                             }


================================================================================

> testIdLookup :: [LocalBindingsUpdate]
>              -> String
>              -> String
>              -> Either [TypeError] (String,String,String,Type)
>              -> Test.Framework.Test
> testIdLookup lbus cn i res = testCase ("lookup " ++ cn ++ "." ++ i) $ do
>     let lb = foldr lbUpdate emptyBindings lbus
>         r = lbLookupID lb cn i
>     assertEqual "lookupid" res r

> testStarExpand :: [LocalBindingsUpdate]
>                -> String
>                -> Either [TypeError] [(String,String,String,Type)]
>                -> Test.Framework.Test
> testStarExpand lbus cn res = undefined

> itemToTft :: Item -> [Test.Framework.Test]
> itemToTft (Lookup es) = map (\(a,b,c,d) -> testIdLookup a b c d) es
> itemToTft (StarExpand es) = map (\(a,b,c) -> testStarExpand a b c) es
> itemToTft (Group s is) = [testGroup s $ concatMap itemToTft is]
