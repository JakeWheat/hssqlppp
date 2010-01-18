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
>                     ,Either [TypeError] (String,Type))] -- source, type
>           | StarExpand [([LocalBindingsUpdate], String, Either [TypeError] [(String,(String,Type))])]

> localBindingsTests :: [Test.Framework.Test]
> localBindingsTests = itemToTft testData

> testData :: Item
> testData =
>   Group "local bindings tests" []

================================================================================

> testIdLookup :: [LocalBindingsUpdate]
>              -> String
>              -> String
>              -> Either [TypeError] (String,Type)
>              -> Test.Framework.Test
> testIdLookup lbus cn i res = undefined

> testStarExpand :: [LocalBindingsUpdate]
>                -> String
>                -> Either [TypeError] [(String,(String,Type))]
>                -> Test.Framework.Test
> testStarExpand lbus cn res = undefined

> itemToTft :: Item -> [Test.Framework.Test]
> itemToTft (Lookup es) = map (\(a,b,c,d) -> testIdLookup a b c d) es
> itemToTft (StarExpand es) = map (\(a,b,c) -> testStarExpand a b c) es
> itemToTft (Group s is) = [testGroup s $ concatMap itemToTft is]
