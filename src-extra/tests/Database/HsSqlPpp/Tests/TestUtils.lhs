
> module Database.HsSqlPpp.Tests.TestUtils
>     (assertTrace,resetAnnotations) where
>
> import Test.HUnit
> --import Test.Framework
> --import Test.Framework.Providers.HUnit
> --import Data.List
> import Data.Generics.Uniplate.Data
> import Data.Data

> import Debug.Trace
> import Control.Monad

> --import Database.HsSqlPpp.Utils.Here
> --import Database.HsSqlPpp.Parser
> --import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> --import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.SqlTypes
> --import Database.HsSqlPpp.Utils.PPExpr


> assertTrace :: (Show a,Eq a) => String -> String -> a -> a -> IO ()
> assertTrace nem s a1 a2 = do
>     when (a1 /= a2) $ trace nem $ return ()
>     assertEqual s a1 a2

> resetAnnotations :: Data a => a -> a
> resetAnnotations = transformBi (const emptyAnnotation)