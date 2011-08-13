This code tests the typechecking of precision types, at the moment
this is limited to numeric, char and varchar.

numeric:
unary closed operators: returns same precision as input
binary closed operators: returns highest precision of two inputs (have
  to combine if also have scales)
tricky: int8 factorial (!,!!) returns numeric, what precision?
few other fns whose input is not numeric, but output is
casts to numeric: what precision?


> module Database.HsSqlPpp.Tests.TypeChecking.PrecisionTests
>     (precisionTests
>     ,precisionTestData
>     ,Item(..)) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> --import Data.List
> import Data.Generics.Uniplate.Data
> import Control.Monad
>
> --import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.SqlTypes
> import Database.HsSqlPpp.Utils.PPExpr
> --import Database.HsSqlPpp.Tests.TestUtils
> import Database.HsSqlPpp.PrettyPrinter
>
> data Item = Group String [Item]
>           | Query [CatalogUpdate] String Type
>
> precisionTests :: Test.Framework.Test
> precisionTests = itemToTft precisionTestData
>
> precisionTestData :: Item
> precisionTestData = Group "precision typechecking" []
