
> module Database.HsSqlPpp.Tests.TypeChecking.Utils where

> --import Test.HUnit
> --import Test.Framework
> --import Test.Framework.Providers.HUnit
> --import Data.List
> --import Data.Generics.Uniplate.Data
>
> --import Database.HsSqlPpp.Utils.Here
> --import Database.HsSqlPpp.Parser
> --import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> --import Text.Groom
> --import Database.HsSqlPpp.Tests.TestUtils
>
> data Item = Group String [Item]
>           | Expr String (Either [TypeError] Type)
>           | StmtType String (Either [TypeError] [Maybe StatementType])
>           | CatStmtType String [CatalogUpdate] (Either [TypeError] [Maybe StatementType])
>           | Ddl String [CatalogUpdate]
