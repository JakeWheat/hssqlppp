> module Database.HsSqlPpp.Tests.TestTypes where

> import Database.HsSqlPpp.Syntax
> import Database.HsSqlPpp.LexicalSyntax (Token)
> import Database.HsSqlPpp.Annotation
> import qualified Data.Text as T
> --import Data.Text (Text)
> import qualified Data.Text.Lazy as L
> --import Control.Arrow
> --import Test.HUnit
> --import Test.Framework.Providers.HUnit
> --import Test.Framework
> --import Data.List
> --import Data.Generics.Uniplate.Data
> --import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeCheck
> --import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.Ast hiding (App)
> import Database.HsSqlPpp.Types
> --import Database.HsSqlPpp.Pretty
> --import Database.HsSqlPpp.Utility
> --import Database.HsSqlPpp.Internals.TypeChecking.Environment
> --import Text.Show.Pretty
> --import Debug.Trace
> --import Database.HsSqlPpp.Tests.TestUtils
> --import Control.Monad

> --import Database.HsSqlPpp.Utils.GroomUtils
> --import qualified Data.Text.Lazy as L
> import Database.HsSqlPpp.Internals.TypeChecking.TypeConversion.TypeConversion2

> data Item = Group String [Item]
>           | Expr L.Text ScalarExpr
>           | Stmt L.Text [Statement]
>           | QueryExpr L.Text QueryExpr
>           | TSQL L.Text [Statement]
>           | OracleX L.Text [Statement]
>           | PgSqlStmt L.Text [Statement]
>           | Lex Dialect T.Text [Token]
>           | ScalExpr L.Text (Either [TypeError] Type)
>           | TCQueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | TCStatements [CatalogUpdate] L.Text (Maybe [TypeError])
>           | InsertQueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | TSQLQueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | OracleQueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | RewriteQueryExpr TypeCheckFlags [CatalogUpdate] L.Text L.Text
>           | ImpCastsScalar TypeCheckFlags L.Text L.Text
>           | ScalarExprExtra Catalog Environment L.Text (Either [TypeError] TypeExtra)
>           | MatchApp Dialect Catalog [NameComponent]
>                      [(TypeExtra, Maybe LitArg)]
>                      (Either [TypeError] ([TypeExtra],TypeExtra))


