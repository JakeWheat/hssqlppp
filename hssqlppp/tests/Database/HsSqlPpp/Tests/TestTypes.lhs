> module Database.HsSqlPpp.Tests.TestTypes (
>     defaultParseFlags
>    ,defaultTypeCheckFlags
>    ,Dialect(..)
>    ,ParseFlags(..)
>    ,TypeCheckFlags(..)
>    ,Item(..)
>    --,defaultTemplate1Catalog
>    --,ansiCatalog
>    ,emptyEnvironment
>    ,makeCatalog
>    ,hackCanonicalizeEnvTypeNames
>   ) where

> import Database.HsSqlPpp.Syntax
> import Database.HsSqlPpp.Lex (Token)
> --import Database.HsSqlPpp.Annotation
> import qualified Data.Text as T
> --import Data.Text (Text)
> import qualified Data.Text.Lazy as L
> --import Control.Arrow
> --import Test.HUnit
> --import Test.Framework.Providers.HUnit
> --import Test.Framework
> --import Data.List
> import Data.Generics.Uniplate.Data
> import Database.HsSqlPpp.Parse
> import Database.HsSqlPpp.TypeCheck
> --import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Dialect
> --import Database.HsSqlPpp.Ast hiding (App)
> import Database.HsSqlPpp.Types
> --import Database.HsSqlPpp.Pretty
> -- import Database.HsSqlPpp.Utility
> --import Database.HsSqlPpp.Internals.TypeChecking.Environment
> --import Text.Show.Pretty
> --import Debug.Trace
> --import Database.HsSqlPpp.Tests.TestUtils
> --import Control.Monad

> --import Database.HsSqlPpp.Utils.GroomUtils
> --import qualified Data.Text.Lazy as L
> import Database.HsSqlPpp.Internals.TypeChecking.TypeConversion.TypeConversion2
> import Data.Data


> data Item = Group String [Item]
>           | ParseScalarExpr ParseFlags L.Text ScalarExpr
>           | ParseStmts ParseFlags L.Text [Statement]
>           | ParseProcSql ParseFlags L.Text [Statement]
>           | ParseQueryExpr ParseFlags L.Text QueryExpr
>           | Lex Dialect T.Text [Token]
>           | TCScalExpr Catalog Environment TypeCheckFlags
>                        L.Text (Either [TypeError] Type)
>           | TCQueryExpr Catalog TypeCheckFlags
>                         L.Text (Either [TypeError] Type)
>           | TCStatements Catalog TypeCheckFlags
>                          L.Text (Maybe [TypeError])
>           | InsertQueryExpr [CatalogUpdate] L.Text (Either [TypeError] Type)
>           | RewriteQueryExpr TypeCheckFlags [CatalogUpdate] L.Text L.Text

>           | ImpCastsScalar TypeCheckFlags L.Text L.Text
>             -- todo: combine this with tcscalexpr
>           | ScalarExprExtra Dialect Catalog Environment L.Text (Either [TypeError] TypeExtra)
>           | MatchApp Dialect Catalog [NameComponent]
>                      [(TypeExtra, Maybe LitArg)]
>                      (Either [TypeError] ([TypeExtra],TypeExtra))
>           | Custom String (IO ())

> makeCatalog :: Dialect -> [CatalogUpdate] -> Catalog
> makeCatalog d cus =
>     either (error . show) id
>     $ updateCatalog (hackCanonicalizeEnvTypeNames d cus) $ diDefaultCatalog d

This takes a type name and canonicalizes it, first by trying to see if
it is the ansi type name and the dialect uses a different name for
that type, and then checks to see if this is a built in alias of a
type for that dialect and gets the canonical name instead.

> hackCanonicalizeEnvTypeNames :: Data a => Dialect -> a -> a
> hackCanonicalizeEnvTypeNames d = transformBi $ \a -> case a of
>     s -> maybe (canonicalizeTypeName d s) id $ ansiTypeNameToDialect d s
