
> module Database.HsSqlPpp.Tests.Parsing.Utils where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Generics
>
> import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Pretty

> data Item = Expr String ScalarExpr
>           | Stmt String [Statement]
>           | MSStmt String [Statement]
>           | PgSqlStmt String [Statement]
>           | Group String [Item]

-------------------------------------------------------------------------------

shortcuts for constructing test data and asts

> stringQ :: String -> ScalarExpr
> stringQ = StringLit ea
>
> selectFrom :: SelectItemList
>            -> TableRef
>            -> QueryExpr
> selectFrom selList frm = Select ea Dupes (SelectList ea selList)
>                            [frm] Nothing [] Nothing [] Nothing Nothing
>
> selectE :: SelectList -> QueryExpr
> selectE selList = Select ea Dupes selList
>                     [] Nothing [] Nothing [] Nothing Nothing
>
> selIL :: [String] -> [SelectItem]
> selIL = map selI
> selEL :: [ScalarExpr] -> [SelectItem]
> selEL = map (SelExp ea)
>
> i :: String -> SQIdentifier
> i = SQIdentifier ea . (:[])

> dqi :: String -> SQIdentifier
> dqi ii = SQIdentifier ea [ii]

> eqi :: String -> String -> ScalarExpr
> eqi c n = QIdentifier ea (Identifier ea c) n

> ei :: String -> ScalarExpr
> ei = Identifier ea
>
> qi :: String -> String -> SQIdentifier
> qi c n = SQIdentifier ea [c,n]
>
> selI :: String -> SelectItem
> selI = SelExp ea . Identifier ea
>
> sl :: SelectItemList -> SelectList
> sl a = SelectList ea a
>
> selectFromWhere :: SelectItemList
>                 -> TableRef
>                 -> ScalarExpr
>                 -> QueryExpr
> selectFromWhere selList frm whr =
>     Select ea Dupes (SelectList ea selList)
>                [frm] (Just whr) [] Nothing [] Nothing Nothing
>
> att :: String -> String -> AttributeDef
> att n t = AttributeDef ea n (SimpleTypeName ea t) Nothing []

> ea :: Annotation
> ea = emptyAnnotation
