
test item data type

shortcuts for constructing test data and asts

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.Utils where

> import Database.HsSqlPpp.Syntax
> --import Database.HsSqlPpp.Lex (Token)
> import Database.HsSqlPpp.Annotation
> import qualified Data.Text as T
> import Data.Text (Text)
> --import qualified Data.Text.Lazy as L
> import Control.Arrow
> --import Database.HsSqlPpp.Internals.Dialect
> --import Database.HsSqlPpp.Tests.TestTypes

> stringQ :: Text -> ScalarExpr
> stringQ = StringLit ea . T.unpack
>
> eqi :: Text -> Text -> ScalarExpr
> eqi c x = Identifier ea $ qn c x

> ei :: Text -> ScalarExpr
> ei j = Identifier ea $ name j
>
> qn :: Text -> Text -> Name
> qn c n = Name ea [Nmc $ T.unpack c, Nmc $ T.unpack n]
>
> sl :: [SelectItem] -> SelectList
> sl = SelectList ea
>
>
> att :: Text -> Text -> AttributeDef
> att n t = AttributeDef ea (Nmc $ T.unpack n) (SimpleTypeName ea $ name t) Nothing []

> ea :: Annotation
> ea = emptyAnnotation

> name :: Text -> Name
> name n = Name ea [Nmc $ T.unpack n]

> member :: ScalarExpr -> ScalarExpr -> ScalarExpr
> member a b = BinaryOp ea (name ".") a b

> num :: Text -> ScalarExpr
> num n = NumberLit ea $ T.unpack n

> app :: Text -> [ScalarExpr] -> ScalarExpr
> app n as = App ea (name n) as

> specop :: Text -> [ScalarExpr] -> ScalarExpr
> specop n as = SpecialOp ea (name n) as


> prefop :: Text -> ScalarExpr -> ScalarExpr
> prefop n a = PrefixOp ea (name n) a

> postop :: Text -> ScalarExpr -> ScalarExpr
> postop n a = PostfixOp ea (name n) a

> binop :: Text -> ScalarExpr -> ScalarExpr -> ScalarExpr
> binop n a0 a1 = BinaryOp ea (name n) a0 a1

> lTrue,lFalse,lNull :: ScalarExpr
> lTrue = BooleanLit ea True
> lFalse = BooleanLit ea False
> lNull = NullLit ea

> st :: Text -> TypeName
> st n = SimpleTypeName ea (Name ea [Nmc $ T.unpack n])

> parenQual :: ScalarExpr -> ScalarExpr -> ScalarExpr
> parenQual a b = BinaryOp ea (name ".") (Parens ea a) b

> tref :: Text -> TableRef
> tref s = Tref ea (name s)

> trefa :: Text -> Text -> TableRef
> trefa t a = TableAlias ea (Nmc $ T.unpack a) $ Tref ea (name t)

> treffa :: Text -> Text -> [Text] -> TableRef
> treffa t a cs = FullAlias ea (Nmc $ T.unpack a) (map (Nmc . T.unpack) cs)
>                 $ Tref ea (name t)


> qtref :: Text -> Text -> TableRef
> qtref q i = Tref ea (qn q i)

> si :: ScalarExpr -> SelectItem
> si = SelExp ea

> sia :: ScalarExpr -> NameComponent -> SelectItem
> sia e a = SelectItem ea e a


> str :: Text -> ScalarExpr
> str = StringLit ea . T.unpack

> set :: Text -> ScalarExpr -> SetClause
> set n v = SetClause ea (Nmc $ T.unpack n) v

> varDef :: Text -> TypeName -> VarDef
> varDef nm t = VarDef ea (Nmc $ T.unpack nm) t Nothing

> varDefv :: Text -> TypeName -> ScalarExpr -> VarDef
> varDefv nm t v = VarDef ea (Nmc $ T.unpack nm) t (Just v)

> paramDef :: Text -> TypeName -> ParamDef
> paramDef nm t = ParamDef ea (Nmc $ T.unpack nm) t

> at :: Text -> TypeName
> at = ArrayTypeName ea . st

> innerJoin :: TableRef -> TableRef -> Maybe ScalarExpr -> TableRef
> innerJoin a b o = JoinTref ea a Unnatural Inner Nothing b
>                            (fmap (JoinOn ea) o)

> naturalInnerJoin :: TableRef -> TableRef -> TableRef
> naturalInnerJoin a b  = JoinTref ea a Natural Inner Nothing b Nothing

> usingInnerJoin :: TableRef -> TableRef -> [Text] -> TableRef
> usingInnerJoin a b us = JoinTref ea a Unnatural Inner Nothing b
>                            (Just $ JoinUsing ea $ map (Nmc . T.unpack) us)

> tjoin :: TableRef -> JoinType -> TableRef -> Maybe ScalarExpr -> TableRef
> tjoin a b c o = JoinTref ea a Unnatural b Nothing c (fmap (JoinOn ea) o)

> with :: [(Text,QueryExpr)] -> QueryExpr -> QueryExpr
> with ws e =
>   WithQueryExpr ea
>    (map (\(n,ne) -> WithQuery ea (Nmc n) Nothing ne) $ map (first T.unpack) ws)
>    e

> tfp :: TableRef -> TableRef
> tfp = TableRefParens ea
