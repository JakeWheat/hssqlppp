
test item data type

shortcuts for constructing test data and asts

> module Database.HsSqlPpp.Tests.Parsing.Utils where

> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation

> data Item = Expr String ScalarExpr
>           | Stmt String [Statement]
>           | QueryExpr String QueryExpr
>           | TSQL String [Statement]
>           | PgSqlStmt String [Statement]
>           | Group String [Item]

> stringQ :: String -> ScalarExpr
> stringQ = StringLit ea
>
> eqi :: String -> String -> ScalarExpr
> eqi c x = Identifier ea $ qn c x

> ei :: String -> ScalarExpr
> ei j = Identifier ea $ name j
>
> qn :: String -> String -> Name
> qn c n = Name ea [Nmc c, Nmc n]
>
> sl :: SelectItemList -> SelectList
> sl = SelectList ea
>
>
> att :: String -> String -> AttributeDef
> att n t = AttributeDef ea (Nmc n) (SimpleTypeName ea $ name t) Nothing []

> ea :: Annotation
> ea = emptyAnnotation

> name :: String -> Name
> name n = Name ea [Nmc n]

> member :: ScalarExpr -> ScalarExpr -> ScalarExpr
> member a b = BinaryOp ea (name ".") a b

> num :: String -> ScalarExpr
> num n = NumberLit ea n

> app :: String -> [ScalarExpr] -> ScalarExpr
> app n as = App ea (name n) as

> specop :: String -> [ScalarExpr] -> ScalarExpr
> specop n as = SpecialOp ea (name n) as


> prefop :: String -> ScalarExpr -> ScalarExpr
> prefop n a = PrefixOp ea (name n) a

> postop :: String -> ScalarExpr -> ScalarExpr
> postop n a = PostfixOp ea (name n) a

> binop :: String -> ScalarExpr -> ScalarExpr -> ScalarExpr
> binop n a0 a1 = BinaryOp ea (name n) a0 a1

> lTrue,lFalse,lNull :: ScalarExpr
> lTrue = BooleanLit ea True
> lFalse = BooleanLit ea False
> lNull = NullLit ea

> st :: String -> TypeName
> st n = SimpleTypeName ea (Name ea [Nmc n])

> parenQual :: ScalarExpr -> ScalarExpr -> ScalarExpr
> parenQual a b = BinaryOp ea (name ".") (Parens ea a) b

> tref :: String -> TableRef
> tref s = Tref ea (name s) (NoAlias ea)

> trefa :: String -> String -> TableRef
> trefa t a = Tref ea (name t) (TableAlias ea $ Nmc a)

> treffa :: String -> String -> [String] -> TableRef
> treffa t a cs = Tref ea (name t) (FullAlias ea (Nmc a) $ map Nmc cs)


> qtref :: String -> String -> TableRef
> qtref q i = Tref ea (qn q i) (NoAlias ea)

> si :: ScalarExpr -> SelectItem
> si = SelExp ea

> sia :: ScalarExpr -> NameComponent -> SelectItem
> sia e a = SelectItem ea e a


> str :: String -> ScalarExpr
> str = StringLit ea

> set :: String -> ScalarExpr -> SetClause
> set n v = SetClause ea (Nmc n) v

> varDef :: String -> TypeName -> VarDef
> varDef nm t = VarDef ea (Nmc nm) t Nothing

> varDefv :: String -> TypeName -> ScalarExpr -> VarDef
> varDefv nm t v = VarDef ea (Nmc nm) t (Just v)

> paramDef :: String -> TypeName -> ParamDef
> paramDef nm t = ParamDef ea (Nmc nm) t

> at :: String -> TypeName
> at = ArrayTypeName ea . st

> innerJoin :: TableRef -> TableRef -> Maybe ScalarExpr -> TableRef
> innerJoin a b o = JoinTref ea a Unnatural Inner b
>                            (fmap (JoinOn ea) o) (NoAlias ea)

> naturalInnerJoin :: TableRef -> TableRef -> TableRef
> naturalInnerJoin a b  = JoinTref ea a Natural Inner b Nothing
>                            (NoAlias ea)

> usingInnerJoin :: TableRef -> TableRef -> [String] -> TableRef
> usingInnerJoin a b us = JoinTref ea a Unnatural Inner b
>                            (Just $ JoinUsing ea $ map Nmc us) (NoAlias ea)

> join :: TableRef -> JoinType -> TableRef -> Maybe ScalarExpr -> TableRef
> join a b c o = JoinTref ea a Unnatural b c (fmap (JoinOn ea) o) (NoAlias ea)

> with :: [(String,QueryExpr)] -> QueryExpr -> QueryExpr
> with ws e =
>   WithQueryExpr ea
>    (map (\(n,ne) -> WithQuery ea (Nmc n) Nothing ne) ws)
>    e
