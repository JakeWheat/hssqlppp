Copyright 2009 Jake Wheat

This is the public module for the ast nodes.

> {- | This module contains the ast node data types. They are very permissive, in that they allow
> a lot of invalid SQL to be represented. The type checking process should event


>    -}
> module Database.HsSqlPpp.TypeChecking.Ast
>     (
>      -- * Main nodes
>      StatementList
>     ,Statement (..)
>     ,Expression (..)
>     ,SelectExpression (..)
>      -- * Components
>      -- ** Selects
>     ,SelectList (..)
>     ,SelectItem (..)
>     ,TableRef (..)
>     ,JoinExpression (..)
>     ,JoinType (..)
>     ,Natural (..)
>     ,CombineType (..)
>     ,Direction (..)
>     ,Distinct (..)
>     ,InList (..)
>      -- ** dml
>     ,SetClause (..)
>     ,CopySource (..)
>     ,RestartIdentity (..)
>      -- ** ddl
>     ,AttributeDef (..)
>     ,RowConstraint (..)
>     ,Constraint (..)
>     ,TypeAttributeDef (..)
>     ,TypeName (..)
>     ,DropType (..)
>     ,IfExists (..)
>     ,Cascade (..)
>      -- ** functions
>     ,FnBody (..)
>     ,ParamDef (..)
>     ,VarDef (..)
>     ,RaiseType (..)
>     ,Volatility (..)
>     ,Language (..)
>      -- ** typedefs
>     ,ExpressionListStatementListPairList
>     ,ExpressionListStatementListPair
>     ,ExpressionList
>     ,StringList
>     ,ParamDefList
>     ,AttributeDefList
>     ,ConstraintList
>     ,TypeAttributeDefList
>     ,Where
>     ,StringStringListPairList
>     ,StringStringListPair
>     ,ExpressionStatementListPairList
>     ,SetClauseList
>     ,CaseExpressionListExpressionPairList
>     ,MaybeExpression
>     ,MTableRef
>     ,ExpressionListList
>     ,SelectItemList
>     ,OnExpr
>     ,RowConstraintList
>     ,VarDefList
>     ,ExpressionStatementListPair
>     ,MExpression
>     ,CaseExpressionListExpressionPair
>     ,CaseExpressionList
>      -- * type aliases
>      -- | aliases for all the sql types with multiple names
>      -- these give you the canonical names
>     ,typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4
>     ,typeFloat8,typeVarChar,typeChar,typeBool
>     ) where

> import Database.HsSqlPpp.TypeChecking.AstInternal
> import Database.HsSqlPpp.TypeChecking.TypeType

