Copyright 2009 Jake Wheat

This is the public module for the ast nodes.

> {- | This module contains the ast node data types. They are very
>      permissive, in that they allow a lot of invalid SQL to be
>      represented. The type checking process should catch
>      all invalid trees, but doesn't quite manage at the moment. -}
> module Database.HsSqlPpp.Ast.Ast
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
>     ,TableAlias(..)
>     ,JoinExpression (..)
>     ,JoinType (..)
>     ,Natural (..)
>     ,CombineType (..)
>     ,Direction (..)
>     ,Distinct (..)
>     ,InList (..)
>     ,LiftFlavour (..)
>     ,FrameClause(..)
>      -- ** dml
>     ,SetClause (..)
>     ,CopySource (..)
>     ,RestartIdentity (..)
>      -- ** ddl
>     ,AttributeDef (..)
>     ,RowConstraint (..)
>     ,Constraint (..)
>     ,AlterTableAction(..)
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
>      -- ** misc
>      ,SetValue(..)
>      -- ** typedefs
>     ,ExpressionListStatementListPairList
>     ,ExpressionListStatementListPair
>     ,ExpressionList
>     ,StringList
>     ,ParamDefList
>     ,AttributeDefList
>     ,ConstraintList
>     ,TypeAttributeDefList
>     ,StringStringListPairList
>     ,StringStringListPair
>     ,ExpressionStatementListPairList
>     ,SetClauseList
>     ,CaseExpressionListExpressionPairList
>     ,MaybeExpression
>     ,MaybeBoolExpression
>     ,MaybeTableRef
>     ,ExpressionListList
>     ,SelectItemList
>     ,OnExpr
>     ,RowConstraintList
>     ,VarDefList
>     ,ExpressionStatementListPair
>     ,CaseExpressionListExpressionPair
>     ,CaseExpressionList
>     ) where

> import Database.HsSqlPpp.AstInternals.AstInternal

