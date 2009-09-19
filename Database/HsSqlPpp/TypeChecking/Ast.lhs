Copyright 2009 Jake Wheat

This is the public module for the ast nodes.

> {- | This module contains the ast node data types.
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
>      -- * operator utils
>     ,OperatorType(..)
>     ,getOperatorType
>      -- * type aliases
>      -- | aliases for all the sql types with multiple names
>      -- these give you the canonical names
>     ,typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4
>     ,typeFloat8,typeVarChar,typeChar,typeBool
>     ) where

> import Database.HsSqlPpp.TypeChecking.AstInternal

