Copyright 2009 Jake Wheat

> {- | This module contains the ast node data types. They are very
>      permissive, in that they allow a lot of invalid SQL to be
>      represented. The type checking process should catch all invalid
>      trees, but doesn't quite manage at the moment.  Sorry about all
>      the seemingly pointless type synonyms below, they are an
>      artefact of using UUAGC. You can see labels for the fields by
>      looking at the ag source here:
>      <http://community.haskell.org/~JakeWheat/hssqlppp/pandoc_source/src/Database/HsSqlPpp/AstInternals/AstInternal.ag.html>
>      -}
>
> module Database.HsSqlPpp.Ast
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
>     ,WithQueryList
>     ,WithQuery(..)
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
>     ,Replace(..)
>     ,Cascade (..)
>     ,TriggerWhen(..)
>     ,TriggerEvent(..)
>     ,TriggerFire(..)
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
>     ,MaybeSelectList
>     --,StringList
>     ,ParamDefList
>     ,AttributeDefList
>     ,ConstraintList
>     ,TypeAttributeDefList
>     ,TypeNameList
>     ,StringTypeNameListPair
>     ,StringTypeNameListPairList
>     ,ExpressionStatementListPairList
>     ,SetClauseList
>     ,CaseExpressionListExpressionPairList
>     ,MaybeExpression
>     ,MaybeBoolExpression
>     ,TableRefList
>     ,ExpressionListList
>     ,SelectItemList
>     ,OnExpr
>     ,RowConstraintList
>     ,VarDefList
>     ,ExpressionStatementListPair
>     ,CaseExpressionListExpressionPair
>     ,CaseExpressionList
>     ,ExpressionDirectionPair
>     ,ExpressionDirectionPairList
>     ,AlterTableActionList
>     ) where
>
> import Database.HsSqlPpp.AstInternals.AstInternal

