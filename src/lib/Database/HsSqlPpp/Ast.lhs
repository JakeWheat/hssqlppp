
> {- | This module contains the ast node data types. They are very
>      permissive, in that they allow a lot of invalid SQL to be
>      represented. The type checking process should catch all invalid
>      trees, but doesn't quite manage at the moment.  Sorry about all
>      the seemingly pointless type synonyms below, they are an
>      artefact of using UUAGC. You can see labels for the fields by
>      looking at the ag source here:
>      <http://jakewheat.github.com/hssqlppp/source/src/lib/Database/HsSqlPpp/AstInternals/AstInternal.ag.html>
>      -}
>
> module Database.HsSqlPpp.Ast
>     (
>      -- * Main nodes
>      StatementList
>     ,Statement (..)
>     ,ScalarExpr (..)
>     ,QueryExpr (..)
>      -- * Components
>      -- ** Selects
>     ,SelectList (..)
>     ,SelectItem (..)
>     ,TableRef (..)
>     ,TableAlias(..)
>     ,JoinExpr (..)
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
>     ,SetValue(..)
>     -- ,Name(..)
>      -- ** typedefs
>     ,ScalarExprListStatementListPairList
>     ,ScalarExprListStatementListPair
>     ,ScalarExprList
>     ,MaybeSelectList
>     --,StringList
>     ,ParamDefList
>     ,AttributeDefList
>     ,ConstraintList
>     ,TypeAttributeDefList
>     ,TypeNameList
>     ,StringTypeNameListPair
>     ,StringTypeNameListPairList
>     ,ScalarExprStatementListPairList
>     ,CaseScalarExprListScalarExprPairList
>     ,MaybeScalarExpr
>     ,MaybeBoolExpr
>     ,TableRefList
>     ,ScalarExprListList
>     ,SelectItemList
>     ,OnExpr
>     ,RowConstraintList
>     ,VarDefList
>     ,ScalarExprStatementListPair
>     ,CaseScalarExprListScalarExprPair
>     ,ScalarExprDirectionPair
>     ,ScalarExprDirectionPairList
>     ,AlterTableActionList
>     ) where
>
> import Database.HsSqlPpp.AstInternals.AstInternal
> --import Database.HsSqlPpp.AstInternals.Name
