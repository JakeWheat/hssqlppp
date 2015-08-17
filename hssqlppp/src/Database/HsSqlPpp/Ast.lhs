
> {- | This module contains the ast node data types. They are very
>      permissive, in that they allow a lot of invalid SQL to be
>      represented. The type checking process should catch all invalid
>      trees, but doesn't quite manage at the moment.  Sorry about all
>      the seemingly pointless type synonyms below, they are an
>      artefact of using UUAGC. You can see labels for the fields by
>      looking at the ag source here:
>      <https://github.com/JakeWheat/hssqlppp/blob/master/src/Database/HsSqlPpp/Internals/AstInternal.ag>
>      -}
>
> module Database.HsSqlPpp.Ast
>     (
>      -- * Name and TypeName
>      Name(..)
>     ,NameComponent(..)
>     ,nameComponents
>     ,ncStr
>     ,TypeName(..)
>      -- * Scalar expressions
>     ,ScalarExpr(..)
>     ,InList(..)
>     ,LiftFlavour(..)
>     ,Direction(..)
>     ,NullsOrder(..)
>     ,Distinct(..)
>     ,CombineType(..)
>     ,IntervalField(..)
>     ,ExtractField(..)
>     ,FrameClause(..)
>      -- * Query expressions
>     ,QueryExpr(..)
>     ,makeSelect
>     ,WithQuery(..)
>     ,SelectList(..)
>     ,SelectItem(..)
>     ,TableRef(..)
>     ,JoinExpr(..)
>     ,JoinType(..)
>     ,JoinHint(..)
>     ,QueryHint(..)
>     ,OnExpr
>     ,Natural(..)
>      -- * Statements
>     ,Statement(..)
>      -- ** dml components
>     ,CopyToSource(..)
>     ,CopyFromSource(..)
>     ,CopyOption(..)
>     ,SetClause(..)
>      -- ** ddl components
>     ,AttributeDef(..)
>     ,RowConstraint(..)
>     ,Constraint(..)
>     ,TablePartitionDef(..)
>     ,TablePartitionDateTimeInterval(..)
>     ,TypeAttributeDef(..)
>     ,AlterDatabaseOperation(..)
>     ,AlterTableOperation(..)
>     ,AlterTableAction(..)
>     ,AlterColumnAction(..)
>     ,TriggerWhen(..)
>     ,TriggerEvent(..)
>     ,TriggerFire(..)
>     ,DropType(..)
>     ,Cascade(..)
>     ,IfExists(..)
>     ,RestartIdentity(..)
>      -- *** function ddl components
>     ,Replace(..)
>     ,Volatility(..)
>     ,Language(..)
>     ,FnBody(..)
>      -- ** PlPgsql components
>     ,ParamDef(..)
>     ,VarDef(..)
>     ,RaiseType(..)
>      -- * utility
>     ,SetValue(..)
>      -- * misc
>     ,WithQueryList
>     ,MaybeSelectList
>     ,TableRefList
>     ,MaybeScalarExpr
>     ,ScalarExprList
>     ,ScalarExprListList
>     ,SetClauseList
>     ,AttributeDefList
>     ,ConstraintList
>     ,TypeAttributeDefList
>     ,ParamDefList
>     ,TypeNameList
>     ,NameTypeNameListPair
>     ,NameTypeNameListPairList
>     ,ScalarExprListStatementListPair
>     ,ScalarExprListStatementListPairList
>     ,ScalarExprStatementListPair
>     ,ScalarExprStatementListPairList
>     ,VarDefList
>     ,SelectItemList
>     ,RowConstraintList
>     ,CaseScalarExprListScalarExprPair
>     ,CaseScalarExprListScalarExprPairList
>     ,StatementList
>     ,ScalarExprDirectionPair
>     ,ScalarExprDirectionPairList
>     ,AlterTableActionList
>     ,MaybeNameComponentList
>     ,NameComponentList
>     ,MaybeBoolExpr
>     ) where
>
> import Database.HsSqlPpp.Internals.AstInternal
> --import Database.HsSqlPpp.Internals.Name
