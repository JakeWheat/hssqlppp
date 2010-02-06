{-# LANGUAGE DeriveDataTypeable #-}
module Database.HsSqlPpp.AstInternals.AstAnti where
import Data.Generics
import Database.HsSqlPpp.AstInternals.AstAnnotation
 
data AlterTableAction = AddConstraint (Annotation) (Constraint)
                      | AlterColumnDefault (Annotation) (String) (Expression)
                      deriving (Data, Eq, Show, Typeable)
 
data AttributeDef = AttributeDef (Annotation) (String) (TypeName)
                                 (MaybeExpression) (RowConstraintList)
                  deriving (Data, Eq, Show, Typeable)
 
data Cascade = Cascade
             | Restrict
             deriving (Data, Eq, Show, Typeable)
 
data CombineType = Except
                 | Intersect
                 | Union
                 | UnionAll
                 deriving (Data, Eq, Show, Typeable)
 
data Constraint = CheckConstraint (Annotation) (String)
                                  (Expression)
                | PrimaryKeyConstraint (Annotation) (String) (StringList)
                | ReferenceConstraint (Annotation) (String) (StringList) (String)
                                      (StringList) (Cascade) (Cascade)
                | UniqueConstraint (Annotation) (String) (StringList)
                deriving (Data, Eq, Show, Typeable)
 
data CopySource = CopyFilename (String)
                | Stdin
                deriving (Data, Eq, Show, Typeable)
 
data Direction = Asc
               | Desc
               deriving (Data, Eq, Show, Typeable)
 
data Distinct = Distinct
              | Dupes
              deriving (Data, Eq, Show, Typeable)
 
data DropType = Domain
              | Table
              | Type
              | View
              deriving (Data, Eq, Show, Typeable)
 
data Expression = AntiExpression (String)
                | BooleanLit (Annotation) (Bool)
                | Case (Annotation) (CaseExpressionListExpressionPairList)
                       (MaybeExpression)
                | CaseSimple (Annotation) (Expression)
                             (CaseExpressionListExpressionPairList) (MaybeExpression)
                | Cast (Annotation) (Expression) (TypeName)
                | Exists (Annotation) (SelectExpression)
                | FloatLit (Annotation) (Double)
                | FunCall (Annotation) (String) (ExpressionList)
                | Identifier (Annotation) (String)
                | InPredicate (Annotation) (Expression) (Bool) (InList)
                | IntegerLit (Annotation) (Integer)
                | LiftOperator (Annotation) (String) (LiftFlavour) (ExpressionList)
                | NullLit (Annotation)
                | Placeholder (Annotation)
                | PositionalArg (Annotation) (Integer)
                | ScalarSubQuery (Annotation) (SelectExpression)
                | StringLit (Annotation) (String) (String)
                | WindowFn (Annotation) (Expression) (ExpressionList)
                           (ExpressionList) (Direction) (FrameClause)
                deriving (Data, Eq, Show, Typeable)
 
data FnBody = PlpgsqlFnBody (Annotation) (VarDefList)
                            (StatementList)
            | SqlFnBody (Annotation) (StatementList)
            deriving (Data, Eq, Show, Typeable)
 
data FrameClause = FrameRowsUnboundedPreceding
                 | FrameUnboundedFull
                 | FrameUnboundedPreceding
                 deriving (Data, Eq, Show, Typeable)
 
data IfExists = IfExists
              | Require
              deriving (Data, Eq, Show, Typeable)
 
data InList = InList (Annotation) (ExpressionList)
            | InSelect (Annotation) (SelectExpression)
            deriving (Data, Eq, Show, Typeable)
 
data JoinExpression = JoinOn (Annotation) (Expression)
                    | JoinUsing (Annotation) (StringList)
                    deriving (Data, Eq, Show, Typeable)
 
data JoinType = Cross
              | FullOuter
              | Inner
              | LeftOuter
              | RightOuter
              deriving (Data, Eq, Show, Typeable)
 
data Language = Plpgsql
              | Sql
              deriving (Data, Eq, Show, Typeable)
 
data LiftFlavour = LiftAll
                 | LiftAny
                 deriving (Data, Eq, Show, Typeable)
 
data Natural = Natural
             | Unnatural
             deriving (Data, Eq, Show, Typeable)
 
data ParamDef = ParamDef (Annotation) (String) (TypeName)
              | ParamDefTp (Annotation) (TypeName)
              deriving (Data, Eq, Show, Typeable)
 
data RaiseType = RError
               | RException
               | RNotice
               deriving (Data, Eq, Show, Typeable)
 
data RestartIdentity = ContinueIdentity
                     | RestartIdentity
                     deriving (Data, Eq, Show, Typeable)
 
data RowConstraint = NotNullConstraint (Annotation) (String)
                   | NullConstraint (Annotation) (String)
                   | RowCheckConstraint (Annotation) (String) (Expression)
                   | RowPrimaryKeyConstraint (Annotation) (String)
                   | RowReferenceConstraint (Annotation) (String) (String)
                                            (Maybe String) (Cascade) (Cascade)
                   | RowUniqueConstraint (Annotation) (String)
                   deriving (Data, Eq, Show, Typeable)
 
data SelectExpression = CombineSelect (Annotation) (CombineType)
                                      (SelectExpression) (SelectExpression)
                      | Select (Annotation) (Distinct) (SelectList) (TableRefList)
                               (MaybeBoolExpression) (ExpressionList) (MaybeBoolExpression)
                               (ExpressionDirectionPairList) (MaybeExpression) (MaybeExpression)
                      | Values (Annotation) (ExpressionListList)
                      deriving (Data, Eq, Show, Typeable)
 
data SelectItem = SelExp (Annotation) (Expression)
                | SelectItem (Annotation) (Expression) (String)
                deriving (Data, Eq, Show, Typeable)
 
data SelectList = SelectList (Annotation) (SelectItemList)
                             (StringList)
                deriving (Data, Eq, Show, Typeable)
 
data SetClause = RowSetClause (Annotation) (StringList)
                              (ExpressionList)
               | SetClause (Annotation) (String) (Expression)
               deriving (Data, Eq, Show, Typeable)
 
data SetValue = SetId (Annotation) (String)
              | SetNum (Annotation) (Double)
              | SetStr (Annotation) (String)
              deriving (Data, Eq, Show, Typeable)
 
data Statement = AlterSequence (Annotation) (String) (String)
               | AlterTable (Annotation) (String) ([AlterTableAction])
               | Assignment (Annotation) (String) (Expression)
               | CaseStatement (Annotation) (Expression)
                               (ExpressionListStatementListPairList) (StatementList)
               | ContinueStatement (Annotation)
               | Copy (Annotation) (String) (StringList) (CopySource)
               | CopyData (Annotation) (String)
               | CreateDomain (Annotation) (String) (TypeName) (String)
                              (MaybeBoolExpression)
               | CreateFunction (Annotation) (String) (ParamDefList) (TypeName)
                                (Language) (String) (FnBody) (Volatility)
               | CreateLanguage (Annotation) (String)
               | CreateSequence (Annotation) (String) (Integer) (Integer)
                                (Integer) (Integer) (Integer)
               | CreateTable (Annotation) (String) (AttributeDefList)
                             (ConstraintList)
               | CreateTableAs (Annotation) (String) (SelectExpression)
               | CreateTrigger (Annotation) (String) (TriggerWhen)
                               ([TriggerEvent]) (String) (TriggerFire) (String) ([Expression])
               | CreateType (Annotation) (String) (TypeAttributeDefList)
               | CreateView (Annotation) (String) (SelectExpression)
               | Delete (Annotation) (String) (MaybeBoolExpression)
                        (MaybeSelectList)
               | DropFunction (Annotation) (IfExists) (StringTypeNameListPairList)
                              (Cascade)
               | DropSomething (Annotation) (DropType) (IfExists) (StringList)
                               (Cascade)
               | Execute (Annotation) (Expression)
               | ExecuteInto (Annotation) (Expression) (StringList)
               | ForIntegerStatement (Annotation) (String) (Expression)
                                     (Expression) (StatementList)
               | ForSelectStatement (Annotation) (String) (SelectExpression)
                                    (StatementList)
               | If (Annotation) (ExpressionStatementListPairList) (StatementList)
               | Insert (Annotation) (String) (StringList) (SelectExpression)
                        (MaybeSelectList)
               | Notify (Annotation) (String)
               | NullStatement (Annotation)
               | Perform (Annotation) (Expression)
               | Raise (Annotation) (RaiseType) (String) (ExpressionList)
               | Return (Annotation) (MaybeExpression)
               | ReturnNext (Annotation) (Expression)
               | ReturnQuery (Annotation) (SelectExpression)
               | SelectStatement (Annotation) (SelectExpression)
               | Set (Annotation) (String) ([SetValue])
               | Truncate (Annotation) (StringList) (RestartIdentity) (Cascade)
               | Update (Annotation) (String) (SetClauseList)
                        (MaybeBoolExpression) (MaybeSelectList)
               | WhileStatement (Annotation) (Expression) (StatementList)
               deriving (Data, Eq, Show, Typeable)
 
data TableAlias = FullAlias (String) ([String])
                | NoAlias
                | TableAlias (String)
                deriving (Data, Eq, Show, Typeable)
 
data TableRef = JoinedTref (Annotation) (TableRef) (Natural)
                           (JoinType) (TableRef) (OnExpr) (TableAlias)
              | SubTref (Annotation) (SelectExpression) (TableAlias)
              | Tref (Annotation) (String) (TableAlias)
              | TrefFun (Annotation) (Expression) (TableAlias)
              deriving (Data, Eq, Show, Typeable)
 
data TriggerEvent = TDelete
                  | TInsert
                  | TUpdate
                  deriving (Data, Eq, Show, Typeable)
 
data TriggerFire = EachRow
                 | EachStatement
                 deriving (Data, Eq, Show, Typeable)
 
data TriggerWhen = TriggerAfter
                 | TriggerBefore
                 deriving (Data, Eq, Show, Typeable)
 
data TypeAttributeDef = TypeAttDef (Annotation) (String) (TypeName)
                      deriving (Data, Eq, Show, Typeable)
 
data TypeName = ArrayTypeName (Annotation) (TypeName)
              | PrecTypeName (Annotation) (String) (Integer)
              | SetOfTypeName (Annotation) (TypeName)
              | SimpleTypeName (Annotation) (String)
              deriving (Data, Eq, Show, Typeable)
 
data VarDef = VarDef (Annotation) (String) (TypeName)
                     (Maybe Expression)
            deriving (Data, Eq, Show, Typeable)
 
data Volatility = Immutable
                | Stable
                | Volatile
                deriving (Data, Eq, Show, Typeable)
 
type AttributeDefList = [(AttributeDef)]
 
type CaseExpressionList = [(Expression)]
 
type CaseExpressionListExpressionPair =
     ((CaseExpressionList), (Expression))
 
type CaseExpressionListExpressionPairList =
     [(CaseExpressionListExpressionPair)]
 
type ConstraintList = [(Constraint)]
 
type ExpressionDirectionPair = ((Expression), (Direction))
 
type ExpressionDirectionPairList = [(ExpressionDirectionPair)]
 
type ExpressionList = [(Expression)]
 
type ExpressionListList = [(ExpressionList)]
 
type ExpressionListStatementListPair =
     ((ExpressionList), (StatementList))
 
type ExpressionListStatementListPairList =
     [(ExpressionListStatementListPair)]
 
type ExpressionStatementListPair = ((Expression), (StatementList))
 
type ExpressionStatementListPairList =
     [(ExpressionStatementListPair)]
 
type MaybeBoolExpression = (Maybe (Expression))
 
type MaybeExpression = (Maybe (Expression))
 
type MaybeSelectList = (Maybe (SelectList))
 
type OnExpr = (Maybe (JoinExpression))
 
type ParamDefList = [(ParamDef)]
 
type RowConstraintList = [(RowConstraint)]
 
type SelectItemList = [(SelectItem)]
 
type SetClauseList = [(SetClause)]
 
type StatementList = [(Statement)]
 
type StringList = [(String)]
 
type StringTypeNameListPair = ((String), (TypeNameList))
 
type StringTypeNameListPairList = [(StringTypeNameListPair)]
 
type TableRefList = [(TableRef)]
 
type TypeAttributeDefList = [(TypeAttributeDef)]
 
type TypeNameList = [(TypeName)]
 
type VarDefList = [(VarDef)]