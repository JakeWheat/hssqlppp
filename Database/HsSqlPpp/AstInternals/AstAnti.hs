{-# LANGUAGE DeriveDataTypeable #-}
module Database.HsSqlPpp.AstInternals.AstAnti
       (convertStatements, convertExpression, Statement(..),
        SelectExpression(..), FnBody(..), SetClause(..), TableRef(..),
        TableAlias(..), JoinExpression(..), JoinType(..), SelectList(..),
        SelectItem(..), CopySource(..), AttributeDef(..),
        RowConstraint(..), AlterTableAction(..), Constraint(..),
        TypeAttributeDef(..), ParamDef(..), VarDef(..), RaiseType(..),
        CombineType(..), Volatility(..), Language(..), TypeName(..),
        DropType(..), Cascade(..), Direction(..), Distinct(..),
        Natural(..), IfExists(..), RestartIdentity(..), Expression(..),
        FrameClause(..), InList(..), LiftFlavour(..), TriggerWhen(..),
        TriggerEvent(..), TriggerFire(..), SetValue(..), StatementList,
        ExpressionListStatementListPairList,
        ExpressionListStatementListPair, ExpressionList, StringList,
        ParamDefList, AttributeDefList, ConstraintList,
        TypeAttributeDefList, TypeNameList, StringTypeNameListPair,
        StringTypeNameListPairList, ExpressionStatementListPairList,
        SetClauseList, CaseExpressionListExpressionPairList,
        MaybeExpression, TableRefList, ExpressionListList, SelectItemList,
        OnExpr, RowConstraintList, VarDefList, ExpressionStatementListPair,
        CaseExpressionListExpressionPair, CaseExpressionList,
        ExpressionDirectionPair, ExpressionDirectionPairList,
        MaybeBoolExpression, MaybeSelectList, AlterTableActionList,
        TriggerEventList, SetValueList)
       where
import Data.Generics
import Database.HsSqlPpp.AstInternals.AstAnnotation
import qualified Database.HsSqlPpp.AstInternals.AstInternal as A
 
convertStatements :: [Statement] -> [A.Statement]
convertStatements = statementList
 
convertExpression :: Expression -> A.Expression
convertExpression = expression
 
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
 
data Expression = BooleanLit (Annotation) (Bool)
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
                | AntiExpression String
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
               | AlterTable (Annotation) (String) (AlterTableActionList)
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
                               (TriggerEventList) (String) (TriggerFire) (String) (ExpressionList)
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
               | Set (Annotation) (String) (SetValueList)
               | Truncate (Annotation) (StringList) (RestartIdentity) (Cascade)
               | Update (Annotation) (String) (SetClauseList)
                        (MaybeBoolExpression) (MaybeSelectList)
               | WhileStatement (Annotation) (Expression) (StatementList)
               deriving (Data, Eq, Show, Typeable)
 
data TableAlias = FullAlias (String) (StringList)
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
                  | AntiTriggerEvent String
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
 
type AlterTableActionList = [(AlterTableAction)]
 
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
 
type SetValueList = [(SetValue)]
 
type StatementList = [(Statement)]
 
type StringList = [(String)]
 
type StringTypeNameListPair = ((String), (TypeNameList))
 
type StringTypeNameListPairList = [(StringTypeNameListPair)]
 
type TableRefList = [(TableRef)]
 
type TriggerEventList = [(TriggerEvent)]
 
type TypeAttributeDefList = [(TypeAttributeDef)]
 
type TypeNameList = [(TypeName)]
 
type VarDefList = [(VarDef)]
 
statement :: Statement -> A.Statement
statement x
  = case x of
        AlterSequence a1 a2 a3 -> A.AlterSequence a1 a2 a3
        AlterTable a1 a2 a3 -> A.AlterTable a1 a2 (alterTableActionList a3)
        Assignment a1 a2 a3 -> A.Assignment a1 a2 (expression a3)
        CaseStatement a1 a2 a3 a4 -> A.CaseStatement a1 (expression a2)
                                       (expressionListStatementListPairList a3)
                                       (statementList a4)
        ContinueStatement a1 -> A.ContinueStatement a1
        Copy a1 a2 a3 a4 -> A.Copy a1 a2 (stringList a3) (copySource a4)
        CopyData a1 a2 -> A.CopyData a1 a2
        CreateDomain a1 a2 a3 a4 a5 -> A.CreateDomain a1 a2 (typeName a3)
                                         a4
                                         (maybeBoolExpression a5)
        CreateFunction a1 a2 a3 a4 a5 a6 a7 a8 -> A.CreateFunction a1 a2
                                                    (paramDefList a3)
                                                    (typeName a4)
                                                    (language a5)
                                                    a6
                                                    (fnBody a7)
                                                    (volatility a8)
        CreateLanguage a1 a2 -> A.CreateLanguage a1 a2
        CreateSequence a1 a2 a3 a4 a5 a6 a7 -> A.CreateSequence a1 a2 a3 a4
                                                 a5
                                                 a6
                                                 a7
        CreateTable a1 a2 a3 a4 -> A.CreateTable a1 a2
                                     (attributeDefList a3)
                                     (constraintList a4)
        CreateTableAs a1 a2 a3 -> A.CreateTableAs a1 a2
                                    (selectExpression a3)
        CreateTrigger a1 a2 a3 a4 a5 a6 a7 a8 -> A.CreateTrigger a1 a2
                                                   (triggerWhen a3)
                                                   (triggerEventList a4)
                                                   a5
                                                   (triggerFire a6)
                                                   a7
                                                   (expressionList a8)
        CreateType a1 a2 a3 -> A.CreateType a1 a2 (typeAttributeDefList a3)
        CreateView a1 a2 a3 -> A.CreateView a1 a2 (selectExpression a3)
        Delete a1 a2 a3 a4 -> A.Delete a1 a2 (maybeBoolExpression a3)
                                (maybeSelectList a4)
        DropFunction a1 a2 a3 a4 -> A.DropFunction a1 (ifExists a2)
                                      (stringTypeNameListPairList a3)
                                      (cascade a4)
        DropSomething a1 a2 a3 a4 a5 -> A.DropSomething a1 (dropType a2)
                                          (ifExists a3)
                                          (stringList a4)
                                          (cascade a5)
        Execute a1 a2 -> A.Execute a1 (expression a2)
        ExecuteInto a1 a2 a3 -> A.ExecuteInto a1 (expression a2)
                                  (stringList a3)
        ForIntegerStatement a1 a2 a3 a4 a5 -> A.ForIntegerStatement a1 a2
                                                (expression a3)
                                                (expression a4)
                                                (statementList a5)
        ForSelectStatement a1 a2 a3 a4 -> A.ForSelectStatement a1 a2
                                            (selectExpression a3)
                                            (statementList a4)
        If a1 a2 a3 -> A.If a1 (expressionStatementListPairList a2)
                         (statementList a3)
        Insert a1 a2 a3 a4 a5 -> A.Insert a1 a2 (stringList a3)
                                   (selectExpression a4)
                                   (maybeSelectList a5)
        Notify a1 a2 -> A.Notify a1 a2
        NullStatement a1 -> A.NullStatement a1
        Perform a1 a2 -> A.Perform a1 (expression a2)
        Raise a1 a2 a3 a4 -> A.Raise a1 (raiseType a2) a3
                               (expressionList a4)
        Return a1 a2 -> A.Return a1 (maybeExpression a2)
        ReturnNext a1 a2 -> A.ReturnNext a1 (expression a2)
        ReturnQuery a1 a2 -> A.ReturnQuery a1 (selectExpression a2)
        SelectStatement a1 a2 -> A.SelectStatement a1 (selectExpression a2)
        Set a1 a2 a3 -> A.Set a1 a2 (setValueList a3)
        Truncate a1 a2 a3 a4 -> A.Truncate a1 (stringList a2)
                                  (restartIdentity a3)
                                  (cascade a4)
        Update a1 a2 a3 a4 a5 -> A.Update a1 a2 (setClauseList a3)
                                   (maybeBoolExpression a4)
                                   (maybeSelectList a5)
        WhileStatement a1 a2 a3 -> A.WhileStatement a1 (expression a2)
                                     (statementList a3)
 
selectExpression :: SelectExpression -> A.SelectExpression
selectExpression x
  = case x of
        CombineSelect a1 a2 a3 a4 -> A.CombineSelect a1 (combineType a2)
                                       (selectExpression a3)
                                       (selectExpression a4)
        Select a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> A.Select a1 (distinct a2)
                                                   (selectList a3)
                                                   (tableRefList a4)
                                                   (maybeBoolExpression a5)
                                                   (expressionList a6)
                                                   (maybeBoolExpression a7)
                                                   (expressionDirectionPairList a8)
                                                   (maybeExpression a9)
                                                   (maybeExpression a10)
        Values a1 a2 -> A.Values a1 (expressionListList a2)
 
fnBody :: FnBody -> A.FnBody
fnBody x
  = case x of
        PlpgsqlFnBody a1 a2 a3 -> A.PlpgsqlFnBody a1 (varDefList a2)
                                    (statementList a3)
        SqlFnBody a1 a2 -> A.SqlFnBody a1 (statementList a2)
 
setClause :: SetClause -> A.SetClause
setClause x
  = case x of
        RowSetClause a1 a2 a3 -> A.RowSetClause a1 (stringList a2)
                                   (expressionList a3)
        SetClause a1 a2 a3 -> A.SetClause a1 a2 (expression a3)
 
tableRef :: TableRef -> A.TableRef
tableRef x
  = case x of
        JoinedTref a1 a2 a3 a4 a5 a6 a7 -> A.JoinedTref a1 (tableRef a2)
                                             (natural a3)
                                             (joinType a4)
                                             (tableRef a5)
                                             (onExpr a6)
                                             (tableAlias a7)
        SubTref a1 a2 a3 -> A.SubTref a1 (selectExpression a2)
                              (tableAlias a3)
        Tref a1 a2 a3 -> A.Tref a1 a2 (tableAlias a3)
        TrefFun a1 a2 a3 -> A.TrefFun a1 (expression a2) (tableAlias a3)
 
tableAlias :: TableAlias -> A.TableAlias
tableAlias x
  = case x of
        FullAlias a1 a2 -> A.FullAlias a1 (stringList a2)
        NoAlias -> A.NoAlias
        TableAlias a1 -> A.TableAlias a1
 
joinExpression :: JoinExpression -> A.JoinExpression
joinExpression x
  = case x of
        JoinOn a1 a2 -> A.JoinOn a1 (expression a2)
        JoinUsing a1 a2 -> A.JoinUsing a1 (stringList a2)
 
joinType :: JoinType -> A.JoinType
joinType x
  = case x of
        Cross -> A.Cross
        FullOuter -> A.FullOuter
        Inner -> A.Inner
        LeftOuter -> A.LeftOuter
        RightOuter -> A.RightOuter
 
selectList :: SelectList -> A.SelectList
selectList x
  = case x of
        SelectList a1 a2 a3 -> A.SelectList a1 (selectItemList a2)
                                 (stringList a3)
 
selectItem :: SelectItem -> A.SelectItem
selectItem x
  = case x of
        SelExp a1 a2 -> A.SelExp a1 (expression a2)
        SelectItem a1 a2 a3 -> A.SelectItem a1 (expression a2) a3
 
copySource :: CopySource -> A.CopySource
copySource x
  = case x of
        CopyFilename a1 -> A.CopyFilename a1
        Stdin -> A.Stdin
 
attributeDef :: AttributeDef -> A.AttributeDef
attributeDef x
  = case x of
        AttributeDef a1 a2 a3 a4 a5 -> A.AttributeDef a1 a2 (typeName a3)
                                         (maybeExpression a4)
                                         (rowConstraintList a5)
 
rowConstraint :: RowConstraint -> A.RowConstraint
rowConstraint x
  = case x of
        NotNullConstraint a1 a2 -> A.NotNullConstraint a1 a2
        NullConstraint a1 a2 -> A.NullConstraint a1 a2
        RowCheckConstraint a1 a2 a3 -> A.RowCheckConstraint a1 a2
                                         (expression a3)
        RowPrimaryKeyConstraint a1 a2 -> A.RowPrimaryKeyConstraint a1 a2
        RowReferenceConstraint a1 a2 a3 a4 a5
          a6 -> A.RowReferenceConstraint a1 a2 a3 a4 (cascade a5)
                  (cascade a6)
        RowUniqueConstraint a1 a2 -> A.RowUniqueConstraint a1 a2
 
alterTableAction :: AlterTableAction -> A.AlterTableAction
alterTableAction x
  = case x of
        AddConstraint a1 a2 -> A.AddConstraint a1 (constraint a2)
        AlterColumnDefault a1 a2 a3 -> A.AlterColumnDefault a1 a2
                                         (expression a3)
 
constraint :: Constraint -> A.Constraint
constraint x
  = case x of
        CheckConstraint a1 a2 a3 -> A.CheckConstraint a1 a2 (expression a3)
        PrimaryKeyConstraint a1 a2 a3 -> A.PrimaryKeyConstraint a1 a2
                                           (stringList a3)
        ReferenceConstraint a1 a2 a3 a4 a5 a6 a7 -> A.ReferenceConstraint
                                                      a1
                                                      a2
                                                      (stringList a3)
                                                      a4
                                                      (stringList a5)
                                                      (cascade a6)
                                                      (cascade a7)
        UniqueConstraint a1 a2 a3 -> A.UniqueConstraint a1 a2
                                       (stringList a3)
 
typeAttributeDef :: TypeAttributeDef -> A.TypeAttributeDef
typeAttributeDef x
  = case x of
        TypeAttDef a1 a2 a3 -> A.TypeAttDef a1 a2 (typeName a3)
 
paramDef :: ParamDef -> A.ParamDef
paramDef x
  = case x of
        ParamDef a1 a2 a3 -> A.ParamDef a1 a2 (typeName a3)
        ParamDefTp a1 a2 -> A.ParamDefTp a1 (typeName a2)
 
varDef :: VarDef -> A.VarDef
varDef x
  = case x of
        VarDef a1 a2 a3 a4 -> A.VarDef a1 a2 (typeName a3)
                                (maybeExpression a4)
 
raiseType :: RaiseType -> A.RaiseType
raiseType x
  = case x of
        RError -> A.RError
        RException -> A.RException
        RNotice -> A.RNotice
 
combineType :: CombineType -> A.CombineType
combineType x
  = case x of
        Except -> A.Except
        Intersect -> A.Intersect
        Union -> A.Union
        UnionAll -> A.UnionAll
 
volatility :: Volatility -> A.Volatility
volatility x
  = case x of
        Immutable -> A.Immutable
        Stable -> A.Stable
        Volatile -> A.Volatile
 
language :: Language -> A.Language
language x
  = case x of
        Plpgsql -> A.Plpgsql
        Sql -> A.Sql
 
typeName :: TypeName -> A.TypeName
typeName x
  = case x of
        ArrayTypeName a1 a2 -> A.ArrayTypeName a1 (typeName a2)
        PrecTypeName a1 a2 a3 -> A.PrecTypeName a1 a2 a3
        SetOfTypeName a1 a2 -> A.SetOfTypeName a1 (typeName a2)
        SimpleTypeName a1 a2 -> A.SimpleTypeName a1 a2
 
dropType :: DropType -> A.DropType
dropType x
  = case x of
        Domain -> A.Domain
        Table -> A.Table
        Type -> A.Type
        View -> A.View
 
cascade :: Cascade -> A.Cascade
cascade x
  = case x of
        Cascade -> A.Cascade
        Restrict -> A.Restrict
 
direction :: Direction -> A.Direction
direction x
  = case x of
        Asc -> A.Asc
        Desc -> A.Desc
 
distinct :: Distinct -> A.Distinct
distinct x
  = case x of
        Distinct -> A.Distinct
        Dupes -> A.Dupes
 
natural :: Natural -> A.Natural
natural x
  = case x of
        Natural -> A.Natural
        Unnatural -> A.Unnatural
 
ifExists :: IfExists -> A.IfExists
ifExists x
  = case x of
        IfExists -> A.IfExists
        Require -> A.Require
 
restartIdentity :: RestartIdentity -> A.RestartIdentity
restartIdentity x
  = case x of
        ContinueIdentity -> A.ContinueIdentity
        RestartIdentity -> A.RestartIdentity
 
expression :: Expression -> A.Expression
expression x
  = case x of
        BooleanLit a1 a2 -> A.BooleanLit a1 a2
        Case a1 a2 a3 -> A.Case a1
                           (caseExpressionListExpressionPairList a2)
                           (maybeExpression a3)
        CaseSimple a1 a2 a3 a4 -> A.CaseSimple a1 (expression a2)
                                    (caseExpressionListExpressionPairList a3)
                                    (maybeExpression a4)
        Cast a1 a2 a3 -> A.Cast a1 (expression a2) (typeName a3)
        Exists a1 a2 -> A.Exists a1 (selectExpression a2)
        FloatLit a1 a2 -> A.FloatLit a1 a2
        FunCall a1 a2 a3 -> A.FunCall a1 a2 (expressionList a3)
        Identifier a1 a2 -> A.Identifier a1 a2
        InPredicate a1 a2 a3 a4 -> A.InPredicate a1 (expression a2) a3
                                     (inList a4)
        IntegerLit a1 a2 -> A.IntegerLit a1 a2
        LiftOperator a1 a2 a3 a4 -> A.LiftOperator a1 a2 (liftFlavour a3)
                                      (expressionList a4)
        NullLit a1 -> A.NullLit a1
        Placeholder a1 -> A.Placeholder a1
        PositionalArg a1 a2 -> A.PositionalArg a1 a2
        ScalarSubQuery a1 a2 -> A.ScalarSubQuery a1 (selectExpression a2)
        StringLit a1 a2 a3 -> A.StringLit a1 a2 a3
        WindowFn a1 a2 a3 a4 a5 a6 -> A.WindowFn a1 (expression a2)
                                        (expressionList a3)
                                        (expressionList a4)
                                        (direction a5)
                                        (frameClause a6)
        AntiExpression s -> error "can't convert anti expression"
 
frameClause :: FrameClause -> A.FrameClause
frameClause x
  = case x of
        FrameRowsUnboundedPreceding -> A.FrameRowsUnboundedPreceding
        FrameUnboundedFull -> A.FrameUnboundedFull
        FrameUnboundedPreceding -> A.FrameUnboundedPreceding
 
inList :: InList -> A.InList
inList x
  = case x of
        InList a1 a2 -> A.InList a1 (expressionList a2)
        InSelect a1 a2 -> A.InSelect a1 (selectExpression a2)
 
liftFlavour :: LiftFlavour -> A.LiftFlavour
liftFlavour x
  = case x of
        LiftAll -> A.LiftAll
        LiftAny -> A.LiftAny
 
triggerWhen :: TriggerWhen -> A.TriggerWhen
triggerWhen x
  = case x of
        TriggerAfter -> A.TriggerAfter
        TriggerBefore -> A.TriggerBefore
 
triggerEvent :: TriggerEvent -> A.TriggerEvent
triggerEvent x
  = case x of
        TDelete -> A.TDelete
        TInsert -> A.TInsert
        TUpdate -> A.TUpdate
        AntiTriggerEvent s -> error "can't convert anti triggerEvent"
 
triggerFire :: TriggerFire -> A.TriggerFire
triggerFire x
  = case x of
        EachRow -> A.EachRow
        EachStatement -> A.EachStatement
 
setValue :: SetValue -> A.SetValue
setValue x
  = case x of
        SetId a1 a2 -> A.SetId a1 a2
        SetNum a1 a2 -> A.SetNum a1 a2
        SetStr a1 a2 -> A.SetStr a1 a2
 
statementList :: StatementList -> A.StatementList
statementList = map statement
 
expressionListStatementListPairList ::
                                    ExpressionListStatementListPairList ->
                                      A.ExpressionListStatementListPairList
expressionListStatementListPairList
  = map expressionListStatementListPair
 
expressionListStatementListPair ::
                                ExpressionListStatementListPair ->
                                  A.ExpressionListStatementListPair
expressionListStatementListPair (a, b)
  = (expressionList a, statementList b)
 
expressionList :: ExpressionList -> A.ExpressionList
expressionList = map expression
 
stringList :: StringList -> A.StringList
stringList = id
 
paramDefList :: ParamDefList -> A.ParamDefList
paramDefList = map paramDef
 
attributeDefList :: AttributeDefList -> A.AttributeDefList
attributeDefList = map attributeDef
 
constraintList :: ConstraintList -> A.ConstraintList
constraintList = map constraint
 
typeAttributeDefList ::
                     TypeAttributeDefList -> A.TypeAttributeDefList
typeAttributeDefList = map typeAttributeDef
 
typeNameList :: TypeNameList -> A.TypeNameList
typeNameList = map typeName
 
stringTypeNameListPair ::
                       StringTypeNameListPair -> A.StringTypeNameListPair
stringTypeNameListPair (a, b) = (a, typeNameList b)
 
stringTypeNameListPairList ::
                           StringTypeNameListPairList -> A.StringTypeNameListPairList
stringTypeNameListPairList = map stringTypeNameListPair
 
expressionStatementListPairList ::
                                ExpressionStatementListPairList ->
                                  A.ExpressionStatementListPairList
expressionStatementListPairList = map expressionStatementListPair
 
setClauseList :: SetClauseList -> A.SetClauseList
setClauseList = map setClause
 
caseExpressionListExpressionPairList ::
                                     CaseExpressionListExpressionPairList ->
                                       A.CaseExpressionListExpressionPairList
caseExpressionListExpressionPairList
  = map caseExpressionListExpressionPair
 
maybeExpression :: MaybeExpression -> A.MaybeExpression
maybeExpression = fmap expression
 
tableRefList :: TableRefList -> A.TableRefList
tableRefList = map tableRef
 
expressionListList :: ExpressionListList -> A.ExpressionListList
expressionListList = map expressionList
 
selectItemList :: SelectItemList -> A.SelectItemList
selectItemList = map selectItem
 
onExpr :: OnExpr -> A.OnExpr
onExpr = fmap joinExpression
 
rowConstraintList :: RowConstraintList -> A.RowConstraintList
rowConstraintList = map rowConstraint
 
varDefList :: VarDefList -> A.VarDefList
varDefList = map varDef
 
expressionStatementListPair ::
                            ExpressionStatementListPair -> A.ExpressionStatementListPair
expressionStatementListPair (a, b)
  = (expression a, statementList b)
 
caseExpressionListExpressionPair ::
                                 CaseExpressionListExpressionPair ->
                                   A.CaseExpressionListExpressionPair
caseExpressionListExpressionPair (a, b)
  = (caseExpressionList a, expression b)
 
caseExpressionList :: CaseExpressionList -> A.CaseExpressionList
caseExpressionList = map expression
 
expressionDirectionPair ::
                        ExpressionDirectionPair -> A.ExpressionDirectionPair
expressionDirectionPair (a, b) = (expression a, direction b)
 
expressionDirectionPairList ::
                            ExpressionDirectionPairList -> A.ExpressionDirectionPairList
expressionDirectionPairList = map expressionDirectionPair
 
maybeBoolExpression :: MaybeBoolExpression -> A.MaybeBoolExpression
maybeBoolExpression = fmap expression
 
maybeSelectList :: MaybeSelectList -> A.MaybeSelectList
maybeSelectList = fmap selectList
 
alterTableActionList ::
                     AlterTableActionList -> A.AlterTableActionList
alterTableActionList = map alterTableAction
 
triggerEventList :: TriggerEventList -> A.TriggerEventList
triggerEventList = map triggerEvent
 
setValueList :: SetValueList -> A.SetValueList
setValueList = map setValue