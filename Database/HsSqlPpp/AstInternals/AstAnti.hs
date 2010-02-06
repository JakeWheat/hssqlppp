{-# LANGUAGE DeriveDataTypeable #-}
module Database.HsSqlPpp.AstInternals.AstAnti where
import Data.Generics
import Database.HsSqlPpp.AstInternals.AstAnnotation
import qualified Database.HsSqlPpp.AstInternals.AstInternal as A
 
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
 
statement :: Statement -> A.Statement
statement x
  = case x of
        AlterSequence a1 a2 a3 -> undefined
        AlterTable a1 a2 a3 -> undefined
        Assignment a1 a2 a3 -> undefined
        CaseStatement a1 a2 a3 a4 -> undefined
        ContinueStatement a1 -> undefined
        Copy a1 a2 a3 a4 -> undefined
        CopyData a1 a2 -> undefined
        CreateDomain a1 a2 a3 a4 a5 -> undefined
        CreateFunction a1 a2 a3 a4 a5 a6 a7 a8 -> undefined
        CreateLanguage a1 a2 -> undefined
        CreateSequence a1 a2 a3 a4 a5 a6 a7 -> undefined
        CreateTable a1 a2 a3 a4 -> undefined
        CreateTableAs a1 a2 a3 -> undefined
        CreateTrigger a1 a2 a3 a4 a5 a6 a7 a8 -> undefined
        CreateType a1 a2 a3 -> undefined
        CreateView a1 a2 a3 -> undefined
        Delete a1 a2 a3 a4 -> undefined
        DropFunction a1 a2 a3 a4 -> undefined
        DropSomething a1 a2 a3 a4 a5 -> undefined
        Execute a1 a2 -> undefined
        ExecuteInto a1 a2 a3 -> undefined
        ForIntegerStatement a1 a2 a3 a4 a5 -> undefined
        ForSelectStatement a1 a2 a3 a4 -> undefined
        If a1 a2 a3 -> undefined
        Insert a1 a2 a3 a4 a5 -> undefined
        Notify a1 a2 -> undefined
        NullStatement a1 -> undefined
        Perform a1 a2 -> undefined
        Raise a1 a2 a3 a4 -> undefined
        Return a1 a2 -> undefined
        ReturnNext a1 a2 -> undefined
        ReturnQuery a1 a2 -> undefined
        SelectStatement a1 a2 -> undefined
        Set a1 a2 a3 -> undefined
        Truncate a1 a2 a3 a4 -> undefined
        Update a1 a2 a3 a4 a5 -> undefined
        WhileStatement a1 a2 a3 -> undefined
 
selectExpression :: SelectExpression -> A.SelectExpression
selectExpression x
  = case x of
        CombineSelect a1 a2 a3 a4 -> undefined
        Select a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> undefined
        Values a1 a2 -> undefined
 
fnBody :: FnBody -> A.FnBody
fnBody x
  = case x of
        PlpgsqlFnBody a1 a2 a3 -> undefined
        SqlFnBody a1 a2 -> undefined
 
setClause :: SetClause -> A.SetClause
setClause x
  = case x of
        RowSetClause a1 a2 a3 -> undefined
        SetClause a1 a2 a3 -> undefined
 
tableRef :: TableRef -> A.TableRef
tableRef x
  = case x of
        JoinedTref a1 a2 a3 a4 a5 a6 a7 -> undefined
        SubTref a1 a2 a3 -> undefined
        Tref a1 a2 a3 -> undefined
        TrefFun a1 a2 a3 -> undefined
 
tableAlias :: TableAlias -> A.TableAlias
tableAlias x
  = case x of
        FullAlias a1 a2 -> undefined
        NoAlias -> undefined
        TableAlias a1 -> undefined
 
joinExpression :: JoinExpression -> A.JoinExpression
joinExpression x
  = case x of
        JoinOn a1 a2 -> undefined
        JoinUsing a1 a2 -> undefined
 
joinType :: JoinType -> A.JoinType
joinType x
  = case x of
        Cross -> undefined
        FullOuter -> undefined
        Inner -> undefined
        LeftOuter -> undefined
        RightOuter -> undefined
 
selectList :: SelectList -> A.SelectList
selectList x
  = case x of
        SelectList a1 a2 a3 -> undefined
 
selectItem :: SelectItem -> A.SelectItem
selectItem x
  = case x of
        SelExp a1 a2 -> undefined
        SelectItem a1 a2 a3 -> undefined
 
copySource :: CopySource -> A.CopySource
copySource x
  = case x of
        CopyFilename a1 -> undefined
        Stdin -> undefined
 
attributeDef :: AttributeDef -> A.AttributeDef
attributeDef x
  = case x of
        AttributeDef a1 a2 a3 a4 a5 -> undefined
 
rowConstraint :: RowConstraint -> A.RowConstraint
rowConstraint x
  = case x of
        NotNullConstraint a1 a2 -> undefined
        NullConstraint a1 a2 -> undefined
        RowCheckConstraint a1 a2 a3 -> undefined
        RowPrimaryKeyConstraint a1 a2 -> undefined
        RowReferenceConstraint a1 a2 a3 a4 a5 a6 -> undefined
        RowUniqueConstraint a1 a2 -> undefined
 
alterTableAction :: AlterTableAction -> A.AlterTableAction
alterTableAction x
  = case x of
        AddConstraint a1 a2 -> undefined
        AlterColumnDefault a1 a2 a3 -> undefined
 
constraint :: Constraint -> A.Constraint
constraint x
  = case x of
        CheckConstraint a1 a2 a3 -> undefined
        PrimaryKeyConstraint a1 a2 a3 -> undefined
        ReferenceConstraint a1 a2 a3 a4 a5 a6 a7 -> undefined
        UniqueConstraint a1 a2 a3 -> undefined
 
typeAttributeDef :: TypeAttributeDef -> A.TypeAttributeDef
typeAttributeDef x
  = case x of
        TypeAttDef a1 a2 a3 -> undefined
 
paramDef :: ParamDef -> A.ParamDef
paramDef x
  = case x of
        ParamDef a1 a2 a3 -> undefined
        ParamDefTp a1 a2 -> undefined
 
varDef :: VarDef -> A.VarDef
varDef x
  = case x of
        VarDef a1 a2 a3 a4 -> undefined
 
raiseType :: RaiseType -> A.RaiseType
raiseType x
  = case x of
        RError -> undefined
        RException -> undefined
        RNotice -> undefined
 
combineType :: CombineType -> A.CombineType
combineType x
  = case x of
        Except -> undefined
        Intersect -> undefined
        Union -> undefined
        UnionAll -> undefined
 
volatility :: Volatility -> A.Volatility
volatility x
  = case x of
        Immutable -> undefined
        Stable -> undefined
        Volatile -> undefined
 
language :: Language -> A.Language
language x
  = case x of
        Plpgsql -> undefined
        Sql -> undefined
 
typeName :: TypeName -> A.TypeName
typeName x
  = case x of
        ArrayTypeName a1 a2 -> undefined
        PrecTypeName a1 a2 a3 -> undefined
        SetOfTypeName a1 a2 -> undefined
        SimpleTypeName a1 a2 -> undefined
 
dropType :: DropType -> A.DropType
dropType x
  = case x of
        Domain -> undefined
        Table -> undefined
        Type -> undefined
        View -> undefined
 
cascade :: Cascade -> A.Cascade
cascade x
  = case x of
        Cascade -> undefined
        Restrict -> undefined
 
direction :: Direction -> A.Direction
direction x
  = case x of
        Asc -> undefined
        Desc -> undefined
 
distinct :: Distinct -> A.Distinct
distinct x
  = case x of
        Distinct -> undefined
        Dupes -> undefined
 
natural :: Natural -> A.Natural
natural x
  = case x of
        Natural -> undefined
        Unnatural -> undefined
 
ifExists :: IfExists -> A.IfExists
ifExists x
  = case x of
        IfExists -> undefined
        Require -> undefined
 
restartIdentity :: RestartIdentity -> A.RestartIdentity
restartIdentity x
  = case x of
        ContinueIdentity -> undefined
        RestartIdentity -> undefined
 
expression :: Expression -> A.Expression
expression x
  = case x of
        AntiExpression a1 -> undefined
        BooleanLit a1 a2 -> undefined
        Case a1 a2 a3 -> undefined
        CaseSimple a1 a2 a3 a4 -> undefined
        Cast a1 a2 a3 -> undefined
        Exists a1 a2 -> undefined
        FloatLit a1 a2 -> undefined
        FunCall a1 a2 a3 -> undefined
        Identifier a1 a2 -> undefined
        InPredicate a1 a2 a3 a4 -> undefined
        IntegerLit a1 a2 -> undefined
        LiftOperator a1 a2 a3 a4 -> undefined
        NullLit a1 -> undefined
        Placeholder a1 -> undefined
        PositionalArg a1 a2 -> undefined
        ScalarSubQuery a1 a2 -> undefined
        StringLit a1 a2 a3 -> undefined
        WindowFn a1 a2 a3 a4 a5 a6 -> undefined
 
frameClause :: FrameClause -> A.FrameClause
frameClause x
  = case x of
        FrameRowsUnboundedPreceding -> undefined
        FrameUnboundedFull -> undefined
        FrameUnboundedPreceding -> undefined
 
inList :: InList -> A.InList
inList x
  = case x of
        InList a1 a2 -> undefined
        InSelect a1 a2 -> undefined
 
liftFlavour :: LiftFlavour -> A.LiftFlavour
liftFlavour x
  = case x of
        LiftAll -> undefined
        LiftAny -> undefined
 
triggerWhen :: TriggerWhen -> A.TriggerWhen
triggerWhen x
  = case x of
        TriggerAfter -> undefined
        TriggerBefore -> undefined
 
triggerEvent :: TriggerEvent -> A.TriggerEvent
triggerEvent x
  = case x of
        TDelete -> undefined
        TInsert -> undefined
        TUpdate -> undefined
 
triggerFire :: TriggerFire -> A.TriggerFire
triggerFire x
  = case x of
        EachRow -> undefined
        EachStatement -> undefined
 
setValue :: SetValue -> A.SetValue
setValue x
  = case x of
        SetId a1 a2 -> undefined
        SetNum a1 a2 -> undefined
        SetStr a1 a2 -> undefined
 
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