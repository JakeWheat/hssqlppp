

-- UUAGC 0.9.10 (Ast.ag)
module Database.HsSqlPpp.Ast(
    --from the ag files:
    --ast nodes
    Statement (..)
   ,SelectExpression (..)
   ,FnBody (..)
   ,SetClause (..)
   ,TableRef (..)
   ,JoinExpression (..)
   ,JoinType (..)
   ,SelectList (..)
   ,SelectItem (..)
   ,CopySource (..)
   ,AttributeDef (..)
   ,RowConstraint (..)
   ,Constraint (..)
   ,TypeAttributeDef (..)
   ,ParamDef (..)
   ,VarDef (..)
   ,RaiseType (..)
   ,CombineType (..)
   ,Volatility (..)
   ,Language (..)
   ,TypeName (..)
   ,DropType (..)
   ,Cascade (..)
   ,Direction (..)
   ,Distinct (..)
   ,Natural (..)
   ,IfExists (..)
   ,RestartIdentity (..)
   ,Expression (..)
   ,InList (..)
   ,StatementList

   -- annotations
   ,annotateAst
   ,annotateAstScope
   ,annotateExpression
   ,getTopLevelTypes
   ,getTypeErrors
   ,getTopLevelInfos
   --annotation utils
   ,wipeAnnotations

   --AstAnnotationForwards
   ,Annotation
   ,AnnotationElement(..)

   --astutils forwards
   ,OperatorType(..)
   ,getOperatorType
   ,typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4
   ,typeFloat8,typeVarChar,typeChar,typeBool
   ,Message(..)
   ,MessageStuff(..)
   --typetype
   ,Type (..)
   ,PseudoType (..)
   ,TypeError (..)
   --,StatementInfo (..)
) where

import Data.Maybe
import Data.List
import Debug.Trace
import Control.Monad.Error
import Control.Arrow

import Database.HsSqlPpp.TypeType
import Database.HsSqlPpp.AstUtils
import Database.HsSqlPpp.TypeConversion
import Database.HsSqlPpp.TypeCheckingH
import Database.HsSqlPpp.Scope
import Database.HsSqlPpp.ScopeData
import Database.HsSqlPpp.AstAnnotation




{-
checkAst :: StatementList -> [Message]
checkAst sts = let t = sem_Root (Root sts)
               in (messages_Syn_Root (wrap_Root t Inh_Root {scope_Inh_Root = defaultScope}))
-}

annotateAst :: StatementList -> StatementList
annotateAst = annotateAstScope defaultScope

annotateAstScope :: Scope -> StatementList -> StatementList
annotateAstScope scope sts = undefined
{-    let t = sem_Root (Root sts)
        ta = wrap_Root t Inh_Root {scope_Inh_Root = combineScopes defaultScope scope}
        tl = annotatedTree_Syn_Root ta
    in tl-}

annotateExpression :: Scope -> Expression -> Expression
annotateExpression scope ex = undefined
{-    let t = sem_ExpressionRoot (ExpressionRoot ex)
    in (nodeType_Syn_ExpressionRoot
        (wrap_ExpressionRoot t Inh_ExpressionRoot {scope_Inh_ExpressionRoot = combineScopes defaultScope scope}))
-}

getTopLevelTypes :: Annotated a => [a] -> [Type]
getTopLevelTypes sts = undefined {-map (\st -> getTypeAnnotation $ ann st) sts-}

getTypeAnnotation :: Annotation -> Type
getTypeAnnotation (x:xs) = undefined {-case x of
                             TypeAnnotation t -> t
                             _ -> getTypeAnnotation xs -}
getTypeAnnotation _ = error "couldn't find type annotation"

getTypeErrors :: Annotated a => [a] -> [TypeError]
getTypeErrors sts = undefined {-
    concatMap (\st -> collectAnn (\a -> gte a) st) sts
    where
      gte (a:as) = case a of
                    TypeErrorA e -> [e]
                    _ -> gte as
      gte _ = []-}

data StatementInfo = StatementInfo

getTopLevelInfos :: Annotated a => [a] -> [StatementInfo]
getTopLevelInfos sts = undefined {-map (\st -> getTypeAnnotation $ ann st) sts-}


{-
================================================================================

= Types

These are the utility functions which clients use to typecheck sql.

-}
{-
getExpressionType :: Scope -> Expression -> Type
getExpressionType scope ex =
    let t = sem_ExpressionRoot (ExpressionRoot ex)
    in (nodeType_Syn_ExpressionRoot
        (wrap_ExpressionRoot t Inh_ExpressionRoot {scope_Inh_ExpressionRoot = combineScopes defaultScope scope}))
-}
{-
getStatementsType :: StatementList -> [Type]
getStatementsType = getStatementsTypeScope emptyScope

getStatementsTypeScope :: Scope -> StatementList -> [Type]
getStatementsTypeScope scope st =
.    let t = sem_Root (Root st)
        ta = wrap_Root t Inh_Root {scope_Inh_Root = combineScopes defaultScope scope}
        tl = nodeType_Syn_Root ta
    in (unwrapTypeList tl)

getStatementsInfo :: StatementList -> [StatementInfo]
getStatementsInfo = getStatementsInfoScope emptyScope

getStatementsInfoScope :: Scope -> StatementList -> [StatementInfo]
getStatementsInfoScope scope st =
    let t = sem_Root (Root st)
        ta = wrap_Root t Inh_Root {scope_Inh_Root = combineScopes defaultScope scope}
        t2 = statementInfo_Syn_Root ta
    in t2
-}

instance Annotated Statement where
  ann a =
      case a of
        SelectStatement ann _ -> ann
        Insert ann _ _ _ _ -> ann
        Update ann _ _ _ _ -> ann
        Delete ann _ _ _ -> ann
        Copy ann _ _ _ -> ann
        CopyData ann _ -> ann
        Truncate ann _ _ _ -> ann
        CreateTable ann _ _ _ -> ann
        CreateTableAs ann _ _ -> ann
        CreateView ann _ _ -> ann
        CreateType ann _ _ -> ann
        CreateFunction ann _ _ _ _ _ _ _ -> ann
        CreateDomain ann _ _ _ -> ann
        DropFunction ann _ _ _ -> ann
        DropSomething ann _ _ _ _ -> ann
        Assignment ann _ _ -> ann
        Return ann _ -> ann
        ReturnNext ann _ -> ann
        ReturnQuery ann _ -> ann
        Raise ann _ _ _ -> ann
        NullStatement  ann -> ann
        Perform ann _ -> ann
        Execute ann _ -> ann
        ExecuteInto ann _ _ -> ann
        ForSelectStatement ann _ _ _ -> ann
        ForIntegerStatement ann _ _ _ _ -> ann
        WhileStatement ann _ _ -> ann
        ContinueStatement ann -> ann
        CaseStatement ann _ _ _ -> ann
        If ann _ _  -> ann
  setAnn st a =
      case st of
        SelectStatement _ ex -> SelectStatement a ex
        Insert _ tbl cols ins ret -> Insert a tbl cols ins ret
        Update _ tbl as whr ret -> Update a tbl as whr ret
        Delete _ tbl whr ret -> Delete a tbl whr ret
        Copy _ tbl cols src -> Copy a tbl cols src
        CopyData _ i -> CopyData a i
        Truncate _ tbls ri cs -> Truncate a tbls ri cs
        CreateTable _ name atts cons -> CreateTable a name atts cons
        CreateTableAs _ name ex -> CreateTableAs a name ex
        CreateView _ name expr -> CreateView a name expr
        CreateType _ name atts -> CreateType a name atts
        CreateFunction _ lang name params rettype bodyQuote body vol ->
            CreateFunction a lang name params rettype bodyQuote body vol
        CreateDomain _ name typ check -> CreateDomain a name typ check
        DropFunction _ i s cs -> DropFunction a i s cs
        DropSomething _ dt i nms cs -> DropSomething a dt i nms cs
        Assignment _ tgt val -> Assignment a tgt val
        Return _ v -> Return a v
        ReturnNext _ ex -> ReturnNext a ex
        ReturnQuery _ sel -> ReturnQuery a sel
        Raise _ l m args -> Raise a l m args
        NullStatement _ -> NullStatement a
        Perform _ expr -> Perform a expr
        Execute _ expr -> Execute a expr
        ExecuteInto _ expr tgts -> ExecuteInto a expr tgts
        ForSelectStatement _ var sel sts -> ForSelectStatement a var sel sts
        ForIntegerStatement _ var from to sts -> ForIntegerStatement a var from to sts
        WhileStatement _ expr sts -> WhileStatement a expr sts
        ContinueStatement _ -> ContinueStatement a
        CaseStatement _ val cases els -> CaseStatement a val cases els
        If _ cases els -> If a cases els

  changeAnnRecurse f st =
    case st of
        SelectStatement a ex -> SelectStatement (f a) ex
        Insert a tbl cols ins ret -> Insert (f a) tbl cols ins ret
        Update a tbl as whr ret -> Update (f a) tbl as whr ret
        Delete a tbl whr ret -> Delete (f a) tbl whr ret
        Copy a tbl cols src -> Copy (f a) tbl cols src
        CopyData a i -> CopyData (f a) i
        Truncate a tbls ri cs -> Truncate (f a) tbls ri cs
        CreateTable a name atts cons -> CreateTable (f a) name atts cons
        CreateTableAs a name ex -> CreateTableAs (f a) name ex
        CreateView a name expr -> CreateView (f a) name expr
        CreateType a name atts -> CreateType (f a) name atts
        CreateFunction a lang name params rettype bodyQuote body vol ->
            CreateFunction (f a) lang name params rettype bodyQuote doBody vol
            where
              doBody = case body of
                         SqlFnBody sts -> SqlFnBody $ cars f sts
                         PlpgsqlFnBody vars sts -> PlpgsqlFnBody vars $ cars f sts
        CreateDomain a name typ check -> CreateDomain (f a) name typ check
        DropFunction a i s cs -> DropFunction (f a) i s cs
        DropSomething a dt i nms cs -> DropSomething (f a) dt i nms cs
        Assignment a tgt val -> Assignment (f a) tgt val
        Return a v -> Return (f a) v
        ReturnNext a ex -> ReturnNext (f a) ex
        ReturnQuery a sel -> ReturnQuery (f a) sel
        Raise a l m args -> Raise (f a) l m args
        NullStatement a -> NullStatement (f a)
        Perform a expr -> Perform (f a) expr
        Execute a expr -> Execute (f a) expr
        ExecuteInto a expr tgts -> ExecuteInto (f a) expr tgts
        ForSelectStatement a var sel sts -> ForSelectStatement (f a) var sel $ cars f sts
        ForIntegerStatement a var from to sts -> ForIntegerStatement (f a) var from to $ cars f sts
        WhileStatement a expr sts -> WhileStatement (f a) expr $ cars f sts
        ContinueStatement a -> ContinueStatement (f a)
        CaseStatement a val cases els -> CaseStatement (f a) val doCases $ cars f els
            where
              doCases = map (\(ex,sts) -> (ex,cars f sts)) cases
        If a cases els -> If (f a) doCases $ cars f els
            where
              doCases = map (\(ex,sts) -> (ex,cars f sts)) cases

cars f sts = map (changeAnnRecurse f) sts

instance Annotated Expression where
  ann a =
      case a of
        IntegerLit ann _ -> ann
        FloatLit ann _ -> ann
        StringLit ann _ _ -> ann
        NullLit ann -> ann
        BooleanLit ann _ -> ann
        PositionalArg ann _ -> ann
        Cast ann _ _ -> ann
        Identifier ann _ -> ann
        Case ann _ _ -> ann
        CaseSimple ann _ _ _ -> ann
        Exists ann _ -> ann
        FunCall ann _ _ -> ann
        InPredicate ann _ _ _ -> ann
        WindowFn ann _ _ _ _ -> ann
        ScalarSubQuery ann _ -> ann
  setAnn st a =
    case st of
      IntegerLit _ i -> IntegerLit a i
      FloatLit _ d -> FloatLit a d
      StringLit _ q v -> StringLit a q v
      NullLit _ -> NullLit a
      BooleanLit _ b -> BooleanLit a b
      PositionalArg _ p -> PositionalArg a p
      Cast _ expr tn -> Cast a expr tn
      Identifier _ i -> Identifier a i
      Case _ cases els -> Case a cases els
      CaseSimple _ val cases els -> CaseSimple a val cases els
      Exists _ sel -> Exists a sel
      FunCall _ funName args -> FunCall a funName args
      InPredicate _ expr i list -> InPredicate a expr i list
      WindowFn _ fn par ord dir -> WindowFn a fn par ord dir
      ScalarSubQuery _ sel -> ScalarSubQuery a sel

  changeAnnRecurse f st =
    case st of
      IntegerLit a i -> IntegerLit (f a) i
      FloatLit a d -> FloatLit (f a) d
      StringLit a q v -> StringLit (f a) q v
      NullLit a -> NullLit a
      BooleanLit a b -> BooleanLit (f a) b
      PositionalArg a p -> PositionalArg (f a) p
      Cast a expr tn -> Cast (f a) (changeAnnRecurse f expr) tn
      Identifier a i -> Identifier (f a) i
      Case a cases els -> Case (f a) cases els
      CaseSimple a val cases els -> CaseSimple (f a) val cases els
      Exists a sel -> Exists (f a) sel
      FunCall a funName args -> FunCall (f a) funName args
      InPredicate a expr i list -> InPredicate (f a) expr i list
      WindowFn a fn par ord dir -> WindowFn (f a) fn par ord dir
      ScalarSubQuery a sel -> ScalarSubQuery (f a) sel


-- hack job, often not interested in the source positions when testing
-- the asts produced, so this function will reset all the source
-- positions to empty ("", 0, 0) so we can compare them for equality, etc.
-- without having to get the positions correct.

wipeAnnotations :: Annotated a => a -> a
wipeAnnotations a = changeAnnRecurse (const []) a

-- AttributeDef ------------------------------------------------
data AttributeDef  = AttributeDef (String) (TypeName) (Maybe Expression) (RowConstraintList) 
                   deriving ( Eq,Show)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _name _typ _check _cons )  =
    (sem_AttributeDef_AttributeDef _name (sem_TypeName _typ ) _check (sem_RowConstraintList _cons ) )
-- semantic domain
type T_AttributeDef  = Scope ->
                       ( )
data Inh_AttributeDef  = Inh_AttributeDef {scope_Inh_AttributeDef :: Scope}
data Syn_AttributeDef  = Syn_AttributeDef {}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_AttributeDef ))
sem_AttributeDef_AttributeDef :: String ->
                                 T_TypeName  ->
                                 (Maybe Expression) ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef name_ typ_ check_ cons_  =
    (\ _lhsIscope ->
         (let _typOscope :: Scope
              _consOscope :: Scope
              _typOscope =
                  _lhsIscope
              _consOscope =
                  _lhsIscope
          in  ( )))
-- AttributeDefList --------------------------------------------
type AttributeDefList  = [(AttributeDef)]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = Scope ->
                           ( )
data Inh_AttributeDefList  = Inh_AttributeDefList {scope_Inh_AttributeDefList :: Scope}
data Syn_AttributeDefList  = Syn_AttributeDefList {}
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_AttributeDefList ))
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Cascade -----------------------------------------------------
data Cascade  = Cascade 
              | Restrict 
              deriving ( Eq,Show)
-- cata
sem_Cascade :: Cascade  ->
               T_Cascade 
sem_Cascade (Cascade )  =
    (sem_Cascade_Cascade )
sem_Cascade (Restrict )  =
    (sem_Cascade_Restrict )
-- semantic domain
type T_Cascade  = Scope ->
                  ( )
data Inh_Cascade  = Inh_Cascade {scope_Inh_Cascade :: Scope}
data Syn_Cascade  = Syn_Cascade {}
wrap_Cascade :: T_Cascade  ->
                Inh_Cascade  ->
                Syn_Cascade 
wrap_Cascade sem (Inh_Cascade _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_Cascade ))
sem_Cascade_Cascade :: T_Cascade 
sem_Cascade_Cascade  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Cascade_Restrict :: T_Cascade 
sem_Cascade_Restrict  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- CaseExpressionList ------------------------------------------
type CaseExpressionList  = [(Expression)]
-- cata
sem_CaseExpressionList :: CaseExpressionList  ->
                          T_CaseExpressionList 
sem_CaseExpressionList list  =
    (Prelude.foldr sem_CaseExpressionList_Cons sem_CaseExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_CaseExpressionList  = Scope ->
                             ( )
data Inh_CaseExpressionList  = Inh_CaseExpressionList {scope_Inh_CaseExpressionList :: Scope}
data Syn_CaseExpressionList  = Syn_CaseExpressionList {}
wrap_CaseExpressionList :: T_CaseExpressionList  ->
                           Inh_CaseExpressionList  ->
                           Syn_CaseExpressionList 
wrap_CaseExpressionList sem (Inh_CaseExpressionList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_CaseExpressionList ))
sem_CaseExpressionList_Cons :: T_Expression  ->
                               T_CaseExpressionList  ->
                               T_CaseExpressionList 
sem_CaseExpressionList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdInodeType :: Type
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
              ( _hdInodeType) =
                  (hd_ _hdOscope )
          in  ( )))
sem_CaseExpressionList_Nil :: T_CaseExpressionList 
sem_CaseExpressionList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- CaseExpressionListExpressionPair ----------------------------
type CaseExpressionListExpressionPair  = ( (CaseExpressionList),(Expression))
-- cata
sem_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair  ->
                                        T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair ( x1,x2)  =
    (sem_CaseExpressionListExpressionPair_Tuple (sem_CaseExpressionList x1 ) (sem_Expression x2 ) )
-- semantic domain
type T_CaseExpressionListExpressionPair  = Scope ->
                                           ( )
data Inh_CaseExpressionListExpressionPair  = Inh_CaseExpressionListExpressionPair {scope_Inh_CaseExpressionListExpressionPair :: Scope}
data Syn_CaseExpressionListExpressionPair  = Syn_CaseExpressionListExpressionPair {}
wrap_CaseExpressionListExpressionPair :: T_CaseExpressionListExpressionPair  ->
                                         Inh_CaseExpressionListExpressionPair  ->
                                         Syn_CaseExpressionListExpressionPair 
wrap_CaseExpressionListExpressionPair sem (Inh_CaseExpressionListExpressionPair _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_CaseExpressionListExpressionPair ))
sem_CaseExpressionListExpressionPair_Tuple :: T_CaseExpressionList  ->
                                              T_Expression  ->
                                              T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair_Tuple x1_ x2_  =
    (\ _lhsIscope ->
         (let _x1Oscope :: Scope
              _x2Oscope :: Scope
              _x2InodeType :: Type
              _x1Oscope =
                  _lhsIscope
              _x2Oscope =
                  _lhsIscope
              ( _x2InodeType) =
                  (x2_ _x2Oscope )
          in  ( )))
-- CaseExpressionListExpressionPairList ------------------------
type CaseExpressionListExpressionPairList  = [(CaseExpressionListExpressionPair)]
-- cata
sem_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList  ->
                                            T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList list  =
    (Prelude.foldr sem_CaseExpressionListExpressionPairList_Cons sem_CaseExpressionListExpressionPairList_Nil (Prelude.map sem_CaseExpressionListExpressionPair list) )
-- semantic domain
type T_CaseExpressionListExpressionPairList  = Scope ->
                                               ( )
data Inh_CaseExpressionListExpressionPairList  = Inh_CaseExpressionListExpressionPairList {scope_Inh_CaseExpressionListExpressionPairList :: Scope}
data Syn_CaseExpressionListExpressionPairList  = Syn_CaseExpressionListExpressionPairList {}
wrap_CaseExpressionListExpressionPairList :: T_CaseExpressionListExpressionPairList  ->
                                             Inh_CaseExpressionListExpressionPairList  ->
                                             Syn_CaseExpressionListExpressionPairList 
wrap_CaseExpressionListExpressionPairList sem (Inh_CaseExpressionListExpressionPairList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_CaseExpressionListExpressionPairList ))
sem_CaseExpressionListExpressionPairList_Cons :: T_CaseExpressionListExpressionPair  ->
                                                 T_CaseExpressionListExpressionPairList  ->
                                                 T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_CaseExpressionListExpressionPairList_Nil :: T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- CombineType -------------------------------------------------
data CombineType  = Except 
                  | Intersect 
                  | Union 
                  | UnionAll 
                  deriving ( Eq,Show)
-- cata
sem_CombineType :: CombineType  ->
                   T_CombineType 
sem_CombineType (Except )  =
    (sem_CombineType_Except )
sem_CombineType (Intersect )  =
    (sem_CombineType_Intersect )
sem_CombineType (Union )  =
    (sem_CombineType_Union )
sem_CombineType (UnionAll )  =
    (sem_CombineType_UnionAll )
-- semantic domain
type T_CombineType  = Scope ->
                      ( )
data Inh_CombineType  = Inh_CombineType {scope_Inh_CombineType :: Scope}
data Syn_CombineType  = Syn_CombineType {}
wrap_CombineType :: T_CombineType  ->
                    Inh_CombineType  ->
                    Syn_CombineType 
wrap_CombineType sem (Inh_CombineType _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_CombineType ))
sem_CombineType_Except :: T_CombineType 
sem_CombineType_Except  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_CombineType_Intersect :: T_CombineType 
sem_CombineType_Intersect  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_CombineType_Union :: T_CombineType 
sem_CombineType_Union  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_CombineType_UnionAll :: T_CombineType 
sem_CombineType_UnionAll  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Constraint --------------------------------------------------
data Constraint  = CheckConstraint (Expression) 
                 | PrimaryKeyConstraint (StringList) 
                 | ReferenceConstraint (StringList) (String) (StringList) (Cascade) (Cascade) 
                 | UniqueConstraint (StringList) 
                 deriving ( Eq,Show)
-- cata
sem_Constraint :: Constraint  ->
                  T_Constraint 
sem_Constraint (CheckConstraint _expression )  =
    (sem_Constraint_CheckConstraint (sem_Expression _expression ) )
sem_Constraint (PrimaryKeyConstraint _stringList )  =
    (sem_Constraint_PrimaryKeyConstraint (sem_StringList _stringList ) )
sem_Constraint (ReferenceConstraint _atts _table _tableAtts _onUpdate _onDelete )  =
    (sem_Constraint_ReferenceConstraint (sem_StringList _atts ) _table (sem_StringList _tableAtts ) (sem_Cascade _onUpdate ) (sem_Cascade _onDelete ) )
sem_Constraint (UniqueConstraint _stringList )  =
    (sem_Constraint_UniqueConstraint (sem_StringList _stringList ) )
-- semantic domain
type T_Constraint  = Scope ->
                     ( )
data Inh_Constraint  = Inh_Constraint {scope_Inh_Constraint :: Scope}
data Syn_Constraint  = Syn_Constraint {}
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_Constraint ))
sem_Constraint_CheckConstraint :: T_Expression  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint expression_  =
    (\ _lhsIscope ->
         (let _expressionOscope :: Scope
              _expressionInodeType :: Type
              _expressionOscope =
                  _lhsIscope
              ( _expressionInodeType) =
                  (expression_ _expressionOscope )
          in  ( )))
sem_Constraint_PrimaryKeyConstraint :: T_StringList  ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint stringList_  =
    (\ _lhsIscope ->
         (let _stringListOscope :: Scope
              _stringListOscope =
                  _lhsIscope
          in  ( )))
sem_Constraint_ReferenceConstraint :: T_StringList  ->
                                      String ->
                                      T_StringList  ->
                                      T_Cascade  ->
                                      T_Cascade  ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIscope ->
         (let _attsOscope :: Scope
              _tableAttsOscope :: Scope
              _onUpdateOscope :: Scope
              _onDeleteOscope :: Scope
              _attsOscope =
                  _lhsIscope
              _tableAttsOscope =
                  _lhsIscope
              _onUpdateOscope =
                  _lhsIscope
              _onDeleteOscope =
                  _lhsIscope
          in  ( )))
sem_Constraint_UniqueConstraint :: T_StringList  ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint stringList_  =
    (\ _lhsIscope ->
         (let _stringListOscope :: Scope
              _stringListOscope =
                  _lhsIscope
          in  ( )))
-- ConstraintList ----------------------------------------------
type ConstraintList  = [(Constraint)]
-- cata
sem_ConstraintList :: ConstraintList  ->
                      T_ConstraintList 
sem_ConstraintList list  =
    (Prelude.foldr sem_ConstraintList_Cons sem_ConstraintList_Nil (Prelude.map sem_Constraint list) )
-- semantic domain
type T_ConstraintList  = Scope ->
                         ( )
data Inh_ConstraintList  = Inh_ConstraintList {scope_Inh_ConstraintList :: Scope}
data Syn_ConstraintList  = Syn_ConstraintList {}
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_ConstraintList ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- CopySource --------------------------------------------------
data CopySource  = CopyFilename (String) 
                 | Stdin 
                 deriving ( Eq,Show)
-- cata
sem_CopySource :: CopySource  ->
                  T_CopySource 
sem_CopySource (CopyFilename _string )  =
    (sem_CopySource_CopyFilename _string )
sem_CopySource (Stdin )  =
    (sem_CopySource_Stdin )
-- semantic domain
type T_CopySource  = Scope ->
                     ( )
data Inh_CopySource  = Inh_CopySource {scope_Inh_CopySource :: Scope}
data Syn_CopySource  = Syn_CopySource {}
wrap_CopySource :: T_CopySource  ->
                   Inh_CopySource  ->
                   Syn_CopySource 
wrap_CopySource sem (Inh_CopySource _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_CopySource ))
sem_CopySource_CopyFilename :: String ->
                               T_CopySource 
sem_CopySource_CopyFilename string_  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_CopySource_Stdin :: T_CopySource 
sem_CopySource_Stdin  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Direction ---------------------------------------------------
data Direction  = Asc 
                | Desc 
                deriving ( Eq,Show)
-- cata
sem_Direction :: Direction  ->
                 T_Direction 
sem_Direction (Asc )  =
    (sem_Direction_Asc )
sem_Direction (Desc )  =
    (sem_Direction_Desc )
-- semantic domain
type T_Direction  = Scope ->
                    ( )
data Inh_Direction  = Inh_Direction {scope_Inh_Direction :: Scope}
data Syn_Direction  = Syn_Direction {}
wrap_Direction :: T_Direction  ->
                  Inh_Direction  ->
                  Syn_Direction 
wrap_Direction sem (Inh_Direction _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_Direction ))
sem_Direction_Asc :: T_Direction 
sem_Direction_Asc  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Direction_Desc :: T_Direction 
sem_Direction_Desc  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Distinct ----------------------------------------------------
data Distinct  = Distinct 
               | Dupes 
               deriving ( Eq,Show)
-- cata
sem_Distinct :: Distinct  ->
                T_Distinct 
sem_Distinct (Distinct )  =
    (sem_Distinct_Distinct )
sem_Distinct (Dupes )  =
    (sem_Distinct_Dupes )
-- semantic domain
type T_Distinct  = Scope ->
                   ( )
data Inh_Distinct  = Inh_Distinct {scope_Inh_Distinct :: Scope}
data Syn_Distinct  = Syn_Distinct {}
wrap_Distinct :: T_Distinct  ->
                 Inh_Distinct  ->
                 Syn_Distinct 
wrap_Distinct sem (Inh_Distinct _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_Distinct ))
sem_Distinct_Distinct :: T_Distinct 
sem_Distinct_Distinct  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Distinct_Dupes :: T_Distinct 
sem_Distinct_Dupes  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- DropType ----------------------------------------------------
data DropType  = Domain 
               | Table 
               | Type 
               | View 
               deriving ( Eq,Show)
-- cata
sem_DropType :: DropType  ->
                T_DropType 
sem_DropType (Domain )  =
    (sem_DropType_Domain )
sem_DropType (Table )  =
    (sem_DropType_Table )
sem_DropType (Type )  =
    (sem_DropType_Type )
sem_DropType (View )  =
    (sem_DropType_View )
-- semantic domain
type T_DropType  = Scope ->
                   ( )
data Inh_DropType  = Inh_DropType {scope_Inh_DropType :: Scope}
data Syn_DropType  = Syn_DropType {}
wrap_DropType :: T_DropType  ->
                 Inh_DropType  ->
                 Syn_DropType 
wrap_DropType sem (Inh_DropType _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_DropType ))
sem_DropType_Domain :: T_DropType 
sem_DropType_Domain  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_DropType_Table :: T_DropType 
sem_DropType_Table  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_DropType_Type :: T_DropType 
sem_DropType_Type  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_DropType_View :: T_DropType 
sem_DropType_View  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Expression --------------------------------------------------
data Expression  = BooleanLit (Annotation) (Bool) 
                 | Case (Annotation) (CaseExpressionListExpressionPairList) (MaybeExpression) 
                 | CaseSimple (Annotation) (Expression) (CaseExpressionListExpressionPairList) (MaybeExpression) 
                 | Cast (Annotation) (Expression) (TypeName) 
                 | Exists (Annotation) (SelectExpression) 
                 | FloatLit (Annotation) (Double) 
                 | FunCall (Annotation) (String) (ExpressionList) 
                 | Identifier (Annotation) (String) 
                 | InPredicate (Annotation) (Expression) (Bool) (InList) 
                 | IntegerLit (Annotation) (Integer) 
                 | NullLit (Annotation) 
                 | PositionalArg (Annotation) (Integer) 
                 | ScalarSubQuery (Annotation) (SelectExpression) 
                 | StringLit (Annotation) (String) (String) 
                 | WindowFn (Annotation) (Expression) (ExpressionList) (ExpressionList) (Direction) 
                 deriving ( Eq,Show)
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (BooleanLit _ann _b )  =
    (sem_Expression_BooleanLit _ann _b )
sem_Expression (Case _ann _cases _els )  =
    (sem_Expression_Case _ann (sem_CaseExpressionListExpressionPairList _cases ) (sem_MaybeExpression _els ) )
sem_Expression (CaseSimple _ann _value _cases _els )  =
    (sem_Expression_CaseSimple _ann (sem_Expression _value ) (sem_CaseExpressionListExpressionPairList _cases ) (sem_MaybeExpression _els ) )
sem_Expression (Cast _ann _expr _tn )  =
    (sem_Expression_Cast _ann (sem_Expression _expr ) (sem_TypeName _tn ) )
sem_Expression (Exists _ann _sel )  =
    (sem_Expression_Exists _ann (sem_SelectExpression _sel ) )
sem_Expression (FloatLit _ann _d )  =
    (sem_Expression_FloatLit _ann _d )
sem_Expression (FunCall _ann _funName _args )  =
    (sem_Expression_FunCall _ann _funName (sem_ExpressionList _args ) )
sem_Expression (Identifier _ann _i )  =
    (sem_Expression_Identifier _ann _i )
sem_Expression (InPredicate _ann _expr _i _list )  =
    (sem_Expression_InPredicate _ann (sem_Expression _expr ) _i (sem_InList _list ) )
sem_Expression (IntegerLit _ann _i )  =
    (sem_Expression_IntegerLit _ann _i )
sem_Expression (NullLit _ann )  =
    (sem_Expression_NullLit _ann )
sem_Expression (PositionalArg _ann _p )  =
    (sem_Expression_PositionalArg _ann _p )
sem_Expression (ScalarSubQuery _ann _sel )  =
    (sem_Expression_ScalarSubQuery _ann (sem_SelectExpression _sel ) )
sem_Expression (StringLit _ann _quote _value )  =
    (sem_Expression_StringLit _ann _quote _value )
sem_Expression (WindowFn _ann _fn _partitionBy _orderBy _dir )  =
    (sem_Expression_WindowFn _ann (sem_Expression _fn ) (sem_ExpressionList _partitionBy ) (sem_ExpressionList _orderBy ) (sem_Direction _dir ) )
-- semantic domain
type T_Expression  = Scope ->
                     ( Type)
data Inh_Expression  = Inh_Expression {scope_Inh_Expression :: Scope}
data Syn_Expression  = Syn_Expression {nodeType_Syn_Expression :: Type}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression sem (Inh_Expression _lhsIscope )  =
    (let ( _lhsOnodeType) =
             (sem _lhsIscope )
     in  (Syn_Expression _lhsOnodeType ))
sem_Expression_BooleanLit :: Annotation ->
                             Bool ->
                             T_Expression 
sem_Expression_BooleanLit ann_ b_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
          in  ( _lhsOnodeType)))
sem_Expression_Case :: Annotation ->
                       T_CaseExpressionListExpressionPairList  ->
                       T_MaybeExpression  ->
                       T_Expression 
sem_Expression_Case ann_ cases_ els_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _casesOscope :: Scope
              _elsOscope :: Scope
              _lhsOnodeType =
                  TypeCheckFailed
              _casesOscope =
                  _lhsIscope
              _elsOscope =
                  _lhsIscope
          in  ( _lhsOnodeType)))
sem_Expression_CaseSimple :: Annotation ->
                             T_Expression  ->
                             T_CaseExpressionListExpressionPairList  ->
                             T_MaybeExpression  ->
                             T_Expression 
sem_Expression_CaseSimple ann_ value_ cases_ els_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _valueOscope :: Scope
              _casesOscope :: Scope
              _elsOscope :: Scope
              _valueInodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
              _valueOscope =
                  _lhsIscope
              _casesOscope =
                  _lhsIscope
              _elsOscope =
                  _lhsIscope
              ( _valueInodeType) =
                  (value_ _valueOscope )
          in  ( _lhsOnodeType)))
sem_Expression_Cast :: Annotation ->
                       T_Expression  ->
                       T_TypeName  ->
                       T_Expression 
sem_Expression_Cast ann_ expr_ tn_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _exprOscope :: Scope
              _tnOscope :: Scope
              _exprInodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
              _exprOscope =
                  _lhsIscope
              _tnOscope =
                  _lhsIscope
              ( _exprInodeType) =
                  (expr_ _exprOscope )
          in  ( _lhsOnodeType)))
sem_Expression_Exists :: Annotation ->
                         T_SelectExpression  ->
                         T_Expression 
sem_Expression_Exists ann_ sel_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _selOscope :: Scope
              _lhsOnodeType =
                  TypeCheckFailed
              _selOscope =
                  _lhsIscope
          in  ( _lhsOnodeType)))
sem_Expression_FloatLit :: Annotation ->
                           Double ->
                           T_Expression 
sem_Expression_FloatLit ann_ d_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
          in  ( _lhsOnodeType)))
sem_Expression_FunCall :: Annotation ->
                          String ->
                          T_ExpressionList  ->
                          T_Expression 
sem_Expression_FunCall ann_ funName_ args_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _argsOscope :: Scope
              _lhsOnodeType =
                  TypeCheckFailed
              _argsOscope =
                  _lhsIscope
          in  ( _lhsOnodeType)))
sem_Expression_Identifier :: Annotation ->
                             String ->
                             T_Expression 
sem_Expression_Identifier ann_ i_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
          in  ( _lhsOnodeType)))
sem_Expression_InPredicate :: Annotation ->
                              T_Expression  ->
                              Bool ->
                              T_InList  ->
                              T_Expression 
sem_Expression_InPredicate ann_ expr_ i_ list_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _exprOscope :: Scope
              _listOscope :: Scope
              _exprInodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
              _exprOscope =
                  _lhsIscope
              _listOscope =
                  _lhsIscope
              ( _exprInodeType) =
                  (expr_ _exprOscope )
          in  ( _lhsOnodeType)))
sem_Expression_IntegerLit :: Annotation ->
                             Integer ->
                             T_Expression 
sem_Expression_IntegerLit ann_ i_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
          in  ( _lhsOnodeType)))
sem_Expression_NullLit :: Annotation ->
                          T_Expression 
sem_Expression_NullLit ann_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
          in  ( _lhsOnodeType)))
sem_Expression_PositionalArg :: Annotation ->
                                Integer ->
                                T_Expression 
sem_Expression_PositionalArg ann_ p_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
          in  ( _lhsOnodeType)))
sem_Expression_ScalarSubQuery :: Annotation ->
                                 T_SelectExpression  ->
                                 T_Expression 
sem_Expression_ScalarSubQuery ann_ sel_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _selOscope :: Scope
              _lhsOnodeType =
                  TypeCheckFailed
              _selOscope =
                  _lhsIscope
          in  ( _lhsOnodeType)))
sem_Expression_StringLit :: Annotation ->
                            String ->
                            String ->
                            T_Expression 
sem_Expression_StringLit ann_ quote_ value_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
          in  ( _lhsOnodeType)))
sem_Expression_WindowFn :: Annotation ->
                           T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList  ->
                           T_Direction  ->
                           T_Expression 
sem_Expression_WindowFn ann_ fn_ partitionBy_ orderBy_ dir_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _fnOscope :: Scope
              _partitionByOscope :: Scope
              _orderByOscope :: Scope
              _dirOscope :: Scope
              _fnInodeType :: Type
              _lhsOnodeType =
                  TypeCheckFailed
              _fnOscope =
                  _lhsIscope
              _partitionByOscope =
                  _lhsIscope
              _orderByOscope =
                  _lhsIscope
              _dirOscope =
                  _lhsIscope
              ( _fnInodeType) =
                  (fn_ _fnOscope )
          in  ( _lhsOnodeType)))
-- ExpressionList ----------------------------------------------
type ExpressionList  = [(Expression)]
-- cata
sem_ExpressionList :: ExpressionList  ->
                      T_ExpressionList 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_ExpressionList  = Scope ->
                         ( )
data Inh_ExpressionList  = Inh_ExpressionList {scope_Inh_ExpressionList :: Scope}
data Syn_ExpressionList  = Syn_ExpressionList {}
wrap_ExpressionList :: T_ExpressionList  ->
                       Inh_ExpressionList  ->
                       Syn_ExpressionList 
wrap_ExpressionList sem (Inh_ExpressionList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_ExpressionList ))
sem_ExpressionList_Cons :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList 
sem_ExpressionList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdInodeType :: Type
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
              ( _hdInodeType) =
                  (hd_ _hdOscope )
          in  ( )))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- ExpressionListList ------------------------------------------
type ExpressionListList  = [(ExpressionList)]
-- cata
sem_ExpressionListList :: ExpressionListList  ->
                          T_ExpressionListList 
sem_ExpressionListList list  =
    (Prelude.foldr sem_ExpressionListList_Cons sem_ExpressionListList_Nil (Prelude.map sem_ExpressionList list) )
-- semantic domain
type T_ExpressionListList  = Scope ->
                             ( )
data Inh_ExpressionListList  = Inh_ExpressionListList {scope_Inh_ExpressionListList :: Scope}
data Syn_ExpressionListList  = Syn_ExpressionListList {}
wrap_ExpressionListList :: T_ExpressionListList  ->
                           Inh_ExpressionListList  ->
                           Syn_ExpressionListList 
wrap_ExpressionListList sem (Inh_ExpressionListList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_ExpressionListList ))
sem_ExpressionListList_Cons :: T_ExpressionList  ->
                               T_ExpressionListList  ->
                               T_ExpressionListList 
sem_ExpressionListList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_ExpressionListList_Nil :: T_ExpressionListList 
sem_ExpressionListList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- ExpressionListStatementListPair -----------------------------
type ExpressionListStatementListPair  = ( (ExpressionList),(StatementList))
-- cata
sem_ExpressionListStatementListPair :: ExpressionListStatementListPair  ->
                                       T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair ( x1,x2)  =
    (sem_ExpressionListStatementListPair_Tuple (sem_ExpressionList x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionListStatementListPair  = Scope ->
                                          ( )
data Inh_ExpressionListStatementListPair  = Inh_ExpressionListStatementListPair {scope_Inh_ExpressionListStatementListPair :: Scope}
data Syn_ExpressionListStatementListPair  = Syn_ExpressionListStatementListPair {}
wrap_ExpressionListStatementListPair :: T_ExpressionListStatementListPair  ->
                                        Inh_ExpressionListStatementListPair  ->
                                        Syn_ExpressionListStatementListPair 
wrap_ExpressionListStatementListPair sem (Inh_ExpressionListStatementListPair _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_ExpressionListStatementListPair ))
sem_ExpressionListStatementListPair_Tuple :: T_ExpressionList  ->
                                             T_StatementList  ->
                                             T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIscope ->
         (let _x1Oscope :: Scope
              _x2Oscope :: Scope
              _x1Oscope =
                  _lhsIscope
              _x2Oscope =
                  _lhsIscope
          in  ( )))
-- ExpressionListStatementListPairList -------------------------
type ExpressionListStatementListPairList  = [(ExpressionListStatementListPair)]
-- cata
sem_ExpressionListStatementListPairList :: ExpressionListStatementListPairList  ->
                                           T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList list  =
    (Prelude.foldr sem_ExpressionListStatementListPairList_Cons sem_ExpressionListStatementListPairList_Nil (Prelude.map sem_ExpressionListStatementListPair list) )
-- semantic domain
type T_ExpressionListStatementListPairList  = Scope ->
                                              ( )
data Inh_ExpressionListStatementListPairList  = Inh_ExpressionListStatementListPairList {scope_Inh_ExpressionListStatementListPairList :: Scope}
data Syn_ExpressionListStatementListPairList  = Syn_ExpressionListStatementListPairList {}
wrap_ExpressionListStatementListPairList :: T_ExpressionListStatementListPairList  ->
                                            Inh_ExpressionListStatementListPairList  ->
                                            Syn_ExpressionListStatementListPairList 
wrap_ExpressionListStatementListPairList sem (Inh_ExpressionListStatementListPairList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_ExpressionListStatementListPairList ))
sem_ExpressionListStatementListPairList_Cons :: T_ExpressionListStatementListPair  ->
                                                T_ExpressionListStatementListPairList  ->
                                                T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_ExpressionListStatementListPairList_Nil :: T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- ExpressionRoot ----------------------------------------------
data ExpressionRoot  = ExpressionRoot (Expression) 
                     deriving ( Show)
-- cata
sem_ExpressionRoot :: ExpressionRoot  ->
                      T_ExpressionRoot 
sem_ExpressionRoot (ExpressionRoot _expr )  =
    (sem_ExpressionRoot_ExpressionRoot (sem_Expression _expr ) )
-- semantic domain
type T_ExpressionRoot  = Scope ->
                         ( Type)
data Inh_ExpressionRoot  = Inh_ExpressionRoot {scope_Inh_ExpressionRoot :: Scope}
data Syn_ExpressionRoot  = Syn_ExpressionRoot {nodeType_Syn_ExpressionRoot :: Type}
wrap_ExpressionRoot :: T_ExpressionRoot  ->
                       Inh_ExpressionRoot  ->
                       Syn_ExpressionRoot 
wrap_ExpressionRoot sem (Inh_ExpressionRoot _lhsIscope )  =
    (let ( _lhsOnodeType) =
             (sem _lhsIscope )
     in  (Syn_ExpressionRoot _lhsOnodeType ))
sem_ExpressionRoot_ExpressionRoot :: T_Expression  ->
                                     T_ExpressionRoot 
sem_ExpressionRoot_ExpressionRoot expr_  =
    (\ _lhsIscope ->
         (let _lhsOnodeType :: Type
              _exprOscope :: Scope
              _exprInodeType :: Type
              _lhsOnodeType =
                  _exprInodeType
              _exprOscope =
                  _lhsIscope
              ( _exprInodeType) =
                  (expr_ _exprOscope )
          in  ( _lhsOnodeType)))
-- ExpressionStatementListPair ---------------------------------
type ExpressionStatementListPair  = ( (Expression),(StatementList))
-- cata
sem_ExpressionStatementListPair :: ExpressionStatementListPair  ->
                                   T_ExpressionStatementListPair 
sem_ExpressionStatementListPair ( x1,x2)  =
    (sem_ExpressionStatementListPair_Tuple (sem_Expression x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionStatementListPair  = Scope ->
                                      ( )
data Inh_ExpressionStatementListPair  = Inh_ExpressionStatementListPair {scope_Inh_ExpressionStatementListPair :: Scope}
data Syn_ExpressionStatementListPair  = Syn_ExpressionStatementListPair {}
wrap_ExpressionStatementListPair :: T_ExpressionStatementListPair  ->
                                    Inh_ExpressionStatementListPair  ->
                                    Syn_ExpressionStatementListPair 
wrap_ExpressionStatementListPair sem (Inh_ExpressionStatementListPair _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_ExpressionStatementListPair ))
sem_ExpressionStatementListPair_Tuple :: T_Expression  ->
                                         T_StatementList  ->
                                         T_ExpressionStatementListPair 
sem_ExpressionStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIscope ->
         (let _x1Oscope :: Scope
              _x2Oscope :: Scope
              _x1InodeType :: Type
              _x1Oscope =
                  _lhsIscope
              _x2Oscope =
                  _lhsIscope
              ( _x1InodeType) =
                  (x1_ _x1Oscope )
          in  ( )))
-- ExpressionStatementListPairList -----------------------------
type ExpressionStatementListPairList  = [(ExpressionStatementListPair)]
-- cata
sem_ExpressionStatementListPairList :: ExpressionStatementListPairList  ->
                                       T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList list  =
    (Prelude.foldr sem_ExpressionStatementListPairList_Cons sem_ExpressionStatementListPairList_Nil (Prelude.map sem_ExpressionStatementListPair list) )
-- semantic domain
type T_ExpressionStatementListPairList  = Scope ->
                                          ( )
data Inh_ExpressionStatementListPairList  = Inh_ExpressionStatementListPairList {scope_Inh_ExpressionStatementListPairList :: Scope}
data Syn_ExpressionStatementListPairList  = Syn_ExpressionStatementListPairList {}
wrap_ExpressionStatementListPairList :: T_ExpressionStatementListPairList  ->
                                        Inh_ExpressionStatementListPairList  ->
                                        Syn_ExpressionStatementListPairList 
wrap_ExpressionStatementListPairList sem (Inh_ExpressionStatementListPairList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_ExpressionStatementListPairList ))
sem_ExpressionStatementListPairList_Cons :: T_ExpressionStatementListPair  ->
                                            T_ExpressionStatementListPairList  ->
                                            T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_ExpressionStatementListPairList_Nil :: T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- FnBody ------------------------------------------------------
data FnBody  = PlpgsqlFnBody (VarDefList) (StatementList) 
             | SqlFnBody (StatementList) 
             deriving ( Eq,Show)
-- cata
sem_FnBody :: FnBody  ->
              T_FnBody 
sem_FnBody (PlpgsqlFnBody _varDefList _sts )  =
    (sem_FnBody_PlpgsqlFnBody (sem_VarDefList _varDefList ) (sem_StatementList _sts ) )
sem_FnBody (SqlFnBody _sts )  =
    (sem_FnBody_SqlFnBody (sem_StatementList _sts ) )
-- semantic domain
type T_FnBody  = Scope ->
                 ( )
data Inh_FnBody  = Inh_FnBody {scope_Inh_FnBody :: Scope}
data Syn_FnBody  = Syn_FnBody {}
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_FnBody ))
sem_FnBody_PlpgsqlFnBody :: T_VarDefList  ->
                            T_StatementList  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody varDefList_ sts_  =
    (\ _lhsIscope ->
         (let _varDefListOscope :: Scope
              _stsOscope :: Scope
              _varDefListOscope =
                  _lhsIscope
              _stsOscope =
                  _lhsIscope
          in  ( )))
sem_FnBody_SqlFnBody :: T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody sts_  =
    (\ _lhsIscope ->
         (let _stsOscope :: Scope
              _stsOscope =
                  _lhsIscope
          in  ( )))
-- IfExists ----------------------------------------------------
data IfExists  = IfExists 
               | Require 
               deriving ( Eq,Show)
-- cata
sem_IfExists :: IfExists  ->
                T_IfExists 
sem_IfExists (IfExists )  =
    (sem_IfExists_IfExists )
sem_IfExists (Require )  =
    (sem_IfExists_Require )
-- semantic domain
type T_IfExists  = Scope ->
                   ( )
data Inh_IfExists  = Inh_IfExists {scope_Inh_IfExists :: Scope}
data Syn_IfExists  = Syn_IfExists {}
wrap_IfExists :: T_IfExists  ->
                 Inh_IfExists  ->
                 Syn_IfExists 
wrap_IfExists sem (Inh_IfExists _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_IfExists ))
sem_IfExists_IfExists :: T_IfExists 
sem_IfExists_IfExists  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_IfExists_Require :: T_IfExists 
sem_IfExists_Require  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- InList ------------------------------------------------------
data InList  = InList (ExpressionList) 
             | InSelect (SelectExpression) 
             deriving ( Eq,Show)
-- cata
sem_InList :: InList  ->
              T_InList 
sem_InList (InList _exprs )  =
    (sem_InList_InList (sem_ExpressionList _exprs ) )
sem_InList (InSelect _sel )  =
    (sem_InList_InSelect (sem_SelectExpression _sel ) )
-- semantic domain
type T_InList  = Scope ->
                 ( )
data Inh_InList  = Inh_InList {scope_Inh_InList :: Scope}
data Syn_InList  = Syn_InList {}
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_InList ))
sem_InList_InList :: T_ExpressionList  ->
                     T_InList 
sem_InList_InList exprs_  =
    (\ _lhsIscope ->
         (let _exprsOscope :: Scope
              _exprsOscope =
                  _lhsIscope
          in  ( )))
sem_InList_InSelect :: T_SelectExpression  ->
                       T_InList 
sem_InList_InSelect sel_  =
    (\ _lhsIscope ->
         (let _selOscope :: Scope
              _selOscope =
                  _lhsIscope
          in  ( )))
-- JoinExpression ----------------------------------------------
data JoinExpression  = JoinOn (Expression) 
                     | JoinUsing (StringList) 
                     deriving ( Eq,Show)
-- cata
sem_JoinExpression :: JoinExpression  ->
                      T_JoinExpression 
sem_JoinExpression (JoinOn _expression )  =
    (sem_JoinExpression_JoinOn (sem_Expression _expression ) )
sem_JoinExpression (JoinUsing _stringList )  =
    (sem_JoinExpression_JoinUsing (sem_StringList _stringList ) )
-- semantic domain
type T_JoinExpression  = Scope ->
                         ( )
data Inh_JoinExpression  = Inh_JoinExpression {scope_Inh_JoinExpression :: Scope}
data Syn_JoinExpression  = Syn_JoinExpression {}
wrap_JoinExpression :: T_JoinExpression  ->
                       Inh_JoinExpression  ->
                       Syn_JoinExpression 
wrap_JoinExpression sem (Inh_JoinExpression _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_JoinExpression ))
sem_JoinExpression_JoinOn :: T_Expression  ->
                             T_JoinExpression 
sem_JoinExpression_JoinOn expression_  =
    (\ _lhsIscope ->
         (let _expressionOscope :: Scope
              _expressionInodeType :: Type
              _expressionOscope =
                  _lhsIscope
              ( _expressionInodeType) =
                  (expression_ _expressionOscope )
          in  ( )))
sem_JoinExpression_JoinUsing :: T_StringList  ->
                                T_JoinExpression 
sem_JoinExpression_JoinUsing stringList_  =
    (\ _lhsIscope ->
         (let _stringListOscope :: Scope
              _stringListOscope =
                  _lhsIscope
          in  ( )))
-- JoinType ----------------------------------------------------
data JoinType  = Cross 
               | FullOuter 
               | Inner 
               | LeftOuter 
               | RightOuter 
               deriving ( Eq,Show)
-- cata
sem_JoinType :: JoinType  ->
                T_JoinType 
sem_JoinType (Cross )  =
    (sem_JoinType_Cross )
sem_JoinType (FullOuter )  =
    (sem_JoinType_FullOuter )
sem_JoinType (Inner )  =
    (sem_JoinType_Inner )
sem_JoinType (LeftOuter )  =
    (sem_JoinType_LeftOuter )
sem_JoinType (RightOuter )  =
    (sem_JoinType_RightOuter )
-- semantic domain
type T_JoinType  = Scope ->
                   ( )
data Inh_JoinType  = Inh_JoinType {scope_Inh_JoinType :: Scope}
data Syn_JoinType  = Syn_JoinType {}
wrap_JoinType :: T_JoinType  ->
                 Inh_JoinType  ->
                 Syn_JoinType 
wrap_JoinType sem (Inh_JoinType _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_JoinType ))
sem_JoinType_Cross :: T_JoinType 
sem_JoinType_Cross  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_JoinType_FullOuter :: T_JoinType 
sem_JoinType_FullOuter  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_JoinType_Inner :: T_JoinType 
sem_JoinType_Inner  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_JoinType_LeftOuter :: T_JoinType 
sem_JoinType_LeftOuter  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_JoinType_RightOuter :: T_JoinType 
sem_JoinType_RightOuter  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Language ----------------------------------------------------
data Language  = Plpgsql 
               | Sql 
               deriving ( Eq,Show)
-- cata
sem_Language :: Language  ->
                T_Language 
sem_Language (Plpgsql )  =
    (sem_Language_Plpgsql )
sem_Language (Sql )  =
    (sem_Language_Sql )
-- semantic domain
type T_Language  = Scope ->
                   ( )
data Inh_Language  = Inh_Language {scope_Inh_Language :: Scope}
data Syn_Language  = Syn_Language {}
wrap_Language :: T_Language  ->
                 Inh_Language  ->
                 Syn_Language 
wrap_Language sem (Inh_Language _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_Language ))
sem_Language_Plpgsql :: T_Language 
sem_Language_Plpgsql  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Language_Sql :: T_Language 
sem_Language_Sql  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- MTableRef ---------------------------------------------------
type MTableRef  = (Maybe (TableRef))
-- cata
sem_MTableRef :: MTableRef  ->
                 T_MTableRef 
sem_MTableRef (Prelude.Just x )  =
    (sem_MTableRef_Just (sem_TableRef x ) )
sem_MTableRef Prelude.Nothing  =
    sem_MTableRef_Nothing
-- semantic domain
type T_MTableRef  = Scope ->
                    ( )
data Inh_MTableRef  = Inh_MTableRef {scope_Inh_MTableRef :: Scope}
data Syn_MTableRef  = Syn_MTableRef {}
wrap_MTableRef :: T_MTableRef  ->
                  Inh_MTableRef  ->
                  Syn_MTableRef 
wrap_MTableRef sem (Inh_MTableRef _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_MTableRef ))
sem_MTableRef_Just :: T_TableRef  ->
                      T_MTableRef 
sem_MTableRef_Just just_  =
    (\ _lhsIscope ->
         (let _justOscope :: Scope
              _justOscope =
                  _lhsIscope
          in  ( )))
sem_MTableRef_Nothing :: T_MTableRef 
sem_MTableRef_Nothing  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- MaybeExpression ---------------------------------------------
type MaybeExpression  = (Maybe (Expression))
-- cata
sem_MaybeExpression :: MaybeExpression  ->
                       T_MaybeExpression 
sem_MaybeExpression (Prelude.Just x )  =
    (sem_MaybeExpression_Just (sem_Expression x ) )
sem_MaybeExpression Prelude.Nothing  =
    sem_MaybeExpression_Nothing
-- semantic domain
type T_MaybeExpression  = Scope ->
                          ( )
data Inh_MaybeExpression  = Inh_MaybeExpression {scope_Inh_MaybeExpression :: Scope}
data Syn_MaybeExpression  = Syn_MaybeExpression {}
wrap_MaybeExpression :: T_MaybeExpression  ->
                        Inh_MaybeExpression  ->
                        Syn_MaybeExpression 
wrap_MaybeExpression sem (Inh_MaybeExpression _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_MaybeExpression ))
sem_MaybeExpression_Just :: T_Expression  ->
                            T_MaybeExpression 
sem_MaybeExpression_Just just_  =
    (\ _lhsIscope ->
         (let _justOscope :: Scope
              _justInodeType :: Type
              _justOscope =
                  _lhsIscope
              ( _justInodeType) =
                  (just_ _justOscope )
          in  ( )))
sem_MaybeExpression_Nothing :: T_MaybeExpression 
sem_MaybeExpression_Nothing  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Natural -----------------------------------------------------
data Natural  = Natural 
              | Unnatural 
              deriving ( Eq,Show)
-- cata
sem_Natural :: Natural  ->
               T_Natural 
sem_Natural (Natural )  =
    (sem_Natural_Natural )
sem_Natural (Unnatural )  =
    (sem_Natural_Unnatural )
-- semantic domain
type T_Natural  = Scope ->
                  ( )
data Inh_Natural  = Inh_Natural {scope_Inh_Natural :: Scope}
data Syn_Natural  = Syn_Natural {}
wrap_Natural :: T_Natural  ->
                Inh_Natural  ->
                Syn_Natural 
wrap_Natural sem (Inh_Natural _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_Natural ))
sem_Natural_Natural :: T_Natural 
sem_Natural_Natural  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Natural_Unnatural :: T_Natural 
sem_Natural_Unnatural  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- OnExpr ------------------------------------------------------
type OnExpr  = (Maybe (JoinExpression))
-- cata
sem_OnExpr :: OnExpr  ->
              T_OnExpr 
sem_OnExpr (Prelude.Just x )  =
    (sem_OnExpr_Just (sem_JoinExpression x ) )
sem_OnExpr Prelude.Nothing  =
    sem_OnExpr_Nothing
-- semantic domain
type T_OnExpr  = Scope ->
                 ( )
data Inh_OnExpr  = Inh_OnExpr {scope_Inh_OnExpr :: Scope}
data Syn_OnExpr  = Syn_OnExpr {}
wrap_OnExpr :: T_OnExpr  ->
               Inh_OnExpr  ->
               Syn_OnExpr 
wrap_OnExpr sem (Inh_OnExpr _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_OnExpr ))
sem_OnExpr_Just :: T_JoinExpression  ->
                   T_OnExpr 
sem_OnExpr_Just just_  =
    (\ _lhsIscope ->
         (let _justOscope :: Scope
              _justOscope =
                  _lhsIscope
          in  ( )))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- ParamDef ----------------------------------------------------
data ParamDef  = ParamDef (String) (TypeName) 
               | ParamDefTp (TypeName) 
               deriving ( Eq,Show)
-- cata
sem_ParamDef :: ParamDef  ->
                T_ParamDef 
sem_ParamDef (ParamDef _name _typ )  =
    (sem_ParamDef_ParamDef _name (sem_TypeName _typ ) )
sem_ParamDef (ParamDefTp _typ )  =
    (sem_ParamDef_ParamDefTp (sem_TypeName _typ ) )
-- semantic domain
type T_ParamDef  = Scope ->
                   ( )
data Inh_ParamDef  = Inh_ParamDef {scope_Inh_ParamDef :: Scope}
data Syn_ParamDef  = Syn_ParamDef {}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_ParamDef ))
sem_ParamDef_ParamDef :: String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef name_ typ_  =
    (\ _lhsIscope ->
         (let _typOscope :: Scope
              _typOscope =
                  _lhsIscope
          in  ( )))
sem_ParamDef_ParamDefTp :: T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp typ_  =
    (\ _lhsIscope ->
         (let _typOscope :: Scope
              _typOscope =
                  _lhsIscope
          in  ( )))
-- ParamDefList ------------------------------------------------
type ParamDefList  = [(ParamDef)]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = Scope ->
                       ( )
data Inh_ParamDefList  = Inh_ParamDefList {scope_Inh_ParamDefList :: Scope}
data Syn_ParamDefList  = Syn_ParamDefList {}
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_ParamDefList ))
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- RaiseType ---------------------------------------------------
data RaiseType  = RError 
                | RException 
                | RNotice 
                deriving ( Eq,Show)
-- cata
sem_RaiseType :: RaiseType  ->
                 T_RaiseType 
sem_RaiseType (RError )  =
    (sem_RaiseType_RError )
sem_RaiseType (RException )  =
    (sem_RaiseType_RException )
sem_RaiseType (RNotice )  =
    (sem_RaiseType_RNotice )
-- semantic domain
type T_RaiseType  = Scope ->
                    ( )
data Inh_RaiseType  = Inh_RaiseType {scope_Inh_RaiseType :: Scope}
data Syn_RaiseType  = Syn_RaiseType {}
wrap_RaiseType :: T_RaiseType  ->
                  Inh_RaiseType  ->
                  Syn_RaiseType 
wrap_RaiseType sem (Inh_RaiseType _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_RaiseType ))
sem_RaiseType_RError :: T_RaiseType 
sem_RaiseType_RError  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_RaiseType_RException :: T_RaiseType 
sem_RaiseType_RException  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_RaiseType_RNotice :: T_RaiseType 
sem_RaiseType_RNotice  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- RestartIdentity ---------------------------------------------
data RestartIdentity  = ContinueIdentity 
                      | RestartIdentity 
                      deriving ( Eq,Show)
-- cata
sem_RestartIdentity :: RestartIdentity  ->
                       T_RestartIdentity 
sem_RestartIdentity (ContinueIdentity )  =
    (sem_RestartIdentity_ContinueIdentity )
sem_RestartIdentity (RestartIdentity )  =
    (sem_RestartIdentity_RestartIdentity )
-- semantic domain
type T_RestartIdentity  = Scope ->
                          ( )
data Inh_RestartIdentity  = Inh_RestartIdentity {scope_Inh_RestartIdentity :: Scope}
data Syn_RestartIdentity  = Syn_RestartIdentity {}
wrap_RestartIdentity :: T_RestartIdentity  ->
                        Inh_RestartIdentity  ->
                        Syn_RestartIdentity 
wrap_RestartIdentity sem (Inh_RestartIdentity _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_RestartIdentity ))
sem_RestartIdentity_ContinueIdentity :: T_RestartIdentity 
sem_RestartIdentity_ContinueIdentity  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_RestartIdentity_RestartIdentity :: T_RestartIdentity 
sem_RestartIdentity_RestartIdentity  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Root --------------------------------------------------------
data Root  = Root (StatementList) 
           deriving ( Show)
-- cata
sem_Root :: Root  ->
            T_Root 
sem_Root (Root _statements )  =
    (sem_Root_Root (sem_StatementList _statements ) )
-- semantic domain
type T_Root  = Scope ->
               ( )
data Inh_Root  = Inh_Root {scope_Inh_Root :: Scope}
data Syn_Root  = Syn_Root {}
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_Root ))
sem_Root_Root :: T_StatementList  ->
                 T_Root 
sem_Root_Root statements_  =
    (\ _lhsIscope ->
         (let _statementsOscope :: Scope
              _statementsOscope =
                  _lhsIscope
          in  ( )))
-- RowConstraint -----------------------------------------------
data RowConstraint  = NotNullConstraint 
                    | NullConstraint 
                    | RowCheckConstraint (Expression) 
                    | RowPrimaryKeyConstraint 
                    | RowReferenceConstraint (String) (Maybe String) (Cascade) (Cascade) 
                    | RowUniqueConstraint 
                    deriving ( Eq,Show)
-- cata
sem_RowConstraint :: RowConstraint  ->
                     T_RowConstraint 
sem_RowConstraint (NotNullConstraint )  =
    (sem_RowConstraint_NotNullConstraint )
sem_RowConstraint (NullConstraint )  =
    (sem_RowConstraint_NullConstraint )
sem_RowConstraint (RowCheckConstraint _expression )  =
    (sem_RowConstraint_RowCheckConstraint (sem_Expression _expression ) )
sem_RowConstraint (RowPrimaryKeyConstraint )  =
    (sem_RowConstraint_RowPrimaryKeyConstraint )
sem_RowConstraint (RowReferenceConstraint _table _att _onUpdate _onDelete )  =
    (sem_RowConstraint_RowReferenceConstraint _table _att (sem_Cascade _onUpdate ) (sem_Cascade _onDelete ) )
sem_RowConstraint (RowUniqueConstraint )  =
    (sem_RowConstraint_RowUniqueConstraint )
-- semantic domain
type T_RowConstraint  = Scope ->
                        ( )
data Inh_RowConstraint  = Inh_RowConstraint {scope_Inh_RowConstraint :: Scope}
data Syn_RowConstraint  = Syn_RowConstraint {}
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_RowConstraint ))
sem_RowConstraint_NotNullConstraint :: T_RowConstraint 
sem_RowConstraint_NotNullConstraint  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_RowConstraint_NullConstraint :: T_RowConstraint 
sem_RowConstraint_NullConstraint  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_RowConstraint_RowCheckConstraint :: T_Expression  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint expression_  =
    (\ _lhsIscope ->
         (let _expressionOscope :: Scope
              _expressionInodeType :: Type
              _expressionOscope =
                  _lhsIscope
              ( _expressionInodeType) =
                  (expression_ _expressionOscope )
          in  ( )))
sem_RowConstraint_RowPrimaryKeyConstraint :: T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_RowConstraint_RowReferenceConstraint :: String ->
                                            (Maybe String) ->
                                            T_Cascade  ->
                                            T_Cascade  ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIscope ->
         (let _onUpdateOscope :: Scope
              _onDeleteOscope :: Scope
              _onUpdateOscope =
                  _lhsIscope
              _onDeleteOscope =
                  _lhsIscope
          in  ( )))
sem_RowConstraint_RowUniqueConstraint :: T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- RowConstraintList -------------------------------------------
type RowConstraintList  = [(RowConstraint)]
-- cata
sem_RowConstraintList :: RowConstraintList  ->
                         T_RowConstraintList 
sem_RowConstraintList list  =
    (Prelude.foldr sem_RowConstraintList_Cons sem_RowConstraintList_Nil (Prelude.map sem_RowConstraint list) )
-- semantic domain
type T_RowConstraintList  = Scope ->
                            ( )
data Inh_RowConstraintList  = Inh_RowConstraintList {scope_Inh_RowConstraintList :: Scope}
data Syn_RowConstraintList  = Syn_RowConstraintList {}
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_RowConstraintList ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- SelectExpression --------------------------------------------
data SelectExpression  = CombineSelect (CombineType) (SelectExpression) (SelectExpression) 
                       | Select (Distinct) (SelectList) (MTableRef) (Where) (ExpressionList) (Maybe Expression) (ExpressionList) (Direction) (Maybe Expression) (Maybe Expression) 
                       | Values (ExpressionListList) 
                       deriving ( Eq,Show)
-- cata
sem_SelectExpression :: SelectExpression  ->
                        T_SelectExpression 
sem_SelectExpression (CombineSelect _ctype _sel1 _sel2 )  =
    (sem_SelectExpression_CombineSelect (sem_CombineType _ctype ) (sem_SelectExpression _sel1 ) (sem_SelectExpression _sel2 ) )
sem_SelectExpression (Select _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selDir _selLimit _selOffset )  =
    (sem_SelectExpression_Select (sem_Distinct _selDistinct ) (sem_SelectList _selSelectList ) (sem_MTableRef _selTref ) (sem_Where _selWhere ) (sem_ExpressionList _selGroupBy ) _selHaving (sem_ExpressionList _selOrderBy ) (sem_Direction _selDir ) _selLimit _selOffset )
sem_SelectExpression (Values _vll )  =
    (sem_SelectExpression_Values (sem_ExpressionListList _vll ) )
-- semantic domain
type T_SelectExpression  = Scope ->
                           ( )
data Inh_SelectExpression  = Inh_SelectExpression {scope_Inh_SelectExpression :: Scope}
data Syn_SelectExpression  = Syn_SelectExpression {}
wrap_SelectExpression :: T_SelectExpression  ->
                         Inh_SelectExpression  ->
                         Syn_SelectExpression 
wrap_SelectExpression sem (Inh_SelectExpression _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_SelectExpression ))
sem_SelectExpression_CombineSelect :: T_CombineType  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression 
sem_SelectExpression_CombineSelect ctype_ sel1_ sel2_  =
    (\ _lhsIscope ->
         (let _ctypeOscope :: Scope
              _sel1Oscope :: Scope
              _sel2Oscope :: Scope
              _ctypeOscope =
                  _lhsIscope
              _sel1Oscope =
                  _lhsIscope
              _sel2Oscope =
                  _lhsIscope
          in  ( )))
sem_SelectExpression_Select :: T_Distinct  ->
                               T_SelectList  ->
                               T_MTableRef  ->
                               T_Where  ->
                               T_ExpressionList  ->
                               (Maybe Expression) ->
                               T_ExpressionList  ->
                               T_Direction  ->
                               (Maybe Expression) ->
                               (Maybe Expression) ->
                               T_SelectExpression 
sem_SelectExpression_Select selDistinct_ selSelectList_ selTref_ selWhere_ selGroupBy_ selHaving_ selOrderBy_ selDir_ selLimit_ selOffset_  =
    (\ _lhsIscope ->
         (let _selDistinctOscope :: Scope
              _selSelectListOscope :: Scope
              _selTrefOscope :: Scope
              _selWhereOscope :: Scope
              _selGroupByOscope :: Scope
              _selOrderByOscope :: Scope
              _selDirOscope :: Scope
              _selDistinctOscope =
                  _lhsIscope
              _selSelectListOscope =
                  _lhsIscope
              _selTrefOscope =
                  _lhsIscope
              _selWhereOscope =
                  _lhsIscope
              _selGroupByOscope =
                  _lhsIscope
              _selOrderByOscope =
                  _lhsIscope
              _selDirOscope =
                  _lhsIscope
          in  ( )))
sem_SelectExpression_Values :: T_ExpressionListList  ->
                               T_SelectExpression 
sem_SelectExpression_Values vll_  =
    (\ _lhsIscope ->
         (let _vllOscope :: Scope
              _vllOscope =
                  _lhsIscope
          in  ( )))
-- SelectItem --------------------------------------------------
data SelectItem  = SelExp (Expression) 
                 | SelectItem (Expression) (String) 
                 deriving ( Eq,Show)
-- cata
sem_SelectItem :: SelectItem  ->
                  T_SelectItem 
sem_SelectItem (SelExp _ex )  =
    (sem_SelectItem_SelExp (sem_Expression _ex ) )
sem_SelectItem (SelectItem _ex _name )  =
    (sem_SelectItem_SelectItem (sem_Expression _ex ) _name )
-- semantic domain
type T_SelectItem  = Scope ->
                     ( )
data Inh_SelectItem  = Inh_SelectItem {scope_Inh_SelectItem :: Scope}
data Syn_SelectItem  = Syn_SelectItem {}
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_SelectItem ))
sem_SelectItem_SelExp :: T_Expression  ->
                         T_SelectItem 
sem_SelectItem_SelExp ex_  =
    (\ _lhsIscope ->
         (let _exOscope :: Scope
              _exInodeType :: Type
              _exOscope =
                  _lhsIscope
              ( _exInodeType) =
                  (ex_ _exOscope )
          in  ( )))
sem_SelectItem_SelectItem :: T_Expression  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem ex_ name_  =
    (\ _lhsIscope ->
         (let _exOscope :: Scope
              _exInodeType :: Type
              _exOscope =
                  _lhsIscope
              ( _exInodeType) =
                  (ex_ _exOscope )
          in  ( )))
-- SelectItemList ----------------------------------------------
type SelectItemList  = [(SelectItem)]
-- cata
sem_SelectItemList :: SelectItemList  ->
                      T_SelectItemList 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList  = Scope ->
                         ( )
data Inh_SelectItemList  = Inh_SelectItemList {scope_Inh_SelectItemList :: Scope}
data Syn_SelectItemList  = Syn_SelectItemList {}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_SelectItemList ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- SelectList --------------------------------------------------
data SelectList  = SelectList (SelectItemList) (StringList) 
                 deriving ( Eq,Show)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _items _stringList )  =
    (sem_SelectList_SelectList (sem_SelectItemList _items ) (sem_StringList _stringList ) )
-- semantic domain
type T_SelectList  = Scope ->
                     ( )
data Inh_SelectList  = Inh_SelectList {scope_Inh_SelectList :: Scope}
data Syn_SelectList  = Syn_SelectList {}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_SelectList ))
sem_SelectList_SelectList :: T_SelectItemList  ->
                             T_StringList  ->
                             T_SelectList 
sem_SelectList_SelectList items_ stringList_  =
    (\ _lhsIscope ->
         (let _itemsOscope :: Scope
              _stringListOscope :: Scope
              _itemsOscope =
                  _lhsIscope
              _stringListOscope =
                  _lhsIscope
          in  ( )))
-- SetClause ---------------------------------------------------
data SetClause  = RowSetClause (StringList) (ExpressionList) 
                | SetClause (String) (Expression) 
                deriving ( Eq,Show)
-- cata
sem_SetClause :: SetClause  ->
                 T_SetClause 
sem_SetClause (RowSetClause _atts _vals )  =
    (sem_SetClause_RowSetClause (sem_StringList _atts ) (sem_ExpressionList _vals ) )
sem_SetClause (SetClause _att _val )  =
    (sem_SetClause_SetClause _att (sem_Expression _val ) )
-- semantic domain
type T_SetClause  = Scope ->
                    ( )
data Inh_SetClause  = Inh_SetClause {scope_Inh_SetClause :: Scope}
data Syn_SetClause  = Syn_SetClause {}
wrap_SetClause :: T_SetClause  ->
                  Inh_SetClause  ->
                  Syn_SetClause 
wrap_SetClause sem (Inh_SetClause _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_SetClause ))
sem_SetClause_RowSetClause :: T_StringList  ->
                              T_ExpressionList  ->
                              T_SetClause 
sem_SetClause_RowSetClause atts_ vals_  =
    (\ _lhsIscope ->
         (let _attsOscope :: Scope
              _valsOscope :: Scope
              _attsOscope =
                  _lhsIscope
              _valsOscope =
                  _lhsIscope
          in  ( )))
sem_SetClause_SetClause :: String ->
                           T_Expression  ->
                           T_SetClause 
sem_SetClause_SetClause att_ val_  =
    (\ _lhsIscope ->
         (let _valOscope :: Scope
              _valInodeType :: Type
              _valOscope =
                  _lhsIscope
              ( _valInodeType) =
                  (val_ _valOscope )
          in  ( )))
-- SetClauseList -----------------------------------------------
type SetClauseList  = [(SetClause)]
-- cata
sem_SetClauseList :: SetClauseList  ->
                     T_SetClauseList 
sem_SetClauseList list  =
    (Prelude.foldr sem_SetClauseList_Cons sem_SetClauseList_Nil (Prelude.map sem_SetClause list) )
-- semantic domain
type T_SetClauseList  = Scope ->
                        ( )
data Inh_SetClauseList  = Inh_SetClauseList {scope_Inh_SetClauseList :: Scope}
data Syn_SetClauseList  = Syn_SetClauseList {}
wrap_SetClauseList :: T_SetClauseList  ->
                      Inh_SetClauseList  ->
                      Syn_SetClauseList 
wrap_SetClauseList sem (Inh_SetClauseList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_SetClauseList ))
sem_SetClauseList_Cons :: T_SetClause  ->
                          T_SetClauseList  ->
                          T_SetClauseList 
sem_SetClauseList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Statement ---------------------------------------------------
data Statement  = Assignment (Annotation) (String) (Expression) 
                | CaseStatement (Annotation) (Expression) (ExpressionListStatementListPairList) (StatementList) 
                | ContinueStatement (Annotation) 
                | Copy (Annotation) (String) (StringList) (CopySource) 
                | CopyData (Annotation) (String) 
                | CreateDomain (Annotation) (String) (TypeName) (Maybe Expression) 
                | CreateFunction (Annotation) (Language) (String) (ParamDefList) (TypeName) (String) (FnBody) (Volatility) 
                | CreateTable (Annotation) (String) (AttributeDefList) (ConstraintList) 
                | CreateTableAs (Annotation) (String) (SelectExpression) 
                | CreateType (Annotation) (String) (TypeAttributeDefList) 
                | CreateView (Annotation) (String) (SelectExpression) 
                | Delete (Annotation) (String) (Where) (Maybe SelectList) 
                | DropFunction (Annotation) (IfExists) (StringStringListPairList) (Cascade) 
                | DropSomething (Annotation) (DropType) (IfExists) (StringList) (Cascade) 
                | Execute (Annotation) (Expression) 
                | ExecuteInto (Annotation) (Expression) (StringList) 
                | ForIntegerStatement (Annotation) (String) (Expression) (Expression) (StatementList) 
                | ForSelectStatement (Annotation) (String) (SelectExpression) (StatementList) 
                | If (Annotation) (ExpressionStatementListPairList) (StatementList) 
                | Insert (Annotation) (String) (StringList) (SelectExpression) (Maybe SelectList) 
                | NullStatement (Annotation) 
                | Perform (Annotation) (Expression) 
                | Raise (Annotation) (RaiseType) (String) (ExpressionList) 
                | Return (Annotation) (Maybe Expression) 
                | ReturnNext (Annotation) (Expression) 
                | ReturnQuery (Annotation) (SelectExpression) 
                | SelectStatement (Annotation) (SelectExpression) 
                | Truncate (Annotation) (StringList) (RestartIdentity) (Cascade) 
                | Update (Annotation) (String) (SetClauseList) (Where) (Maybe SelectList) 
                | WhileStatement (Annotation) (Expression) (StatementList) 
                deriving ( Eq,Show)
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (Assignment _ann _target _value )  =
    (sem_Statement_Assignment _ann _target (sem_Expression _value ) )
sem_Statement (CaseStatement _ann _val _cases _els )  =
    (sem_Statement_CaseStatement _ann (sem_Expression _val ) (sem_ExpressionListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (ContinueStatement _ann )  =
    (sem_Statement_ContinueStatement _ann )
sem_Statement (Copy _ann _table _targetCols _source )  =
    (sem_Statement_Copy _ann _table (sem_StringList _targetCols ) (sem_CopySource _source ) )
sem_Statement (CopyData _ann _insData )  =
    (sem_Statement_CopyData _ann _insData )
sem_Statement (CreateDomain _ann _name _typ _check )  =
    (sem_Statement_CreateDomain _ann _name (sem_TypeName _typ ) _check )
sem_Statement (CreateFunction _ann _lang _name _params _rettype _bodyQuote _body _vol )  =
    (sem_Statement_CreateFunction _ann (sem_Language _lang ) _name (sem_ParamDefList _params ) (sem_TypeName _rettype ) _bodyQuote (sem_FnBody _body ) (sem_Volatility _vol ) )
sem_Statement (CreateTable _ann _name _atts _cons )  =
    (sem_Statement_CreateTable _ann _name (sem_AttributeDefList _atts ) (sem_ConstraintList _cons ) )
sem_Statement (CreateTableAs _ann _name _expr )  =
    (sem_Statement_CreateTableAs _ann _name (sem_SelectExpression _expr ) )
sem_Statement (CreateType _ann _name _atts )  =
    (sem_Statement_CreateType _ann _name (sem_TypeAttributeDefList _atts ) )
sem_Statement (CreateView _ann _name _expr )  =
    (sem_Statement_CreateView _ann _name (sem_SelectExpression _expr ) )
sem_Statement (Delete _ann _table _whr _returning )  =
    (sem_Statement_Delete _ann _table (sem_Where _whr ) _returning )
sem_Statement (DropFunction _ann _ifE _sigs _cascade )  =
    (sem_Statement_DropFunction _ann (sem_IfExists _ifE ) (sem_StringStringListPairList _sigs ) (sem_Cascade _cascade ) )
sem_Statement (DropSomething _ann _dropType _ifE _names _cascade )  =
    (sem_Statement_DropSomething _ann (sem_DropType _dropType ) (sem_IfExists _ifE ) (sem_StringList _names ) (sem_Cascade _cascade ) )
sem_Statement (Execute _ann _expr )  =
    (sem_Statement_Execute _ann (sem_Expression _expr ) )
sem_Statement (ExecuteInto _ann _expr _targets )  =
    (sem_Statement_ExecuteInto _ann (sem_Expression _expr ) (sem_StringList _targets ) )
sem_Statement (ForIntegerStatement _ann _var _from _to _sts )  =
    (sem_Statement_ForIntegerStatement _ann _var (sem_Expression _from ) (sem_Expression _to ) (sem_StatementList _sts ) )
sem_Statement (ForSelectStatement _ann _var _sel _sts )  =
    (sem_Statement_ForSelectStatement _ann _var (sem_SelectExpression _sel ) (sem_StatementList _sts ) )
sem_Statement (If _ann _cases _els )  =
    (sem_Statement_If _ann (sem_ExpressionStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (Insert _ann _table _targetCols _insData _returning )  =
    (sem_Statement_Insert _ann _table (sem_StringList _targetCols ) (sem_SelectExpression _insData ) _returning )
sem_Statement (NullStatement _ann )  =
    (sem_Statement_NullStatement _ann )
sem_Statement (Perform _ann _expr )  =
    (sem_Statement_Perform _ann (sem_Expression _expr ) )
sem_Statement (Raise _ann _level _message _args )  =
    (sem_Statement_Raise _ann (sem_RaiseType _level ) _message (sem_ExpressionList _args ) )
sem_Statement (Return _ann _value )  =
    (sem_Statement_Return _ann _value )
sem_Statement (ReturnNext _ann _expr )  =
    (sem_Statement_ReturnNext _ann (sem_Expression _expr ) )
sem_Statement (ReturnQuery _ann _sel )  =
    (sem_Statement_ReturnQuery _ann (sem_SelectExpression _sel ) )
sem_Statement (SelectStatement _ann _ex )  =
    (sem_Statement_SelectStatement _ann (sem_SelectExpression _ex ) )
sem_Statement (Truncate _ann _tables _restartIdentity _cascade )  =
    (sem_Statement_Truncate _ann (sem_StringList _tables ) (sem_RestartIdentity _restartIdentity ) (sem_Cascade _cascade ) )
sem_Statement (Update _ann _table _assigns _whr _returning )  =
    (sem_Statement_Update _ann _table (sem_SetClauseList _assigns ) (sem_Where _whr ) _returning )
sem_Statement (WhileStatement _ann _expr _sts )  =
    (sem_Statement_WhileStatement _ann (sem_Expression _expr ) (sem_StatementList _sts ) )
-- semantic domain
type T_Statement  = Scope ->
                    ( )
data Inh_Statement  = Inh_Statement {scope_Inh_Statement :: Scope}
data Syn_Statement  = Syn_Statement {}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_Statement ))
sem_Statement_Assignment :: Annotation ->
                            String ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIscope ->
         (let _valueOscope :: Scope
              _valueInodeType :: Type
              _valueOscope =
                  _lhsIscope
              ( _valueInodeType) =
                  (value_ _valueOscope )
          in  ( )))
sem_Statement_CaseStatement :: Annotation ->
                               T_Expression  ->
                               T_ExpressionListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement ann_ val_ cases_ els_  =
    (\ _lhsIscope ->
         (let _valOscope :: Scope
              _casesOscope :: Scope
              _elsOscope :: Scope
              _valInodeType :: Type
              _valOscope =
                  _lhsIscope
              _casesOscope =
                  _lhsIscope
              _elsOscope =
                  _lhsIscope
              ( _valInodeType) =
                  (val_ _valOscope )
          in  ( )))
sem_Statement_ContinueStatement :: Annotation ->
                                   T_Statement 
sem_Statement_ContinueStatement ann_  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Statement_Copy :: Annotation ->
                      String ->
                      T_StringList  ->
                      T_CopySource  ->
                      T_Statement 
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIscope ->
         (let _targetColsOscope :: Scope
              _sourceOscope :: Scope
              _targetColsOscope =
                  _lhsIscope
              _sourceOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_CopyData :: Annotation ->
                          String ->
                          T_Statement 
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Statement_CreateDomain :: Annotation ->
                              String ->
                              T_TypeName  ->
                              (Maybe Expression) ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ check_  =
    (\ _lhsIscope ->
         (let _typOscope :: Scope
              _typOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_CreateFunction :: Annotation ->
                                T_Language  ->
                                String ->
                                T_ParamDefList  ->
                                T_TypeName  ->
                                String ->
                                T_FnBody  ->
                                T_Volatility  ->
                                T_Statement 
sem_Statement_CreateFunction ann_ lang_ name_ params_ rettype_ bodyQuote_ body_ vol_  =
    (\ _lhsIscope ->
         (let _langOscope :: Scope
              _paramsOscope :: Scope
              _rettypeOscope :: Scope
              _bodyOscope :: Scope
              _volOscope :: Scope
              _langOscope =
                  _lhsIscope
              _paramsOscope =
                  _lhsIscope
              _rettypeOscope =
                  _lhsIscope
              _bodyOscope =
                  _lhsIscope
              _volOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_CreateTable :: Annotation ->
                             String ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIscope ->
         (let _attsOscope :: Scope
              _consOscope :: Scope
              _attsOscope =
                  _lhsIscope
              _consOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_CreateTableAs :: Annotation ->
                               String ->
                               T_SelectExpression  ->
                               T_Statement 
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIscope ->
         (let _exprOscope :: Scope
              _exprOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_CreateType :: Annotation ->
                            String ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIscope ->
         (let _attsOscope :: Scope
              _attsOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_CreateView :: Annotation ->
                            String ->
                            T_SelectExpression  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ expr_  =
    (\ _lhsIscope ->
         (let _exprOscope :: Scope
              _exprOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_Delete :: Annotation ->
                        String ->
                        T_Where  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Delete ann_ table_ whr_ returning_  =
    (\ _lhsIscope ->
         (let _whrOscope :: Scope
              _whrOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_DropFunction :: Annotation ->
                              T_IfExists  ->
                              T_StringStringListPairList  ->
                              T_Cascade  ->
                              T_Statement 
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIscope ->
         (let _ifEOscope :: Scope
              _sigsOscope :: Scope
              _cascadeOscope :: Scope
              _ifEOscope =
                  _lhsIscope
              _sigsOscope =
                  _lhsIscope
              _cascadeOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_DropSomething :: Annotation ->
                               T_DropType  ->
                               T_IfExists  ->
                               T_StringList  ->
                               T_Cascade  ->
                               T_Statement 
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIscope ->
         (let _dropTypeOscope :: Scope
              _ifEOscope :: Scope
              _namesOscope :: Scope
              _cascadeOscope :: Scope
              _dropTypeOscope =
                  _lhsIscope
              _ifEOscope =
                  _lhsIscope
              _namesOscope =
                  _lhsIscope
              _cascadeOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_Execute :: Annotation ->
                         T_Expression  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIscope ->
         (let _exprOscope :: Scope
              _exprInodeType :: Type
              _exprOscope =
                  _lhsIscope
              ( _exprInodeType) =
                  (expr_ _exprOscope )
          in  ( )))
sem_Statement_ExecuteInto :: Annotation ->
                             T_Expression  ->
                             T_StringList  ->
                             T_Statement 
sem_Statement_ExecuteInto ann_ expr_ targets_  =
    (\ _lhsIscope ->
         (let _exprOscope :: Scope
              _targetsOscope :: Scope
              _exprInodeType :: Type
              _exprOscope =
                  _lhsIscope
              _targetsOscope =
                  _lhsIscope
              ( _exprInodeType) =
                  (expr_ _exprOscope )
          in  ( )))
sem_Statement_ForIntegerStatement :: Annotation ->
                                     String ->
                                     T_Expression  ->
                                     T_Expression  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement ann_ var_ from_ to_ sts_  =
    (\ _lhsIscope ->
         (let _fromOscope :: Scope
              _toOscope :: Scope
              _stsOscope :: Scope
              _fromInodeType :: Type
              _toInodeType :: Type
              _fromOscope =
                  _lhsIscope
              _toOscope =
                  _lhsIscope
              _stsOscope =
                  _lhsIscope
              ( _fromInodeType) =
                  (from_ _fromOscope )
              ( _toInodeType) =
                  (to_ _toOscope )
          in  ( )))
sem_Statement_ForSelectStatement :: Annotation ->
                                    String ->
                                    T_SelectExpression  ->
                                    T_StatementList  ->
                                    T_Statement 
sem_Statement_ForSelectStatement ann_ var_ sel_ sts_  =
    (\ _lhsIscope ->
         (let _selOscope :: Scope
              _stsOscope :: Scope
              _selOscope =
                  _lhsIscope
              _stsOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_If :: Annotation ->
                    T_ExpressionStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIscope ->
         (let _casesOscope :: Scope
              _elsOscope :: Scope
              _casesOscope =
                  _lhsIscope
              _elsOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_Insert :: Annotation ->
                        String ->
                        T_StringList  ->
                        T_SelectExpression  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIscope ->
         (let _targetColsOscope :: Scope
              _insDataOscope :: Scope
              _targetColsOscope =
                  _lhsIscope
              _insDataOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_NullStatement :: Annotation ->
                               T_Statement 
sem_Statement_NullStatement ann_  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Statement_Perform :: Annotation ->
                         T_Expression  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIscope ->
         (let _exprOscope :: Scope
              _exprInodeType :: Type
              _exprOscope =
                  _lhsIscope
              ( _exprInodeType) =
                  (expr_ _exprOscope )
          in  ( )))
sem_Statement_Raise :: Annotation ->
                       T_RaiseType  ->
                       String ->
                       T_ExpressionList  ->
                       T_Statement 
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIscope ->
         (let _levelOscope :: Scope
              _argsOscope :: Scope
              _levelOscope =
                  _lhsIscope
              _argsOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_Return :: Annotation ->
                        (Maybe Expression) ->
                        T_Statement 
sem_Statement_Return ann_ value_  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Statement_ReturnNext :: Annotation ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIscope ->
         (let _exprOscope :: Scope
              _exprInodeType :: Type
              _exprOscope =
                  _lhsIscope
              ( _exprInodeType) =
                  (expr_ _exprOscope )
          in  ( )))
sem_Statement_ReturnQuery :: Annotation ->
                             T_SelectExpression  ->
                             T_Statement 
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIscope ->
         (let _selOscope :: Scope
              _selOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_SelectStatement :: Annotation ->
                                 T_SelectExpression  ->
                                 T_Statement 
sem_Statement_SelectStatement ann_ ex_  =
    (\ _lhsIscope ->
         (let _exOscope :: Scope
              _exOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_Truncate :: Annotation ->
                          T_StringList  ->
                          T_RestartIdentity  ->
                          T_Cascade  ->
                          T_Statement 
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIscope ->
         (let _tablesOscope :: Scope
              _restartIdentityOscope :: Scope
              _cascadeOscope :: Scope
              _tablesOscope =
                  _lhsIscope
              _restartIdentityOscope =
                  _lhsIscope
              _cascadeOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_Update :: Annotation ->
                        String ->
                        T_SetClauseList  ->
                        T_Where  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Update ann_ table_ assigns_ whr_ returning_  =
    (\ _lhsIscope ->
         (let _assignsOscope :: Scope
              _whrOscope :: Scope
              _assignsOscope =
                  _lhsIscope
              _whrOscope =
                  _lhsIscope
          in  ( )))
sem_Statement_WhileStatement :: Annotation ->
                                T_Expression  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement ann_ expr_ sts_  =
    (\ _lhsIscope ->
         (let _exprOscope :: Scope
              _stsOscope :: Scope
              _exprInodeType :: Type
              _exprOscope =
                  _lhsIscope
              _stsOscope =
                  _lhsIscope
              ( _exprInodeType) =
                  (expr_ _exprOscope )
          in  ( )))
-- StatementList -----------------------------------------------
type StatementList  = [(Statement)]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_Statement list) )
-- semantic domain
type T_StatementList  = Scope ->
                        ( )
data Inh_StatementList  = Inh_StatementList {scope_Inh_StatementList :: Scope}
data Syn_StatementList  = Syn_StatementList {}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_StatementList ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- StringList --------------------------------------------------
type StringList  = [(String)]
-- cata
sem_StringList :: StringList  ->
                  T_StringList 
sem_StringList list  =
    (Prelude.foldr sem_StringList_Cons sem_StringList_Nil list )
-- semantic domain
type T_StringList  = Scope ->
                     ( )
data Inh_StringList  = Inh_StringList {scope_Inh_StringList :: Scope}
data Syn_StringList  = Syn_StringList {}
wrap_StringList :: T_StringList  ->
                   Inh_StringList  ->
                   Syn_StringList 
wrap_StringList sem (Inh_StringList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_StringList ))
sem_StringList_Cons :: String ->
                       T_StringList  ->
                       T_StringList 
sem_StringList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _tlOscope :: Scope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_StringList_Nil :: T_StringList 
sem_StringList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- StringStringListPair ----------------------------------------
type StringStringListPair  = ( (String),(StringList))
-- cata
sem_StringStringListPair :: StringStringListPair  ->
                            T_StringStringListPair 
sem_StringStringListPair ( x1,x2)  =
    (sem_StringStringListPair_Tuple x1 (sem_StringList x2 ) )
-- semantic domain
type T_StringStringListPair  = Scope ->
                               ( )
data Inh_StringStringListPair  = Inh_StringStringListPair {scope_Inh_StringStringListPair :: Scope}
data Syn_StringStringListPair  = Syn_StringStringListPair {}
wrap_StringStringListPair :: T_StringStringListPair  ->
                             Inh_StringStringListPair  ->
                             Syn_StringStringListPair 
wrap_StringStringListPair sem (Inh_StringStringListPair _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_StringStringListPair ))
sem_StringStringListPair_Tuple :: String ->
                                  T_StringList  ->
                                  T_StringStringListPair 
sem_StringStringListPair_Tuple x1_ x2_  =
    (\ _lhsIscope ->
         (let _x2Oscope :: Scope
              _x2Oscope =
                  _lhsIscope
          in  ( )))
-- StringStringListPairList ------------------------------------
type StringStringListPairList  = [(StringStringListPair)]
-- cata
sem_StringStringListPairList :: StringStringListPairList  ->
                                T_StringStringListPairList 
sem_StringStringListPairList list  =
    (Prelude.foldr sem_StringStringListPairList_Cons sem_StringStringListPairList_Nil (Prelude.map sem_StringStringListPair list) )
-- semantic domain
type T_StringStringListPairList  = Scope ->
                                   ( )
data Inh_StringStringListPairList  = Inh_StringStringListPairList {scope_Inh_StringStringListPairList :: Scope}
data Syn_StringStringListPairList  = Syn_StringStringListPairList {}
wrap_StringStringListPairList :: T_StringStringListPairList  ->
                                 Inh_StringStringListPairList  ->
                                 Syn_StringStringListPairList 
wrap_StringStringListPairList sem (Inh_StringStringListPairList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_StringStringListPairList ))
sem_StringStringListPairList_Cons :: T_StringStringListPair  ->
                                     T_StringStringListPairList  ->
                                     T_StringStringListPairList 
sem_StringStringListPairList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_StringStringListPairList_Nil :: T_StringStringListPairList 
sem_StringStringListPairList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- TableRef ----------------------------------------------------
data TableRef  = JoinedTref (TableRef) (Natural) (JoinType) (TableRef) (OnExpr) 
               | SubTref (SelectExpression) (String) 
               | Tref (String) 
               | TrefAlias (String) (String) 
               | TrefFun (Expression) 
               | TrefFunAlias (Expression) (String) 
               deriving ( Eq,Show)
-- cata
sem_TableRef :: TableRef  ->
                T_TableRef 
sem_TableRef (JoinedTref _tbl _nat _joinType _tbl1 _onExpr )  =
    (sem_TableRef_JoinedTref (sem_TableRef _tbl ) (sem_Natural _nat ) (sem_JoinType _joinType ) (sem_TableRef _tbl1 ) (sem_OnExpr _onExpr ) )
sem_TableRef (SubTref _sel _alias )  =
    (sem_TableRef_SubTref (sem_SelectExpression _sel ) _alias )
sem_TableRef (Tref _tbl )  =
    (sem_TableRef_Tref _tbl )
sem_TableRef (TrefAlias _tbl _alias )  =
    (sem_TableRef_TrefAlias _tbl _alias )
sem_TableRef (TrefFun _fn )  =
    (sem_TableRef_TrefFun (sem_Expression _fn ) )
sem_TableRef (TrefFunAlias _fn _alias )  =
    (sem_TableRef_TrefFunAlias (sem_Expression _fn ) _alias )
-- semantic domain
type T_TableRef  = Scope ->
                   ( )
data Inh_TableRef  = Inh_TableRef {scope_Inh_TableRef :: Scope}
data Syn_TableRef  = Syn_TableRef {}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_TableRef ))
sem_TableRef_JoinedTref :: T_TableRef  ->
                           T_Natural  ->
                           T_JoinType  ->
                           T_TableRef  ->
                           T_OnExpr  ->
                           T_TableRef 
sem_TableRef_JoinedTref tbl_ nat_ joinType_ tbl1_ onExpr_  =
    (\ _lhsIscope ->
         (let _tblOscope :: Scope
              _natOscope :: Scope
              _joinTypeOscope :: Scope
              _tbl1Oscope :: Scope
              _onExprOscope :: Scope
              _tblOscope =
                  _lhsIscope
              _natOscope =
                  _lhsIscope
              _joinTypeOscope =
                  _lhsIscope
              _tbl1Oscope =
                  _lhsIscope
              _onExprOscope =
                  _lhsIscope
          in  ( )))
sem_TableRef_SubTref :: T_SelectExpression  ->
                        String ->
                        T_TableRef 
sem_TableRef_SubTref sel_ alias_  =
    (\ _lhsIscope ->
         (let _selOscope :: Scope
              _selOscope =
                  _lhsIscope
          in  ( )))
sem_TableRef_Tref :: String ->
                     T_TableRef 
sem_TableRef_Tref tbl_  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_TableRef_TrefAlias :: String ->
                          String ->
                          T_TableRef 
sem_TableRef_TrefAlias tbl_ alias_  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_TableRef_TrefFun :: T_Expression  ->
                        T_TableRef 
sem_TableRef_TrefFun fn_  =
    (\ _lhsIscope ->
         (let _fnOscope :: Scope
              _fnInodeType :: Type
              _fnOscope =
                  _lhsIscope
              ( _fnInodeType) =
                  (fn_ _fnOscope )
          in  ( )))
sem_TableRef_TrefFunAlias :: T_Expression  ->
                             String ->
                             T_TableRef 
sem_TableRef_TrefFunAlias fn_ alias_  =
    (\ _lhsIscope ->
         (let _fnOscope :: Scope
              _fnInodeType :: Type
              _fnOscope =
                  _lhsIscope
              ( _fnInodeType) =
                  (fn_ _fnOscope )
          in  ( )))
-- TypeAttributeDef --------------------------------------------
data TypeAttributeDef  = TypeAttDef (String) (TypeName) 
                       deriving ( Eq,Show)
-- cata
sem_TypeAttributeDef :: TypeAttributeDef  ->
                        T_TypeAttributeDef 
sem_TypeAttributeDef (TypeAttDef _name _typ )  =
    (sem_TypeAttributeDef_TypeAttDef _name (sem_TypeName _typ ) )
-- semantic domain
type T_TypeAttributeDef  = Scope ->
                           ( )
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {scope_Inh_TypeAttributeDef :: Scope}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_TypeAttributeDef ))
sem_TypeAttributeDef_TypeAttDef :: String ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef name_ typ_  =
    (\ _lhsIscope ->
         (let _typOscope :: Scope
              _typOscope =
                  _lhsIscope
          in  ( )))
-- TypeAttributeDefList ----------------------------------------
type TypeAttributeDefList  = [(TypeAttributeDef)]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = Scope ->
                               ( )
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {scope_Inh_TypeAttributeDefList :: Scope}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {}
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_TypeAttributeDefList ))
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- TypeName ----------------------------------------------------
data TypeName  = ArrayTypeName (TypeName) 
               | PrecTypeName (String) (Integer) 
               | SetOfTypeName (TypeName) 
               | SimpleTypeName (String) 
               deriving ( Eq,Show)
-- cata
sem_TypeName :: TypeName  ->
                T_TypeName 
sem_TypeName (ArrayTypeName _typ )  =
    (sem_TypeName_ArrayTypeName (sem_TypeName _typ ) )
sem_TypeName (PrecTypeName _tn _prec )  =
    (sem_TypeName_PrecTypeName _tn _prec )
sem_TypeName (SetOfTypeName _typ )  =
    (sem_TypeName_SetOfTypeName (sem_TypeName _typ ) )
sem_TypeName (SimpleTypeName _tn )  =
    (sem_TypeName_SimpleTypeName _tn )
-- semantic domain
type T_TypeName  = Scope ->
                   ( )
data Inh_TypeName  = Inh_TypeName {scope_Inh_TypeName :: Scope}
data Syn_TypeName  = Syn_TypeName {}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_TypeName ))
sem_TypeName_ArrayTypeName :: T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName typ_  =
    (\ _lhsIscope ->
         (let _typOscope :: Scope
              _typOscope =
                  _lhsIscope
          in  ( )))
sem_TypeName_PrecTypeName :: String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName tn_ prec_  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_TypeName_SetOfTypeName :: T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName typ_  =
    (\ _lhsIscope ->
         (let _typOscope :: Scope
              _typOscope =
                  _lhsIscope
          in  ( )))
sem_TypeName_SimpleTypeName :: String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName tn_  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- VarDef ------------------------------------------------------
data VarDef  = VarDef (String) (TypeName) (Maybe Expression) 
             deriving ( Eq,Show)
-- cata
sem_VarDef :: VarDef  ->
              T_VarDef 
sem_VarDef (VarDef _name _typ _value )  =
    (sem_VarDef_VarDef _name (sem_TypeName _typ ) _value )
-- semantic domain
type T_VarDef  = Scope ->
                 ( )
data Inh_VarDef  = Inh_VarDef {scope_Inh_VarDef :: Scope}
data Syn_VarDef  = Syn_VarDef {}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_VarDef ))
sem_VarDef_VarDef :: String ->
                     T_TypeName  ->
                     (Maybe Expression) ->
                     T_VarDef 
sem_VarDef_VarDef name_ typ_ value_  =
    (\ _lhsIscope ->
         (let _typOscope :: Scope
              _typOscope =
                  _lhsIscope
          in  ( )))
-- VarDefList --------------------------------------------------
type VarDefList  = [(VarDef)]
-- cata
sem_VarDefList :: VarDefList  ->
                  T_VarDefList 
sem_VarDefList list  =
    (Prelude.foldr sem_VarDefList_Cons sem_VarDefList_Nil (Prelude.map sem_VarDef list) )
-- semantic domain
type T_VarDefList  = Scope ->
                     ( )
data Inh_VarDefList  = Inh_VarDefList {scope_Inh_VarDefList :: Scope}
data Syn_VarDefList  = Syn_VarDefList {}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_VarDefList ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIscope ->
         (let _hdOscope :: Scope
              _tlOscope :: Scope
              _hdOscope =
                  _lhsIscope
              _tlOscope =
                  _lhsIscope
          in  ( )))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Volatility --------------------------------------------------
data Volatility  = Immutable 
                 | Stable 
                 | Volatile 
                 deriving ( Eq,Show)
-- cata
sem_Volatility :: Volatility  ->
                  T_Volatility 
sem_Volatility (Immutable )  =
    (sem_Volatility_Immutable )
sem_Volatility (Stable )  =
    (sem_Volatility_Stable )
sem_Volatility (Volatile )  =
    (sem_Volatility_Volatile )
-- semantic domain
type T_Volatility  = Scope ->
                     ( )
data Inh_Volatility  = Inh_Volatility {scope_Inh_Volatility :: Scope}
data Syn_Volatility  = Syn_Volatility {}
wrap_Volatility :: T_Volatility  ->
                   Inh_Volatility  ->
                   Syn_Volatility 
wrap_Volatility sem (Inh_Volatility _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_Volatility ))
sem_Volatility_Immutable :: T_Volatility 
sem_Volatility_Immutable  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Volatility_Stable :: T_Volatility 
sem_Volatility_Stable  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
sem_Volatility_Volatile :: T_Volatility 
sem_Volatility_Volatile  =
    (\ _lhsIscope ->
         (let 
          in  ( )))
-- Where -------------------------------------------------------
type Where  = (Maybe (Expression))
-- cata
sem_Where :: Where  ->
             T_Where 
sem_Where (Prelude.Just x )  =
    (sem_Where_Just (sem_Expression x ) )
sem_Where Prelude.Nothing  =
    sem_Where_Nothing
-- semantic domain
type T_Where  = Scope ->
                ( )
data Inh_Where  = Inh_Where {scope_Inh_Where :: Scope}
data Syn_Where  = Syn_Where {}
wrap_Where :: T_Where  ->
              Inh_Where  ->
              Syn_Where 
wrap_Where sem (Inh_Where _lhsIscope )  =
    (let ( ) =
             (sem _lhsIscope )
     in  (Syn_Where ))
sem_Where_Just :: T_Expression  ->
                  T_Where 
sem_Where_Just just_  =
    (\ _lhsIscope ->
         (let _justOscope :: Scope
              _justInodeType :: Type
              _justOscope =
                  _lhsIscope
              ( _justInodeType) =
                  (just_ _justOscope )
          in  ( )))
sem_Where_Nothing :: T_Where 
sem_Where_Nothing  =
    (\ _lhsIscope ->
         (let 
          in  ( )))