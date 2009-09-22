

-- UUAGC 0.9.10 (AstInternal.ag)
module Database.HsSqlPpp.TypeChecking.AstInternal(
    -- {-# LANGUAGE DeriveDataTypeable #-}
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
   ,ExpressionListStatementListPairList
   ,ExpressionListStatementListPair
   ,ExpressionList
   ,StringList
   ,ParamDefList
   ,AttributeDefList
   ,ConstraintList
   ,TypeAttributeDefList
   ,Where
   ,StringStringListPairList
   ,StringStringListPair
   ,ExpressionStatementListPairList
   ,SetClauseList
   ,CaseExpressionListExpressionPairList
   ,MaybeExpression
   ,MTableRef
   ,ExpressionListList
   ,SelectItemList
   ,OnExpr
   ,RowConstraintList
   ,VarDefList
   ,ExpressionStatementListPair
   ,MExpression
   ,CaseExpressionListExpressionPair
   ,CaseExpressionList
   -- annotations
   ,annotateAst
   ,annotateAstEnv
   ,annotateExpression
) where

import Data.Maybe
import Data.List
import Debug.Trace
import Control.Monad.Error
import Control.Arrow
import Data.Either
import Control.Applicative
import Data.Generics

import Database.HsSqlPpp.TypeChecking.TypeType
import Database.HsSqlPpp.TypeChecking.AstUtils
import Database.HsSqlPpp.TypeChecking.TypeConversion
import Database.HsSqlPpp.TypeChecking.TypeCheckingH
import Database.HsSqlPpp.TypeChecking.AstAnnotation
import Database.HsSqlPpp.TypeChecking.EnvironmentInternal
import Database.HsSqlPpp.TypeChecking.DefaultTemplate1Environment
import Database.HsSqlPpp.Utils

{-# LINE 591 "AstInternal.ag" #-}

-- | Takes an ast, and adds annotations, including types, type errors,
-- and statement info. Type checks against defaultEnv.
annotateAst :: StatementList -> StatementList
annotateAst = annotateAstEnv defaultTemplate1Environment


-- | As annotateAst but you supply an environment to check
-- against. See Environment module for how to read an Environment from
-- an existing database so you can type check against it.
annotateAstEnv :: Environment -> StatementList -> StatementList
annotateAstEnv env sts =
    let t = sem_Root (Root sts)
        ta = wrap_Root t Inh_Root {env_Inh_Root = env}
        tl = annotatedTree_Syn_Root ta
    in case tl of
         Root r -> r

-- | Testing utility, mainly used to check an expression for type errors
-- or to get its type.
annotateExpression :: Environment -> Expression -> Expression
annotateExpression env ex =
    let t = sem_ExpressionRoot (ExpressionRoot ex)
        rt = (annotatedTree_Syn_ExpressionRoot
              (wrap_ExpressionRoot t Inh_ExpressionRoot {env_Inh_ExpressionRoot = env}))
    in case rt of
         ExpressionRoot e -> e

{-

================================================================================

= instances for Annotated.

Hopefully, some sort of SYB approach can be used to autogenerate these
in the future. It is imperative that this or template haskell or
 something similar be used because doing it by hand guarantees some
bits will be missed.

Can't use attributes to make updating the annotations any easier.

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
  setAnn ex a =
    case ex of
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


instance Annotated FnBody where
  ann a =
      case a of
         SqlFnBody a _ -> a
         PlpgsqlFnBody a _ _ -> a
  setAnn ex a =
    case ex of
         SqlFnBody _ sts -> SqlFnBody a sts
         PlpgsqlFnBody _ vars sts -> PlpgsqlFnBody a vars sts

instance Annotated SelectExpression where
  ann a =
      case a of
        Select ann _ _ _ _ _ _ _ _ _ _ -> ann
        CombineSelect ann _ _ _ -> ann
        Values ann _ -> ann
  setAnn ex a =
    case ex of
        Select _ dis sl tref whr grp hav ord dir lim off ->
          Select a dis sl tref whr grp hav ord dir lim off
        CombineSelect _ ctype sel1 sel2 -> CombineSelect a ctype sel1 sel2
        Values _ vll -> Values a vll

instance Annotated TableRef where
  ann a =
      case a of
        Tref ann _ -> ann
        TrefAlias ann _ _ -> ann
        JoinedTref ann _ _ _ _ _ -> ann
        SubTref ann _ _ -> ann
        TrefFun ann _ -> ann
        TrefFunAlias ann _ _ -> ann
  setAnn ex a =
    case ex of
        Tref _ tbl -> Tref a tbl
        TrefAlias _ tbl alias -> TrefAlias a tbl alias
        JoinedTref _ tbl nat joinType tbl1 onExpr -> JoinedTref a tbl nat joinType tbl1 onExpr
        SubTref _ sel alias -> SubTref a sel alias
        TrefFun _ fn -> TrefFun a fn
        TrefFunAlias _ fn alias -> TrefFunAlias a fn alias
{-# LINE 277 "AstInternal.hs" #-}

{-# LINE 67 "./TypeChecking.ag" #-}

annTypesAndErrors :: (Data a, Annotated a) => a -> Type -> [TypeError]
                  -> Maybe [AnnotationElement] -> a
annTypesAndErrors item nt errs add =
    oneLevel (mkT modifier) item
    --gmapT modifier item
    --changeAnn item modifier
    where
      modifier :: [AnnotationElement] -> [AnnotationElement]
      modifier = (([TypeAnnotation nt] ++ fromMaybe [] add ++
       map TypeErrorA errs) ++)

oneLevel :: (forall a. Data a => a -> a)
         -> (forall a. Data a => a -> a)
oneLevel f = gmapT f



{-# LINE 298 "AstInternal.hs" #-}

{-# LINE 400 "./TypeChecking.ag" #-}

checkExpressionBool :: Maybe Expression -> Either [TypeError] Type
checkExpressionBool whr = do
  let ty = fromMaybe typeBool $ fmap getTypeAnnotation whr
  when (ty `notElem` [typeBool, TypeCheckFailed]) $
       Left [ExpressionMustBeBool]
  return ty
{-# LINE 308 "AstInternal.hs" #-}

{-# LINE 447 "./TypeChecking.ag" #-}

getTbCols c = unwrapSetOfComposite (getTypeAnnotation c)
{-# LINE 313 "AstInternal.hs" #-}

{-# LINE 525 "./TypeChecking.ag" #-}


getFnType :: Environment -> String -> Expression -> Either [TypeError] Type
getFnType env alias =
    either Left (Right . snd) . getFunIdens env alias

getFunIdens :: Environment -> String -> Expression -> Either [TypeError] (String,Type)
getFunIdens env alias fnVal =
   case fnVal of
       FunCall _ f _ ->
           let correlationName = if alias /= ""
                                   then alias
                                   else f
           in Right (correlationName, case getTypeAnnotation fnVal of
                SetOfType (CompositeType t) -> getCompositeType t
                SetOfType x -> UnnamedCompositeType [(correlationName,x)]
                y -> UnnamedCompositeType [(correlationName,y)])
       x -> Left [ContextError "FunCall"]
   where
     getCompositeType t =
                    case getAttrs env [Composite
                                      ,TableComposite
                                      ,ViewComposite] t of
                      Just (_,_,a@(UnnamedCompositeType _), _) -> a
                      _ -> UnnamedCompositeType []
{-# LINE 341 "AstInternal.hs" #-}

{-# LINE 584 "./TypeChecking.ag" #-}


fixStar :: Expression -> Expression
fixStar =
    everywhere (mkT fixStar')
    where
      fixStar' :: Annotation -> Annotation
      fixStar' a =
          if TypeAnnotation TypeCheckFailed `elem` a
              && any (\an ->
                       case an of
                         TypeErrorA (UnrecognisedIdentifier x) |
                           let (_,iden) = splitIdentifier x
                           in iden == "*" -> True
                         _ -> False) a
             then filter (\an -> case an of
                                   TypeAnnotation TypeCheckFailed -> False
                                   TypeErrorA (UnrecognisedIdentifier _) -> False
                                   _ -> True) a
             else a
{-# LINE 364 "AstInternal.hs" #-}

{-# LINE 679 "./TypeChecking.ag" #-}

fixedValue :: a -> a -> a -> a
fixedValue a _ _ = a
{-# LINE 370 "AstInternal.hs" #-}

{-# LINE 713 "./TypeChecking.ag" #-}

getCAtts t =
    case t of
      SetOfType (UnnamedCompositeType t) -> t
      _ -> []
{-# LINE 378 "AstInternal.hs" #-}

{-# LINE 754 "./TypeChecking.ag" #-}


{-# LINE 383 "AstInternal.hs" #-}

{-# LINE 815 "./TypeChecking.ag" #-}

getRowTypes :: [Type] -> [Type]
getRowTypes [RowCtor ts] = ts
getRowTypes ts = ts
{-# LINE 390 "AstInternal.hs" #-}
-- AttributeDef ------------------------------------------------
data AttributeDef  = AttributeDef (String) (TypeName) (Maybe Expression) (RowConstraintList) 
                   deriving ( Data,Eq,Show,Typeable)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _name _typ _check _cons )  =
    (sem_AttributeDef_AttributeDef _name (sem_TypeName _typ ) _check (sem_RowConstraintList _cons ) )
-- semantic domain
type T_AttributeDef  = Environment ->
                       ( AttributeDef,String,Environment,(Either [TypeError] Type))
data Inh_AttributeDef  = Inh_AttributeDef {env_Inh_AttributeDef :: Environment}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef,attrName_Syn_AttributeDef :: String,env_Syn_AttributeDef :: Environment,namedType_Syn_AttributeDef :: Either [TypeError] Type}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOenv,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_AttributeDef _lhsOannotatedTree _lhsOattrName _lhsOenv _lhsOnamedType ))
sem_AttributeDef_AttributeDef :: String ->
                                 T_TypeName  ->
                                 (Maybe Expression) ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef name_ typ_ check_ cons_  =
    (\ _lhsIenv ->
         (let _lhsOattrName :: String
              _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: AttributeDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _consOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: (Either [TypeError] Type)
              _consIannotatedTree :: RowConstraintList
              _consIenv :: Environment
              _lhsOattrName =
                  {-# LINE 859 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 432 "AstInternal.hs" #-}
              _lhsOnamedType =
                  {-# LINE 860 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 436 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  AttributeDef name_ _typIannotatedTree check_ _consIannotatedTree
                  {-# LINE 440 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 444 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _consIenv
                  {-# LINE 448 "AstInternal.hs" #-}
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 452 "AstInternal.hs" #-}
              _consOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 456 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
              ( _consIannotatedTree,_consIenv) =
                  (cons_ _consOenv )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOenv,_lhsOnamedType)))
-- AttributeDefList --------------------------------------------
type AttributeDefList  = [(AttributeDef)]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = Environment ->
                           ( AttributeDefList,([(String, Either [TypeError] Type)]),Environment)
data Inh_AttributeDefList  = Inh_AttributeDefList {env_Inh_AttributeDefList :: Environment}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList,attrs_Syn_AttributeDefList :: [(String, Either [TypeError] Type)],env_Syn_AttributeDefList :: Environment}
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrs,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_AttributeDefList _lhsOannotatedTree _lhsOattrs _lhsOenv ))
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOattrs :: ([(String, Either [TypeError] Type)])
              _lhsOannotatedTree :: AttributeDefList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: AttributeDef
              _hdIattrName :: String
              _hdIenv :: Environment
              _hdInamedType :: (Either [TypeError] Type)
              _tlIannotatedTree :: AttributeDefList
              _tlIattrs :: ([(String, Either [TypeError] Type)])
              _tlIenv :: Environment
              _lhsOattrs =
                  {-# LINE 869 "./TypeChecking.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 501 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 505 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 509 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 513 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 517 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 521 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIattrName,_hdIenv,_hdInamedType) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIattrs,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOenv)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOattrs :: ([(String, Either [TypeError] Type)])
              _lhsOannotatedTree :: AttributeDefList
              _lhsOenv :: Environment
              _lhsOattrs =
                  {-# LINE 870 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 536 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 540 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 544 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 548 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOenv)))
-- Cascade -----------------------------------------------------
data Cascade  = Cascade 
              | Restrict 
              deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Cascade :: Cascade  ->
               T_Cascade 
sem_Cascade (Cascade )  =
    (sem_Cascade_Cascade )
sem_Cascade (Restrict )  =
    (sem_Cascade_Restrict )
-- semantic domain
type T_Cascade  = Environment ->
                  ( Cascade,Environment)
data Inh_Cascade  = Inh_Cascade {env_Inh_Cascade :: Environment}
data Syn_Cascade  = Syn_Cascade {annotatedTree_Syn_Cascade :: Cascade,env_Syn_Cascade :: Environment}
wrap_Cascade :: T_Cascade  ->
                Inh_Cascade  ->
                Syn_Cascade 
wrap_Cascade sem (Inh_Cascade _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Cascade _lhsOannotatedTree _lhsOenv ))
sem_Cascade_Cascade :: T_Cascade 
sem_Cascade_Cascade  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Cascade
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Cascade
                  {-# LINE 581 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 585 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 589 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Cascade_Restrict :: T_Cascade 
sem_Cascade_Restrict  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Cascade
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Restrict
                  {-# LINE 599 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 603 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 607 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- CaseExpressionList ------------------------------------------
type CaseExpressionList  = [(Expression)]
-- cata
sem_CaseExpressionList :: CaseExpressionList  ->
                          T_CaseExpressionList 
sem_CaseExpressionList list  =
    (Prelude.foldr sem_CaseExpressionList_Cons sem_CaseExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_CaseExpressionList  = Environment ->
                             ( CaseExpressionList,Environment)
data Inh_CaseExpressionList  = Inh_CaseExpressionList {env_Inh_CaseExpressionList :: Environment}
data Syn_CaseExpressionList  = Syn_CaseExpressionList {annotatedTree_Syn_CaseExpressionList :: CaseExpressionList,env_Syn_CaseExpressionList :: Environment}
wrap_CaseExpressionList :: T_CaseExpressionList  ->
                           Inh_CaseExpressionList  ->
                           Syn_CaseExpressionList 
wrap_CaseExpressionList sem (Inh_CaseExpressionList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_CaseExpressionList _lhsOannotatedTree _lhsOenv ))
sem_CaseExpressionList_Cons :: T_Expression  ->
                               T_CaseExpressionList  ->
                               T_CaseExpressionList 
sem_CaseExpressionList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CaseExpressionList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: Expression
              _hdIenv :: Environment
              _hdIliftedColumnName :: String
              _tlIannotatedTree :: CaseExpressionList
              _tlIenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 645 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 649 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 653 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 657 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 661 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv,_hdIliftedColumnName) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_CaseExpressionList_Nil :: T_CaseExpressionList 
sem_CaseExpressionList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CaseExpressionList
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 675 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 679 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 683 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- CaseExpressionListExpressionPair ----------------------------
type CaseExpressionListExpressionPair  = ( (CaseExpressionList),(Expression))
-- cata
sem_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair  ->
                                        T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair ( x1,x2)  =
    (sem_CaseExpressionListExpressionPair_Tuple (sem_CaseExpressionList x1 ) (sem_Expression x2 ) )
-- semantic domain
type T_CaseExpressionListExpressionPair  = Environment ->
                                           ( CaseExpressionListExpressionPair,Environment)
data Inh_CaseExpressionListExpressionPair  = Inh_CaseExpressionListExpressionPair {env_Inh_CaseExpressionListExpressionPair :: Environment}
data Syn_CaseExpressionListExpressionPair  = Syn_CaseExpressionListExpressionPair {annotatedTree_Syn_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair,env_Syn_CaseExpressionListExpressionPair :: Environment}
wrap_CaseExpressionListExpressionPair :: T_CaseExpressionListExpressionPair  ->
                                         Inh_CaseExpressionListExpressionPair  ->
                                         Syn_CaseExpressionListExpressionPair 
wrap_CaseExpressionListExpressionPair sem (Inh_CaseExpressionListExpressionPair _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_CaseExpressionListExpressionPair _lhsOannotatedTree _lhsOenv ))
sem_CaseExpressionListExpressionPair_Tuple :: T_CaseExpressionList  ->
                                              T_Expression  ->
                                              T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair_Tuple x1_ x2_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CaseExpressionListExpressionPair
              _lhsOenv :: Environment
              _x1Oenv :: Environment
              _x2Oenv :: Environment
              _x1IannotatedTree :: CaseExpressionList
              _x1Ienv :: Environment
              _x2IannotatedTree :: Expression
              _x2Ienv :: Environment
              _x2IliftedColumnName :: String
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 721 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 725 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 729 "AstInternal.hs" #-}
              _x1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 733 "AstInternal.hs" #-}
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x1Ienv
                  {-# LINE 737 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1Ienv) =
                  (x1_ _x1Oenv )
              ( _x2IannotatedTree,_x2Ienv,_x2IliftedColumnName) =
                  (x2_ _x2Oenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- CaseExpressionListExpressionPairList ------------------------
type CaseExpressionListExpressionPairList  = [(CaseExpressionListExpressionPair)]
-- cata
sem_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList  ->
                                            T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList list  =
    (Prelude.foldr sem_CaseExpressionListExpressionPairList_Cons sem_CaseExpressionListExpressionPairList_Nil (Prelude.map sem_CaseExpressionListExpressionPair list) )
-- semantic domain
type T_CaseExpressionListExpressionPairList  = Environment ->
                                               ( CaseExpressionListExpressionPairList,Environment)
data Inh_CaseExpressionListExpressionPairList  = Inh_CaseExpressionListExpressionPairList {env_Inh_CaseExpressionListExpressionPairList :: Environment}
data Syn_CaseExpressionListExpressionPairList  = Syn_CaseExpressionListExpressionPairList {annotatedTree_Syn_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList,env_Syn_CaseExpressionListExpressionPairList :: Environment}
wrap_CaseExpressionListExpressionPairList :: T_CaseExpressionListExpressionPairList  ->
                                             Inh_CaseExpressionListExpressionPairList  ->
                                             Syn_CaseExpressionListExpressionPairList 
wrap_CaseExpressionListExpressionPairList sem (Inh_CaseExpressionListExpressionPairList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_CaseExpressionListExpressionPairList _lhsOannotatedTree _lhsOenv ))
sem_CaseExpressionListExpressionPairList_Cons :: T_CaseExpressionListExpressionPair  ->
                                                 T_CaseExpressionListExpressionPairList  ->
                                                 T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CaseExpressionListExpressionPairList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: CaseExpressionListExpressionPair
              _hdIenv :: Environment
              _tlIannotatedTree :: CaseExpressionListExpressionPairList
              _tlIenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 778 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 782 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 786 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 790 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 794 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_CaseExpressionListExpressionPairList_Nil :: T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CaseExpressionListExpressionPairList
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 808 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 812 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 816 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- CombineType -------------------------------------------------
data CombineType  = Except 
                  | Intersect 
                  | Union 
                  | UnionAll 
                  deriving ( Data,Eq,Show,Typeable)
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
type T_CombineType  = Environment ->
                      ( CombineType,Environment)
data Inh_CombineType  = Inh_CombineType {env_Inh_CombineType :: Environment}
data Syn_CombineType  = Syn_CombineType {annotatedTree_Syn_CombineType :: CombineType,env_Syn_CombineType :: Environment}
wrap_CombineType :: T_CombineType  ->
                    Inh_CombineType  ->
                    Syn_CombineType 
wrap_CombineType sem (Inh_CombineType _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_CombineType _lhsOannotatedTree _lhsOenv ))
sem_CombineType_Except :: T_CombineType 
sem_CombineType_Except  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Except
                  {-# LINE 855 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 859 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 863 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_CombineType_Intersect :: T_CombineType 
sem_CombineType_Intersect  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Intersect
                  {-# LINE 873 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 877 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 881 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_CombineType_Union :: T_CombineType 
sem_CombineType_Union  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Union
                  {-# LINE 891 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 895 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 899 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_CombineType_UnionAll :: T_CombineType 
sem_CombineType_UnionAll  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  UnionAll
                  {-# LINE 909 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 913 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 917 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Constraint --------------------------------------------------
data Constraint  = CheckConstraint (Expression) 
                 | PrimaryKeyConstraint (StringList) 
                 | ReferenceConstraint (StringList) (String) (StringList) (Cascade) (Cascade) 
                 | UniqueConstraint (StringList) 
                 deriving ( Data,Eq,Show,Typeable)
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
type T_Constraint  = Environment ->
                     ( Constraint,Environment)
data Inh_Constraint  = Inh_Constraint {env_Inh_Constraint :: Environment}
data Syn_Constraint  = Syn_Constraint {annotatedTree_Syn_Constraint :: Constraint,env_Syn_Constraint :: Environment}
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Constraint _lhsOannotatedTree _lhsOenv ))
sem_Constraint_CheckConstraint :: T_Expression  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint expression_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOenv :: Environment
              _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIenv :: Environment
              _expressionIliftedColumnName :: String
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CheckConstraint _expressionIannotatedTree
                  {-# LINE 961 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 965 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _expressionIenv
                  {-# LINE 969 "AstInternal.hs" #-}
              _expressionOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 973 "AstInternal.hs" #-}
              ( _expressionIannotatedTree,_expressionIenv,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Constraint_PrimaryKeyConstraint :: T_StringList  ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint stringList_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOenv :: Environment
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIenv :: Environment
              _stringListIstrings :: ([String])
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PrimaryKeyConstraint _stringListIannotatedTree
                  {-# LINE 990 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 994 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 998 "AstInternal.hs" #-}
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1002 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIenv,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Constraint_ReferenceConstraint :: T_StringList  ->
                                      String ->
                                      T_StringList  ->
                                      T_Cascade  ->
                                      T_Cascade  ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOenv :: Environment
              _attsOenv :: Environment
              _tableAttsOenv :: Environment
              _onUpdateOenv :: Environment
              _onDeleteOenv :: Environment
              _attsIannotatedTree :: StringList
              _attsIenv :: Environment
              _attsIstrings :: ([String])
              _tableAttsIannotatedTree :: StringList
              _tableAttsIenv :: Environment
              _tableAttsIstrings :: ([String])
              _onUpdateIannotatedTree :: Cascade
              _onUpdateIenv :: Environment
              _onDeleteIannotatedTree :: Cascade
              _onDeleteIenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ReferenceConstraint _attsIannotatedTree table_ _tableAttsIannotatedTree _onUpdateIannotatedTree _onDeleteIannotatedTree
                  {-# LINE 1033 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1037 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onDeleteIenv
                  {-# LINE 1041 "AstInternal.hs" #-}
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1045 "AstInternal.hs" #-}
              _tableAttsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 1049 "AstInternal.hs" #-}
              _onUpdateOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tableAttsIenv
                  {-# LINE 1053 "AstInternal.hs" #-}
              _onDeleteOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onUpdateIenv
                  {-# LINE 1057 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIenv,_attsIstrings) =
                  (atts_ _attsOenv )
              ( _tableAttsIannotatedTree,_tableAttsIenv,_tableAttsIstrings) =
                  (tableAtts_ _tableAttsOenv )
              ( _onUpdateIannotatedTree,_onUpdateIenv) =
                  (onUpdate_ _onUpdateOenv )
              ( _onDeleteIannotatedTree,_onDeleteIenv) =
                  (onDelete_ _onDeleteOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Constraint_UniqueConstraint :: T_StringList  ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint stringList_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOenv :: Environment
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIenv :: Environment
              _stringListIstrings :: ([String])
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  UniqueConstraint _stringListIannotatedTree
                  {-# LINE 1080 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1084 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 1088 "AstInternal.hs" #-}
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1092 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIenv,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ConstraintList ----------------------------------------------
type ConstraintList  = [(Constraint)]
-- cata
sem_ConstraintList :: ConstraintList  ->
                      T_ConstraintList 
sem_ConstraintList list  =
    (Prelude.foldr sem_ConstraintList_Cons sem_ConstraintList_Nil (Prelude.map sem_Constraint list) )
-- semantic domain
type T_ConstraintList  = Environment ->
                         ( ConstraintList,Environment)
data Inh_ConstraintList  = Inh_ConstraintList {env_Inh_ConstraintList :: Environment}
data Syn_ConstraintList  = Syn_ConstraintList {annotatedTree_Syn_ConstraintList :: ConstraintList,env_Syn_ConstraintList :: Environment}
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_ConstraintList _lhsOannotatedTree _lhsOenv ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ConstraintList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: Constraint
              _hdIenv :: Environment
              _tlIannotatedTree :: ConstraintList
              _tlIenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1131 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1135 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 1139 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1143 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 1147 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ConstraintList
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 1161 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1165 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1169 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- CopySource --------------------------------------------------
data CopySource  = CopyFilename (String) 
                 | Stdin 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_CopySource :: CopySource  ->
                  T_CopySource 
sem_CopySource (CopyFilename _string )  =
    (sem_CopySource_CopyFilename _string )
sem_CopySource (Stdin )  =
    (sem_CopySource_Stdin )
-- semantic domain
type T_CopySource  = Environment ->
                     ( CopySource,Environment)
data Inh_CopySource  = Inh_CopySource {env_Inh_CopySource :: Environment}
data Syn_CopySource  = Syn_CopySource {annotatedTree_Syn_CopySource :: CopySource,env_Syn_CopySource :: Environment}
wrap_CopySource :: T_CopySource  ->
                   Inh_CopySource  ->
                   Syn_CopySource 
wrap_CopySource sem (Inh_CopySource _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_CopySource _lhsOannotatedTree _lhsOenv ))
sem_CopySource_CopyFilename :: String ->
                               T_CopySource 
sem_CopySource_CopyFilename string_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CopySource
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CopyFilename string_
                  {-# LINE 1203 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1207 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1211 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_CopySource_Stdin :: T_CopySource 
sem_CopySource_Stdin  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CopySource
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Stdin
                  {-# LINE 1221 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1225 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1229 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Direction ---------------------------------------------------
data Direction  = Asc 
                | Desc 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Direction :: Direction  ->
                 T_Direction 
sem_Direction (Asc )  =
    (sem_Direction_Asc )
sem_Direction (Desc )  =
    (sem_Direction_Desc )
-- semantic domain
type T_Direction  = Environment ->
                    ( Direction,Environment)
data Inh_Direction  = Inh_Direction {env_Inh_Direction :: Environment}
data Syn_Direction  = Syn_Direction {annotatedTree_Syn_Direction :: Direction,env_Syn_Direction :: Environment}
wrap_Direction :: T_Direction  ->
                  Inh_Direction  ->
                  Syn_Direction 
wrap_Direction sem (Inh_Direction _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Direction _lhsOannotatedTree _lhsOenv ))
sem_Direction_Asc :: T_Direction 
sem_Direction_Asc  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Direction
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Asc
                  {-# LINE 1262 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1266 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1270 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Direction_Desc :: T_Direction 
sem_Direction_Desc  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Direction
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Desc
                  {-# LINE 1280 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1284 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1288 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Distinct ----------------------------------------------------
data Distinct  = Distinct 
               | Dupes 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Distinct :: Distinct  ->
                T_Distinct 
sem_Distinct (Distinct )  =
    (sem_Distinct_Distinct )
sem_Distinct (Dupes )  =
    (sem_Distinct_Dupes )
-- semantic domain
type T_Distinct  = Environment ->
                   ( Distinct,Environment)
data Inh_Distinct  = Inh_Distinct {env_Inh_Distinct :: Environment}
data Syn_Distinct  = Syn_Distinct {annotatedTree_Syn_Distinct :: Distinct,env_Syn_Distinct :: Environment}
wrap_Distinct :: T_Distinct  ->
                 Inh_Distinct  ->
                 Syn_Distinct 
wrap_Distinct sem (Inh_Distinct _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Distinct _lhsOannotatedTree _lhsOenv ))
sem_Distinct_Distinct :: T_Distinct 
sem_Distinct_Distinct  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Distinct
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Distinct
                  {-# LINE 1321 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1325 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1329 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Distinct_Dupes :: T_Distinct 
sem_Distinct_Dupes  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Distinct
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Dupes
                  {-# LINE 1339 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1343 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1347 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- DropType ----------------------------------------------------
data DropType  = Domain 
               | Table 
               | Type 
               | View 
               deriving ( Data,Eq,Show,Typeable)
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
type T_DropType  = Environment ->
                   ( DropType,Environment)
data Inh_DropType  = Inh_DropType {env_Inh_DropType :: Environment}
data Syn_DropType  = Syn_DropType {annotatedTree_Syn_DropType :: DropType,env_Syn_DropType :: Environment}
wrap_DropType :: T_DropType  ->
                 Inh_DropType  ->
                 Syn_DropType 
wrap_DropType sem (Inh_DropType _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_DropType _lhsOannotatedTree _lhsOenv ))
sem_DropType_Domain :: T_DropType 
sem_DropType_Domain  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Domain
                  {-# LINE 1386 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1390 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1394 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_DropType_Table :: T_DropType 
sem_DropType_Table  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Table
                  {-# LINE 1404 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1408 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1412 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_DropType_Type :: T_DropType 
sem_DropType_Type  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Type
                  {-# LINE 1422 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1426 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1430 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_DropType_View :: T_DropType 
sem_DropType_View  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  View
                  {-# LINE 1440 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1444 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1448 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
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
                 deriving ( Data,Eq,Show,Typeable)
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
type T_Expression  = Environment ->
                     ( Expression,Environment,String)
data Inh_Expression  = Inh_Expression {env_Inh_Expression :: Environment}
data Syn_Expression  = Syn_Expression {annotatedTree_Syn_Expression :: Expression,env_Syn_Expression :: Environment,liftedColumnName_Syn_Expression :: String}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression sem (Inh_Expression _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName) =
             (sem _lhsIenv )
     in  (Syn_Expression _lhsOannotatedTree _lhsOenv _lhsOliftedColumnName ))
sem_Expression_BooleanLit :: Annotation ->
                             Bool ->
                             T_Expression 
sem_Expression_BooleanLit ann_ b_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1526 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 107 "./TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 1530 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 115 "./TypeChecking.ag" #-}
                  Right typeBool
                  {-# LINE 1534 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1538 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 1542 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1546 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_Case :: Annotation ->
                       T_CaseExpressionListExpressionPairList  ->
                       T_MaybeExpression  ->
                       T_Expression 
sem_Expression_Case ann_ cases_ els_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _casesOenv :: Environment
              _elsOenv :: Environment
              _casesIannotatedTree :: CaseExpressionListExpressionPairList
              _casesIenv :: Environment
              _elsIannotatedTree :: MaybeExpression
              _elsIenv :: Environment
              _elsIexprType :: (Maybe Type)
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1570 "AstInternal.hs" #-}
              _whenTypes =
                  {-# LINE 185 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
                  {-# LINE 1575 "AstInternal.hs" #-}
              _thenTypes =
                  {-# LINE 187 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
                  {-# LINE 1581 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 193 "./TypeChecking.ag" #-}
                  checkTypes _whenTypes     $ do
                     when (any (/= typeBool) _whenTypes    ) $
                       Left [WrongTypes typeBool _whenTypes    ]
                     checkTypes _thenTypes     $
                              resolveResultSetType
                                _lhsIenv
                                _thenTypes
                  {-# LINE 1591 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 201 "./TypeChecking.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1595 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1599 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1603 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 1607 "AstInternal.hs" #-}
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1611 "AstInternal.hs" #-}
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 1615 "AstInternal.hs" #-}
              ( _casesIannotatedTree,_casesIenv) =
                  (cases_ _casesOenv )
              ( _elsIannotatedTree,_elsIenv,_elsIexprType) =
                  (els_ _elsOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_CaseSimple :: Annotation ->
                             T_Expression  ->
                             T_CaseExpressionListExpressionPairList  ->
                             T_MaybeExpression  ->
                             T_Expression 
sem_Expression_CaseSimple ann_ value_ cases_ els_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _valueOenv :: Environment
              _casesOenv :: Environment
              _elsOenv :: Environment
              _valueIannotatedTree :: Expression
              _valueIenv :: Environment
              _valueIliftedColumnName :: String
              _casesIannotatedTree :: CaseExpressionListExpressionPairList
              _casesIenv :: Environment
              _elsIannotatedTree :: MaybeExpression
              _elsIenv :: Environment
              _elsIexprType :: (Maybe Type)
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1648 "AstInternal.hs" #-}
              _whenTypes =
                  {-# LINE 185 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
                  {-# LINE 1653 "AstInternal.hs" #-}
              _thenTypes =
                  {-# LINE 187 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
                  {-# LINE 1659 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 206 "./TypeChecking.ag" #-}
                  checkTypes _whenTypes     $ do
                  checkWhenTypes <- resolveResultSetType
                                         _lhsIenv
                                         (getTypeAnnotation _valueIannotatedTree: _whenTypes    )
                  checkTypes _thenTypes     $
                             resolveResultSetType
                                      _lhsIenv
                                      _thenTypes
                  {-# LINE 1670 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 215 "./TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1674 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  _valueIliftedColumnName
                  {-# LINE 1678 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1682 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 1686 "AstInternal.hs" #-}
              _valueOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1690 "AstInternal.hs" #-}
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valueIenv
                  {-# LINE 1694 "AstInternal.hs" #-}
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 1698 "AstInternal.hs" #-}
              ( _valueIannotatedTree,_valueIenv,_valueIliftedColumnName) =
                  (value_ _valueOenv )
              ( _casesIannotatedTree,_casesIenv) =
                  (cases_ _casesOenv )
              ( _elsIannotatedTree,_elsIenv,_elsIexprType) =
                  (els_ _elsOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_Cast :: Annotation ->
                       T_Expression  ->
                       T_TypeName  ->
                       T_Expression 
sem_Expression_Cast ann_ expr_ tn_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _exprOenv :: Environment
              _tnOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIenv :: Environment
              _exprIliftedColumnName :: String
              _tnIannotatedTree :: TypeName
              _tnIenv :: Environment
              _tnInamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1729 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 127 "./TypeChecking.ag" #-}
                  _tnInamedType
                  {-# LINE 1733 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 128 "./TypeChecking.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 1737 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 695 "./TypeChecking.ag" #-}
                  case _tnIannotatedTree of
                    SimpleTypeName tn -> tn
                    _ -> ""
                  {-# LINE 1743 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 1747 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tnIenv
                  {-# LINE 1751 "AstInternal.hs" #-}
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1755 "AstInternal.hs" #-}
              _tnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 1759 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              ( _tnIannotatedTree,_tnIenv,_tnInamedType) =
                  (tn_ _tnOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_Exists :: Annotation ->
                         T_SelectExpression  ->
                         T_Expression 
sem_Expression_Exists ann_ sel_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1782 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 231 "./TypeChecking.ag" #-}
                  Right typeBool
                  {-# LINE 1786 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 232 "./TypeChecking.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 1790 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1794 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 1798 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 1802 "AstInternal.hs" #-}
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1806 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIenv) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_FloatLit :: Annotation ->
                           Double ->
                           T_Expression 
sem_Expression_FloatLit ann_ d_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1824 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 105 "./TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 1828 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 114 "./TypeChecking.ag" #-}
                  Right typeNumeric
                  {-# LINE 1832 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1836 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 1840 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1844 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_FunCall :: Annotation ->
                          String ->
                          T_ExpressionList  ->
                          T_Expression 
sem_Expression_FunCall ann_ funName_ args_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _argsOenv :: Environment
              _argsIannotatedTree :: ExpressionList
              _argsIenv :: Environment
              _argsItypeList :: ([Type])
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1865 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 164 "./TypeChecking.ag" #-}
                  checkTypes _argsItypeList $
                    typeCheckFunCall
                      _lhsIenv
                      funName_
                      _argsItypeList
                  {-# LINE 1873 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 169 "./TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 1877 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 691 "./TypeChecking.ag" #-}
                  if isOperatorName funName_
                     then ""
                     else funName_
                  {-# LINE 1883 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 1887 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _argsIenv
                  {-# LINE 1891 "AstInternal.hs" #-}
              _argsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1895 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsIenv,_argsItypeList) =
                  (args_ _argsOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_Identifier :: Annotation ->
                             String ->
                             T_Expression 
sem_Expression_Identifier ann_ i_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1913 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 225 "./TypeChecking.ag" #-}
                  let (correlationName,iden) = splitIdentifier i_
                  in envLookupID _lhsIenv correlationName iden
                  {-# LINE 1918 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 227 "./TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 1922 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 690 "./TypeChecking.ag" #-}
                  i_
                  {-# LINE 1926 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 1930 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1934 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_InPredicate :: Annotation ->
                              T_Expression  ->
                              Bool ->
                              T_InList  ->
                              T_Expression 
sem_Expression_InPredicate ann_ expr_ i_ list_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _exprOenv :: Environment
              _listOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIenv :: Environment
              _exprIliftedColumnName :: String
              _listIannotatedTree :: InList
              _listIenv :: Environment
              _listIlistType :: (Either [TypeError] Type)
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1960 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 259 "./TypeChecking.ag" #-}
                  do
                    lt <- _listIlistType
                    ty <- resolveResultSetType
                            _lhsIenv
                            [getTypeAnnotation _exprIannotatedTree, lt]
                    return typeBool
                  {-# LINE 1969 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 265 "./TypeChecking.ag" #-}
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                  {-# LINE 1973 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  _exprIliftedColumnName
                  {-# LINE 1977 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                  {-# LINE 1981 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _listIenv
                  {-# LINE 1985 "AstInternal.hs" #-}
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1989 "AstInternal.hs" #-}
              _listOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 1993 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              ( _listIannotatedTree,_listIenv,_listIlistType) =
                  (list_ _listOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_IntegerLit :: Annotation ->
                             Integer ->
                             T_Expression 
sem_Expression_IntegerLit ann_ i_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2013 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 101 "./TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 2017 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 112 "./TypeChecking.ag" #-}
                  Right typeInt
                  {-# LINE 2021 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2025 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 2029 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2033 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_NullLit :: Annotation ->
                          T_Expression 
sem_Expression_NullLit ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2048 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 109 "./TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 2052 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 117 "./TypeChecking.ag" #-}
                  Right UnknownStringLit
                  {-# LINE 2056 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2060 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 2064 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2068 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_PositionalArg :: Annotation ->
                                Integer ->
                                T_Expression 
sem_Expression_PositionalArg ann_ p_  =
    (\ _lhsIenv ->
         (let _lhsOliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOenv :: Environment
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2081 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 2085 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2089 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2093 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_ScalarSubQuery :: Annotation ->
                                 T_SelectExpression  ->
                                 T_Expression 
sem_Expression_ScalarSubQuery ann_ sel_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2112 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 243 "./TypeChecking.ag" #-}
                  let selType = getTypeAnnotation _selIannotatedTree
                  in checkTypes [selType]
                       $ do
                         f <- map snd <$> unwrapSetOfComposite selType
                         case length f of
                              0 -> Left [InternalError "no columns in scalar subquery?"]
                              1 -> Right $ head f
                              _ -> Right $ RowCtor f
                  {-# LINE 2123 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 252 "./TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 2127 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2131 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 2135 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 2139 "AstInternal.hs" #-}
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2143 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIenv) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_StringLit :: Annotation ->
                            String ->
                            String ->
                            T_Expression 
sem_Expression_StringLit ann_ quote_ value_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 90 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2162 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 103 "./TypeChecking.ag" #-}
                  StringLit ann_ quote_ value_
                  {-# LINE 2166 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 113 "./TypeChecking.ag" #-}
                  Right UnknownStringLit
                  {-# LINE 2170 "AstInternal.hs" #-}
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2174 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  StringLit ann_ quote_ value_
                  {-# LINE 2178 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2182 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_WindowFn :: Annotation ->
                           T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList  ->
                           T_Direction  ->
                           T_Expression 
sem_Expression_WindowFn ann_ fn_ partitionBy_ orderBy_ dir_  =
    (\ _lhsIenv ->
         (let _lhsOliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOenv :: Environment
              _fnOenv :: Environment
              _partitionByOenv :: Environment
              _orderByOenv :: Environment
              _dirOenv :: Environment
              _fnIannotatedTree :: Expression
              _fnIenv :: Environment
              _fnIliftedColumnName :: String
              _partitionByIannotatedTree :: ExpressionList
              _partitionByIenv :: Environment
              _partitionByItypeList :: ([Type])
              _orderByIannotatedTree :: ExpressionList
              _orderByIenv :: Environment
              _orderByItypeList :: ([Type])
              _dirIannotatedTree :: Direction
              _dirIenv :: Environment
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  _fnIliftedColumnName
                  {-# LINE 2213 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree _dirIannotatedTree
                  {-# LINE 2217 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2221 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _dirIenv
                  {-# LINE 2225 "AstInternal.hs" #-}
              _fnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2229 "AstInternal.hs" #-}
              _partitionByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fnIenv
                  {-# LINE 2233 "AstInternal.hs" #-}
              _orderByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _partitionByIenv
                  {-# LINE 2237 "AstInternal.hs" #-}
              _dirOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _orderByIenv
                  {-# LINE 2241 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIenv,_fnIliftedColumnName) =
                  (fn_ _fnOenv )
              ( _partitionByIannotatedTree,_partitionByIenv,_partitionByItypeList) =
                  (partitionBy_ _partitionByOenv )
              ( _orderByIannotatedTree,_orderByIenv,_orderByItypeList) =
                  (orderBy_ _orderByOenv )
              ( _dirIannotatedTree,_dirIenv) =
                  (dir_ _dirOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
-- ExpressionList ----------------------------------------------
type ExpressionList  = [(Expression)]
-- cata
sem_ExpressionList :: ExpressionList  ->
                      T_ExpressionList 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_ExpressionList  = Environment ->
                         ( ExpressionList,Environment,([Type]))
data Inh_ExpressionList  = Inh_ExpressionList {env_Inh_ExpressionList :: Environment}
data Syn_ExpressionList  = Syn_ExpressionList {annotatedTree_Syn_ExpressionList :: ExpressionList,env_Syn_ExpressionList :: Environment,typeList_Syn_ExpressionList :: [Type]}
wrap_ExpressionList :: T_ExpressionList  ->
                       Inh_ExpressionList  ->
                       Syn_ExpressionList 
wrap_ExpressionList sem (Inh_ExpressionList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOtypeList) =
             (sem _lhsIenv )
     in  (Syn_ExpressionList _lhsOannotatedTree _lhsOenv _lhsOtypeList ))
sem_ExpressionList_Cons :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList 
sem_ExpressionList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOtypeList :: ([Type])
              _lhsOannotatedTree :: ExpressionList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: Expression
              _hdIenv :: Environment
              _hdIliftedColumnName :: String
              _tlIannotatedTree :: ExpressionList
              _tlIenv :: Environment
              _tlItypeList :: ([Type])
              _lhsOtypeList =
                  {-# LINE 300 "./TypeChecking.ag" #-}
                  getTypeAnnotation _hdIannotatedTree : _tlItypeList
                  {-# LINE 2289 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2293 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2297 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 2301 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2305 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 2309 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv,_hdIliftedColumnName) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv,_tlItypeList) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOtypeList)))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOtypeList :: ([Type])
              _lhsOannotatedTree :: ExpressionList
              _lhsOenv :: Environment
              _lhsOtypeList =
                  {-# LINE 301 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2324 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2328 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2332 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2336 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOtypeList)))
-- ExpressionListList ------------------------------------------
type ExpressionListList  = [(ExpressionList)]
-- cata
sem_ExpressionListList :: ExpressionListList  ->
                          T_ExpressionListList 
sem_ExpressionListList list  =
    (Prelude.foldr sem_ExpressionListList_Cons sem_ExpressionListList_Nil (Prelude.map sem_ExpressionList list) )
-- semantic domain
type T_ExpressionListList  = Environment ->
                             ( ExpressionListList,Environment,([[Type]]))
data Inh_ExpressionListList  = Inh_ExpressionListList {env_Inh_ExpressionListList :: Environment}
data Syn_ExpressionListList  = Syn_ExpressionListList {annotatedTree_Syn_ExpressionListList :: ExpressionListList,env_Syn_ExpressionListList :: Environment,typeListList_Syn_ExpressionListList :: [[Type]]}
wrap_ExpressionListList :: T_ExpressionListList  ->
                           Inh_ExpressionListList  ->
                           Syn_ExpressionListList 
wrap_ExpressionListList sem (Inh_ExpressionListList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOtypeListList) =
             (sem _lhsIenv )
     in  (Syn_ExpressionListList _lhsOannotatedTree _lhsOenv _lhsOtypeListList ))
sem_ExpressionListList_Cons :: T_ExpressionList  ->
                               T_ExpressionListList  ->
                               T_ExpressionListList 
sem_ExpressionListList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOtypeListList :: ([[Type]])
              _lhsOannotatedTree :: ExpressionListList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: ExpressionList
              _hdIenv :: Environment
              _hdItypeList :: ([Type])
              _tlIannotatedTree :: ExpressionListList
              _tlIenv :: Environment
              _tlItypeListList :: ([[Type]])
              _lhsOtypeListList =
                  {-# LINE 311 "./TypeChecking.ag" #-}
                  _hdItypeList : _tlItypeListList
                  {-# LINE 2376 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2380 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2384 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 2388 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2392 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 2396 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv,_hdItypeList) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv,_tlItypeListList) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOtypeListList)))
sem_ExpressionListList_Nil :: T_ExpressionListList 
sem_ExpressionListList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOtypeListList :: ([[Type]])
              _lhsOannotatedTree :: ExpressionListList
              _lhsOenv :: Environment
              _lhsOtypeListList =
                  {-# LINE 312 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2411 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2415 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2419 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2423 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOtypeListList)))
-- ExpressionListStatementListPair -----------------------------
type ExpressionListStatementListPair  = ( (ExpressionList),(StatementList))
-- cata
sem_ExpressionListStatementListPair :: ExpressionListStatementListPair  ->
                                       T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair ( x1,x2)  =
    (sem_ExpressionListStatementListPair_Tuple (sem_ExpressionList x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionListStatementListPair  = Environment ->
                                          ( ExpressionListStatementListPair,Environment)
data Inh_ExpressionListStatementListPair  = Inh_ExpressionListStatementListPair {env_Inh_ExpressionListStatementListPair :: Environment}
data Syn_ExpressionListStatementListPair  = Syn_ExpressionListStatementListPair {annotatedTree_Syn_ExpressionListStatementListPair :: ExpressionListStatementListPair,env_Syn_ExpressionListStatementListPair :: Environment}
wrap_ExpressionListStatementListPair :: T_ExpressionListStatementListPair  ->
                                        Inh_ExpressionListStatementListPair  ->
                                        Syn_ExpressionListStatementListPair 
wrap_ExpressionListStatementListPair sem (Inh_ExpressionListStatementListPair _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_ExpressionListStatementListPair _lhsOannotatedTree _lhsOenv ))
sem_ExpressionListStatementListPair_Tuple :: T_ExpressionList  ->
                                             T_StatementList  ->
                                             T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIenv ->
         (let _x2OenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: ExpressionListStatementListPair
              _lhsOenv :: Environment
              _x1Oenv :: Environment
              _x2Oenv :: Environment
              _x1IannotatedTree :: ExpressionList
              _x1Ienv :: Environment
              _x1ItypeList :: ([Type])
              _x2IannotatedTree :: StatementList
              _x2Ienv :: Environment
              _x2IenvUpdates :: ([EnvironmentUpdate])
              _x2OenvUpdates =
                  {-# LINE 364 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2463 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 2467 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2471 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 2475 "AstInternal.hs" #-}
              _x1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2479 "AstInternal.hs" #-}
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x1Ienv
                  {-# LINE 2483 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1Ienv,_x1ItypeList) =
                  (x1_ _x1Oenv )
              ( _x2IannotatedTree,_x2Ienv,_x2IenvUpdates) =
                  (x2_ _x2Oenv _x2OenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ExpressionListStatementListPairList -------------------------
type ExpressionListStatementListPairList  = [(ExpressionListStatementListPair)]
-- cata
sem_ExpressionListStatementListPairList :: ExpressionListStatementListPairList  ->
                                           T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList list  =
    (Prelude.foldr sem_ExpressionListStatementListPairList_Cons sem_ExpressionListStatementListPairList_Nil (Prelude.map sem_ExpressionListStatementListPair list) )
-- semantic domain
type T_ExpressionListStatementListPairList  = Environment ->
                                              ( ExpressionListStatementListPairList,Environment)
data Inh_ExpressionListStatementListPairList  = Inh_ExpressionListStatementListPairList {env_Inh_ExpressionListStatementListPairList :: Environment}
data Syn_ExpressionListStatementListPairList  = Syn_ExpressionListStatementListPairList {annotatedTree_Syn_ExpressionListStatementListPairList :: ExpressionListStatementListPairList,env_Syn_ExpressionListStatementListPairList :: Environment}
wrap_ExpressionListStatementListPairList :: T_ExpressionListStatementListPairList  ->
                                            Inh_ExpressionListStatementListPairList  ->
                                            Syn_ExpressionListStatementListPairList 
wrap_ExpressionListStatementListPairList sem (Inh_ExpressionListStatementListPairList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_ExpressionListStatementListPairList _lhsOannotatedTree _lhsOenv ))
sem_ExpressionListStatementListPairList_Cons :: T_ExpressionListStatementListPair  ->
                                                T_ExpressionListStatementListPairList  ->
                                                T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionListStatementListPairList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: ExpressionListStatementListPair
              _hdIenv :: Environment
              _tlIannotatedTree :: ExpressionListStatementListPairList
              _tlIenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2524 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2528 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 2532 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2536 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 2540 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_ExpressionListStatementListPairList_Nil :: T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionListStatementListPairList
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2554 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2558 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2562 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ExpressionRoot ----------------------------------------------
data ExpressionRoot  = ExpressionRoot (Expression) 
                     deriving ( Show)
-- cata
sem_ExpressionRoot :: ExpressionRoot  ->
                      T_ExpressionRoot 
sem_ExpressionRoot (ExpressionRoot _expr )  =
    (sem_ExpressionRoot_ExpressionRoot (sem_Expression _expr ) )
-- semantic domain
type T_ExpressionRoot  = Environment ->
                         ( ExpressionRoot,Environment)
data Inh_ExpressionRoot  = Inh_ExpressionRoot {env_Inh_ExpressionRoot :: Environment}
data Syn_ExpressionRoot  = Syn_ExpressionRoot {annotatedTree_Syn_ExpressionRoot :: ExpressionRoot,env_Syn_ExpressionRoot :: Environment}
wrap_ExpressionRoot :: T_ExpressionRoot  ->
                       Inh_ExpressionRoot  ->
                       Syn_ExpressionRoot 
wrap_ExpressionRoot sem (Inh_ExpressionRoot _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_ExpressionRoot _lhsOannotatedTree _lhsOenv ))
sem_ExpressionRoot_ExpressionRoot :: T_Expression  ->
                                     T_ExpressionRoot 
sem_ExpressionRoot_ExpressionRoot expr_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionRoot
              _lhsOenv :: Environment
              _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIenv :: Environment
              _exprIliftedColumnName :: String
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ExpressionRoot _exprIannotatedTree
                  {-# LINE 2597 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2601 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 2605 "AstInternal.hs" #-}
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2609 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ExpressionStatementListPair ---------------------------------
type ExpressionStatementListPair  = ( (Expression),(StatementList))
-- cata
sem_ExpressionStatementListPair :: ExpressionStatementListPair  ->
                                   T_ExpressionStatementListPair 
sem_ExpressionStatementListPair ( x1,x2)  =
    (sem_ExpressionStatementListPair_Tuple (sem_Expression x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionStatementListPair  = Environment ->
                                      ( ExpressionStatementListPair,Environment)
data Inh_ExpressionStatementListPair  = Inh_ExpressionStatementListPair {env_Inh_ExpressionStatementListPair :: Environment}
data Syn_ExpressionStatementListPair  = Syn_ExpressionStatementListPair {annotatedTree_Syn_ExpressionStatementListPair :: ExpressionStatementListPair,env_Syn_ExpressionStatementListPair :: Environment}
wrap_ExpressionStatementListPair :: T_ExpressionStatementListPair  ->
                                    Inh_ExpressionStatementListPair  ->
                                    Syn_ExpressionStatementListPair 
wrap_ExpressionStatementListPair sem (Inh_ExpressionStatementListPair _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_ExpressionStatementListPair _lhsOannotatedTree _lhsOenv ))
sem_ExpressionStatementListPair_Tuple :: T_Expression  ->
                                         T_StatementList  ->
                                         T_ExpressionStatementListPair 
sem_ExpressionStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIenv ->
         (let _x2OenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: ExpressionStatementListPair
              _lhsOenv :: Environment
              _x1Oenv :: Environment
              _x2Oenv :: Environment
              _x1IannotatedTree :: Expression
              _x1Ienv :: Environment
              _x1IliftedColumnName :: String
              _x2IannotatedTree :: StatementList
              _x2Ienv :: Environment
              _x2IenvUpdates :: ([EnvironmentUpdate])
              _x2OenvUpdates =
                  {-# LINE 366 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2651 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 2655 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2659 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 2663 "AstInternal.hs" #-}
              _x1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2667 "AstInternal.hs" #-}
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x1Ienv
                  {-# LINE 2671 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1Ienv,_x1IliftedColumnName) =
                  (x1_ _x1Oenv )
              ( _x2IannotatedTree,_x2Ienv,_x2IenvUpdates) =
                  (x2_ _x2Oenv _x2OenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ExpressionStatementListPairList -----------------------------
type ExpressionStatementListPairList  = [(ExpressionStatementListPair)]
-- cata
sem_ExpressionStatementListPairList :: ExpressionStatementListPairList  ->
                                       T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList list  =
    (Prelude.foldr sem_ExpressionStatementListPairList_Cons sem_ExpressionStatementListPairList_Nil (Prelude.map sem_ExpressionStatementListPair list) )
-- semantic domain
type T_ExpressionStatementListPairList  = Environment ->
                                          ( ExpressionStatementListPairList,Environment)
data Inh_ExpressionStatementListPairList  = Inh_ExpressionStatementListPairList {env_Inh_ExpressionStatementListPairList :: Environment}
data Syn_ExpressionStatementListPairList  = Syn_ExpressionStatementListPairList {annotatedTree_Syn_ExpressionStatementListPairList :: ExpressionStatementListPairList,env_Syn_ExpressionStatementListPairList :: Environment}
wrap_ExpressionStatementListPairList :: T_ExpressionStatementListPairList  ->
                                        Inh_ExpressionStatementListPairList  ->
                                        Syn_ExpressionStatementListPairList 
wrap_ExpressionStatementListPairList sem (Inh_ExpressionStatementListPairList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_ExpressionStatementListPairList _lhsOannotatedTree _lhsOenv ))
sem_ExpressionStatementListPairList_Cons :: T_ExpressionStatementListPair  ->
                                            T_ExpressionStatementListPairList  ->
                                            T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionStatementListPairList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: ExpressionStatementListPair
              _hdIenv :: Environment
              _tlIannotatedTree :: ExpressionStatementListPairList
              _tlIenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2712 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2716 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 2720 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2724 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 2728 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_ExpressionStatementListPairList_Nil :: T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionStatementListPairList
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2742 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2746 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2750 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- FnBody ------------------------------------------------------
data FnBody  = PlpgsqlFnBody (Annotation) (VarDefList) (StatementList) 
             | SqlFnBody (Annotation) (StatementList) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_FnBody :: FnBody  ->
              T_FnBody 
sem_FnBody (PlpgsqlFnBody _ann _vars _sts )  =
    (sem_FnBody_PlpgsqlFnBody _ann (sem_VarDefList _vars ) (sem_StatementList _sts ) )
sem_FnBody (SqlFnBody _ann _sts )  =
    (sem_FnBody_SqlFnBody _ann (sem_StatementList _sts ) )
-- semantic domain
type T_FnBody  = Environment ->
                 ( FnBody,Environment)
data Inh_FnBody  = Inh_FnBody {env_Inh_FnBody :: Environment}
data Syn_FnBody  = Syn_FnBody {annotatedTree_Syn_FnBody :: FnBody,env_Syn_FnBody :: Environment}
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_FnBody _lhsOannotatedTree _lhsOenv ))
sem_FnBody_PlpgsqlFnBody :: Annotation ->
                            T_VarDefList  ->
                            T_StatementList  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody ann_ vars_ sts_  =
    (\ _lhsIenv ->
         (let _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsOenv :: Environment
              _lhsOannotatedTree :: FnBody
              _lhsOenv :: Environment
              _varsOenv :: Environment
              _varsIannotatedTree :: VarDefList
              _varsIdefs :: ([(String,Type)])
              _varsIenv :: Environment
              _stsIannotatedTree :: StatementList
              _stsIenv :: Environment
              _stsIenvUpdates :: ([EnvironmentUpdate])
              _stsOenvUpdates =
                  {-# LINE 368 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2795 "AstInternal.hs" #-}
              _stsOenv =
                  {-# LINE 1044 "./TypeChecking.ag" #-}
                  fromRight _lhsIenv $ updateEnvironment _lhsIenv [EnvStackIDs [("", _varsIdefs)]]
                  {-# LINE 2799 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _varsIannotatedTree _stsIannotatedTree
                  {-# LINE 2803 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2807 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 2811 "AstInternal.hs" #-}
              _varsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2815 "AstInternal.hs" #-}
              ( _varsIannotatedTree,_varsIdefs,_varsIenv) =
                  (vars_ _varsOenv )
              ( _stsIannotatedTree,_stsIenv,_stsIenvUpdates) =
                  (sts_ _stsOenv _stsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_FnBody_SqlFnBody :: Annotation ->
                        T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody ann_ sts_  =
    (\ _lhsIenv ->
         (let _stsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: FnBody
              _lhsOenv :: Environment
              _stsOenv :: Environment
              _stsIannotatedTree :: StatementList
              _stsIenv :: Environment
              _stsIenvUpdates :: ([EnvironmentUpdate])
              _stsOenvUpdates =
                  {-# LINE 368 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2836 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIannotatedTree
                  {-# LINE 2840 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2844 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 2848 "AstInternal.hs" #-}
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2852 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIenv,_stsIenvUpdates) =
                  (sts_ _stsOenv _stsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- IfExists ----------------------------------------------------
data IfExists  = IfExists 
               | Require 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_IfExists :: IfExists  ->
                T_IfExists 
sem_IfExists (IfExists )  =
    (sem_IfExists_IfExists )
sem_IfExists (Require )  =
    (sem_IfExists_Require )
-- semantic domain
type T_IfExists  = Environment ->
                   ( IfExists,Environment)
data Inh_IfExists  = Inh_IfExists {env_Inh_IfExists :: Environment}
data Syn_IfExists  = Syn_IfExists {annotatedTree_Syn_IfExists :: IfExists,env_Syn_IfExists :: Environment}
wrap_IfExists :: T_IfExists  ->
                 Inh_IfExists  ->
                 Syn_IfExists 
wrap_IfExists sem (Inh_IfExists _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_IfExists _lhsOannotatedTree _lhsOenv ))
sem_IfExists_IfExists :: T_IfExists 
sem_IfExists_IfExists  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: IfExists
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  IfExists
                  {-# LINE 2887 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2891 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2895 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_IfExists_Require :: T_IfExists 
sem_IfExists_Require  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: IfExists
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Require
                  {-# LINE 2905 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2909 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2913 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- InList ------------------------------------------------------
data InList  = InList (ExpressionList) 
             | InSelect (SelectExpression) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_InList :: InList  ->
              T_InList 
sem_InList (InList _exprs )  =
    (sem_InList_InList (sem_ExpressionList _exprs ) )
sem_InList (InSelect _sel )  =
    (sem_InList_InSelect (sem_SelectExpression _sel ) )
-- semantic domain
type T_InList  = Environment ->
                 ( InList,Environment,(Either [TypeError] Type))
data Inh_InList  = Inh_InList {env_Inh_InList :: Environment}
data Syn_InList  = Syn_InList {annotatedTree_Syn_InList :: InList,env_Syn_InList :: Environment,listType_Syn_InList :: Either [TypeError] Type}
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType) =
             (sem _lhsIenv )
     in  (Syn_InList _lhsOannotatedTree _lhsOenv _lhsOlistType ))
sem_InList_InList :: T_ExpressionList  ->
                     T_InList 
sem_InList_InList exprs_  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: InList
              _lhsOenv :: Environment
              _exprsOenv :: Environment
              _exprsIannotatedTree :: ExpressionList
              _exprsIenv :: Environment
              _exprsItypeList :: ([Type])
              _lhsOlistType =
                  {-# LINE 276 "./TypeChecking.ag" #-}
                  resolveResultSetType
                    _lhsIenv
                    _exprsItypeList
                  {-# LINE 2954 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  InList _exprsIannotatedTree
                  {-# LINE 2958 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2962 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprsIenv
                  {-# LINE 2966 "AstInternal.hs" #-}
              _exprsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2970 "AstInternal.hs" #-}
              ( _exprsIannotatedTree,_exprsIenv,_exprsItypeList) =
                  (exprs_ _exprsOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType)))
sem_InList_InSelect :: T_SelectExpression  ->
                       T_InList 
sem_InList_InSelect sel_  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: InList
              _lhsOenv :: Environment
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIenv :: Environment
              _lhsOlistType =
                  {-# LINE 280 "./TypeChecking.ag" #-}
                  do
                    attrs <-  map snd <$> (unwrapSetOfComposite $
                                let a = getTypeAnnotation _selIannotatedTree
                                in                                      a)
                    typ <- case length attrs of
                                 0 -> Left [InternalError "got subquery with no columns? in inselect"]
                                 1 -> Right $ head attrs
                                 _ -> Right $ RowCtor attrs
                    checkTypes attrs $ Right typ
                  {-# LINE 2995 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  InSelect _selIannotatedTree
                  {-# LINE 2999 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3003 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 3007 "AstInternal.hs" #-}
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3011 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIenv) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType)))
-- JoinExpression ----------------------------------------------
data JoinExpression  = JoinOn (Expression) 
                     | JoinUsing (StringList) 
                     deriving ( Data,Eq,Show,Typeable)
-- cata
sem_JoinExpression :: JoinExpression  ->
                      T_JoinExpression 
sem_JoinExpression (JoinOn _expression )  =
    (sem_JoinExpression_JoinOn (sem_Expression _expression ) )
sem_JoinExpression (JoinUsing _stringList )  =
    (sem_JoinExpression_JoinUsing (sem_StringList _stringList ) )
-- semantic domain
type T_JoinExpression  = Environment ->
                         ( JoinExpression,Environment)
data Inh_JoinExpression  = Inh_JoinExpression {env_Inh_JoinExpression :: Environment}
data Syn_JoinExpression  = Syn_JoinExpression {annotatedTree_Syn_JoinExpression :: JoinExpression,env_Syn_JoinExpression :: Environment}
wrap_JoinExpression :: T_JoinExpression  ->
                       Inh_JoinExpression  ->
                       Syn_JoinExpression 
wrap_JoinExpression sem (Inh_JoinExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_JoinExpression _lhsOannotatedTree _lhsOenv ))
sem_JoinExpression_JoinOn :: T_Expression  ->
                             T_JoinExpression 
sem_JoinExpression_JoinOn expression_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinExpression
              _lhsOenv :: Environment
              _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIenv :: Environment
              _expressionIliftedColumnName :: String
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  JoinOn _expressionIannotatedTree
                  {-# LINE 3051 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3055 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _expressionIenv
                  {-# LINE 3059 "AstInternal.hs" #-}
              _expressionOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3063 "AstInternal.hs" #-}
              ( _expressionIannotatedTree,_expressionIenv,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_JoinExpression_JoinUsing :: T_StringList  ->
                                T_JoinExpression 
sem_JoinExpression_JoinUsing stringList_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinExpression
              _lhsOenv :: Environment
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIenv :: Environment
              _stringListIstrings :: ([String])
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  JoinUsing _stringListIannotatedTree
                  {-# LINE 3080 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3084 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 3088 "AstInternal.hs" #-}
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3092 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIenv,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- JoinType ----------------------------------------------------
data JoinType  = Cross 
               | FullOuter 
               | Inner 
               | LeftOuter 
               | RightOuter 
               deriving ( Data,Eq,Show,Typeable)
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
type T_JoinType  = Environment ->
                   ( JoinType,Environment)
data Inh_JoinType  = Inh_JoinType {env_Inh_JoinType :: Environment}
data Syn_JoinType  = Syn_JoinType {annotatedTree_Syn_JoinType :: JoinType,env_Syn_JoinType :: Environment}
wrap_JoinType :: T_JoinType  ->
                 Inh_JoinType  ->
                 Syn_JoinType 
wrap_JoinType sem (Inh_JoinType _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_JoinType _lhsOannotatedTree _lhsOenv ))
sem_JoinType_Cross :: T_JoinType 
sem_JoinType_Cross  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Cross
                  {-# LINE 3136 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3140 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3144 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_JoinType_FullOuter :: T_JoinType 
sem_JoinType_FullOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  FullOuter
                  {-# LINE 3154 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3158 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3162 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_JoinType_Inner :: T_JoinType 
sem_JoinType_Inner  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Inner
                  {-# LINE 3172 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3176 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3180 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_JoinType_LeftOuter :: T_JoinType 
sem_JoinType_LeftOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  LeftOuter
                  {-# LINE 3190 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3194 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3198 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_JoinType_RightOuter :: T_JoinType 
sem_JoinType_RightOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RightOuter
                  {-# LINE 3208 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3212 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3216 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Language ----------------------------------------------------
data Language  = Plpgsql 
               | Sql 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Language :: Language  ->
                T_Language 
sem_Language (Plpgsql )  =
    (sem_Language_Plpgsql )
sem_Language (Sql )  =
    (sem_Language_Sql )
-- semantic domain
type T_Language  = Environment ->
                   ( Language,Environment)
data Inh_Language  = Inh_Language {env_Inh_Language :: Environment}
data Syn_Language  = Syn_Language {annotatedTree_Syn_Language :: Language,env_Syn_Language :: Environment}
wrap_Language :: T_Language  ->
                 Inh_Language  ->
                 Syn_Language 
wrap_Language sem (Inh_Language _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Language _lhsOannotatedTree _lhsOenv ))
sem_Language_Plpgsql :: T_Language 
sem_Language_Plpgsql  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Language
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Plpgsql
                  {-# LINE 3249 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3253 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3257 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Language_Sql :: T_Language 
sem_Language_Sql  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Language
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Sql
                  {-# LINE 3267 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3271 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3275 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- MExpression -------------------------------------------------
type MExpression  = (Maybe (Expression))
-- cata
sem_MExpression :: MExpression  ->
                   T_MExpression 
sem_MExpression (Prelude.Just x )  =
    (sem_MExpression_Just (sem_Expression x ) )
sem_MExpression Prelude.Nothing  =
    sem_MExpression_Nothing
-- semantic domain
type T_MExpression  = Environment ->
                      ( MExpression,Environment)
data Inh_MExpression  = Inh_MExpression {env_Inh_MExpression :: Environment}
data Syn_MExpression  = Syn_MExpression {annotatedTree_Syn_MExpression :: MExpression,env_Syn_MExpression :: Environment}
wrap_MExpression :: T_MExpression  ->
                    Inh_MExpression  ->
                    Syn_MExpression 
wrap_MExpression sem (Inh_MExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_MExpression _lhsOannotatedTree _lhsOenv ))
sem_MExpression_Just :: T_Expression  ->
                        T_MExpression 
sem_MExpression_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MExpression
              _lhsOenv :: Environment
              _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIenv :: Environment
              _justIliftedColumnName :: String
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3311 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3315 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 3319 "AstInternal.hs" #-}
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3323 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv,_justIliftedColumnName) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_MExpression_Nothing :: T_MExpression 
sem_MExpression_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MExpression
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3335 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3339 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3343 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
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
type T_MTableRef  = Environment ->
                    ( MTableRef,Environment,([(String,([(String,Type)],[(String,Type)]))]),([String]))
data Inh_MTableRef  = Inh_MTableRef {env_Inh_MTableRef :: Environment}
data Syn_MTableRef  = Syn_MTableRef {annotatedTree_Syn_MTableRef :: MTableRef,env_Syn_MTableRef :: Environment,idens_Syn_MTableRef :: [(String,([(String,Type)],[(String,Type)]))],joinIdens_Syn_MTableRef :: [String]}
wrap_MTableRef :: T_MTableRef  ->
                  Inh_MTableRef  ->
                  Syn_MTableRef 
wrap_MTableRef sem (Inh_MTableRef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens) =
             (sem _lhsIenv )
     in  (Syn_MTableRef _lhsOannotatedTree _lhsOenv _lhsOidens _lhsOjoinIdens ))
sem_MTableRef_Just :: T_TableRef  ->
                      T_MTableRef 
sem_MTableRef_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MTableRef
              _lhsOenv :: Environment
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              _justOenv :: Environment
              _justIannotatedTree :: TableRef
              _justIenv :: Environment
              _justIidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _justIjoinIdens :: ([String])
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3382 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3386 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 3390 "AstInternal.hs" #-}
              _lhsOidens =
                  {-# LINE 454 "./TypeChecking.ag" #-}
                  _justIidens
                  {-# LINE 3394 "AstInternal.hs" #-}
              _lhsOjoinIdens =
                  {-# LINE 455 "./TypeChecking.ag" #-}
                  _justIjoinIdens
                  {-# LINE 3398 "AstInternal.hs" #-}
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3402 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv,_justIidens,_justIjoinIdens) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
sem_MTableRef_Nothing :: T_MTableRef 
sem_MTableRef_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              _lhsOannotatedTree :: MTableRef
              _lhsOenv :: Environment
              _lhsOidens =
                  {-# LINE 554 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3416 "AstInternal.hs" #-}
              _lhsOjoinIdens =
                  {-# LINE 555 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3420 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3424 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3428 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3432 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
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
type T_MaybeExpression  = Environment ->
                          ( MaybeExpression,Environment,(Maybe Type))
data Inh_MaybeExpression  = Inh_MaybeExpression {env_Inh_MaybeExpression :: Environment}
data Syn_MaybeExpression  = Syn_MaybeExpression {annotatedTree_Syn_MaybeExpression :: MaybeExpression,env_Syn_MaybeExpression :: Environment,exprType_Syn_MaybeExpression :: Maybe Type}
wrap_MaybeExpression :: T_MaybeExpression  ->
                        Inh_MaybeExpression  ->
                        Syn_MaybeExpression 
wrap_MaybeExpression sem (Inh_MaybeExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOexprType) =
             (sem _lhsIenv )
     in  (Syn_MaybeExpression _lhsOannotatedTree _lhsOenv _lhsOexprType ))
sem_MaybeExpression_Just :: T_Expression  ->
                            T_MaybeExpression 
sem_MaybeExpression_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOexprType :: (Maybe Type)
              _lhsOannotatedTree :: MaybeExpression
              _lhsOenv :: Environment
              _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIenv :: Environment
              _justIliftedColumnName :: String
              _lhsOexprType =
                  {-# LINE 1088 "./TypeChecking.ag" #-}
                  Just $ getTypeAnnotation _justIannotatedTree
                  {-# LINE 3469 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3473 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3477 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 3481 "AstInternal.hs" #-}
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3485 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv,_justIliftedColumnName) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOexprType)))
sem_MaybeExpression_Nothing :: T_MaybeExpression 
sem_MaybeExpression_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOexprType :: (Maybe Type)
              _lhsOannotatedTree :: MaybeExpression
              _lhsOenv :: Environment
              _lhsOexprType =
                  {-# LINE 1089 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3498 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3502 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3506 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3510 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOexprType)))
-- Natural -----------------------------------------------------
data Natural  = Natural 
              | Unnatural 
              deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Natural :: Natural  ->
               T_Natural 
sem_Natural (Natural )  =
    (sem_Natural_Natural )
sem_Natural (Unnatural )  =
    (sem_Natural_Unnatural )
-- semantic domain
type T_Natural  = Environment ->
                  ( Natural,Environment)
data Inh_Natural  = Inh_Natural {env_Inh_Natural :: Environment}
data Syn_Natural  = Syn_Natural {annotatedTree_Syn_Natural :: Natural,env_Syn_Natural :: Environment}
wrap_Natural :: T_Natural  ->
                Inh_Natural  ->
                Syn_Natural 
wrap_Natural sem (Inh_Natural _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Natural _lhsOannotatedTree _lhsOenv ))
sem_Natural_Natural :: T_Natural 
sem_Natural_Natural  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Natural
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Natural
                  {-# LINE 3543 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3547 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3551 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Natural_Unnatural :: T_Natural 
sem_Natural_Unnatural  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Natural
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Unnatural
                  {-# LINE 3561 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3565 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3569 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
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
type T_OnExpr  = Environment ->
                 ( OnExpr,Environment)
data Inh_OnExpr  = Inh_OnExpr {env_Inh_OnExpr :: Environment}
data Syn_OnExpr  = Syn_OnExpr {annotatedTree_Syn_OnExpr :: OnExpr,env_Syn_OnExpr :: Environment}
wrap_OnExpr :: T_OnExpr  ->
               Inh_OnExpr  ->
               Syn_OnExpr 
wrap_OnExpr sem (Inh_OnExpr _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_OnExpr _lhsOannotatedTree _lhsOenv ))
sem_OnExpr_Just :: T_JoinExpression  ->
                   T_OnExpr 
sem_OnExpr_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: OnExpr
              _lhsOenv :: Environment
              _justOenv :: Environment
              _justIannotatedTree :: JoinExpression
              _justIenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3604 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3608 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 3612 "AstInternal.hs" #-}
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3616 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: OnExpr
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3628 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3632 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3636 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ParamDef ----------------------------------------------------
data ParamDef  = ParamDef (String) (TypeName) 
               | ParamDefTp (TypeName) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_ParamDef :: ParamDef  ->
                T_ParamDef 
sem_ParamDef (ParamDef _name _typ )  =
    (sem_ParamDef_ParamDef _name (sem_TypeName _typ ) )
sem_ParamDef (ParamDefTp _typ )  =
    (sem_ParamDef_ParamDefTp (sem_TypeName _typ ) )
-- semantic domain
type T_ParamDef  = Environment ->
                   ( ParamDef,Environment,(Either [TypeError] Type),String)
data Inh_ParamDef  = Inh_ParamDef {env_Inh_ParamDef :: Environment}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef,env_Syn_ParamDef :: Environment,namedType_Syn_ParamDef :: Either [TypeError] Type,paramName_Syn_ParamDef :: String}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType,_lhsOparamName) =
             (sem _lhsIenv )
     in  (Syn_ParamDef _lhsOannotatedTree _lhsOenv _lhsOnamedType _lhsOparamName ))
sem_ParamDef_ParamDef :: String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef name_ typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOparamName :: String
              _lhsOannotatedTree :: ParamDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: (Either [TypeError] Type)
              _lhsOnamedType =
                  {-# LINE 1000 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 3677 "AstInternal.hs" #-}
              _lhsOparamName =
                  {-# LINE 1002 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 3681 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ParamDef name_ _typIannotatedTree
                  {-# LINE 3685 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3689 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 3693 "AstInternal.hs" #-}
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3697 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType,_lhsOparamName)))
sem_ParamDef_ParamDefTp :: T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOparamName :: String
              _lhsOannotatedTree :: ParamDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: (Either [TypeError] Type)
              _lhsOnamedType =
                  {-# LINE 1000 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 3716 "AstInternal.hs" #-}
              _lhsOparamName =
                  {-# LINE 1004 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 3720 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ParamDefTp _typIannotatedTree
                  {-# LINE 3724 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3728 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 3732 "AstInternal.hs" #-}
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3736 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType,_lhsOparamName)))
-- ParamDefList ------------------------------------------------
type ParamDefList  = [(ParamDef)]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = Environment ->
                       ( ParamDefList,Environment,([(String,Either [TypeError] Type)]))
data Inh_ParamDefList  = Inh_ParamDefList {env_Inh_ParamDefList :: Environment}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList,env_Syn_ParamDefList :: Environment,params_Syn_ParamDefList :: [(String,Either [TypeError] Type)]}
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOparams) =
             (sem _lhsIenv )
     in  (Syn_ParamDefList _lhsOannotatedTree _lhsOenv _lhsOparams ))
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOparams :: ([(String,Either [TypeError] Type)])
              _lhsOannotatedTree :: ParamDefList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: ParamDef
              _hdIenv :: Environment
              _hdInamedType :: (Either [TypeError] Type)
              _hdIparamName :: String
              _tlIannotatedTree :: ParamDefList
              _tlIenv :: Environment
              _tlIparams :: ([(String,Either [TypeError] Type)])
              _lhsOparams =
                  {-# LINE 1008 "./TypeChecking.ag" #-}
                  ((_hdIparamName, _hdInamedType) : _tlIparams)
                  {-# LINE 3779 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 3783 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3787 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 3791 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3795 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 3799 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv,_hdInamedType,_hdIparamName) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv,_tlIparams) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOparams)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOparams :: ([(String,Either [TypeError] Type)])
              _lhsOannotatedTree :: ParamDefList
              _lhsOenv :: Environment
              _lhsOparams =
                  {-# LINE 1007 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3814 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3818 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3822 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3826 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOparams)))
-- RaiseType ---------------------------------------------------
data RaiseType  = RError 
                | RException 
                | RNotice 
                deriving ( Data,Eq,Show,Typeable)
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
type T_RaiseType  = Environment ->
                    ( RaiseType,Environment)
data Inh_RaiseType  = Inh_RaiseType {env_Inh_RaiseType :: Environment}
data Syn_RaiseType  = Syn_RaiseType {annotatedTree_Syn_RaiseType :: RaiseType,env_Syn_RaiseType :: Environment}
wrap_RaiseType :: T_RaiseType  ->
                  Inh_RaiseType  ->
                  Syn_RaiseType 
wrap_RaiseType sem (Inh_RaiseType _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_RaiseType _lhsOannotatedTree _lhsOenv ))
sem_RaiseType_RError :: T_RaiseType 
sem_RaiseType_RError  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RaiseType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RError
                  {-# LINE 3862 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3866 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3870 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RaiseType_RException :: T_RaiseType 
sem_RaiseType_RException  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RaiseType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RException
                  {-# LINE 3880 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3884 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3888 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RaiseType_RNotice :: T_RaiseType 
sem_RaiseType_RNotice  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RaiseType
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RNotice
                  {-# LINE 3898 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3902 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3906 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- RestartIdentity ---------------------------------------------
data RestartIdentity  = ContinueIdentity 
                      | RestartIdentity 
                      deriving ( Data,Eq,Show,Typeable)
-- cata
sem_RestartIdentity :: RestartIdentity  ->
                       T_RestartIdentity 
sem_RestartIdentity (ContinueIdentity )  =
    (sem_RestartIdentity_ContinueIdentity )
sem_RestartIdentity (RestartIdentity )  =
    (sem_RestartIdentity_RestartIdentity )
-- semantic domain
type T_RestartIdentity  = Environment ->
                          ( RestartIdentity,Environment)
data Inh_RestartIdentity  = Inh_RestartIdentity {env_Inh_RestartIdentity :: Environment}
data Syn_RestartIdentity  = Syn_RestartIdentity {annotatedTree_Syn_RestartIdentity :: RestartIdentity,env_Syn_RestartIdentity :: Environment}
wrap_RestartIdentity :: T_RestartIdentity  ->
                        Inh_RestartIdentity  ->
                        Syn_RestartIdentity 
wrap_RestartIdentity sem (Inh_RestartIdentity _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_RestartIdentity _lhsOannotatedTree _lhsOenv ))
sem_RestartIdentity_ContinueIdentity :: T_RestartIdentity 
sem_RestartIdentity_ContinueIdentity  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RestartIdentity
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ContinueIdentity
                  {-# LINE 3939 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3943 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3947 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RestartIdentity_RestartIdentity :: T_RestartIdentity 
sem_RestartIdentity_RestartIdentity  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RestartIdentity
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RestartIdentity
                  {-# LINE 3957 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3961 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3965 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Root --------------------------------------------------------
data Root  = Root (StatementList) 
           deriving ( Show)
-- cata
sem_Root :: Root  ->
            T_Root 
sem_Root (Root _statements )  =
    (sem_Root_Root (sem_StatementList _statements ) )
-- semantic domain
type T_Root  = Environment ->
               ( Root,Environment)
data Inh_Root  = Inh_Root {env_Inh_Root :: Environment}
data Syn_Root  = Syn_Root {annotatedTree_Syn_Root :: Root,env_Syn_Root :: Environment}
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Root _lhsOannotatedTree _lhsOenv ))
sem_Root_Root :: T_StatementList  ->
                 T_Root 
sem_Root_Root statements_  =
    (\ _lhsIenv ->
         (let _statementsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Root
              _lhsOenv :: Environment
              _statementsOenv :: Environment
              _statementsIannotatedTree :: StatementList
              _statementsIenv :: Environment
              _statementsIenvUpdates :: ([EnvironmentUpdate])
              _statementsOenvUpdates =
                  {-# LINE 353 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4001 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Root _statementsIannotatedTree
                  {-# LINE 4005 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4009 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _statementsIenv
                  {-# LINE 4013 "AstInternal.hs" #-}
              _statementsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4017 "AstInternal.hs" #-}
              ( _statementsIannotatedTree,_statementsIenv,_statementsIenvUpdates) =
                  (statements_ _statementsOenv _statementsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- RowConstraint -----------------------------------------------
data RowConstraint  = NotNullConstraint 
                    | NullConstraint 
                    | RowCheckConstraint (Expression) 
                    | RowPrimaryKeyConstraint 
                    | RowReferenceConstraint (String) (Maybe String) (Cascade) (Cascade) 
                    | RowUniqueConstraint 
                    deriving ( Data,Eq,Show,Typeable)
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
type T_RowConstraint  = Environment ->
                        ( RowConstraint,Environment)
data Inh_RowConstraint  = Inh_RowConstraint {env_Inh_RowConstraint :: Environment}
data Syn_RowConstraint  = Syn_RowConstraint {annotatedTree_Syn_RowConstraint :: RowConstraint,env_Syn_RowConstraint :: Environment}
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_RowConstraint _lhsOannotatedTree _lhsOenv ))
sem_RowConstraint_NotNullConstraint :: T_RowConstraint 
sem_RowConstraint_NotNullConstraint  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  NotNullConstraint
                  {-# LINE 4064 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4068 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4072 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraint_NullConstraint :: T_RowConstraint 
sem_RowConstraint_NullConstraint  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  NullConstraint
                  {-# LINE 4082 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4086 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4090 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraint_RowCheckConstraint :: T_Expression  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint expression_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIenv :: Environment
              _expressionIliftedColumnName :: String
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowCheckConstraint _expressionIannotatedTree
                  {-# LINE 4105 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4109 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _expressionIenv
                  {-# LINE 4113 "AstInternal.hs" #-}
              _expressionOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4117 "AstInternal.hs" #-}
              ( _expressionIannotatedTree,_expressionIenv,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraint_RowPrimaryKeyConstraint :: T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint
                  {-# LINE 4129 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4133 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4137 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraint_RowReferenceConstraint :: String ->
                                            (Maybe String) ->
                                            T_Cascade  ->
                                            T_Cascade  ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              _onUpdateOenv :: Environment
              _onDeleteOenv :: Environment
              _onUpdateIannotatedTree :: Cascade
              _onUpdateIenv :: Environment
              _onDeleteIannotatedTree :: Cascade
              _onDeleteIenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowReferenceConstraint table_ att_ _onUpdateIannotatedTree _onDeleteIannotatedTree
                  {-# LINE 4157 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4161 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onDeleteIenv
                  {-# LINE 4165 "AstInternal.hs" #-}
              _onUpdateOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4169 "AstInternal.hs" #-}
              _onDeleteOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onUpdateIenv
                  {-# LINE 4173 "AstInternal.hs" #-}
              ( _onUpdateIannotatedTree,_onUpdateIenv) =
                  (onUpdate_ _onUpdateOenv )
              ( _onDeleteIannotatedTree,_onDeleteIenv) =
                  (onDelete_ _onDeleteOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraint_RowUniqueConstraint :: T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowUniqueConstraint
                  {-# LINE 4187 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4191 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4195 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- RowConstraintList -------------------------------------------
type RowConstraintList  = [(RowConstraint)]
-- cata
sem_RowConstraintList :: RowConstraintList  ->
                         T_RowConstraintList 
sem_RowConstraintList list  =
    (Prelude.foldr sem_RowConstraintList_Cons sem_RowConstraintList_Nil (Prelude.map sem_RowConstraint list) )
-- semantic domain
type T_RowConstraintList  = Environment ->
                            ( RowConstraintList,Environment)
data Inh_RowConstraintList  = Inh_RowConstraintList {env_Inh_RowConstraintList :: Environment}
data Syn_RowConstraintList  = Syn_RowConstraintList {annotatedTree_Syn_RowConstraintList :: RowConstraintList,env_Syn_RowConstraintList :: Environment}
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_RowConstraintList _lhsOannotatedTree _lhsOenv ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraintList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: RowConstraint
              _hdIenv :: Environment
              _tlIannotatedTree :: RowConstraintList
              _tlIenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 4232 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4236 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 4240 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4244 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 4248 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraintList
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4262 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4266 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4270 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- SelectExpression --------------------------------------------
data SelectExpression  = CombineSelect (Annotation) (CombineType) (SelectExpression) (SelectExpression) 
                       | Select (Annotation) (Distinct) (SelectList) (MTableRef) (Where) (ExpressionList) (MExpression) (ExpressionList) (Direction) (MExpression) (MExpression) 
                       | Values (Annotation) (ExpressionListList) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectExpression :: SelectExpression  ->
                        T_SelectExpression 
sem_SelectExpression (CombineSelect _ann _ctype _sel1 _sel2 )  =
    (sem_SelectExpression_CombineSelect _ann (sem_CombineType _ctype ) (sem_SelectExpression _sel1 ) (sem_SelectExpression _sel2 ) )
sem_SelectExpression (Select _ann _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selDir _selLimit _selOffset )  =
    (sem_SelectExpression_Select _ann (sem_Distinct _selDistinct ) (sem_SelectList _selSelectList ) (sem_MTableRef _selTref ) (sem_Where _selWhere ) (sem_ExpressionList _selGroupBy ) (sem_MExpression _selHaving ) (sem_ExpressionList _selOrderBy ) (sem_Direction _selDir ) (sem_MExpression _selLimit ) (sem_MExpression _selOffset ) )
sem_SelectExpression (Values _ann _vll )  =
    (sem_SelectExpression_Values _ann (sem_ExpressionListList _vll ) )
-- semantic domain
type T_SelectExpression  = Environment ->
                           ( SelectExpression,Environment)
data Inh_SelectExpression  = Inh_SelectExpression {env_Inh_SelectExpression :: Environment}
data Syn_SelectExpression  = Syn_SelectExpression {annotatedTree_Syn_SelectExpression :: SelectExpression,env_Syn_SelectExpression :: Environment}
wrap_SelectExpression :: T_SelectExpression  ->
                         Inh_SelectExpression  ->
                         Syn_SelectExpression 
wrap_SelectExpression sem (Inh_SelectExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_SelectExpression _lhsOannotatedTree _lhsOenv ))
sem_SelectExpression_CombineSelect :: Annotation ->
                                      T_CombineType  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression 
sem_SelectExpression_CombineSelect ann_ ctype_ sel1_ sel2_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: SelectExpression
              _lhsOenv :: Environment
              _ctypeOenv :: Environment
              _sel1Oenv :: Environment
              _sel2Oenv :: Environment
              _ctypeIannotatedTree :: CombineType
              _ctypeIenv :: Environment
              _sel1IannotatedTree :: SelectExpression
              _sel1Ienv :: Environment
              _sel2IannotatedTree :: SelectExpression
              _sel2Ienv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 395 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 4322 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 438 "./TypeChecking.ag" #-}
                  let sel1t = getTypeAnnotation _sel1IannotatedTree
                      sel2t = getTypeAnnotation _sel2IannotatedTree
                  in checkTypes [sel1t, sel2t] $
                        typeCheckCombineSelect _lhsIenv sel1t sel2t
                  {-# LINE 4329 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 443 "./TypeChecking.ag" #-}
                  CombineSelect ann_ _ctypeIannotatedTree
                                _sel1IannotatedTree
                                _sel2IannotatedTree
                  {-# LINE 4335 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CombineSelect ann_ _ctypeIannotatedTree _sel1IannotatedTree _sel2IannotatedTree
                  {-# LINE 4339 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sel2Ienv
                  {-# LINE 4343 "AstInternal.hs" #-}
              _ctypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4347 "AstInternal.hs" #-}
              _sel1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _ctypeIenv
                  {-# LINE 4351 "AstInternal.hs" #-}
              _sel2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sel1Ienv
                  {-# LINE 4355 "AstInternal.hs" #-}
              ( _ctypeIannotatedTree,_ctypeIenv) =
                  (ctype_ _ctypeOenv )
              ( _sel1IannotatedTree,_sel1Ienv) =
                  (sel1_ _sel1Oenv )
              ( _sel2IannotatedTree,_sel2Ienv) =
                  (sel2_ _sel2Oenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_SelectExpression_Select :: Annotation ->
                               T_Distinct  ->
                               T_SelectList  ->
                               T_MTableRef  ->
                               T_Where  ->
                               T_ExpressionList  ->
                               T_MExpression  ->
                               T_ExpressionList  ->
                               T_Direction  ->
                               T_MExpression  ->
                               T_MExpression  ->
                               T_SelectExpression 
sem_SelectExpression_Select ann_ selDistinct_ selSelectList_ selTref_ selWhere_ selGroupBy_ selHaving_ selOrderBy_ selDir_ selLimit_ selOffset_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: SelectExpression
              _selSelectListOenv :: Environment
              _selWhereOenv :: Environment
              _lhsOenv :: Environment
              _selDistinctOenv :: Environment
              _selTrefOenv :: Environment
              _selGroupByOenv :: Environment
              _selHavingOenv :: Environment
              _selOrderByOenv :: Environment
              _selDirOenv :: Environment
              _selLimitOenv :: Environment
              _selOffsetOenv :: Environment
              _selDistinctIannotatedTree :: Distinct
              _selDistinctIenv :: Environment
              _selSelectListIannotatedTree :: SelectList
              _selSelectListIenv :: Environment
              _selSelectListIlistType :: Type
              _selTrefIannotatedTree :: MTableRef
              _selTrefIenv :: Environment
              _selTrefIidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _selTrefIjoinIdens :: ([String])
              _selWhereIannotatedTree :: Where
              _selWhereIenv :: Environment
              _selGroupByIannotatedTree :: ExpressionList
              _selGroupByIenv :: Environment
              _selGroupByItypeList :: ([Type])
              _selHavingIannotatedTree :: MExpression
              _selHavingIenv :: Environment
              _selOrderByIannotatedTree :: ExpressionList
              _selOrderByIenv :: Environment
              _selOrderByItypeList :: ([Type])
              _selDirIannotatedTree :: Direction
              _selDirIenv :: Environment
              _selLimitIannotatedTree :: MExpression
              _selLimitIenv :: Environment
              _selOffsetIannotatedTree :: MExpression
              _selOffsetIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 395 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 4420 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 416 "./TypeChecking.ag" #-}
                  do
                  whereType <- checkExpressionBool _selWhereIannotatedTree
                  let trefType = fromMaybe typeBool $ fmap getTypeAnnotation
                                                           _selTrefIannotatedTree
                      slType = _selSelectListIlistType
                  chainTypeCheckFailed [trefType, whereType, slType] $
                    Right $ case slType of
                              UnnamedCompositeType [(_,Pseudo Void)] -> Pseudo Void
                              _ -> SetOfType slType
                  {-# LINE 4432 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 426 "./TypeChecking.ag" #-}
                  Select ann_
                         _selDistinctIannotatedTree
                         _selSelectListIannotatedTree
                         _selTrefIannotatedTree
                         _selWhereIannotatedTree
                         _selGroupByIannotatedTree
                         _selHavingIannotatedTree
                         _selOrderByIannotatedTree
                         _selDirIannotatedTree
                         _selLimitIannotatedTree
                         _selOffsetIannotatedTree
                  {-# LINE 4446 "AstInternal.hs" #-}
              _newEnv =
                  {-# LINE 635 "./TypeChecking.ag" #-}
                  case updateEnvironment _lhsIenv
                        (convertToNewStyleUpdates _selTrefIidens _selTrefIjoinIdens) of
                    Left x -> error $ show x
                    Right e -> e
                  {-# LINE 4453 "AstInternal.hs" #-}
              _selSelectListOenv =
                  {-# LINE 639 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 4457 "AstInternal.hs" #-}
              _selWhereOenv =
                  {-# LINE 640 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 4461 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Select ann_ _selDistinctIannotatedTree _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selDirIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
                  {-# LINE 4465 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selOffsetIenv
                  {-# LINE 4469 "AstInternal.hs" #-}
              _selDistinctOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4473 "AstInternal.hs" #-}
              _selTrefOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selSelectListIenv
                  {-# LINE 4477 "AstInternal.hs" #-}
              _selGroupByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selWhereIenv
                  {-# LINE 4481 "AstInternal.hs" #-}
              _selHavingOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selGroupByIenv
                  {-# LINE 4485 "AstInternal.hs" #-}
              _selOrderByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selHavingIenv
                  {-# LINE 4489 "AstInternal.hs" #-}
              _selDirOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selOrderByIenv
                  {-# LINE 4493 "AstInternal.hs" #-}
              _selLimitOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selDirIenv
                  {-# LINE 4497 "AstInternal.hs" #-}
              _selOffsetOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selLimitIenv
                  {-# LINE 4501 "AstInternal.hs" #-}
              ( _selDistinctIannotatedTree,_selDistinctIenv) =
                  (selDistinct_ _selDistinctOenv )
              ( _selSelectListIannotatedTree,_selSelectListIenv,_selSelectListIlistType) =
                  (selSelectList_ _selSelectListOenv )
              ( _selTrefIannotatedTree,_selTrefIenv,_selTrefIidens,_selTrefIjoinIdens) =
                  (selTref_ _selTrefOenv )
              ( _selWhereIannotatedTree,_selWhereIenv) =
                  (selWhere_ _selWhereOenv )
              ( _selGroupByIannotatedTree,_selGroupByIenv,_selGroupByItypeList) =
                  (selGroupBy_ _selGroupByOenv )
              ( _selHavingIannotatedTree,_selHavingIenv) =
                  (selHaving_ _selHavingOenv )
              ( _selOrderByIannotatedTree,_selOrderByIenv,_selOrderByItypeList) =
                  (selOrderBy_ _selOrderByOenv )
              ( _selDirIannotatedTree,_selDirIenv) =
                  (selDir_ _selDirOenv )
              ( _selLimitIannotatedTree,_selLimitIenv) =
                  (selLimit_ _selLimitOenv )
              ( _selOffsetIannotatedTree,_selOffsetIenv) =
                  (selOffset_ _selOffsetOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_SelectExpression_Values :: Annotation ->
                               T_ExpressionListList  ->
                               T_SelectExpression 
sem_SelectExpression_Values ann_ vll_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: SelectExpression
              _lhsOenv :: Environment
              _vllOenv :: Environment
              _vllIannotatedTree :: ExpressionListList
              _vllIenv :: Environment
              _vllItypeListList :: ([[Type]])
              _lhsOannotatedTree =
                  {-# LINE 395 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 4540 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 411 "./TypeChecking.ag" #-}
                  typeCheckValuesExpr
                              _lhsIenv
                              _vllItypeListList
                  {-# LINE 4546 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 414 "./TypeChecking.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 4550 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 4554 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _vllIenv
                  {-# LINE 4558 "AstInternal.hs" #-}
              _vllOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4562 "AstInternal.hs" #-}
              ( _vllIannotatedTree,_vllIenv,_vllItypeListList) =
                  (vll_ _vllOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- SelectItem --------------------------------------------------
data SelectItem  = SelExp (Expression) 
                 | SelectItem (Expression) (String) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectItem :: SelectItem  ->
                  T_SelectItem 
sem_SelectItem (SelExp _ex )  =
    (sem_SelectItem_SelExp (sem_Expression _ex ) )
sem_SelectItem (SelectItem _ex _name )  =
    (sem_SelectItem_SelectItem (sem_Expression _ex ) _name )
-- semantic domain
type T_SelectItem  = Environment ->
                     ( SelectItem,String,Environment,Type)
data Inh_SelectItem  = Inh_SelectItem {env_Inh_SelectItem :: Environment}
data Syn_SelectItem  = Syn_SelectItem {annotatedTree_Syn_SelectItem :: SelectItem,columnName_Syn_SelectItem :: String,env_Syn_SelectItem :: Environment,itemType_Syn_SelectItem :: Type}
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOenv,_lhsOitemType) =
             (sem _lhsIenv )
     in  (Syn_SelectItem _lhsOannotatedTree _lhsOcolumnName _lhsOenv _lhsOitemType ))
sem_SelectItem_SelExp :: T_Expression  ->
                         T_SelectItem 
sem_SelectItem_SelExp ex_  =
    (\ _lhsIenv ->
         (let _lhsOitemType :: Type
              _lhsOcolumnName :: String
              _lhsOannotatedTree :: SelectItem
              _lhsOenv :: Environment
              _exOenv :: Environment
              _exIannotatedTree :: Expression
              _exIenv :: Environment
              _exIliftedColumnName :: String
              _lhsOitemType =
                  {-# LINE 575 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exIannotatedTree
                  {-# LINE 4604 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 580 "./TypeChecking.ag" #-}
                  SelExp $ fixStar _exIannotatedTree
                  {-# LINE 4608 "AstInternal.hs" #-}
              _lhsOcolumnName =
                  {-# LINE 701 "./TypeChecking.ag" #-}
                  case _exIliftedColumnName of
                    "" -> "?column?"
                    s -> s
                  {-# LINE 4614 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4618 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exIenv
                  {-# LINE 4622 "AstInternal.hs" #-}
              _exOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4626 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIenv,_exIliftedColumnName) =
                  (ex_ _exOenv )
          in  ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOenv,_lhsOitemType)))
sem_SelectItem_SelectItem :: T_Expression  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem ex_ name_  =
    (\ _lhsIenv ->
         (let _lhsOitemType :: Type
              _lhsOcolumnName :: String
              _lhsOannotatedTree :: SelectItem
              _lhsOenv :: Environment
              _exOenv :: Environment
              _exIannotatedTree :: Expression
              _exIenv :: Environment
              _exIliftedColumnName :: String
              _lhsOitemType =
                  {-# LINE 575 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exIannotatedTree
                  {-# LINE 4646 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 582 "./TypeChecking.ag" #-}
                  SelectItem (fixStar _exIannotatedTree) name_
                  {-# LINE 4650 "AstInternal.hs" #-}
              _lhsOcolumnName =
                  {-# LINE 704 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 4654 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SelectItem _exIannotatedTree name_
                  {-# LINE 4658 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4662 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exIenv
                  {-# LINE 4666 "AstInternal.hs" #-}
              _exOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4670 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIenv,_exIliftedColumnName) =
                  (ex_ _exOenv )
          in  ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOenv,_lhsOitemType)))
-- SelectItemList ----------------------------------------------
type SelectItemList  = [(SelectItem)]
-- cata
sem_SelectItemList :: SelectItemList  ->
                      T_SelectItemList 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList  = Environment ->
                         ( SelectItemList,Environment,Type)
data Inh_SelectItemList  = Inh_SelectItemList {env_Inh_SelectItemList :: Environment}
data Syn_SelectItemList  = Syn_SelectItemList {annotatedTree_Syn_SelectItemList :: SelectItemList,env_Syn_SelectItemList :: Environment,listType_Syn_SelectItemList :: Type}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType) =
             (sem _lhsIenv )
     in  (Syn_SelectItemList _lhsOannotatedTree _lhsOenv _lhsOlistType ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: Type
              _lhsOannotatedTree :: SelectItemList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: SelectItem
              _hdIcolumnName :: String
              _hdIenv :: Environment
              _hdIitemType :: Type
              _tlIannotatedTree :: SelectItemList
              _tlIenv :: Environment
              _tlIlistType :: Type
              _lhsOlistType =
                  {-# LINE 564 "./TypeChecking.ag" #-}
                  doSelectItemListTpe _lhsIenv _hdIcolumnName _hdIitemType _tlIlistType
                  {-# LINE 4713 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 4717 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4721 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 4725 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4729 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 4733 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIcolumnName,_hdIenv,_hdIitemType) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv,_tlIlistType) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: Type
              _lhsOannotatedTree :: SelectItemList
              _lhsOenv :: Environment
              _lhsOlistType =
                  {-# LINE 565 "./TypeChecking.ag" #-}
                  UnnamedCompositeType []
                  {-# LINE 4748 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4752 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4756 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4760 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType)))
-- SelectList --------------------------------------------------
data SelectList  = SelectList (SelectItemList) (StringList) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _items _stringList )  =
    (sem_SelectList_SelectList (sem_SelectItemList _items ) (sem_StringList _stringList ) )
-- semantic domain
type T_SelectList  = Environment ->
                     ( SelectList,Environment,Type)
data Inh_SelectList  = Inh_SelectList {env_Inh_SelectList :: Environment}
data Syn_SelectList  = Syn_SelectList {annotatedTree_Syn_SelectList :: SelectList,env_Syn_SelectList :: Environment,listType_Syn_SelectList :: Type}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType) =
             (sem _lhsIenv )
     in  (Syn_SelectList _lhsOannotatedTree _lhsOenv _lhsOlistType ))
sem_SelectList_SelectList :: T_SelectItemList  ->
                             T_StringList  ->
                             T_SelectList 
sem_SelectList_SelectList items_ stringList_  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: Type
              _lhsOannotatedTree :: SelectList
              _lhsOenv :: Environment
              _itemsOenv :: Environment
              _stringListOenv :: Environment
              _itemsIannotatedTree :: SelectItemList
              _itemsIenv :: Environment
              _itemsIlistType :: Type
              _stringListIannotatedTree :: StringList
              _stringListIenv :: Environment
              _stringListIstrings :: ([String])
              _lhsOlistType =
                  {-# LINE 610 "./TypeChecking.ag" #-}
                  _itemsIlistType
                  {-# LINE 4801 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SelectList _itemsIannotatedTree _stringListIannotatedTree
                  {-# LINE 4805 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4809 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 4813 "AstInternal.hs" #-}
              _itemsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4817 "AstInternal.hs" #-}
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _itemsIenv
                  {-# LINE 4821 "AstInternal.hs" #-}
              ( _itemsIannotatedTree,_itemsIenv,_itemsIlistType) =
                  (items_ _itemsOenv )
              ( _stringListIannotatedTree,_stringListIenv,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType)))
-- SetClause ---------------------------------------------------
data SetClause  = RowSetClause (StringList) (ExpressionList) 
                | SetClause (String) (Expression) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SetClause :: SetClause  ->
                 T_SetClause 
sem_SetClause (RowSetClause _atts _vals )  =
    (sem_SetClause_RowSetClause (sem_StringList _atts ) (sem_ExpressionList _vals ) )
sem_SetClause (SetClause _att _val )  =
    (sem_SetClause_SetClause _att (sem_Expression _val ) )
-- semantic domain
type T_SetClause  = Environment ->
                    ( SetClause,Environment,([(String,Type)]),(Maybe TypeError))
data Inh_SetClause  = Inh_SetClause {env_Inh_SetClause :: Environment}
data Syn_SetClause  = Syn_SetClause {annotatedTree_Syn_SetClause :: SetClause,env_Syn_SetClause :: Environment,pairs_Syn_SetClause :: [(String,Type)],rowSetError_Syn_SetClause :: Maybe TypeError}
wrap_SetClause :: T_SetClause  ->
                  Inh_SetClause  ->
                  Syn_SetClause 
wrap_SetClause sem (Inh_SetClause _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOpairs,_lhsOrowSetError) =
             (sem _lhsIenv )
     in  (Syn_SetClause _lhsOannotatedTree _lhsOenv _lhsOpairs _lhsOrowSetError ))
sem_SetClause_RowSetClause :: T_StringList  ->
                              T_ExpressionList  ->
                              T_SetClause 
sem_SetClause_RowSetClause atts_ vals_  =
    (\ _lhsIenv ->
         (let _lhsOpairs :: ([(String,Type)])
              _lhsOannotatedTree :: SetClause
              _lhsOenv :: Environment
              _lhsOrowSetError :: (Maybe TypeError)
              _attsOenv :: Environment
              _valsOenv :: Environment
              _attsIannotatedTree :: StringList
              _attsIenv :: Environment
              _attsIstrings :: ([String])
              _valsIannotatedTree :: ExpressionList
              _valsIenv :: Environment
              _valsItypeList :: ([Type])
              _rowSetError =
                  {-# LINE 807 "./TypeChecking.ag" #-}
                  let atts = _attsIstrings
                      types = getRowTypes _valsItypeList
                  in if length atts /= length types
                       then Just WrongNumberOfColumns
                       else Nothing
                  {-# LINE 4874 "AstInternal.hs" #-}
              _lhsOpairs =
                  {-# LINE 813 "./TypeChecking.ag" #-}
                  zip _attsIstrings $ getRowTypes _valsItypeList
                  {-# LINE 4878 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowSetClause _attsIannotatedTree _valsIannotatedTree
                  {-# LINE 4882 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4886 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valsIenv
                  {-# LINE 4890 "AstInternal.hs" #-}
              _lhsOrowSetError =
                  {-# LINE 798 "./TypeChecking.ag" #-}
                  _rowSetError
                  {-# LINE 4894 "AstInternal.hs" #-}
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4898 "AstInternal.hs" #-}
              _valsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 4902 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIenv,_attsIstrings) =
                  (atts_ _attsOenv )
              ( _valsIannotatedTree,_valsIenv,_valsItypeList) =
                  (vals_ _valsOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOpairs,_lhsOrowSetError)))
sem_SetClause_SetClause :: String ->
                           T_Expression  ->
                           T_SetClause 
sem_SetClause_SetClause att_ val_  =
    (\ _lhsIenv ->
         (let _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetError :: (Maybe TypeError)
              _lhsOannotatedTree :: SetClause
              _lhsOenv :: Environment
              _valOenv :: Environment
              _valIannotatedTree :: Expression
              _valIenv :: Environment
              _valIliftedColumnName :: String
              _lhsOpairs =
                  {-# LINE 804 "./TypeChecking.ag" #-}
                  [(att_, getTypeAnnotation _valIannotatedTree)]
                  {-# LINE 4924 "AstInternal.hs" #-}
              _lhsOrowSetError =
                  {-# LINE 805 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 4928 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SetClause att_ _valIannotatedTree
                  {-# LINE 4932 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4936 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valIenv
                  {-# LINE 4940 "AstInternal.hs" #-}
              _valOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4944 "AstInternal.hs" #-}
              ( _valIannotatedTree,_valIenv,_valIliftedColumnName) =
                  (val_ _valOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOpairs,_lhsOrowSetError)))
-- SetClauseList -----------------------------------------------
type SetClauseList  = [(SetClause)]
-- cata
sem_SetClauseList :: SetClauseList  ->
                     T_SetClauseList 
sem_SetClauseList list  =
    (Prelude.foldr sem_SetClauseList_Cons sem_SetClauseList_Nil (Prelude.map sem_SetClause list) )
-- semantic domain
type T_SetClauseList  = Environment ->
                        ( SetClauseList,Environment,([(String,Type)]),([TypeError]))
data Inh_SetClauseList  = Inh_SetClauseList {env_Inh_SetClauseList :: Environment}
data Syn_SetClauseList  = Syn_SetClauseList {annotatedTree_Syn_SetClauseList :: SetClauseList,env_Syn_SetClauseList :: Environment,pairs_Syn_SetClauseList :: [(String,Type)],rowSetErrors_Syn_SetClauseList :: [TypeError]}
wrap_SetClauseList :: T_SetClauseList  ->
                      Inh_SetClauseList  ->
                      Syn_SetClauseList 
wrap_SetClauseList sem (Inh_SetClauseList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOpairs,_lhsOrowSetErrors) =
             (sem _lhsIenv )
     in  (Syn_SetClauseList _lhsOannotatedTree _lhsOenv _lhsOpairs _lhsOrowSetErrors ))
sem_SetClauseList_Cons :: T_SetClause  ->
                          T_SetClauseList  ->
                          T_SetClauseList 
sem_SetClauseList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetErrors :: ([TypeError])
              _lhsOannotatedTree :: SetClauseList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: SetClause
              _hdIenv :: Environment
              _hdIpairs :: ([(String,Type)])
              _hdIrowSetError :: (Maybe TypeError)
              _tlIannotatedTree :: SetClauseList
              _tlIenv :: Environment
              _tlIpairs :: ([(String,Type)])
              _tlIrowSetErrors :: ([TypeError])
              _lhsOpairs =
                  {-# LINE 789 "./TypeChecking.ag" #-}
                  _hdIpairs ++ _tlIpairs
                  {-# LINE 4989 "AstInternal.hs" #-}
              _lhsOrowSetErrors =
                  {-# LINE 790 "./TypeChecking.ag" #-}
                  maybeToList _hdIrowSetError ++ _tlIrowSetErrors
                  {-# LINE 4993 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 4997 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5001 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 5005 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5009 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 5013 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv,_hdIpairs,_hdIrowSetError) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv,_tlIpairs,_tlIrowSetErrors) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOpairs,_lhsOrowSetErrors)))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetErrors :: ([TypeError])
              _lhsOannotatedTree :: SetClauseList
              _lhsOenv :: Environment
              _lhsOpairs =
                  {-# LINE 791 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5029 "AstInternal.hs" #-}
              _lhsOrowSetErrors =
                  {-# LINE 792 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5033 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5037 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5041 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5045 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOpairs,_lhsOrowSetErrors)))
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
                | Return (Annotation) (MaybeExpression) 
                | ReturnNext (Annotation) (Expression) 
                | ReturnQuery (Annotation) (SelectExpression) 
                | SelectStatement (Annotation) (SelectExpression) 
                | Truncate (Annotation) (StringList) (RestartIdentity) (Cascade) 
                | Update (Annotation) (String) (SetClauseList) (Where) (Maybe SelectList) 
                | WhileStatement (Annotation) (Expression) (StatementList) 
                deriving ( Data,Eq,Show,Typeable)
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
    (sem_Statement_Return _ann (sem_MaybeExpression _value ) )
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
type T_Statement  = Environment ->
                    ( Statement,Environment,([EnvironmentUpdate]))
data Inh_Statement  = Inh_Statement {env_Inh_Statement :: Environment}
data Syn_Statement  = Syn_Statement {annotatedTree_Syn_Statement :: Statement,env_Syn_Statement :: Environment,envUpdates_Syn_Statement :: [EnvironmentUpdate]}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates) =
             (sem _lhsIenv )
     in  (Syn_Statement _lhsOannotatedTree _lhsOenv _lhsOenvUpdates ))
sem_Statement_Assignment :: Annotation ->
                            String ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _valueOenv :: Environment
              _valueIannotatedTree :: Expression
              _valueIenv :: Environment
              _valueIliftedColumnName :: String
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5170 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Assignment ann_ target_ _valueIannotatedTree
                  {-# LINE 5174 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5178 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valueIenv
                  {-# LINE 5182 "AstInternal.hs" #-}
              _valueOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5186 "AstInternal.hs" #-}
              ( _valueIannotatedTree,_valueIenv,_valueIliftedColumnName) =
                  (value_ _valueOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_CaseStatement :: Annotation ->
                               T_Expression  ->
                               T_ExpressionListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement ann_ val_ cases_ els_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _elsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _valOenv :: Environment
              _casesOenv :: Environment
              _elsOenv :: Environment
              _valIannotatedTree :: Expression
              _valIenv :: Environment
              _valIliftedColumnName :: String
              _casesIannotatedTree :: ExpressionListStatementListPairList
              _casesIenv :: Environment
              _elsIannotatedTree :: StatementList
              _elsIenv :: Environment
              _elsIenvUpdates :: ([EnvironmentUpdate])
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5215 "AstInternal.hs" #-}
              _elsOenvUpdates =
                  {-# LINE 370 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5219 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CaseStatement ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 5223 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5227 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 5231 "AstInternal.hs" #-}
              _valOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5235 "AstInternal.hs" #-}
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valIenv
                  {-# LINE 5239 "AstInternal.hs" #-}
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 5243 "AstInternal.hs" #-}
              ( _valIannotatedTree,_valIenv,_valIliftedColumnName) =
                  (val_ _valOenv )
              ( _casesIannotatedTree,_casesIenv) =
                  (cases_ _casesOenv )
              ( _elsIannotatedTree,_elsIenv,_elsIenvUpdates) =
                  (els_ _elsOenv _elsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_ContinueStatement :: Annotation ->
                                   T_Statement 
sem_Statement_ContinueStatement ann_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5261 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ContinueStatement ann_
                  {-# LINE 5265 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5269 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5273 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_Copy :: Annotation ->
                      String ->
                      T_StringList  ->
                      T_CopySource  ->
                      T_Statement 
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _targetColsOenv :: Environment
              _sourceOenv :: Environment
              _targetColsIannotatedTree :: StringList
              _targetColsIenv :: Environment
              _targetColsIstrings :: ([String])
              _sourceIannotatedTree :: CopySource
              _sourceIenv :: Environment
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5295 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Copy ann_ table_ _targetColsIannotatedTree _sourceIannotatedTree
                  {-# LINE 5299 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5303 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sourceIenv
                  {-# LINE 5307 "AstInternal.hs" #-}
              _targetColsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5311 "AstInternal.hs" #-}
              _sourceOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _targetColsIenv
                  {-# LINE 5315 "AstInternal.hs" #-}
              ( _targetColsIannotatedTree,_targetColsIenv,_targetColsIstrings) =
                  (targetCols_ _targetColsOenv )
              ( _sourceIannotatedTree,_sourceIenv) =
                  (source_ _sourceOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_CopyData :: Annotation ->
                          String ->
                          T_Statement 
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5332 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 5336 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5340 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5344 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_CreateDomain :: Annotation ->
                              String ->
                              T_TypeName  ->
                              (Maybe Expression) ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ check_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree =
                  {-# LINE 325 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 5367 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 5371 "AstInternal.hs" #-}
              _namedTypeType =
                  {-# LINE 965 "./TypeChecking.ag" #-}
                  case _typInamedType of
                    Left _ -> TypeCheckFailed
                    Right x -> x
                  {-# LINE 5377 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 968 "./TypeChecking.ag" #-}
                  checkTypes [_namedTypeType    ] $ Right $ Pseudo Void
                  {-# LINE 5381 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 969 "./TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree check_
                  {-# LINE 5385 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 970 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5389 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 971 "./TypeChecking.ag" #-}
                  [EnvCreateDomain (ScalarType name_) _namedTypeType    ]
                  {-# LINE 5393 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree check_
                  {-# LINE 5397 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 5401 "AstInternal.hs" #-}
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5405 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
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
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _bodyOenv :: Environment
              _lhsOenv :: Environment
              _langOenv :: Environment
              _paramsOenv :: Environment
              _rettypeOenv :: Environment
              _volOenv :: Environment
              _langIannotatedTree :: Language
              _langIenv :: Environment
              _paramsIannotatedTree :: ParamDefList
              _paramsIenv :: Environment
              _paramsIparams :: ([(String,Either [TypeError] Type)])
              _rettypeIannotatedTree :: TypeName
              _rettypeIenv :: Environment
              _rettypeInamedType :: (Either [TypeError] Type)
              _bodyIannotatedTree :: FnBody
              _bodyIenv :: Environment
              _volIannotatedTree :: Volatility
              _volIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 325 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 5447 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 5451 "AstInternal.hs" #-}
              _retTypeType =
                  {-# LINE 1012 "./TypeChecking.ag" #-}
                  errorToTypeFail _rettypeInamedType
                  {-# LINE 5455 "AstInternal.hs" #-}
              _paramTypes =
                  {-# LINE 1013 "./TypeChecking.ag" #-}
                  let tpes = map snd _paramsIparams
                  in if null $ concat $ lefts tpes
                     then rights tpes
                     else [TypeCheckFailed]
                  {-# LINE 5462 "AstInternal.hs" #-}
              _paramNameTypes =
                  {-# LINE 1018 "./TypeChecking.ag" #-}
                  mapMaybe (\(n,tpe) -> case tpe of
                                        Left _ -> Nothing
                                        Right t -> Just (n,t)) _paramsIparams
                  {-# LINE 5468 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 1022 "./TypeChecking.ag" #-}
                  do
                    _rettypeInamedType
                    let tpes = map snd _paramsIparams
                    checkErrorList (concat $ lefts tpes) $ Pseudo Void
                  {-# LINE 5475 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 1027 "./TypeChecking.ag" #-}
                  CreateFunction ann_
                                 _langIannotatedTree
                                 name_
                                 _paramsIannotatedTree
                                 _rettypeIannotatedTree
                                 bodyQuote_
                                 _bodyIannotatedTree
                                 _volIannotatedTree
                  {-# LINE 5486 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 1035 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5490 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 1036 "./TypeChecking.ag" #-}
                  [EnvCreateFunction FunName name_ _paramTypes     _retTypeType    ]
                  {-# LINE 5494 "AstInternal.hs" #-}
              _bodyOenv =
                  {-# LINE 1038 "./TypeChecking.ag" #-}
                  if _paramTypes     == [TypeCheckFailed]
                    then _lhsIenv
                    else fromRight _lhsIenv $ updateEnvironment _lhsIenv [EnvStackIDs [("", _paramNameTypes    ), (name_, _paramNameTypes    )]]
                  {-# LINE 5500 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateFunction ann_ _langIannotatedTree name_ _paramsIannotatedTree _rettypeIannotatedTree bodyQuote_ _bodyIannotatedTree _volIannotatedTree
                  {-# LINE 5504 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _volIenv
                  {-# LINE 5508 "AstInternal.hs" #-}
              _langOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5512 "AstInternal.hs" #-}
              _paramsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _langIenv
                  {-# LINE 5516 "AstInternal.hs" #-}
              _rettypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _paramsIenv
                  {-# LINE 5520 "AstInternal.hs" #-}
              _volOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _bodyIenv
                  {-# LINE 5524 "AstInternal.hs" #-}
              ( _langIannotatedTree,_langIenv) =
                  (lang_ _langOenv )
              ( _paramsIannotatedTree,_paramsIenv,_paramsIparams) =
                  (params_ _paramsOenv )
              ( _rettypeIannotatedTree,_rettypeIenv,_rettypeInamedType) =
                  (rettype_ _rettypeOenv )
              ( _bodyIannotatedTree,_bodyIenv) =
                  (body_ _bodyOenv )
              ( _volIannotatedTree,_volIenv) =
                  (vol_ _volOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_CreateTable :: Annotation ->
                             String ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _attsOenv :: Environment
              _consOenv :: Environment
              _attsIannotatedTree :: AttributeDefList
              _attsIattrs :: ([(String, Either [TypeError] Type)])
              _attsIenv :: Environment
              _consIannotatedTree :: ConstraintList
              _consIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 325 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 5560 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 5564 "AstInternal.hs" #-}
              _attrTypes =
                  {-# LINE 874 "./TypeChecking.ag" #-}
                  map snd _attsIattrs
                  {-# LINE 5568 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 875 "./TypeChecking.ag" #-}
                  checkErrorList (concat $ lefts _attrTypes    ) $ Pseudo Void
                  {-# LINE 5572 "AstInternal.hs" #-}
              _compositeType =
                  {-# LINE 876 "./TypeChecking.ag" #-}
                  errorToTypeFailF (const $ UnnamedCompositeType doneAtts) _tpe
                  where
                    doneAtts = map (second errorToTypeFail) _attsIattrs
                  {-# LINE 5578 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 880 "./TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
                  {-# LINE 5582 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 881 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5586 "AstInternal.hs" #-}
              _attrs =
                  {-# LINE 882 "./TypeChecking.ag" #-}
                  case _compositeType     of
                    UnnamedCompositeType c -> c
                    _-> []
                  {-# LINE 5592 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 885 "./TypeChecking.ag" #-}
                  [EnvCreateTable name_ _attrs     []]
                  {-# LINE 5596 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
                  {-# LINE 5600 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _consIenv
                  {-# LINE 5604 "AstInternal.hs" #-}
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5608 "AstInternal.hs" #-}
              _consOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 5612 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIattrs,_attsIenv) =
                  (atts_ _attsOenv )
              ( _consIannotatedTree,_consIenv) =
                  (cons_ _consOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_CreateTableAs :: Annotation ->
                               String ->
                               T_SelectExpression  ->
                               T_Statement 
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _exprOenv :: Environment
              _exprIannotatedTree :: SelectExpression
              _exprIenv :: Environment
              _selType =
                  {-# LINE 889 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exprIannotatedTree
                  {-# LINE 5633 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 890 "./TypeChecking.ag" #-}
                  Right _selType
                  {-# LINE 5637 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 891 "./TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 5641 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 892 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5645 "AstInternal.hs" #-}
              _attrs =
                  {-# LINE 893 "./TypeChecking.ag" #-}
                  case _selType     of
                    UnnamedCompositeType c -> c
                    _-> []
                  {-# LINE 5651 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 896 "./TypeChecking.ag" #-}
                  [EnvCreateTable name_ _attrs     []]
                  {-# LINE 5655 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 5659 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5663 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 5667 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 335 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 5671 "AstInternal.hs" #-}
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5675 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_CreateType :: Annotation ->
                            String ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _attsOenv :: Environment
              _attsIannotatedTree :: TypeAttributeDefList
              _attsIattrs :: ([(String, Either [TypeError] Type)])
              _attsIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 325 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 5699 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 5703 "AstInternal.hs" #-}
              _attrTypes =
                  {-# LINE 947 "./TypeChecking.ag" #-}
                  map snd _attsIattrs
                  {-# LINE 5707 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 948 "./TypeChecking.ag" #-}
                  checkErrorList (concat $ lefts _attrTypes    ) $ Pseudo Void
                  {-# LINE 5711 "AstInternal.hs" #-}
              _doneAtts =
                  {-# LINE 950 "./TypeChecking.ag" #-}
                  map (second errorToTypeFail) _attsIattrs
                  {-# LINE 5715 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 951 "./TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 5719 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 952 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5723 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 953 "./TypeChecking.ag" #-}
                  [EnvCreateComposite name_ _doneAtts    ]
                  {-# LINE 5727 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 5731 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 5735 "AstInternal.hs" #-}
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5739 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIattrs,_attsIenv) =
                  (atts_ _attsOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_CreateView :: Annotation ->
                            String ->
                            T_SelectExpression  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ expr_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _exprOenv :: Environment
              _exprIannotatedTree :: SelectExpression
              _exprIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 325 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 5762 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 5766 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 906 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _exprIannotatedTree] $ Right $ Pseudo Void
                  {-# LINE 5770 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 907 "./TypeChecking.ag" #-}
                  CreateView ann_ name_ _exprIannotatedTree
                  {-# LINE 5774 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 908 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5778 "AstInternal.hs" #-}
              _attrs =
                  {-# LINE 909 "./TypeChecking.ag" #-}
                  case getTypeAnnotation _exprIannotatedTree of
                    SetOfType (UnnamedCompositeType c) -> c
                    _ -> []
                  {-# LINE 5784 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 912 "./TypeChecking.ag" #-}
                  [EnvCreateView name_ _attrs    ]
                  {-# LINE 5788 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateView ann_ name_ _exprIannotatedTree
                  {-# LINE 5792 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 5796 "AstInternal.hs" #-}
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5800 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_Delete :: Annotation ->
                        String ->
                        T_Where  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Delete ann_ table_ whr_ returning_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _whrOenv :: Environment
              _whrIannotatedTree :: Where
              _whrIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 325 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 5824 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 5828 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 829 "./TypeChecking.ag" #-}
                  case checkRelationExists _lhsIenv table_ of
                    Just e -> Left [e]
                    Nothing -> do
                      whereType <- checkExpressionBool _whrIannotatedTree
                      return $ Pseudo Void
                  {-# LINE 5836 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 835 "./TypeChecking.ag" #-}
                  [DeleteInfo table_]
                  {-# LINE 5840 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 836 "./TypeChecking.ag" #-}
                  Delete ann_ table_ _whrIannotatedTree returning_
                  {-# LINE 5844 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 837 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5848 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Delete ann_ table_ _whrIannotatedTree returning_
                  {-# LINE 5852 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _whrIenv
                  {-# LINE 5856 "AstInternal.hs" #-}
              _whrOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5860 "AstInternal.hs" #-}
              ( _whrIannotatedTree,_whrIenv) =
                  (whr_ _whrOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_DropFunction :: Annotation ->
                              T_IfExists  ->
                              T_StringStringListPairList  ->
                              T_Cascade  ->
                              T_Statement 
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _ifEOenv :: Environment
              _sigsOenv :: Environment
              _cascadeOenv :: Environment
              _ifEIannotatedTree :: IfExists
              _ifEIenv :: Environment
              _sigsIannotatedTree :: StringStringListPairList
              _sigsIenv :: Environment
              _cascadeIannotatedTree :: Cascade
              _cascadeIenv :: Environment
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5886 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  DropFunction ann_ _ifEIannotatedTree _sigsIannotatedTree _cascadeIannotatedTree
                  {-# LINE 5890 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5894 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _cascadeIenv
                  {-# LINE 5898 "AstInternal.hs" #-}
              _ifEOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5902 "AstInternal.hs" #-}
              _sigsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _ifEIenv
                  {-# LINE 5906 "AstInternal.hs" #-}
              _cascadeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sigsIenv
                  {-# LINE 5910 "AstInternal.hs" #-}
              ( _ifEIannotatedTree,_ifEIenv) =
                  (ifE_ _ifEOenv )
              ( _sigsIannotatedTree,_sigsIenv) =
                  (sigs_ _sigsOenv )
              ( _cascadeIannotatedTree,_cascadeIenv) =
                  (cascade_ _cascadeOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_DropSomething :: Annotation ->
                               T_DropType  ->
                               T_IfExists  ->
                               T_StringList  ->
                               T_Cascade  ->
                               T_Statement 
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _dropTypeOenv :: Environment
              _ifEOenv :: Environment
              _namesOenv :: Environment
              _cascadeOenv :: Environment
              _dropTypeIannotatedTree :: DropType
              _dropTypeIenv :: Environment
              _ifEIannotatedTree :: IfExists
              _ifEIenv :: Environment
              _namesIannotatedTree :: StringList
              _namesIenv :: Environment
              _namesIstrings :: ([String])
              _cascadeIannotatedTree :: Cascade
              _cascadeIenv :: Environment
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5945 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  DropSomething ann_ _dropTypeIannotatedTree _ifEIannotatedTree _namesIannotatedTree _cascadeIannotatedTree
                  {-# LINE 5949 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5953 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _cascadeIenv
                  {-# LINE 5957 "AstInternal.hs" #-}
              _dropTypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5961 "AstInternal.hs" #-}
              _ifEOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _dropTypeIenv
                  {-# LINE 5965 "AstInternal.hs" #-}
              _namesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _ifEIenv
                  {-# LINE 5969 "AstInternal.hs" #-}
              _cascadeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _namesIenv
                  {-# LINE 5973 "AstInternal.hs" #-}
              ( _dropTypeIannotatedTree,_dropTypeIenv) =
                  (dropType_ _dropTypeOenv )
              ( _ifEIannotatedTree,_ifEIenv) =
                  (ifE_ _ifEOenv )
              ( _namesIannotatedTree,_namesIenv,_namesIstrings) =
                  (names_ _namesOenv )
              ( _cascadeIannotatedTree,_cascadeIenv) =
                  (cascade_ _cascadeOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_Execute :: Annotation ->
                         T_Expression  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIenv :: Environment
              _exprIliftedColumnName :: String
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5998 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Execute ann_ _exprIannotatedTree
                  {-# LINE 6002 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6006 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 6010 "AstInternal.hs" #-}
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6014 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_ExecuteInto :: Annotation ->
                             T_Expression  ->
                             T_StringList  ->
                             T_Statement 
sem_Statement_ExecuteInto ann_ expr_ targets_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _exprOenv :: Environment
              _targetsOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIenv :: Environment
              _exprIliftedColumnName :: String
              _targetsIannotatedTree :: StringList
              _targetsIenv :: Environment
              _targetsIstrings :: ([String])
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6038 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIannotatedTree _targetsIannotatedTree
                  {-# LINE 6042 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6046 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _targetsIenv
                  {-# LINE 6050 "AstInternal.hs" #-}
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6054 "AstInternal.hs" #-}
              _targetsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 6058 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              ( _targetsIannotatedTree,_targetsIenv,_targetsIstrings) =
                  (targets_ _targetsOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_ForIntegerStatement :: Annotation ->
                                     String ->
                                     T_Expression  ->
                                     T_Expression  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement ann_ var_ from_ to_ sts_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _fromOenv :: Environment
              _toOenv :: Environment
              _stsOenv :: Environment
              _fromIannotatedTree :: Expression
              _fromIenv :: Environment
              _fromIliftedColumnName :: String
              _toIannotatedTree :: Expression
              _toIenv :: Environment
              _toIliftedColumnName :: String
              _stsIannotatedTree :: StatementList
              _stsIenv :: Environment
              _stsIenvUpdates :: ([EnvironmentUpdate])
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6091 "AstInternal.hs" #-}
              _stsOenvUpdates =
                  {-# LINE 373 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6095 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ForIntegerStatement ann_ var_ _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 6099 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6103 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 6107 "AstInternal.hs" #-}
              _fromOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6111 "AstInternal.hs" #-}
              _toOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fromIenv
                  {-# LINE 6115 "AstInternal.hs" #-}
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _toIenv
                  {-# LINE 6119 "AstInternal.hs" #-}
              ( _fromIannotatedTree,_fromIenv,_fromIliftedColumnName) =
                  (from_ _fromOenv )
              ( _toIannotatedTree,_toIenv,_toIliftedColumnName) =
                  (to_ _toOenv )
              ( _stsIannotatedTree,_stsIenv,_stsIenvUpdates) =
                  (sts_ _stsOenv _stsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_ForSelectStatement :: Annotation ->
                                    String ->
                                    T_SelectExpression  ->
                                    T_StatementList  ->
                                    T_Statement 
sem_Statement_ForSelectStatement ann_ var_ sel_ sts_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _selOenv :: Environment
              _stsOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIenv :: Environment
              _stsIannotatedTree :: StatementList
              _stsIenv :: Environment
              _stsIenvUpdates :: ([EnvironmentUpdate])
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6148 "AstInternal.hs" #-}
              _stsOenvUpdates =
                  {-# LINE 373 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6152 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ForSelectStatement ann_ var_ _selIannotatedTree _stsIannotatedTree
                  {-# LINE 6156 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6160 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 6164 "AstInternal.hs" #-}
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6168 "AstInternal.hs" #-}
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 6172 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIenv) =
                  (sel_ _selOenv )
              ( _stsIannotatedTree,_stsIenv,_stsIenvUpdates) =
                  (sts_ _stsOenv _stsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_If :: Annotation ->
                    T_ExpressionStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _elsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _casesOenv :: Environment
              _elsOenv :: Environment
              _casesIannotatedTree :: ExpressionStatementListPairList
              _casesIenv :: Environment
              _elsIannotatedTree :: StatementList
              _elsIenv :: Environment
              _elsIenvUpdates :: ([EnvironmentUpdate])
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6198 "AstInternal.hs" #-}
              _elsOenvUpdates =
                  {-# LINE 370 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6202 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  If ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 6206 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6210 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 6214 "AstInternal.hs" #-}
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6218 "AstInternal.hs" #-}
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 6222 "AstInternal.hs" #-}
              ( _casesIannotatedTree,_casesIenv) =
                  (cases_ _casesOenv )
              ( _elsIannotatedTree,_elsIenv,_elsIenvUpdates) =
                  (els_ _elsOenv _elsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_Insert :: Annotation ->
                        String ->
                        T_StringList  ->
                        T_SelectExpression  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _targetColsOenv :: Environment
              _insDataOenv :: Environment
              _targetColsIannotatedTree :: StringList
              _targetColsIenv :: Environment
              _targetColsIstrings :: ([String])
              _insDataIannotatedTree :: SelectExpression
              _insDataIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 325 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6253 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6257 "AstInternal.hs" #-}
              _columnStuff =
                  {-# LINE 722 "./TypeChecking.ag" #-}
                  checkColumnConsistency _lhsIenv
                                         table_
                                         _targetColsIstrings
                                         (getCAtts $ getTypeAnnotation _insDataIannotatedTree)
                  {-# LINE 6264 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 727 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _insDataIannotatedTree] $ do
                    _columnStuff
                    Right $ Pseudo Void
                  {-# LINE 6270 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 731 "./TypeChecking.ag" #-}
                  [InsertInfo table_ $ errorToTypeFailF UnnamedCompositeType _columnStuff    ]
                  {-# LINE 6274 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 733 "./TypeChecking.ag" #-}
                  Insert ann_ table_ _targetColsIannotatedTree
                         _insDataIannotatedTree returning_
                  {-# LINE 6279 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 735 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6283 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Insert ann_ table_ _targetColsIannotatedTree _insDataIannotatedTree returning_
                  {-# LINE 6287 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _insDataIenv
                  {-# LINE 6291 "AstInternal.hs" #-}
              _targetColsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6295 "AstInternal.hs" #-}
              _insDataOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _targetColsIenv
                  {-# LINE 6299 "AstInternal.hs" #-}
              ( _targetColsIannotatedTree,_targetColsIenv,_targetColsIstrings) =
                  (targetCols_ _targetColsOenv )
              ( _insDataIannotatedTree,_insDataIenv) =
                  (insData_ _insDataOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_NullStatement :: Annotation ->
                               T_Statement 
sem_Statement_NullStatement ann_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6315 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 6319 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6323 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6327 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_Perform :: Annotation ->
                         T_Expression  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIenv :: Environment
              _exprIliftedColumnName :: String
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6344 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Perform ann_ _exprIannotatedTree
                  {-# LINE 6348 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6352 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 6356 "AstInternal.hs" #-}
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6360 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_Raise :: Annotation ->
                       T_RaiseType  ->
                       String ->
                       T_ExpressionList  ->
                       T_Statement 
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _levelOenv :: Environment
              _argsOenv :: Environment
              _levelIannotatedTree :: RaiseType
              _levelIenv :: Environment
              _argsIannotatedTree :: ExpressionList
              _argsIenv :: Environment
              _argsItypeList :: ([Type])
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6384 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Raise ann_ _levelIannotatedTree message_ _argsIannotatedTree
                  {-# LINE 6388 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6392 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _argsIenv
                  {-# LINE 6396 "AstInternal.hs" #-}
              _levelOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6400 "AstInternal.hs" #-}
              _argsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _levelIenv
                  {-# LINE 6404 "AstInternal.hs" #-}
              ( _levelIannotatedTree,_levelIenv) =
                  (level_ _levelOenv )
              ( _argsIannotatedTree,_argsIenv,_argsItypeList) =
                  (args_ _argsOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_Return :: Annotation ->
                        T_MaybeExpression  ->
                        T_Statement 
sem_Statement_Return ann_ value_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _valueOenv :: Environment
              _valueIannotatedTree :: MaybeExpression
              _valueIenv :: Environment
              _valueIexprType :: (Maybe Type)
              _lhsOannotatedTree =
                  {-# LINE 325 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6429 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6433 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 1075 "./TypeChecking.ag" #-}
                  checkTypes [fromMaybe typeBool _valueIexprType] $ Right $ Pseudo Void
                  {-# LINE 6437 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 1076 "./TypeChecking.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 6441 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 1077 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6445 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 1078 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6449 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 6453 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valueIenv
                  {-# LINE 6457 "AstInternal.hs" #-}
              _valueOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6461 "AstInternal.hs" #-}
              ( _valueIannotatedTree,_valueIenv,_valueIexprType) =
                  (value_ _valueOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_ReturnNext :: Annotation ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIenv :: Environment
              _exprIliftedColumnName :: String
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6480 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIannotatedTree
                  {-# LINE 6484 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6488 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 6492 "AstInternal.hs" #-}
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6496 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_ReturnQuery :: Annotation ->
                             T_SelectExpression  ->
                             T_Statement 
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIenv :: Environment
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6514 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIannotatedTree
                  {-# LINE 6518 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6522 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 6526 "AstInternal.hs" #-}
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6530 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIenv) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_SelectStatement :: Annotation ->
                                 T_SelectExpression  ->
                                 T_Statement 
sem_Statement_SelectStatement ann_ ex_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _exOenv :: Environment
              _exIannotatedTree :: SelectExpression
              _exIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 325 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6552 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6556 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 387 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _exIannotatedTree] $ Right $ Pseudo Void
                  {-# LINE 6560 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 388 "./TypeChecking.ag" #-}
                  [SelectInfo $ getTypeAnnotation _exIannotatedTree]
                  {-# LINE 6564 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 389 "./TypeChecking.ag" #-}
                  SelectStatement ann_ _exIannotatedTree
                  {-# LINE 6568 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 390 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6572 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SelectStatement ann_ _exIannotatedTree
                  {-# LINE 6576 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exIenv
                  {-# LINE 6580 "AstInternal.hs" #-}
              _exOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6584 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIenv) =
                  (ex_ _exOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_Truncate :: Annotation ->
                          T_StringList  ->
                          T_RestartIdentity  ->
                          T_Cascade  ->
                          T_Statement 
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _tablesOenv :: Environment
              _restartIdentityOenv :: Environment
              _cascadeOenv :: Environment
              _tablesIannotatedTree :: StringList
              _tablesIenv :: Environment
              _tablesIstrings :: ([String])
              _restartIdentityIannotatedTree :: RestartIdentity
              _restartIdentityIenv :: Environment
              _cascadeIannotatedTree :: Cascade
              _cascadeIenv :: Environment
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6611 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Truncate ann_ _tablesIannotatedTree _restartIdentityIannotatedTree _cascadeIannotatedTree
                  {-# LINE 6615 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6619 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _cascadeIenv
                  {-# LINE 6623 "AstInternal.hs" #-}
              _tablesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6627 "AstInternal.hs" #-}
              _restartIdentityOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tablesIenv
                  {-# LINE 6631 "AstInternal.hs" #-}
              _cascadeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _restartIdentityIenv
                  {-# LINE 6635 "AstInternal.hs" #-}
              ( _tablesIannotatedTree,_tablesIenv,_tablesIstrings) =
                  (tables_ _tablesOenv )
              ( _restartIdentityIannotatedTree,_restartIdentityIenv) =
                  (restartIdentity_ _restartIdentityOenv )
              ( _cascadeIannotatedTree,_cascadeIenv) =
                  (cascade_ _cascadeOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_Update :: Annotation ->
                        String ->
                        T_SetClauseList  ->
                        T_Where  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Update ann_ table_ assigns_ whr_ returning_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _assignsOenv :: Environment
              _whrOenv :: Environment
              _assignsIannotatedTree :: SetClauseList
              _assignsIenv :: Environment
              _assignsIpairs :: ([(String,Type)])
              _assignsIrowSetErrors :: ([TypeError])
              _whrIannotatedTree :: Where
              _whrIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 325 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6669 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6673 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 760 "./TypeChecking.ag" #-}
                  do
                  let re = checkRelationExists _lhsIenv table_
                  when (isJust re) $
                       Left [fromJust $ re]
                  whereType <- checkExpressionBool _whrIannotatedTree
                  chainTypeCheckFailed (whereType:map snd _assignsIpairs) $ do
                    _columnsConsistent
                    checkErrorList _assignsIrowSetErrors $ Pseudo Void
                  {-# LINE 6684 "AstInternal.hs" #-}
              _columnsConsistent =
                  {-# LINE 769 "./TypeChecking.ag" #-}
                  checkColumnConsistency _lhsIenv table_ (map fst _assignsIpairs) _assignsIpairs
                  {-# LINE 6688 "AstInternal.hs" #-}
              _statementInfo =
                  {-# LINE 771 "./TypeChecking.ag" #-}
                  [UpdateInfo table_ $ flip errorToTypeFailF _columnsConsistent     $
                                           \c -> let colNames = map fst _assignsIpairs
                                                 in UnnamedCompositeType $ map (\t -> (t,getType c t)) colNames]
                  where
                    getType cols t = fromJust $ lookup t cols
                  {-# LINE 6696 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 778 "./TypeChecking.ag" #-}
                  Update ann_ table_ _assignsIannotatedTree _whrIannotatedTree returning_
                  {-# LINE 6700 "AstInternal.hs" #-}
              _envUpdates =
                  {-# LINE 779 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6704 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Update ann_ table_ _assignsIannotatedTree _whrIannotatedTree returning_
                  {-# LINE 6708 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _whrIenv
                  {-# LINE 6712 "AstInternal.hs" #-}
              _assignsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6716 "AstInternal.hs" #-}
              _whrOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _assignsIenv
                  {-# LINE 6720 "AstInternal.hs" #-}
              ( _assignsIannotatedTree,_assignsIenv,_assignsIpairs,_assignsIrowSetErrors) =
                  (assigns_ _assignsOenv )
              ( _whrIannotatedTree,_whrIenv) =
                  (whr_ _whrOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_WhileStatement :: Annotation ->
                                T_Expression  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement ann_ expr_ sts_  =
    (\ _lhsIenv ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOenv :: Environment
              _exprOenv :: Environment
              _stsOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIenv :: Environment
              _exprIliftedColumnName :: String
              _stsIannotatedTree :: StatementList
              _stsIenv :: Environment
              _stsIenvUpdates :: ([EnvironmentUpdate])
              _lhsOenvUpdates =
                  {-# LINE 343 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6747 "AstInternal.hs" #-}
              _stsOenvUpdates =
                  {-# LINE 373 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6751 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  WhileStatement ann_ _exprIannotatedTree _stsIannotatedTree
                  {-# LINE 6755 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6759 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 6763 "AstInternal.hs" #-}
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6767 "AstInternal.hs" #-}
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 6771 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              ( _stsIannotatedTree,_stsIenv,_stsIenvUpdates) =
                  (sts_ _stsOenv _stsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
-- StatementList -----------------------------------------------
type StatementList  = [(Statement)]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_Statement list) )
-- semantic domain
type T_StatementList  = Environment ->
                        ([EnvironmentUpdate]) ->
                        ( StatementList,Environment,([EnvironmentUpdate]))
data Inh_StatementList  = Inh_StatementList {env_Inh_StatementList :: Environment,envUpdates_Inh_StatementList :: [EnvironmentUpdate]}
data Syn_StatementList  = Syn_StatementList {annotatedTree_Syn_StatementList :: StatementList,env_Syn_StatementList :: Environment,envUpdates_Syn_StatementList :: [EnvironmentUpdate]}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIenv _lhsIenvUpdates )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates) =
             (sem _lhsIenv _lhsIenvUpdates )
     in  (Syn_StatementList _lhsOannotatedTree _lhsOenv _lhsOenvUpdates ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIenvUpdates ->
         (let _hdOenv :: Environment
              _tlOenv :: Environment
              _tlOenvUpdates :: ([EnvironmentUpdate])
              _lhsOannotatedTree :: StatementList
              _lhsOenv :: Environment
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _hdIannotatedTree :: Statement
              _hdIenv :: Environment
              _hdIenvUpdates :: ([EnvironmentUpdate])
              _tlIannotatedTree :: StatementList
              _tlIenv :: Environment
              _tlIenvUpdates :: ([EnvironmentUpdate])
              _newEnv =
                  {-# LINE 357 "./TypeChecking.ag" #-}
                  fromRight _lhsIenv $ updateEnvironment _lhsIenv _lhsIenvUpdates
                  {-# LINE 6818 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 358 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 6822 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 359 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 6826 "AstInternal.hs" #-}
              _tlOenvUpdates =
                  {-# LINE 360 "./TypeChecking.ag" #-}
                  _hdIenvUpdates
                  {-# LINE 6830 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 6834 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6838 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 6842 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 348 "./TypeChecking.ag" #-}
                  _tlIenvUpdates
                  {-# LINE 6846 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv,_hdIenvUpdates) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv,_tlIenvUpdates) =
                  (tl_ _tlOenv _tlOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIenv
       _lhsIenvUpdates ->
         (let _lhsOannotatedTree :: StatementList
              _lhsOenv :: Environment
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6862 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6866 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6870 "AstInternal.hs" #-}
              _lhsOenvUpdates =
                  {-# LINE 348 "./TypeChecking.ag" #-}
                  _lhsIenvUpdates
                  {-# LINE 6874 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
-- StringList --------------------------------------------------
type StringList  = [(String)]
-- cata
sem_StringList :: StringList  ->
                  T_StringList 
sem_StringList list  =
    (Prelude.foldr sem_StringList_Cons sem_StringList_Nil list )
-- semantic domain
type T_StringList  = Environment ->
                     ( StringList,Environment,([String]))
data Inh_StringList  = Inh_StringList {env_Inh_StringList :: Environment}
data Syn_StringList  = Syn_StringList {annotatedTree_Syn_StringList :: StringList,env_Syn_StringList :: Environment,strings_Syn_StringList :: [String]}
wrap_StringList :: T_StringList  ->
                   Inh_StringList  ->
                   Syn_StringList 
wrap_StringList sem (Inh_StringList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOstrings) =
             (sem _lhsIenv )
     in  (Syn_StringList _lhsOannotatedTree _lhsOenv _lhsOstrings ))
sem_StringList_Cons :: String ->
                       T_StringList  ->
                       T_StringList 
sem_StringList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOstrings :: ([String])
              _lhsOannotatedTree :: StringList
              _lhsOenv :: Environment
              _tlOenv :: Environment
              _tlIannotatedTree :: StringList
              _tlIenv :: Environment
              _tlIstrings :: ([String])
              _lhsOstrings =
                  {-# LINE 744 "./TypeChecking.ag" #-}
                  hd_ : _tlIstrings
                  {-# LINE 6910 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) hd_ _tlIannotatedTree
                  {-# LINE 6914 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6918 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 6922 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6926 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIenv,_tlIstrings) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOstrings)))
sem_StringList_Nil :: T_StringList 
sem_StringList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOstrings :: ([String])
              _lhsOannotatedTree :: StringList
              _lhsOenv :: Environment
              _lhsOstrings =
                  {-# LINE 745 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6939 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6943 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6947 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6951 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOstrings)))
-- StringStringListPair ----------------------------------------
type StringStringListPair  = ( (String),(StringList))
-- cata
sem_StringStringListPair :: StringStringListPair  ->
                            T_StringStringListPair 
sem_StringStringListPair ( x1,x2)  =
    (sem_StringStringListPair_Tuple x1 (sem_StringList x2 ) )
-- semantic domain
type T_StringStringListPair  = Environment ->
                               ( StringStringListPair,Environment)
data Inh_StringStringListPair  = Inh_StringStringListPair {env_Inh_StringStringListPair :: Environment}
data Syn_StringStringListPair  = Syn_StringStringListPair {annotatedTree_Syn_StringStringListPair :: StringStringListPair,env_Syn_StringStringListPair :: Environment}
wrap_StringStringListPair :: T_StringStringListPair  ->
                             Inh_StringStringListPair  ->
                             Syn_StringStringListPair 
wrap_StringStringListPair sem (Inh_StringStringListPair _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_StringStringListPair _lhsOannotatedTree _lhsOenv ))
sem_StringStringListPair_Tuple :: String ->
                                  T_StringList  ->
                                  T_StringStringListPair 
sem_StringStringListPair_Tuple x1_ x2_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: StringStringListPair
              _lhsOenv :: Environment
              _x2Oenv :: Environment
              _x2IannotatedTree :: StringList
              _x2Ienv :: Environment
              _x2Istrings :: ([String])
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (x1_,_x2IannotatedTree)
                  {-# LINE 6986 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6990 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 6994 "AstInternal.hs" #-}
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6998 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2Ienv,_x2Istrings) =
                  (x2_ _x2Oenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- StringStringListPairList ------------------------------------
type StringStringListPairList  = [(StringStringListPair)]
-- cata
sem_StringStringListPairList :: StringStringListPairList  ->
                                T_StringStringListPairList 
sem_StringStringListPairList list  =
    (Prelude.foldr sem_StringStringListPairList_Cons sem_StringStringListPairList_Nil (Prelude.map sem_StringStringListPair list) )
-- semantic domain
type T_StringStringListPairList  = Environment ->
                                   ( StringStringListPairList,Environment)
data Inh_StringStringListPairList  = Inh_StringStringListPairList {env_Inh_StringStringListPairList :: Environment}
data Syn_StringStringListPairList  = Syn_StringStringListPairList {annotatedTree_Syn_StringStringListPairList :: StringStringListPairList,env_Syn_StringStringListPairList :: Environment}
wrap_StringStringListPairList :: T_StringStringListPairList  ->
                                 Inh_StringStringListPairList  ->
                                 Syn_StringStringListPairList 
wrap_StringStringListPairList sem (Inh_StringStringListPairList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_StringStringListPairList _lhsOannotatedTree _lhsOenv ))
sem_StringStringListPairList_Cons :: T_StringStringListPair  ->
                                     T_StringStringListPairList  ->
                                     T_StringStringListPairList 
sem_StringStringListPairList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: StringStringListPairList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: StringStringListPair
              _hdIenv :: Environment
              _tlIannotatedTree :: StringStringListPairList
              _tlIenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 7037 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7041 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 7045 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7049 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 7053 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_StringStringListPairList_Nil :: T_StringStringListPairList 
sem_StringStringListPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: StringStringListPairList
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7067 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7071 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7075 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- TableRef ----------------------------------------------------
data TableRef  = JoinedTref (Annotation) (TableRef) (Natural) (JoinType) (TableRef) (OnExpr) 
               | SubTref (Annotation) (SelectExpression) (String) 
               | Tref (Annotation) (String) 
               | TrefAlias (Annotation) (String) (String) 
               | TrefFun (Annotation) (Expression) 
               | TrefFunAlias (Annotation) (Expression) (String) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TableRef :: TableRef  ->
                T_TableRef 
sem_TableRef (JoinedTref _ann _tbl _nat _joinType _tbl1 _onExpr )  =
    (sem_TableRef_JoinedTref _ann (sem_TableRef _tbl ) (sem_Natural _nat ) (sem_JoinType _joinType ) (sem_TableRef _tbl1 ) (sem_OnExpr _onExpr ) )
sem_TableRef (SubTref _ann _sel _alias )  =
    (sem_TableRef_SubTref _ann (sem_SelectExpression _sel ) _alias )
sem_TableRef (Tref _ann _tbl )  =
    (sem_TableRef_Tref _ann _tbl )
sem_TableRef (TrefAlias _ann _tbl _alias )  =
    (sem_TableRef_TrefAlias _ann _tbl _alias )
sem_TableRef (TrefFun _ann _fn )  =
    (sem_TableRef_TrefFun _ann (sem_Expression _fn ) )
sem_TableRef (TrefFunAlias _ann _fn _alias )  =
    (sem_TableRef_TrefFunAlias _ann (sem_Expression _fn ) _alias )
-- semantic domain
type T_TableRef  = Environment ->
                   ( TableRef,Environment,([(String,([(String,Type)],[(String,Type)]))]),([String]))
data Inh_TableRef  = Inh_TableRef {env_Inh_TableRef :: Environment}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef,env_Syn_TableRef :: Environment,idens_Syn_TableRef :: [(String,([(String,Type)],[(String,Type)]))],joinIdens_Syn_TableRef :: [String]}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens) =
             (sem _lhsIenv )
     in  (Syn_TableRef _lhsOannotatedTree _lhsOenv _lhsOidens _lhsOjoinIdens ))
sem_TableRef_JoinedTref :: Annotation ->
                           T_TableRef  ->
                           T_Natural  ->
                           T_JoinType  ->
                           T_TableRef  ->
                           T_OnExpr  ->
                           T_TableRef 
sem_TableRef_JoinedTref ann_ tbl_ nat_ joinType_ tbl1_ onExpr_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              _lhsOenv :: Environment
              _tblOenv :: Environment
              _natOenv :: Environment
              _joinTypeOenv :: Environment
              _tbl1Oenv :: Environment
              _onExprOenv :: Environment
              _tblIannotatedTree :: TableRef
              _tblIenv :: Environment
              _tblIidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _tblIjoinIdens :: ([String])
              _natIannotatedTree :: Natural
              _natIenv :: Environment
              _joinTypeIannotatedTree :: JoinType
              _joinTypeIenv :: Environment
              _tbl1IannotatedTree :: TableRef
              _tbl1Ienv :: Environment
              _tbl1Iidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _tbl1IjoinIdens :: ([String])
              _onExprIannotatedTree :: OnExpr
              _onExprIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 460 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7150 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 502 "./TypeChecking.ag" #-}
                  checkTypes [tblt
                            ,tbl1t] $
                     case (_natIannotatedTree, _onExprIannotatedTree) of
                            (Natural, _) -> unionJoinList $
                                            commonFieldNames tblt tbl1t
                            (_,Just (JoinUsing s)) -> unionJoinList s
                            _ -> unionJoinList []
                  where
                    tblt = getTypeAnnotation _tblIannotatedTree
                    tbl1t = getTypeAnnotation _tbl1IannotatedTree
                    unionJoinList s =
                        combineTableTypesWithUsingList _lhsIenv s tblt tbl1t
                  {-# LINE 7165 "AstInternal.hs" #-}
              _lhsOidens =
                  {-# LINE 515 "./TypeChecking.ag" #-}
                  _tblIidens ++ _tbl1Iidens
                  {-# LINE 7169 "AstInternal.hs" #-}
              _lhsOjoinIdens =
                  {-# LINE 516 "./TypeChecking.ag" #-}
                  commonFieldNames (getTypeAnnotation _tblIannotatedTree)
                                   (getTypeAnnotation _tbl1IannotatedTree)
                  {-# LINE 7174 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 518 "./TypeChecking.ag" #-}
                  JoinedTref ann_
                             _tblIannotatedTree
                             _natIannotatedTree
                             _joinTypeIannotatedTree
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
                  {-# LINE 7183 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  JoinedTref ann_ _tblIannotatedTree _natIannotatedTree _joinTypeIannotatedTree _tbl1IannotatedTree _onExprIannotatedTree
                  {-# LINE 7187 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onExprIenv
                  {-# LINE 7191 "AstInternal.hs" #-}
              _tblOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7195 "AstInternal.hs" #-}
              _natOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tblIenv
                  {-# LINE 7199 "AstInternal.hs" #-}
              _joinTypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _natIenv
                  {-# LINE 7203 "AstInternal.hs" #-}
              _tbl1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _joinTypeIenv
                  {-# LINE 7207 "AstInternal.hs" #-}
              _onExprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tbl1Ienv
                  {-# LINE 7211 "AstInternal.hs" #-}
              ( _tblIannotatedTree,_tblIenv,_tblIidens,_tblIjoinIdens) =
                  (tbl_ _tblOenv )
              ( _natIannotatedTree,_natIenv) =
                  (nat_ _natOenv )
              ( _joinTypeIannotatedTree,_joinTypeIenv) =
                  (joinType_ _joinTypeOenv )
              ( _tbl1IannotatedTree,_tbl1Ienv,_tbl1Iidens,_tbl1IjoinIdens) =
                  (tbl1_ _tbl1Oenv )
              ( _onExprIannotatedTree,_onExprIenv) =
                  (onExpr_ _onExprOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_SubTref :: Annotation ->
                        T_SelectExpression  ->
                        String ->
                        T_TableRef 
sem_TableRef_SubTref ann_ sel_ alias_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              _lhsOenv :: Environment
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 460 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7242 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 466 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _selIannotatedTree] <$>
                  unwrapSetOfWhenComposite $ getTypeAnnotation _selIannotatedTree
                  {-# LINE 7247 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 468 "./TypeChecking.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 7251 "AstInternal.hs" #-}
              _lhsOidens =
                  {-# LINE 469 "./TypeChecking.ag" #-}
                  [(alias_, (fromRight [] $ getTbCols _selIannotatedTree, []))]
                  {-# LINE 7255 "AstInternal.hs" #-}
              _lhsOjoinIdens =
                  {-# LINE 470 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7259 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 7263 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 7267 "AstInternal.hs" #-}
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7271 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIenv) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_Tref :: Annotation ->
                     String ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 460 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7290 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 472 "./TypeChecking.ag" #-}
                  either Left (Right . fst) _relType
                  {-# LINE 7294 "AstInternal.hs" #-}
              _lhsOjoinIdens =
                  {-# LINE 473 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7298 "AstInternal.hs" #-}
              _relType =
                  {-# LINE 474 "./TypeChecking.ag" #-}
                  getRelationType _lhsIenv tbl_
                  {-# LINE 7302 "AstInternal.hs" #-}
              _unwrappedRelType =
                  {-# LINE 475 "./TypeChecking.ag" #-}
                  fromRight ([],[]) $
                  do
                    lrt <- _relType
                    let (UnnamedCompositeType a,UnnamedCompositeType b) = lrt
                    return (a,b)
                  {-# LINE 7310 "AstInternal.hs" #-}
              _lhsOidens =
                  {-# LINE 482 "./TypeChecking.ag" #-}
                  [(tbl_, _unwrappedRelType    )]
                  {-# LINE 7314 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 483 "./TypeChecking.ag" #-}
                  Tref ann_ tbl_
                  {-# LINE 7318 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Tref ann_ tbl_
                  {-# LINE 7322 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7326 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_TrefAlias :: Annotation ->
                          String ->
                          String ->
                          T_TableRef 
sem_TableRef_TrefAlias ann_ tbl_ alias_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOenv :: Environment
              _lhsOannotatedTree =
                  {-# LINE 460 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7344 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 472 "./TypeChecking.ag" #-}
                  either Left (Right . fst) _relType
                  {-# LINE 7348 "AstInternal.hs" #-}
              _lhsOjoinIdens =
                  {-# LINE 473 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7352 "AstInternal.hs" #-}
              _relType =
                  {-# LINE 474 "./TypeChecking.ag" #-}
                  getRelationType _lhsIenv tbl_
                  {-# LINE 7356 "AstInternal.hs" #-}
              _unwrappedRelType =
                  {-# LINE 475 "./TypeChecking.ag" #-}
                  fromRight ([],[]) $
                  do
                    lrt <- _relType
                    let (UnnamedCompositeType a,UnnamedCompositeType b) = lrt
                    return (a,b)
                  {-# LINE 7364 "AstInternal.hs" #-}
              _lhsOidens =
                  {-# LINE 485 "./TypeChecking.ag" #-}
                  [(alias_, _unwrappedRelType    )]
                  {-# LINE 7368 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 486 "./TypeChecking.ag" #-}
                  TrefAlias ann_ tbl_ alias_
                  {-# LINE 7372 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TrefAlias ann_ tbl_ alias_
                  {-# LINE 7376 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7380 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_TrefFun :: Annotation ->
                        T_Expression  ->
                        T_TableRef 
sem_TableRef_TrefFun ann_ fn_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOenv :: Environment
              _fnOenv :: Environment
              _fnIannotatedTree :: Expression
              _fnIenv :: Environment
              _fnIliftedColumnName :: String
              _lhsOannotatedTree =
                  {-# LINE 460 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7401 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 488 "./TypeChecking.ag" #-}
                  getFnType _lhsIenv _alias _fnIannotatedTree
                  {-# LINE 7405 "AstInternal.hs" #-}
              _lhsOjoinIdens =
                  {-# LINE 489 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7409 "AstInternal.hs" #-}
              _lhsOidens =
                  {-# LINE 490 "./TypeChecking.ag" #-}
                  case getFunIdens
                            _lhsIenv _alias
                            _fnIannotatedTree of
                    Right (s, UnnamedCompositeType c) -> [(s,(c,[]))]
                    _ -> []
                  {-# LINE 7417 "AstInternal.hs" #-}
              _alias =
                  {-# LINE 496 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 7421 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 497 "./TypeChecking.ag" #-}
                  TrefFun ann_ _fnIannotatedTree
                  {-# LINE 7425 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TrefFun ann_ _fnIannotatedTree
                  {-# LINE 7429 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fnIenv
                  {-# LINE 7433 "AstInternal.hs" #-}
              _fnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7437 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIenv,_fnIliftedColumnName) =
                  (fn_ _fnOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_TrefFunAlias :: Annotation ->
                             T_Expression  ->
                             String ->
                             T_TableRef 
sem_TableRef_TrefFunAlias ann_ fn_ alias_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOenv :: Environment
              _fnOenv :: Environment
              _fnIannotatedTree :: Expression
              _fnIenv :: Environment
              _fnIliftedColumnName :: String
              _lhsOannotatedTree =
                  {-# LINE 460 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7461 "AstInternal.hs" #-}
              _tpe =
                  {-# LINE 488 "./TypeChecking.ag" #-}
                  getFnType _lhsIenv alias_ _fnIannotatedTree
                  {-# LINE 7465 "AstInternal.hs" #-}
              _lhsOjoinIdens =
                  {-# LINE 489 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7469 "AstInternal.hs" #-}
              _lhsOidens =
                  {-# LINE 490 "./TypeChecking.ag" #-}
                  case getFunIdens
                            _lhsIenv _alias
                            _fnIannotatedTree of
                    Right (s, UnnamedCompositeType c) -> [(s,(c,[]))]
                    _ -> []
                  {-# LINE 7477 "AstInternal.hs" #-}
              _alias =
                  {-# LINE 499 "./TypeChecking.ag" #-}
                  alias_
                  {-# LINE 7481 "AstInternal.hs" #-}
              _backTree =
                  {-# LINE 500 "./TypeChecking.ag" #-}
                  TrefFunAlias ann_ _fnIannotatedTree alias_
                  {-# LINE 7485 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TrefFunAlias ann_ _fnIannotatedTree _alias
                  {-# LINE 7489 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fnIenv
                  {-# LINE 7493 "AstInternal.hs" #-}
              _fnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7497 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIenv,_fnIliftedColumnName) =
                  (fn_ _fnOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
-- TypeAttributeDef --------------------------------------------
data TypeAttributeDef  = TypeAttDef (String) (TypeName) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TypeAttributeDef :: TypeAttributeDef  ->
                        T_TypeAttributeDef 
sem_TypeAttributeDef (TypeAttDef _name _typ )  =
    (sem_TypeAttributeDef_TypeAttDef _name (sem_TypeName _typ ) )
-- semantic domain
type T_TypeAttributeDef  = Environment ->
                           ( TypeAttributeDef,String,Environment,(Either [TypeError] Type))
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {env_Inh_TypeAttributeDef :: Environment}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef,attrName_Syn_TypeAttributeDef :: String,env_Syn_TypeAttributeDef :: Environment,namedType_Syn_TypeAttributeDef :: Either [TypeError] Type}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOenv,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_TypeAttributeDef _lhsOannotatedTree _lhsOattrName _lhsOenv _lhsOnamedType ))
sem_TypeAttributeDef_TypeAttDef :: String ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef name_ typ_  =
    (\ _lhsIenv ->
         (let _lhsOattrName :: String
              _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeAttributeDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: (Either [TypeError] Type)
              _lhsOattrName =
                  {-# LINE 932 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 7537 "AstInternal.hs" #-}
              _lhsOnamedType =
                  {-# LINE 933 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 7541 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TypeAttDef name_ _typIannotatedTree
                  {-# LINE 7545 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7549 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 7553 "AstInternal.hs" #-}
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7557 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOenv,_lhsOnamedType)))
-- TypeAttributeDefList ----------------------------------------
type TypeAttributeDefList  = [(TypeAttributeDef)]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = Environment ->
                               ( TypeAttributeDefList,([(String, Either [TypeError] Type)]),Environment)
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {env_Inh_TypeAttributeDefList :: Environment}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,attrs_Syn_TypeAttributeDefList :: [(String, Either [TypeError] Type)],env_Syn_TypeAttributeDefList :: Environment}
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrs,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_TypeAttributeDefList _lhsOannotatedTree _lhsOattrs _lhsOenv ))
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOattrs :: ([(String, Either [TypeError] Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: TypeAttributeDef
              _hdIattrName :: String
              _hdIenv :: Environment
              _hdInamedType :: (Either [TypeError] Type)
              _tlIannotatedTree :: TypeAttributeDefList
              _tlIattrs :: ([(String, Either [TypeError] Type)])
              _tlIenv :: Environment
              _lhsOattrs =
                  {-# LINE 942 "./TypeChecking.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 7600 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 7604 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7608 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 7612 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7616 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 7620 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIattrName,_hdIenv,_hdInamedType) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIattrs,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOenv)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOattrs :: ([(String, Either [TypeError] Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOenv :: Environment
              _lhsOattrs =
                  {-# LINE 943 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7635 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7639 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7643 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7647 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOenv)))
-- TypeName ----------------------------------------------------
data TypeName  = ArrayTypeName (TypeName) 
               | PrecTypeName (String) (Integer) 
               | SetOfTypeName (TypeName) 
               | SimpleTypeName (String) 
               deriving ( Data,Eq,Show,Typeable)
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
type T_TypeName  = Environment ->
                   ( TypeName,Environment,(Either [TypeError] Type))
data Inh_TypeName  = Inh_TypeName {env_Inh_TypeName :: Environment}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName,env_Syn_TypeName :: Environment,namedType_Syn_TypeName :: Either [TypeError] Type}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_TypeName _lhsOannotatedTree _lhsOenv _lhsOnamedType ))
sem_TypeName_ArrayTypeName :: T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: (Either [TypeError] Type)
              _lhsOnamedType =
                  {-# LINE 150 "./TypeChecking.ag" #-}
                  ArrayType <$> _typInamedType
                  {-# LINE 7692 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 151 "./TypeChecking.ag" #-}
                  ArrayTypeName _typIannotatedTree
                  {-# LINE 7696 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ArrayTypeName _typIannotatedTree
                  {-# LINE 7700 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 7704 "AstInternal.hs" #-}
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7708 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType)))
sem_TypeName_PrecTypeName :: String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName tn_ prec_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName
              _lhsOenv :: Environment
              _lhsOnamedType =
                  {-# LINE 156 "./TypeChecking.ag" #-}
                  Right TypeCheckFailed
                  {-# LINE 7723 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 157 "./TypeChecking.ag" #-}
                  PrecTypeName tn_ prec_
                  {-# LINE 7727 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PrecTypeName tn_ prec_
                  {-# LINE 7731 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7735 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType)))
sem_TypeName_SetOfTypeName :: T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: (Either [TypeError] Type)
              _lhsOnamedType =
                  {-# LINE 153 "./TypeChecking.ag" #-}
                  SetOfType <$> _typInamedType
                  {-# LINE 7751 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 154 "./TypeChecking.ag" #-}
                  SetOfTypeName _typIannotatedTree
                  {-# LINE 7755 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SetOfTypeName _typIannotatedTree
                  {-# LINE 7759 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 7763 "AstInternal.hs" #-}
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7767 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType)))
sem_TypeName_SimpleTypeName :: String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName tn_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName
              _lhsOenv :: Environment
              _lhsOnamedType =
                  {-# LINE 147 "./TypeChecking.ag" #-}
                  envLookupType _lhsIenv $ canonicalizeTypeName tn_
                  {-# LINE 7781 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 148 "./TypeChecking.ag" #-}
                  SimpleTypeName tn_
                  {-# LINE 7785 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SimpleTypeName tn_
                  {-# LINE 7789 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7793 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType)))
-- VarDef ------------------------------------------------------
data VarDef  = VarDef (String) (TypeName) (Maybe Expression) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_VarDef :: VarDef  ->
              T_VarDef 
sem_VarDef (VarDef _name _typ _value )  =
    (sem_VarDef_VarDef _name (sem_TypeName _typ ) _value )
-- semantic domain
type T_VarDef  = Environment ->
                 ( VarDef,((String,Type)),Environment)
data Inh_VarDef  = Inh_VarDef {env_Inh_VarDef :: Environment}
data Syn_VarDef  = Syn_VarDef {annotatedTree_Syn_VarDef :: VarDef,def_Syn_VarDef :: (String,Type),env_Syn_VarDef :: Environment}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOdef,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_VarDef _lhsOannotatedTree _lhsOdef _lhsOenv ))
sem_VarDef_VarDef :: String ->
                     T_TypeName  ->
                     (Maybe Expression) ->
                     T_VarDef 
sem_VarDef_VarDef name_ typ_ value_  =
    (\ _lhsIenv ->
         (let _lhsOdef :: ((String,Type))
              _lhsOannotatedTree :: VarDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: (Either [TypeError] Type)
              _lhsOdef =
                  {-# LINE 1059 "./TypeChecking.ag" #-}
                  (name_, errorToTypeFail _typInamedType)
                  {-# LINE 7831 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  VarDef name_ _typIannotatedTree value_
                  {-# LINE 7835 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7839 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 7843 "AstInternal.hs" #-}
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7847 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOdef,_lhsOenv)))
-- VarDefList --------------------------------------------------
type VarDefList  = [(VarDef)]
-- cata
sem_VarDefList :: VarDefList  ->
                  T_VarDefList 
sem_VarDefList list  =
    (Prelude.foldr sem_VarDefList_Cons sem_VarDefList_Nil (Prelude.map sem_VarDef list) )
-- semantic domain
type T_VarDefList  = Environment ->
                     ( VarDefList,([(String,Type)]),Environment)
data Inh_VarDefList  = Inh_VarDefList {env_Inh_VarDefList :: Environment}
data Syn_VarDefList  = Syn_VarDefList {annotatedTree_Syn_VarDefList :: VarDefList,defs_Syn_VarDefList :: [(String,Type)],env_Syn_VarDefList :: Environment}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOdefs,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_VarDefList _lhsOannotatedTree _lhsOdefs _lhsOenv ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOdefs :: ([(String,Type)])
              _lhsOannotatedTree :: VarDefList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: VarDef
              _hdIdef :: ((String,Type))
              _hdIenv :: Environment
              _tlIannotatedTree :: VarDefList
              _tlIdefs :: ([(String,Type)])
              _tlIenv :: Environment
              _lhsOdefs =
                  {-# LINE 1062 "./TypeChecking.ag" #-}
                  _hdIdef : _tlIdefs
                  {-# LINE 7889 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 7893 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7897 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 7901 "AstInternal.hs" #-}
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7905 "AstInternal.hs" #-}
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 7909 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIdef,_hdIenv) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIdefs,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOenv)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOdefs :: ([(String,Type)])
              _lhsOannotatedTree :: VarDefList
              _lhsOenv :: Environment
              _lhsOdefs =
                  {-# LINE 1063 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7924 "AstInternal.hs" #-}
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7928 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7932 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7936 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOenv)))
-- Volatility --------------------------------------------------
data Volatility  = Immutable 
                 | Stable 
                 | Volatile 
                 deriving ( Data,Eq,Show,Typeable)
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
type T_Volatility  = Environment ->
                     ( Volatility,Environment)
data Inh_Volatility  = Inh_Volatility {env_Inh_Volatility :: Environment}
data Syn_Volatility  = Syn_Volatility {annotatedTree_Syn_Volatility :: Volatility,env_Syn_Volatility :: Environment}
wrap_Volatility :: T_Volatility  ->
                   Inh_Volatility  ->
                   Syn_Volatility 
wrap_Volatility sem (Inh_Volatility _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Volatility _lhsOannotatedTree _lhsOenv ))
sem_Volatility_Immutable :: T_Volatility 
sem_Volatility_Immutable  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Volatility
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Immutable
                  {-# LINE 7972 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7976 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7980 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Volatility_Stable :: T_Volatility 
sem_Volatility_Stable  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Volatility
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Stable
                  {-# LINE 7990 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7994 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7998 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Volatility_Volatile :: T_Volatility 
sem_Volatility_Volatile  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Volatility
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Volatile
                  {-# LINE 8008 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8012 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8016 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
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
type T_Where  = Environment ->
                ( Where,Environment)
data Inh_Where  = Inh_Where {env_Inh_Where :: Environment}
data Syn_Where  = Syn_Where {annotatedTree_Syn_Where :: Where,env_Syn_Where :: Environment}
wrap_Where :: T_Where  ->
              Inh_Where  ->
              Syn_Where 
wrap_Where sem (Inh_Where _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Where _lhsOannotatedTree _lhsOenv ))
sem_Where_Just :: T_Expression  ->
                  T_Where 
sem_Where_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Where
              _lhsOenv :: Environment
              _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIenv :: Environment
              _justIliftedColumnName :: String
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 8052 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8056 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 8060 "AstInternal.hs" #-}
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8064 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv,_justIliftedColumnName) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Where_Nothing :: T_Where 
sem_Where_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Where
              _lhsOenv :: Environment
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 8076 "AstInternal.hs" #-}
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8080 "AstInternal.hs" #-}
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8084 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))