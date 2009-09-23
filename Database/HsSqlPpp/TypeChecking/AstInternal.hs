

-- UUAGC 0.9.10 (AstInternal.ag)
module Database.HsSqlPpp.TypeChecking.AstInternal(
    -- {-# LANGUAGE DeriveDataTypeable,RankNTypes,ScopedTypeVariables #-}
    -- {-# OPTIONS_HADDOCK hide  #-}
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
   ,StringStringListPairList
   ,StringStringListPair
   ,ExpressionStatementListPairList
   ,SetClauseList
   ,CaseExpressionListExpressionPairList
   ,MaybeExpression
   ,MaybeTableRef
   ,ExpressionListList
   ,SelectItemList
   ,OnExpr
   ,RowConstraintList
   ,VarDefList
   ,ExpressionStatementListPair
   ,CaseExpressionListExpressionPair
   ,CaseExpressionList
   ,MaybeBoolExpression
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

{-# LINE 597 "AstInternal.ag" #-}

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

{-# LINE 118 "AstInternal.hs" #-}

{-# LINE 63 "./TypeChecking.ag" #-}

annTypesAndErrors :: Data a => a -> Type -> [TypeError]
                  -> Maybe [AnnotationElement] -> a
annTypesAndErrors item nt errs add =
    setAnnotation modifier item
    where
      modifier = (([TypeAnnotation nt] ++ fromMaybe [] add ++
       map TypeErrorA errs) ++)

{-# LINE 130 "AstInternal.hs" #-}

{-# LINE 433 "./TypeChecking.ag" #-}

getTbCols c = unwrapSetOfComposite (getTypeAnnotation c)
{-# LINE 135 "AstInternal.hs" #-}

{-# LINE 511 "./TypeChecking.ag" #-}


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
{-# LINE 163 "AstInternal.hs" #-}

{-# LINE 570 "./TypeChecking.ag" #-}


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
{-# LINE 186 "AstInternal.hs" #-}

{-# LINE 665 "./TypeChecking.ag" #-}

fixedValue :: a -> a -> a -> a
fixedValue a _ _ = a
{-# LINE 192 "AstInternal.hs" #-}

{-# LINE 699 "./TypeChecking.ag" #-}

getCAtts t =
    case t of
      SetOfType (UnnamedCompositeType t) -> t
      _ -> []
{-# LINE 200 "AstInternal.hs" #-}

{-# LINE 740 "./TypeChecking.ag" #-}


{-# LINE 205 "AstInternal.hs" #-}

{-# LINE 800 "./TypeChecking.ag" #-}

getRowTypes :: [Type] -> [Type]
getRowTypes [RowCtor ts] = ts
getRowTypes ts = ts
{-# LINE 212 "AstInternal.hs" #-}
-- AttributeDef ------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         attrName             : String
         namedType            : Type
   alternatives:
      alternative AttributeDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         child def            : MaybeExpression 
         child cons           : RowConstraintList 
         visit 0:
            local annotatedTree : _
-}
data AttributeDef  = AttributeDef (Annotation) (String) (TypeName) (MaybeExpression) (RowConstraintList) 
                   deriving ( Data,Eq,Show,Typeable)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _ann _name _typ _def _cons )  =
    (sem_AttributeDef_AttributeDef _ann _name (sem_TypeName _typ ) (sem_MaybeExpression _def ) (sem_RowConstraintList _cons ) )
-- semantic domain
type T_AttributeDef  = Environment ->
                       ( AttributeDef,String,Environment,Type)
data Inh_AttributeDef  = Inh_AttributeDef {env_Inh_AttributeDef :: Environment}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef,attrName_Syn_AttributeDef :: String,env_Syn_AttributeDef :: Environment,namedType_Syn_AttributeDef :: Type}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOenv,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_AttributeDef _lhsOannotatedTree _lhsOattrName _lhsOenv _lhsOnamedType ))
sem_AttributeDef_AttributeDef :: Annotation ->
                                 String ->
                                 T_TypeName  ->
                                 T_MaybeExpression  ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef ann_ name_ typ_ def_ cons_  =
    (\ _lhsIenv ->
         (let _lhsOattrName :: String
              _lhsOnamedType :: Type
              _lhsOannotatedTree :: AttributeDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _defOenv :: Environment
              _consOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: Type
              _defIannotatedTree :: MaybeExpression
              _defIenv :: Environment
              _defIexprType :: (Maybe Type)
              _consIannotatedTree :: RowConstraintList
              _consIenv :: Environment
              -- "./TypeChecking.ag"(line 842, column 7)
              _lhsOattrName =
                  {-# LINE 842 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 278 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 843, column 7)
              _lhsOnamedType =
                  {-# LINE 843 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 283 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
                  {-# LINE 288 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 293 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _consIenv
                  {-# LINE 298 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 303 "AstInternal.hs" #-}
              -- copy rule (chain)
              _defOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 308 "AstInternal.hs" #-}
              -- copy rule (chain)
              _consOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _defIenv
                  {-# LINE 313 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
              ( _defIannotatedTree,_defIenv,_defIexprType) =
                  (def_ _defOenv )
              ( _consIannotatedTree,_consIenv) =
                  (cons_ _consOenv )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOenv,_lhsOnamedType)))
-- AttributeDefList --------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         attrs                : [(String, Type)]
   alternatives:
      alternative Cons:
         child hd             : AttributeDef 
         child tl             : AttributeDefList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
type AttributeDefList  = [(AttributeDef)]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = Environment ->
                           ( AttributeDefList,([(String, Type)]),Environment)
data Inh_AttributeDefList  = Inh_AttributeDefList {env_Inh_AttributeDefList :: Environment}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList,attrs_Syn_AttributeDefList :: [(String, Type)],env_Syn_AttributeDefList :: Environment}
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
         (let _lhsOattrs :: ([(String, Type)])
              _lhsOannotatedTree :: AttributeDefList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: AttributeDef
              _hdIattrName :: String
              _hdIenv :: Environment
              _hdInamedType :: Type
              _tlIannotatedTree :: AttributeDefList
              _tlIattrs :: ([(String, Type)])
              _tlIenv :: Environment
              -- "./TypeChecking.ag"(line 852, column 12)
              _lhsOattrs =
                  {-# LINE 852 "./TypeChecking.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 378 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 383 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 388 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 393 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 398 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 403 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIattrName,_hdIenv,_hdInamedType) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIattrs,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOenv)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOattrs :: ([(String, Type)])
              _lhsOannotatedTree :: AttributeDefList
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 853, column 11)
              _lhsOattrs =
                  {-# LINE 853 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 419 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 424 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 429 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 434 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOenv)))
-- Cascade -----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cascade:
         visit 0:
            local annotatedTree : _
      alternative Restrict:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Cascade
                  {-# LINE 482 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 487 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 492 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Cascade_Restrict :: T_Cascade 
sem_Cascade_Restrict  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Cascade
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Restrict
                  {-# LINE 503 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 508 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 513 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- CaseExpressionList ------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cons:
         child hd             : Expression 
         child tl             : CaseExpressionList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 568 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 573 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 578 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 583 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 588 "AstInternal.hs" #-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 603 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 608 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 613 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- CaseExpressionListExpressionPair ----------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Tuple:
         child x1             : CaseExpressionList 
         child x2             : Expression 
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 665 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 670 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 675 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 680 "AstInternal.hs" #-}
              -- copy rule (chain)
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x1Ienv
                  {-# LINE 685 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1Ienv) =
                  (x1_ _x1Oenv )
              ( _x2IannotatedTree,_x2Ienv,_x2IliftedColumnName) =
                  (x2_ _x2Oenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- CaseExpressionListExpressionPairList ------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cons:
         child hd             : CaseExpressionListExpressionPair 
         child tl             : CaseExpressionListExpressionPairList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 743 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 748 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 753 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 758 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 763 "AstInternal.hs" #-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 778 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 783 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 788 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- CombineType -------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Except:
         visit 0:
            local annotatedTree : _
      alternative Intersect:
         visit 0:
            local annotatedTree : _
      alternative Union:
         visit 0:
            local annotatedTree : _
      alternative UnionAll:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Except
                  {-# LINE 848 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 853 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 858 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_CombineType_Intersect :: T_CombineType 
sem_CombineType_Intersect  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Intersect
                  {-# LINE 869 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 874 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 879 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_CombineType_Union :: T_CombineType 
sem_CombineType_Union  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Union
                  {-# LINE 890 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 895 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 900 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_CombineType_UnionAll :: T_CombineType 
sem_CombineType_UnionAll  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  UnionAll
                  {-# LINE 911 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 916 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 921 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Constraint --------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative CheckConstraint:
         child ann            : {Annotation}
         child expression     : Expression 
         visit 0:
            local annotatedTree : _
      alternative PrimaryKeyConstraint:
         child ann            : {Annotation}
         child stringList     : StringList 
         visit 0:
            local annotatedTree : _
      alternative ReferenceConstraint:
         child ann            : {Annotation}
         child atts           : StringList 
         child table          : {String}
         child tableAtts      : StringList 
         child onUpdate       : Cascade 
         child onDelete       : Cascade 
         visit 0:
            local annotatedTree : _
      alternative UniqueConstraint:
         child ann            : {Annotation}
         child stringList     : StringList 
         visit 0:
            local annotatedTree : _
-}
data Constraint  = CheckConstraint (Annotation) (Expression) 
                 | PrimaryKeyConstraint (Annotation) (StringList) 
                 | ReferenceConstraint (Annotation) (StringList) (String) (StringList) (Cascade) (Cascade) 
                 | UniqueConstraint (Annotation) (StringList) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Constraint :: Constraint  ->
                  T_Constraint 
sem_Constraint (CheckConstraint _ann _expression )  =
    (sem_Constraint_CheckConstraint _ann (sem_Expression _expression ) )
sem_Constraint (PrimaryKeyConstraint _ann _stringList )  =
    (sem_Constraint_PrimaryKeyConstraint _ann (sem_StringList _stringList ) )
sem_Constraint (ReferenceConstraint _ann _atts _table _tableAtts _onUpdate _onDelete )  =
    (sem_Constraint_ReferenceConstraint _ann (sem_StringList _atts ) _table (sem_StringList _tableAtts ) (sem_Cascade _onUpdate ) (sem_Cascade _onDelete ) )
sem_Constraint (UniqueConstraint _ann _stringList )  =
    (sem_Constraint_UniqueConstraint _ann (sem_StringList _stringList ) )
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
sem_Constraint_CheckConstraint :: Annotation ->
                                  T_Expression  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint ann_ expression_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOenv :: Environment
              _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIenv :: Environment
              _expressionIliftedColumnName :: String
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CheckConstraint ann_ _expressionIannotatedTree
                  {-# LINE 999 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1004 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _expressionIenv
                  {-# LINE 1009 "AstInternal.hs" #-}
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1014 "AstInternal.hs" #-}
              ( _expressionIannotatedTree,_expressionIenv,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Constraint_PrimaryKeyConstraint :: Annotation ->
                                       T_StringList  ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint ann_ stringList_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOenv :: Environment
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIenv :: Environment
              _stringListIstrings :: ([String])
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PrimaryKeyConstraint ann_ _stringListIannotatedTree
                  {-# LINE 1033 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1038 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 1043 "AstInternal.hs" #-}
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1048 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIenv,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Constraint_ReferenceConstraint :: Annotation ->
                                      T_StringList  ->
                                      String ->
                                      T_StringList  ->
                                      T_Cascade  ->
                                      T_Cascade  ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint ann_ atts_ table_ tableAtts_ onUpdate_ onDelete_  =
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ReferenceConstraint ann_ _attsIannotatedTree table_ _tableAttsIannotatedTree _onUpdateIannotatedTree _onDeleteIannotatedTree
                  {-# LINE 1081 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1086 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onDeleteIenv
                  {-# LINE 1091 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1096 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tableAttsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 1101 "AstInternal.hs" #-}
              -- copy rule (chain)
              _onUpdateOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tableAttsIenv
                  {-# LINE 1106 "AstInternal.hs" #-}
              -- copy rule (chain)
              _onDeleteOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onUpdateIenv
                  {-# LINE 1111 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIenv,_attsIstrings) =
                  (atts_ _attsOenv )
              ( _tableAttsIannotatedTree,_tableAttsIenv,_tableAttsIstrings) =
                  (tableAtts_ _tableAttsOenv )
              ( _onUpdateIannotatedTree,_onUpdateIenv) =
                  (onUpdate_ _onUpdateOenv )
              ( _onDeleteIannotatedTree,_onDeleteIenv) =
                  (onDelete_ _onDeleteOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Constraint_UniqueConstraint :: Annotation ->
                                   T_StringList  ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint ann_ stringList_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOenv :: Environment
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIenv :: Environment
              _stringListIstrings :: ([String])
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  UniqueConstraint ann_ _stringListIannotatedTree
                  {-# LINE 1136 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1141 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 1146 "AstInternal.hs" #-}
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1151 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIenv,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ConstraintList ----------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cons:
         child hd             : Constraint 
         child tl             : ConstraintList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1207 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1212 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 1217 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1222 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 1227 "AstInternal.hs" #-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 1242 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1247 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1252 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- CopySource --------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative CopyFilename:
         child string         : {String}
         visit 0:
            local annotatedTree : _
      alternative Stdin:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CopyFilename string_
                  {-# LINE 1302 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1307 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1312 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_CopySource_Stdin :: T_CopySource 
sem_CopySource_Stdin  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CopySource
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Stdin
                  {-# LINE 1323 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1328 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1333 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Direction ---------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Asc:
         visit 0:
            local annotatedTree : _
      alternative Desc:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Asc
                  {-# LINE 1381 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1386 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1391 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Direction_Desc :: T_Direction 
sem_Direction_Desc  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Direction
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Desc
                  {-# LINE 1402 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1407 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1412 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Distinct ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Distinct:
         visit 0:
            local annotatedTree : _
      alternative Dupes:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Distinct
                  {-# LINE 1460 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1465 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1470 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Distinct_Dupes :: T_Distinct 
sem_Distinct_Dupes  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Distinct
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Dupes
                  {-# LINE 1481 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1486 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1491 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- DropType ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Domain:
         visit 0:
            local annotatedTree : _
      alternative Table:
         visit 0:
            local annotatedTree : _
      alternative Type:
         visit 0:
            local annotatedTree : _
      alternative View:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Domain
                  {-# LINE 1551 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1556 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1561 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_DropType_Table :: T_DropType 
sem_DropType_Table  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Table
                  {-# LINE 1572 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1577 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1582 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_DropType_Type :: T_DropType 
sem_DropType_Type  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Type
                  {-# LINE 1593 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1598 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1603 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_DropType_View :: T_DropType 
sem_DropType_View  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  View
                  {-# LINE 1614 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1619 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1624 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Expression --------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         liftedColumnName     : String
   alternatives:
      alternative BooleanLit:
         child ann            : {Annotation}
         child b              : {Bool}
         visit 0:
            local backTree    : _
            local tpe         : _
            local annotatedTree : _
      alternative Case:
         child ann            : {Annotation}
         child cases          : CaseExpressionListExpressionPairList 
         child els            : MaybeExpression 
         visit 0:
            local whenTypes   : _
            local thenTypes   : _
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative CaseSimple:
         child ann            : {Annotation}
         child value          : Expression 
         child cases          : CaseExpressionListExpressionPairList 
         child els            : MaybeExpression 
         visit 0:
            local whenTypes   : _
            local thenTypes   : _
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative Cast:
         child ann            : {Annotation}
         child expr           : Expression 
         child tn             : TypeName 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative Exists:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative FloatLit:
         child ann            : {Annotation}
         child d              : {Double}
         visit 0:
            local backTree    : _
            local tpe         : _
            local annotatedTree : _
      alternative FunCall:
         child ann            : {Annotation}
         child funName        : {String}
         child args           : ExpressionList 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative Identifier:
         child ann            : {Annotation}
         child i              : {String}
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative InPredicate:
         child ann            : {Annotation}
         child expr           : Expression 
         child i              : {Bool}
         child list           : InList 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative IntegerLit:
         child ann            : {Annotation}
         child i              : {Integer}
         visit 0:
            local backTree    : _
            local tpe         : _
            local annotatedTree : _
      alternative NullLit:
         child ann            : {Annotation}
         visit 0:
            local backTree    : _
            local tpe         : _
            local annotatedTree : _
      alternative PositionalArg:
         child ann            : {Annotation}
         child p              : {Integer}
         visit 0:
            local annotatedTree : _
      alternative ScalarSubQuery:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative StringLit:
         child ann            : {Annotation}
         child quote          : {String}
         child value          : {String}
         visit 0:
            local backTree    : _
            local tpe         : _
            local annotatedTree : _
      alternative WindowFn:
         child ann            : {Annotation}
         child fn             : Expression 
         child partitionBy    : ExpressionList 
         child orderBy        : ExpressionList 
         child dir            : Direction 
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1827 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 95, column 9)
              _backTree =
                  {-# LINE 95 "./TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 1832 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 103, column 19)
              _tpe =
                  {-# LINE 103 "./TypeChecking.ag" #-}
                  Right typeBool
                  {-# LINE 1837 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1842 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 1847 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1852 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1877 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 181, column 9)
              _whenTypes =
                  {-# LINE 181 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
                  {-# LINE 1883 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 183, column 9)
              _thenTypes =
                  {-# LINE 183 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
                  {-# LINE 1890 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 189, column 9)
              _tpe =
                  {-# LINE 189 "./TypeChecking.ag" #-}
                  checkTypes _whenTypes     $ do
                     when (any (/= typeBool) _whenTypes    ) $
                       Left [WrongTypes typeBool _whenTypes    ]
                     checkTypes _thenTypes     $
                              resolveResultSetType
                                _lhsIenv
                                _thenTypes
                  {-# LINE 1901 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 197, column 9)
              _backTree =
                  {-# LINE 197 "./TypeChecking.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1906 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1911 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1916 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 1921 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1926 "AstInternal.hs" #-}
              -- copy rule (chain)
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 1931 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1965 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 181, column 9)
              _whenTypes =
                  {-# LINE 181 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
                  {-# LINE 1971 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 183, column 9)
              _thenTypes =
                  {-# LINE 183 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
                  {-# LINE 1978 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 202, column 9)
              _tpe =
                  {-# LINE 202 "./TypeChecking.ag" #-}
                  checkTypes _whenTypes     $ do
                  checkWhenTypes <- resolveResultSetType
                                         _lhsIenv
                                         (getTypeAnnotation _valueIannotatedTree: _whenTypes    )
                  checkTypes _thenTypes     $
                             resolveResultSetType
                                      _lhsIenv
                                      _thenTypes
                  {-# LINE 1990 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 211, column 9)
              _backTree =
                  {-# LINE 211 "./TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1995 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  _valueIliftedColumnName
                  {-# LINE 2000 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 2005 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 2010 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2015 "AstInternal.hs" #-}
              -- copy rule (chain)
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valueIenv
                  {-# LINE 2020 "AstInternal.hs" #-}
              -- copy rule (chain)
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 2025 "AstInternal.hs" #-}
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
              _tnInamedType :: Type
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2057 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 115, column 12)
              _tpe =
                  {-# LINE 115 "./TypeChecking.ag" #-}
                  Right $ _tnInamedType
                  {-# LINE 2062 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 116, column 12)
              _backTree =
                  {-# LINE 116 "./TypeChecking.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 2067 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 681, column 10)
              _lhsOliftedColumnName =
                  {-# LINE 681 "./TypeChecking.ag" #-}
                  case _tnIannotatedTree of
                    SimpleTypeName _ tn -> tn
                    _ -> ""
                  {-# LINE 2074 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 2079 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tnIenv
                  {-# LINE 2084 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2089 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 2094 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2118 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 227, column 9)
              _tpe =
                  {-# LINE 227 "./TypeChecking.ag" #-}
                  Right typeBool
                  {-# LINE 2123 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 228, column 9)
              _backTree =
                  {-# LINE 228 "./TypeChecking.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 2128 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2133 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 2138 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 2143 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2148 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2167 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 93, column 9)
              _backTree =
                  {-# LINE 93 "./TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 2172 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 102, column 17)
              _tpe =
                  {-# LINE 102 "./TypeChecking.ag" #-}
                  Right typeNumeric
                  {-# LINE 2177 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2182 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 2187 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2192 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2214 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 160, column 9)
              _tpe =
                  {-# LINE 160 "./TypeChecking.ag" #-}
                  checkTypes _argsItypeList $
                    typeCheckFunCall
                      _lhsIenv
                      funName_
                      _argsItypeList
                  {-# LINE 2223 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 165, column 9)
              _backTree =
                  {-# LINE 165 "./TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 2228 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 677, column 13)
              _lhsOliftedColumnName =
                  {-# LINE 677 "./TypeChecking.ag" #-}
                  if isOperatorName funName_
                     then ""
                     else funName_
                  {-# LINE 2235 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 2240 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _argsIenv
                  {-# LINE 2245 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2250 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2269 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 221, column 9)
              _tpe =
                  {-# LINE 221 "./TypeChecking.ag" #-}
                  let (correlationName,iden) = splitIdentifier i_
                  in envLookupID _lhsIenv correlationName iden
                  {-# LINE 2275 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 223, column 9)
              _backTree =
                  {-# LINE 223 "./TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 2280 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 676, column 16)
              _lhsOliftedColumnName =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  i_
                  {-# LINE 2285 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 2290 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2295 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2322 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 255, column 9)
              _tpe =
                  {-# LINE 255 "./TypeChecking.ag" #-}
                  do
                    lt <- _listIlistType
                    ty <- resolveResultSetType
                            _lhsIenv
                            [getTypeAnnotation _exprIannotatedTree, lt]
                    return typeBool
                  {-# LINE 2332 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 261, column 9)
              _backTree =
                  {-# LINE 261 "./TypeChecking.ag" #-}
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                  {-# LINE 2337 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  _exprIliftedColumnName
                  {-# LINE 2342 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                  {-# LINE 2347 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _listIenv
                  {-# LINE 2352 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2357 "AstInternal.hs" #-}
              -- copy rule (chain)
              _listOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 2362 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2383 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 89, column 9)
              _backTree =
                  {-# LINE 89 "./TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 2388 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 100, column 19)
              _tpe =
                  {-# LINE 100 "./TypeChecking.ag" #-}
                  Right typeInt
                  {-# LINE 2393 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2398 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 2403 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2408 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_NullLit :: Annotation ->
                          T_Expression 
sem_Expression_NullLit ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2424 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 97, column 9)
              _backTree =
                  {-# LINE 97 "./TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 2429 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 105, column 16)
              _tpe =
                  {-# LINE 105 "./TypeChecking.ag" #-}
                  Right UnknownStringLit
                  {-# LINE 2434 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2439 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 2444 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2449 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_PositionalArg :: Annotation ->
                                Integer ->
                                T_Expression 
sem_Expression_PositionalArg ann_ p_  =
    (\ _lhsIenv ->
         (let _lhsOliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOenv :: Environment
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2463 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 2468 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2473 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2478 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2498 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 239, column 9)
              _tpe =
                  {-# LINE 239 "./TypeChecking.ag" #-}
                  let selType = getTypeAnnotation _selIannotatedTree
                  in checkTypes [selType]
                       $ do
                         f <- map snd <$> unwrapSetOfComposite selType
                         case length f of
                              0 -> Left [InternalError "no columns in scalar subquery?"]
                              1 -> Right $ head f
                              _ -> Right $ RowCtor f
                  {-# LINE 2510 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 248, column 9)
              _backTree =
                  {-# LINE 248 "./TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 2515 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2520 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 2525 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 2530 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2535 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  {-# LINE 78 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2555 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 91, column 9)
              _backTree =
                  {-# LINE 91 "./TypeChecking.ag" #-}
                  StringLit ann_ quote_ value_
                  {-# LINE 2560 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 101, column 18)
              _tpe =
                  {-# LINE 101 "./TypeChecking.ag" #-}
                  Right UnknownStringLit
                  {-# LINE 2565 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2570 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  StringLit ann_ quote_ value_
                  {-# LINE 2575 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2580 "AstInternal.hs" #-}
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
              -- use rule "./TypeChecking.ag"(line 662, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 662 "./TypeChecking.ag" #-}
                  _fnIliftedColumnName
                  {-# LINE 2612 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree _dirIannotatedTree
                  {-# LINE 2617 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2622 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _dirIenv
                  {-# LINE 2627 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2632 "AstInternal.hs" #-}
              -- copy rule (chain)
              _partitionByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fnIenv
                  {-# LINE 2637 "AstInternal.hs" #-}
              -- copy rule (chain)
              _orderByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _partitionByIenv
                  {-# LINE 2642 "AstInternal.hs" #-}
              -- copy rule (chain)
              _dirOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _orderByIenv
                  {-# LINE 2647 "AstInternal.hs" #-}
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
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         typeList             : [Type]
   alternatives:
      alternative Cons:
         child hd             : Expression 
         child tl             : ExpressionList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 296, column 12)
              _lhsOtypeList =
                  {-# LINE 296 "./TypeChecking.ag" #-}
                  getTypeAnnotation _hdIannotatedTree : _tlItypeList
                  {-# LINE 2713 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2718 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2723 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 2728 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2733 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 2738 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 297, column 11)
              _lhsOtypeList =
                  {-# LINE 297 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2754 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2759 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2764 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2769 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOtypeList)))
-- ExpressionListList ------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         typeListList         : [[Type]]
   alternatives:
      alternative Cons:
         child hd             : ExpressionList 
         child tl             : ExpressionListList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 307, column 12)
              _lhsOtypeListList =
                  {-# LINE 307 "./TypeChecking.ag" #-}
                  _hdItypeList : _tlItypeListList
                  {-# LINE 2827 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2832 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2837 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 2842 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2847 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 2852 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 308, column 11)
              _lhsOtypeListList =
                  {-# LINE 308 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2868 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2873 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2878 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2883 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOtypeListList)))
-- ExpressionListStatementListPair -----------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ExpressionList 
         child x2             : StatementList 
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 360, column 13)
              _x2OenvUpdates =
                  {-# LINE 360 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2937 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 2942 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2947 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 2952 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2957 "AstInternal.hs" #-}
              -- copy rule (chain)
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x1Ienv
                  {-# LINE 2962 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1Ienv,_x1ItypeList) =
                  (x1_ _x1Oenv )
              ( _x2IannotatedTree,_x2Ienv,_x2IenvUpdates) =
                  (x2_ _x2Oenv _x2OenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ExpressionListStatementListPairList -------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cons:
         child hd             : ExpressionListStatementListPair 
         child tl             : ExpressionListStatementListPairList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 3020 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3025 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 3030 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3035 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 3040 "AstInternal.hs" #-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3055 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3060 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3065 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ExpressionRoot ----------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative ExpressionRoot:
         child expr           : Expression 
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ExpressionRoot _exprIannotatedTree
                  {-# LINE 3113 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3118 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 3123 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3128 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ExpressionStatementListPair ---------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Tuple:
         child x1             : Expression 
         child x2             : StatementList 
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 362, column 13)
              _x2OenvUpdates =
                  {-# LINE 362 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3184 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 3189 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3194 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 3199 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3204 "AstInternal.hs" #-}
              -- copy rule (chain)
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x1Ienv
                  {-# LINE 3209 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1Ienv,_x1IliftedColumnName) =
                  (x1_ _x1Oenv )
              ( _x2IannotatedTree,_x2Ienv,_x2IenvUpdates) =
                  (x2_ _x2Oenv _x2OenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ExpressionStatementListPairList -----------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cons:
         child hd             : ExpressionStatementListPair 
         child tl             : ExpressionStatementListPairList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 3267 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3272 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 3277 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3282 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 3287 "AstInternal.hs" #-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3302 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3307 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3312 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- FnBody ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative PlpgsqlFnBody:
         child ann            : {Annotation}
         child vars           : VarDefList 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
      alternative SqlFnBody:
         child ann            : {Annotation}
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 364, column 31)
              _stsOenvUpdates =
                  {-# LINE 364 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3377 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1005, column 9)
              _stsOenv =
                  {-# LINE 1005 "./TypeChecking.ag" #-}
                  fromRight _lhsIenv $ updateEnvironment _lhsIenv [EnvStackIDs [("", _varsIdefs)]]
                  {-# LINE 3382 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _varsIannotatedTree _stsIannotatedTree
                  {-# LINE 3387 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3392 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 3397 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3402 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 364, column 31)
              _stsOenvUpdates =
                  {-# LINE 364 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3424 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIannotatedTree
                  {-# LINE 3429 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3434 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 3439 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3444 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIenv,_stsIenvUpdates) =
                  (sts_ _stsOenv _stsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- IfExists ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative IfExists:
         visit 0:
            local annotatedTree : _
      alternative Require:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  IfExists
                  {-# LINE 3494 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3499 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3504 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_IfExists_Require :: T_IfExists 
sem_IfExists_Require  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: IfExists
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Require
                  {-# LINE 3515 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3520 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3525 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- InList ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         listType             : Either [TypeError] Type
   alternatives:
      alternative InList:
         child ann            : {Annotation}
         child exprs          : ExpressionList 
         visit 0:
            local annotatedTree : _
      alternative InSelect:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local annotatedTree : _
-}
data InList  = InList (Annotation) (ExpressionList) 
             | InSelect (Annotation) (SelectExpression) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_InList :: InList  ->
              T_InList 
sem_InList (InList _ann _exprs )  =
    (sem_InList_InList _ann (sem_ExpressionList _exprs ) )
sem_InList (InSelect _ann _sel )  =
    (sem_InList_InSelect _ann (sem_SelectExpression _sel ) )
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
sem_InList_InList :: Annotation ->
                     T_ExpressionList  ->
                     T_InList 
sem_InList_InList ann_ exprs_  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: InList
              _lhsOenv :: Environment
              _exprsOenv :: Environment
              _exprsIannotatedTree :: ExpressionList
              _exprsIenv :: Environment
              _exprsItypeList :: ([Type])
              -- "./TypeChecking.ag"(line 272, column 9)
              _lhsOlistType =
                  {-# LINE 272 "./TypeChecking.ag" #-}
                  resolveResultSetType
                    _lhsIenv
                    _exprsItypeList
                  {-# LINE 3587 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  InList ann_ _exprsIannotatedTree
                  {-# LINE 3592 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3597 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprsIenv
                  {-# LINE 3602 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3607 "AstInternal.hs" #-}
              ( _exprsIannotatedTree,_exprsIenv,_exprsItypeList) =
                  (exprs_ _exprsOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType)))
sem_InList_InSelect :: Annotation ->
                       T_SelectExpression  ->
                       T_InList 
sem_InList_InSelect ann_ sel_  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: InList
              _lhsOenv :: Environment
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIenv :: Environment
              -- "./TypeChecking.ag"(line 276, column 9)
              _lhsOlistType =
                  {-# LINE 276 "./TypeChecking.ag" #-}
                  do
                    attrs <-  map snd <$> (unwrapSetOfComposite $
                                let a = getTypeAnnotation _selIannotatedTree
                                in                                      a)
                    typ <- case length attrs of
                                 0 -> Left [InternalError "got subquery with no columns? in inselect"]
                                 1 -> Right $ head attrs
                                 _ -> Right $ RowCtor attrs
                    checkTypes attrs $ Right typ
                  {-# LINE 3634 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  InSelect ann_ _selIannotatedTree
                  {-# LINE 3639 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3644 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 3649 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3654 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIenv) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType)))
-- JoinExpression ----------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative JoinOn:
         child ann            : {Annotation}
         child expression     : Expression 
         visit 0:
            local annotatedTree : _
      alternative JoinUsing:
         child ann            : {Annotation}
         child stringList     : StringList 
         visit 0:
            local annotatedTree : _
-}
data JoinExpression  = JoinOn (Annotation) (Expression) 
                     | JoinUsing (Annotation) (StringList) 
                     deriving ( Data,Eq,Show,Typeable)
-- cata
sem_JoinExpression :: JoinExpression  ->
                      T_JoinExpression 
sem_JoinExpression (JoinOn _ann _expression )  =
    (sem_JoinExpression_JoinOn _ann (sem_Expression _expression ) )
sem_JoinExpression (JoinUsing _ann _stringList )  =
    (sem_JoinExpression_JoinUsing _ann (sem_StringList _stringList ) )
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
sem_JoinExpression_JoinOn :: Annotation ->
                             T_Expression  ->
                             T_JoinExpression 
sem_JoinExpression_JoinOn ann_ expression_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinExpression
              _lhsOenv :: Environment
              _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIenv :: Environment
              _expressionIliftedColumnName :: String
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  JoinOn ann_ _expressionIannotatedTree
                  {-# LINE 3714 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3719 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _expressionIenv
                  {-# LINE 3724 "AstInternal.hs" #-}
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3729 "AstInternal.hs" #-}
              ( _expressionIannotatedTree,_expressionIenv,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_JoinExpression_JoinUsing :: Annotation ->
                                T_StringList  ->
                                T_JoinExpression 
sem_JoinExpression_JoinUsing ann_ stringList_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinExpression
              _lhsOenv :: Environment
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIenv :: Environment
              _stringListIstrings :: ([String])
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  JoinUsing ann_ _stringListIannotatedTree
                  {-# LINE 3748 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3753 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 3758 "AstInternal.hs" #-}
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3763 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIenv,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- JoinType ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cross:
         visit 0:
            local annotatedTree : _
      alternative FullOuter:
         visit 0:
            local annotatedTree : _
      alternative Inner:
         visit 0:
            local annotatedTree : _
      alternative LeftOuter:
         visit 0:
            local annotatedTree : _
      alternative RightOuter:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Cross
                  {-# LINE 3831 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3836 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3841 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_JoinType_FullOuter :: T_JoinType 
sem_JoinType_FullOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  FullOuter
                  {-# LINE 3852 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3857 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3862 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_JoinType_Inner :: T_JoinType 
sem_JoinType_Inner  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Inner
                  {-# LINE 3873 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3878 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3883 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_JoinType_LeftOuter :: T_JoinType 
sem_JoinType_LeftOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  LeftOuter
                  {-# LINE 3894 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3899 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3904 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_JoinType_RightOuter :: T_JoinType 
sem_JoinType_RightOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RightOuter
                  {-# LINE 3915 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3920 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3925 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Language ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Plpgsql:
         visit 0:
            local annotatedTree : _
      alternative Sql:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Plpgsql
                  {-# LINE 3973 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3978 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3983 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Language_Sql :: T_Language 
sem_Language_Sql  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Language
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Sql
                  {-# LINE 3994 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3999 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4004 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- MaybeBoolExpression -----------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Just:
         child just           : Expression 
         visit 0:
            local annotatedTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
-}
type MaybeBoolExpression  = (Maybe (Expression))
-- cata
sem_MaybeBoolExpression :: MaybeBoolExpression  ->
                           T_MaybeBoolExpression 
sem_MaybeBoolExpression (Prelude.Just x )  =
    (sem_MaybeBoolExpression_Just (sem_Expression x ) )
sem_MaybeBoolExpression Prelude.Nothing  =
    sem_MaybeBoolExpression_Nothing
-- semantic domain
type T_MaybeBoolExpression  = Environment ->
                              ( MaybeBoolExpression,Environment)
data Inh_MaybeBoolExpression  = Inh_MaybeBoolExpression {env_Inh_MaybeBoolExpression :: Environment}
data Syn_MaybeBoolExpression  = Syn_MaybeBoolExpression {annotatedTree_Syn_MaybeBoolExpression :: MaybeBoolExpression,env_Syn_MaybeBoolExpression :: Environment}
wrap_MaybeBoolExpression :: T_MaybeBoolExpression  ->
                            Inh_MaybeBoolExpression  ->
                            Syn_MaybeBoolExpression 
wrap_MaybeBoolExpression sem (Inh_MaybeBoolExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_MaybeBoolExpression _lhsOannotatedTree _lhsOenv ))
sem_MaybeBoolExpression_Just :: T_Expression  ->
                                T_MaybeBoolExpression 
sem_MaybeBoolExpression_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MaybeBoolExpression
              _lhsOenv :: Environment
              _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIenv :: Environment
              _justIliftedColumnName :: String
              -- "./TypeChecking.ag"(line 1035, column 9)
              _lhsOannotatedTree =
                  {-# LINE 1035 "./TypeChecking.ag" #-}
                  if getTypeAnnotation _justIannotatedTree `notElem` [typeBool, TypeCheckFailed]
                    then Just $ setAnnotation ((TypeErrorA ExpressionMustBeBool) :)
                                  _justIannotatedTree
                    else Just $ _justIannotatedTree
                  {-# LINE 4059 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 4064 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 4069 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4074 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv,_justIliftedColumnName) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_MaybeBoolExpression_Nothing :: T_MaybeBoolExpression 
sem_MaybeBoolExpression_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MaybeBoolExpression
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 4087 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4092 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4097 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- MaybeExpression ---------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         exprType             : Maybe Type
   alternatives:
      alternative Just:
         child just           : Expression 
         visit 0:
            local annotatedTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 1049, column 12)
              _lhsOexprType =
                  {-# LINE 1049 "./TypeChecking.ag" #-}
                  Just $ getTypeAnnotation _justIannotatedTree
                  {-# LINE 4151 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 4156 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4161 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 4166 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4171 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv,_justIliftedColumnName) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOexprType)))
sem_MaybeExpression_Nothing :: T_MaybeExpression 
sem_MaybeExpression_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOexprType :: (Maybe Type)
              _lhsOannotatedTree :: MaybeExpression
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 1050, column 15)
              _lhsOexprType =
                  {-# LINE 1050 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 4185 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 4190 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4195 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4200 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOexprType)))
-- MaybeTableRef -----------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         idens                : [(String,([(String,Type)],[(String,Type)]))]
         joinIdens            : [String]
   alternatives:
      alternative Just:
         child just           : TableRef 
         visit 0:
            local annotatedTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
-}
type MaybeTableRef  = (Maybe (TableRef))
-- cata
sem_MaybeTableRef :: MaybeTableRef  ->
                     T_MaybeTableRef 
sem_MaybeTableRef (Prelude.Just x )  =
    (sem_MaybeTableRef_Just (sem_TableRef x ) )
sem_MaybeTableRef Prelude.Nothing  =
    sem_MaybeTableRef_Nothing
-- semantic domain
type T_MaybeTableRef  = Environment ->
                        ( MaybeTableRef,Environment,([(String,([(String,Type)],[(String,Type)]))]),([String]))
data Inh_MaybeTableRef  = Inh_MaybeTableRef {env_Inh_MaybeTableRef :: Environment}
data Syn_MaybeTableRef  = Syn_MaybeTableRef {annotatedTree_Syn_MaybeTableRef :: MaybeTableRef,env_Syn_MaybeTableRef :: Environment,idens_Syn_MaybeTableRef :: [(String,([(String,Type)],[(String,Type)]))],joinIdens_Syn_MaybeTableRef :: [String]}
wrap_MaybeTableRef :: T_MaybeTableRef  ->
                      Inh_MaybeTableRef  ->
                      Syn_MaybeTableRef 
wrap_MaybeTableRef sem (Inh_MaybeTableRef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens) =
             (sem _lhsIenv )
     in  (Syn_MaybeTableRef _lhsOannotatedTree _lhsOenv _lhsOidens _lhsOjoinIdens ))
sem_MaybeTableRef_Just :: T_TableRef  ->
                          T_MaybeTableRef 
sem_MaybeTableRef_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MaybeTableRef
              _lhsOenv :: Environment
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              _justOenv :: Environment
              _justIannotatedTree :: TableRef
              _justIenv :: Environment
              _justIidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _justIjoinIdens :: ([String])
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 4257 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4262 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 4267 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOidens =
                  {-# LINE 440 "./TypeChecking.ag" #-}
                  _justIidens
                  {-# LINE 4272 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOjoinIdens =
                  {-# LINE 441 "./TypeChecking.ag" #-}
                  _justIjoinIdens
                  {-# LINE 4277 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4282 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv,_justIidens,_justIjoinIdens) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
sem_MaybeTableRef_Nothing :: T_MaybeTableRef 
sem_MaybeTableRef_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              _lhsOannotatedTree :: MaybeTableRef
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 540, column 9)
              _lhsOidens =
                  {-# LINE 540 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4297 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 541, column 9)
              _lhsOjoinIdens =
                  {-# LINE 541 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4302 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 4307 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4312 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4317 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
-- Natural -----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Natural:
         visit 0:
            local annotatedTree : _
      alternative Unnatural:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Natural
                  {-# LINE 4365 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4370 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4375 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Natural_Unnatural :: T_Natural 
sem_Natural_Unnatural  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Natural
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Unnatural
                  {-# LINE 4386 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4391 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4396 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- OnExpr ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Just:
         child just           : JoinExpression 
         visit 0:
            local annotatedTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 4447 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4452 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 4457 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4462 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: OnExpr
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 4475 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4480 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4485 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- ParamDef ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         namedType            : Type
         paramName            : String
   alternatives:
      alternative ParamDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
      alternative ParamDefTp:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
-}
data ParamDef  = ParamDef (Annotation) (String) (TypeName) 
               | ParamDefTp (Annotation) (TypeName) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_ParamDef :: ParamDef  ->
                T_ParamDef 
sem_ParamDef (ParamDef _ann _name _typ )  =
    (sem_ParamDef_ParamDef _ann _name (sem_TypeName _typ ) )
sem_ParamDef (ParamDefTp _ann _typ )  =
    (sem_ParamDef_ParamDefTp _ann (sem_TypeName _typ ) )
-- semantic domain
type T_ParamDef  = Environment ->
                   ( ParamDef,Environment,Type,String)
data Inh_ParamDef  = Inh_ParamDef {env_Inh_ParamDef :: Environment}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef,env_Syn_ParamDef :: Environment,namedType_Syn_ParamDef :: Type,paramName_Syn_ParamDef :: String}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType,_lhsOparamName) =
             (sem _lhsIenv )
     in  (Syn_ParamDef _lhsOannotatedTree _lhsOenv _lhsOnamedType _lhsOparamName ))
sem_ParamDef_ParamDef :: Annotation ->
                         String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef ann_ name_ typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: Type
              _lhsOparamName :: String
              _lhsOannotatedTree :: ParamDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: Type
              -- "./TypeChecking.ag"(line 975, column 9)
              _lhsOnamedType =
                  {-# LINE 975 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 4549 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 977, column 9)
              _lhsOparamName =
                  {-# LINE 977 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 4554 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIannotatedTree
                  {-# LINE 4559 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4564 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 4569 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4574 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType,_lhsOparamName)))
sem_ParamDef_ParamDefTp :: Annotation ->
                           T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp ann_ typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: Type
              _lhsOparamName :: String
              _lhsOannotatedTree :: ParamDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: Type
              -- "./TypeChecking.ag"(line 975, column 9)
              _lhsOnamedType =
                  {-# LINE 975 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 4595 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 979, column 9)
              _lhsOparamName =
                  {-# LINE 979 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 4600 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIannotatedTree
                  {-# LINE 4605 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4610 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 4615 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4620 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType,_lhsOparamName)))
-- ParamDefList ------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         params               : [(String, Type)]
   alternatives:
      alternative Cons:
         child hd             : ParamDef 
         child tl             : ParamDefList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
type ParamDefList  = [(ParamDef)]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = Environment ->
                       ( ParamDefList,Environment,([(String, Type)]))
data Inh_ParamDefList  = Inh_ParamDefList {env_Inh_ParamDefList :: Environment}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList,env_Syn_ParamDefList :: Environment,params_Syn_ParamDefList :: [(String, Type)]}
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
         (let _lhsOparams :: ([(String, Type)])
              _lhsOannotatedTree :: ParamDefList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: ParamDef
              _hdIenv :: Environment
              _hdInamedType :: Type
              _hdIparamName :: String
              _tlIannotatedTree :: ParamDefList
              _tlIenv :: Environment
              _tlIparams :: ([(String, Type)])
              -- "./TypeChecking.ag"(line 983, column 13)
              _lhsOparams =
                  {-# LINE 983 "./TypeChecking.ag" #-}
                  ((_hdIparamName, _hdInamedType) : _tlIparams)
                  {-# LINE 4681 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 4686 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4691 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 4696 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4701 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 4706 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenv,_hdInamedType,_hdIparamName) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIenv,_tlIparams) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOparams)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOparams :: ([(String, Type)])
              _lhsOannotatedTree :: ParamDefList
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 982, column 12)
              _lhsOparams =
                  {-# LINE 982 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4722 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4727 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4732 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4737 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOparams)))
-- RaiseType ---------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative RError:
         visit 0:
            local annotatedTree : _
      alternative RException:
         visit 0:
            local annotatedTree : _
      alternative RNotice:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RError
                  {-# LINE 4791 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4796 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4801 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RaiseType_RException :: T_RaiseType 
sem_RaiseType_RException  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RaiseType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RException
                  {-# LINE 4812 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4817 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4822 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RaiseType_RNotice :: T_RaiseType 
sem_RaiseType_RNotice  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RaiseType
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RNotice
                  {-# LINE 4833 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4838 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4843 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- RestartIdentity ---------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative ContinueIdentity:
         visit 0:
            local annotatedTree : _
      alternative RestartIdentity:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ContinueIdentity
                  {-# LINE 4891 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4896 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4901 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RestartIdentity_RestartIdentity :: T_RestartIdentity 
sem_RestartIdentity_RestartIdentity  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RestartIdentity
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RestartIdentity
                  {-# LINE 4912 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4917 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4922 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Root --------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Root:
         child statements     : StatementList 
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 349, column 12)
              _statementsOenvUpdates =
                  {-# LINE 349 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4971 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Root _statementsIannotatedTree
                  {-# LINE 4976 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4981 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _statementsIenv
                  {-# LINE 4986 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4991 "AstInternal.hs" #-}
              ( _statementsIannotatedTree,_statementsIenv,_statementsIenvUpdates) =
                  (statements_ _statementsOenv _statementsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- RowConstraint -----------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative NotNullConstraint:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
      alternative NullConstraint:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
      alternative RowCheckConstraint:
         child ann            : {Annotation}
         child expression     : Expression 
         visit 0:
            local annotatedTree : _
      alternative RowPrimaryKeyConstraint:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
      alternative RowReferenceConstraint:
         child ann            : {Annotation}
         child table          : {String}
         child att            : {Maybe String}
         child onUpdate       : Cascade 
         child onDelete       : Cascade 
         visit 0:
            local annotatedTree : _
      alternative RowUniqueConstraint:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
-}
data RowConstraint  = NotNullConstraint (Annotation) 
                    | NullConstraint (Annotation) 
                    | RowCheckConstraint (Annotation) (Expression) 
                    | RowPrimaryKeyConstraint (Annotation) 
                    | RowReferenceConstraint (Annotation) (String) (Maybe String) (Cascade) (Cascade) 
                    | RowUniqueConstraint (Annotation) 
                    deriving ( Data,Eq,Show,Typeable)
-- cata
sem_RowConstraint :: RowConstraint  ->
                     T_RowConstraint 
sem_RowConstraint (NotNullConstraint _ann )  =
    (sem_RowConstraint_NotNullConstraint _ann )
sem_RowConstraint (NullConstraint _ann )  =
    (sem_RowConstraint_NullConstraint _ann )
sem_RowConstraint (RowCheckConstraint _ann _expression )  =
    (sem_RowConstraint_RowCheckConstraint _ann (sem_Expression _expression ) )
sem_RowConstraint (RowPrimaryKeyConstraint _ann )  =
    (sem_RowConstraint_RowPrimaryKeyConstraint _ann )
sem_RowConstraint (RowReferenceConstraint _ann _table _att _onUpdate _onDelete )  =
    (sem_RowConstraint_RowReferenceConstraint _ann _table _att (sem_Cascade _onUpdate ) (sem_Cascade _onDelete ) )
sem_RowConstraint (RowUniqueConstraint _ann )  =
    (sem_RowConstraint_RowUniqueConstraint _ann )
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
sem_RowConstraint_NotNullConstraint :: Annotation ->
                                       T_RowConstraint 
sem_RowConstraint_NotNullConstraint ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  NotNullConstraint ann_
                  {-# LINE 5077 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5082 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5087 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraint_NullConstraint :: Annotation ->
                                    T_RowConstraint 
sem_RowConstraint_NullConstraint ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  NullConstraint ann_
                  {-# LINE 5099 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5104 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5109 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraint_RowCheckConstraint :: Annotation ->
                                        T_Expression  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint ann_ expression_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIenv :: Environment
              _expressionIliftedColumnName :: String
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowCheckConstraint ann_ _expressionIannotatedTree
                  {-# LINE 5126 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5131 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _expressionIenv
                  {-# LINE 5136 "AstInternal.hs" #-}
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5141 "AstInternal.hs" #-}
              ( _expressionIannotatedTree,_expressionIenv,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraint_RowPrimaryKeyConstraint :: Annotation ->
                                             T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint ann_
                  {-# LINE 5155 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5160 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5165 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraint_RowReferenceConstraint :: Annotation ->
                                            String ->
                                            (Maybe String) ->
                                            T_Cascade  ->
                                            T_Cascade  ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint ann_ table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              _onUpdateOenv :: Environment
              _onDeleteOenv :: Environment
              _onUpdateIannotatedTree :: Cascade
              _onUpdateIenv :: Environment
              _onDeleteIannotatedTree :: Cascade
              _onDeleteIenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowReferenceConstraint ann_ table_ att_ _onUpdateIannotatedTree _onDeleteIannotatedTree
                  {-# LINE 5187 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5192 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onDeleteIenv
                  {-# LINE 5197 "AstInternal.hs" #-}
              -- copy rule (down)
              _onUpdateOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5202 "AstInternal.hs" #-}
              -- copy rule (chain)
              _onDeleteOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onUpdateIenv
                  {-# LINE 5207 "AstInternal.hs" #-}
              ( _onUpdateIannotatedTree,_onUpdateIenv) =
                  (onUpdate_ _onUpdateOenv )
              ( _onDeleteIannotatedTree,_onDeleteIenv) =
                  (onDelete_ _onDeleteOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_RowConstraint_RowUniqueConstraint :: Annotation ->
                                         T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowUniqueConstraint ann_
                  {-# LINE 5223 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5228 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5233 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- RowConstraintList -------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cons:
         child hd             : RowConstraint 
         child tl             : RowConstraintList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5287 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5292 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 5297 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5302 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 5307 "AstInternal.hs" #-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5322 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5327 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5332 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- SelectExpression --------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative CombineSelect:
         child ann            : {Annotation}
         child ctype          : CombineType 
         child sel1           : SelectExpression 
         child sel2           : SelectExpression 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative Select:
         child ann            : {Annotation}
         child selDistinct    : Distinct 
         child selSelectList  : SelectList 
         child selTref        : MaybeTableRef 
         child selWhere       : MaybeBoolExpression 
         child selGroupBy     : ExpressionList 
         child selHaving      : MaybeBoolExpression 
         child selOrderBy     : ExpressionList 
         child selDir         : Direction 
         child selLimit       : MaybeExpression 
         child selOffset      : MaybeExpression 
         visit 0:
            local tpe         : _
            local backTree    : _
            local newEnv      : _
            local annotatedTree : _
      alternative Values:
         child ann            : {Annotation}
         child vll            : ExpressionListList 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
-}
data SelectExpression  = CombineSelect (Annotation) (CombineType) (SelectExpression) (SelectExpression) 
                       | Select (Annotation) (Distinct) (SelectList) (MaybeTableRef) (MaybeBoolExpression) (ExpressionList) (MaybeBoolExpression) (ExpressionList) (Direction) (MaybeExpression) (MaybeExpression) 
                       | Values (Annotation) (ExpressionListList) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectExpression :: SelectExpression  ->
                        T_SelectExpression 
sem_SelectExpression (CombineSelect _ann _ctype _sel1 _sel2 )  =
    (sem_SelectExpression_CombineSelect _ann (sem_CombineType _ctype ) (sem_SelectExpression _sel1 ) (sem_SelectExpression _sel2 ) )
sem_SelectExpression (Select _ann _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selDir _selLimit _selOffset )  =
    (sem_SelectExpression_Select _ann (sem_Distinct _selDistinct ) (sem_SelectList _selSelectList ) (sem_MaybeTableRef _selTref ) (sem_MaybeBoolExpression _selWhere ) (sem_ExpressionList _selGroupBy ) (sem_MaybeBoolExpression _selHaving ) (sem_ExpressionList _selOrderBy ) (sem_Direction _selDir ) (sem_MaybeExpression _selLimit ) (sem_MaybeExpression _selOffset ) )
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
              -- "./TypeChecking.ag"(line 391, column 9)
              _lhsOannotatedTree =
                  {-# LINE 391 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 5426 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 424, column 9)
              _tpe =
                  {-# LINE 424 "./TypeChecking.ag" #-}
                  let sel1t = getTypeAnnotation _sel1IannotatedTree
                      sel2t = getTypeAnnotation _sel2IannotatedTree
                  in checkTypes [sel1t, sel2t] $
                        typeCheckCombineSelect _lhsIenv sel1t sel2t
                  {-# LINE 5434 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 429, column 9)
              _backTree =
                  {-# LINE 429 "./TypeChecking.ag" #-}
                  CombineSelect ann_ _ctypeIannotatedTree
                                _sel1IannotatedTree
                                _sel2IannotatedTree
                  {-# LINE 5441 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CombineSelect ann_ _ctypeIannotatedTree _sel1IannotatedTree _sel2IannotatedTree
                  {-# LINE 5446 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sel2Ienv
                  {-# LINE 5451 "AstInternal.hs" #-}
              -- copy rule (down)
              _ctypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5456 "AstInternal.hs" #-}
              -- copy rule (chain)
              _sel1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _ctypeIenv
                  {-# LINE 5461 "AstInternal.hs" #-}
              -- copy rule (chain)
              _sel2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sel1Ienv
                  {-# LINE 5466 "AstInternal.hs" #-}
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
                               T_MaybeTableRef  ->
                               T_MaybeBoolExpression  ->
                               T_ExpressionList  ->
                               T_MaybeBoolExpression  ->
                               T_ExpressionList  ->
                               T_Direction  ->
                               T_MaybeExpression  ->
                               T_MaybeExpression  ->
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
              _selTrefIannotatedTree :: MaybeTableRef
              _selTrefIenv :: Environment
              _selTrefIidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _selTrefIjoinIdens :: ([String])
              _selWhereIannotatedTree :: MaybeBoolExpression
              _selWhereIenv :: Environment
              _selGroupByIannotatedTree :: ExpressionList
              _selGroupByIenv :: Environment
              _selGroupByItypeList :: ([Type])
              _selHavingIannotatedTree :: MaybeBoolExpression
              _selHavingIenv :: Environment
              _selOrderByIannotatedTree :: ExpressionList
              _selOrderByIenv :: Environment
              _selOrderByItypeList :: ([Type])
              _selDirIannotatedTree :: Direction
              _selDirIenv :: Environment
              _selLimitIannotatedTree :: MaybeExpression
              _selLimitIenv :: Environment
              _selLimitIexprType :: (Maybe Type)
              _selOffsetIannotatedTree :: MaybeExpression
              _selOffsetIenv :: Environment
              _selOffsetIexprType :: (Maybe Type)
              -- "./TypeChecking.ag"(line 391, column 9)
              _lhsOannotatedTree =
                  {-# LINE 391 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 5534 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 403, column 9)
              _tpe =
                  {-# LINE 403 "./TypeChecking.ag" #-}
                  do
                  let trefType = fromMaybe typeBool $ fmap getTypeAnnotation
                                                           _selTrefIannotatedTree
                      slType = _selSelectListIlistType
                  chainTypeCheckFailed [trefType, slType] $
                    Right $ case slType of
                              UnnamedCompositeType [(_,Pseudo Void)] -> Pseudo Void
                              _ -> SetOfType slType
                  {-# LINE 5546 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 412, column 9)
              _backTree =
                  {-# LINE 412 "./TypeChecking.ag" #-}
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
                  {-# LINE 5561 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 621, column 10)
              _newEnv =
                  {-# LINE 621 "./TypeChecking.ag" #-}
                  case updateEnvironment _lhsIenv
                        (convertToNewStyleUpdates _selTrefIidens _selTrefIjoinIdens) of
                    Left x -> error $ show x
                    Right e -> e
                  {-# LINE 5569 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 625, column 10)
              _selSelectListOenv =
                  {-# LINE 625 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 5574 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 626, column 10)
              _selWhereOenv =
                  {-# LINE 626 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 5579 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Select ann_ _selDistinctIannotatedTree _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selDirIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
                  {-# LINE 5584 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selOffsetIenv
                  {-# LINE 5589 "AstInternal.hs" #-}
              -- copy rule (down)
              _selDistinctOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5594 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selTrefOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selSelectListIenv
                  {-# LINE 5599 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selGroupByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selWhereIenv
                  {-# LINE 5604 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selHavingOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selGroupByIenv
                  {-# LINE 5609 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selOrderByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selHavingIenv
                  {-# LINE 5614 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selDirOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selOrderByIenv
                  {-# LINE 5619 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selLimitOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selDirIenv
                  {-# LINE 5624 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selOffsetOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selLimitIenv
                  {-# LINE 5629 "AstInternal.hs" #-}
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
              ( _selLimitIannotatedTree,_selLimitIenv,_selLimitIexprType) =
                  (selLimit_ _selLimitOenv )
              ( _selOffsetIannotatedTree,_selOffsetIenv,_selOffsetIexprType) =
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
              -- "./TypeChecking.ag"(line 391, column 9)
              _lhsOannotatedTree =
                  {-# LINE 391 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 5669 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 398, column 9)
              _tpe =
                  {-# LINE 398 "./TypeChecking.ag" #-}
                  typeCheckValuesExpr
                              _lhsIenv
                              _vllItypeListList
                  {-# LINE 5676 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 401, column 9)
              _backTree =
                  {-# LINE 401 "./TypeChecking.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 5681 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 5686 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _vllIenv
                  {-# LINE 5691 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5696 "AstInternal.hs" #-}
              ( _vllIannotatedTree,_vllIenv,_vllItypeListList) =
                  (vll_ _vllOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- SelectItem --------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         columnName           : String
         itemType             : Type
   alternatives:
      alternative SelExp:
         child ann            : {Annotation}
         child ex             : Expression 
         visit 0:
            local annotatedTree : _
      alternative SelectItem:
         child ann            : {Annotation}
         child ex             : Expression 
         child name           : {String}
         visit 0:
            local backTree    : _
            local annotatedTree : _
-}
data SelectItem  = SelExp (Annotation) (Expression) 
                 | SelectItem (Annotation) (Expression) (String) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectItem :: SelectItem  ->
                  T_SelectItem 
sem_SelectItem (SelExp _ann _ex )  =
    (sem_SelectItem_SelExp _ann (sem_Expression _ex ) )
sem_SelectItem (SelectItem _ann _ex _name )  =
    (sem_SelectItem_SelectItem _ann (sem_Expression _ex ) _name )
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
sem_SelectItem_SelExp :: Annotation ->
                         T_Expression  ->
                         T_SelectItem 
sem_SelectItem_SelExp ann_ ex_  =
    (\ _lhsIenv ->
         (let _lhsOitemType :: Type
              _lhsOcolumnName :: String
              _lhsOannotatedTree :: SelectItem
              _lhsOenv :: Environment
              _exOenv :: Environment
              _exIannotatedTree :: Expression
              _exIenv :: Environment
              _exIliftedColumnName :: String
              -- "./TypeChecking.ag"(line 561, column 9)
              _lhsOitemType =
                  {-# LINE 561 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exIannotatedTree
                  {-# LINE 5762 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 566, column 9)
              _annotatedTree =
                  {-# LINE 566 "./TypeChecking.ag" #-}
                  SelExp ann_ $ fixStar _exIannotatedTree
                  {-# LINE 5767 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 687, column 14)
              _lhsOcolumnName =
                  {-# LINE 687 "./TypeChecking.ag" #-}
                  case _exIliftedColumnName of
                    "" -> "?column?"
                    s -> s
                  {-# LINE 5774 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5779 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exIenv
                  {-# LINE 5784 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5789 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIenv,_exIliftedColumnName) =
                  (ex_ _exOenv )
          in  ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOenv,_lhsOitemType)))
sem_SelectItem_SelectItem :: Annotation ->
                             T_Expression  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem ann_ ex_ name_  =
    (\ _lhsIenv ->
         (let _lhsOitemType :: Type
              _lhsOcolumnName :: String
              _lhsOannotatedTree :: SelectItem
              _lhsOenv :: Environment
              _exOenv :: Environment
              _exIannotatedTree :: Expression
              _exIenv :: Environment
              _exIliftedColumnName :: String
              -- "./TypeChecking.ag"(line 561, column 9)
              _lhsOitemType =
                  {-# LINE 561 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exIannotatedTree
                  {-# LINE 5811 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 568, column 9)
              _backTree =
                  {-# LINE 568 "./TypeChecking.ag" #-}
                  SelectItem ann_ (fixStar _exIannotatedTree) name_
                  {-# LINE 5816 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 690, column 18)
              _lhsOcolumnName =
                  {-# LINE 690 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 5821 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SelectItem ann_ _exIannotatedTree name_
                  {-# LINE 5826 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5831 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exIenv
                  {-# LINE 5836 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5841 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIenv,_exIliftedColumnName) =
                  (ex_ _exOenv )
          in  ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOenv,_lhsOitemType)))
-- SelectItemList ----------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         listType             : Type
   alternatives:
      alternative Cons:
         child hd             : SelectItem 
         child tl             : SelectItemList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 550, column 12)
              _lhsOlistType =
                  {-# LINE 550 "./TypeChecking.ag" #-}
                  doSelectItemListTpe _lhsIenv _hdIcolumnName _hdIitemType _tlIlistType
                  {-# LINE 5902 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5907 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5912 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 5917 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5922 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 5927 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 551, column 11)
              _lhsOlistType =
                  {-# LINE 551 "./TypeChecking.ag" #-}
                  UnnamedCompositeType []
                  {-# LINE 5943 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5948 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5953 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5958 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType)))
-- SelectList --------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         listType             : Type
   alternatives:
      alternative SelectList:
         child ann            : {Annotation}
         child items          : SelectItemList 
         child stringList     : StringList 
         visit 0:
            local annotatedTree : _
-}
data SelectList  = SelectList (Annotation) (SelectItemList) (StringList) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _ann _items _stringList )  =
    (sem_SelectList_SelectList _ann (sem_SelectItemList _items ) (sem_StringList _stringList ) )
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
sem_SelectList_SelectList :: Annotation ->
                             T_SelectItemList  ->
                             T_StringList  ->
                             T_SelectList 
sem_SelectList_SelectList ann_ items_ stringList_  =
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
              -- "./TypeChecking.ag"(line 596, column 9)
              _lhsOlistType =
                  {-# LINE 596 "./TypeChecking.ag" #-}
                  _itemsIlistType
                  {-# LINE 6016 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SelectList ann_ _itemsIannotatedTree _stringListIannotatedTree
                  {-# LINE 6021 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6026 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 6031 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6036 "AstInternal.hs" #-}
              -- copy rule (chain)
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _itemsIenv
                  {-# LINE 6041 "AstInternal.hs" #-}
              ( _itemsIannotatedTree,_itemsIenv,_itemsIlistType) =
                  (items_ _itemsOenv )
              ( _stringListIannotatedTree,_stringListIenv,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOlistType)))
-- SetClause ---------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         pairs                : [(String,Type)]
         rowSetError          : Maybe TypeError
   alternatives:
      alternative RowSetClause:
         child ann            : {Annotation}
         child atts           : StringList 
         child vals           : ExpressionList 
         visit 0:
            local rowSetError : _
            local annotatedTree : _
      alternative SetClause:
         child ann            : {Annotation}
         child att            : {String}
         child val            : Expression 
         visit 0:
            local annotatedTree : _
-}
data SetClause  = RowSetClause (Annotation) (StringList) (ExpressionList) 
                | SetClause (Annotation) (String) (Expression) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SetClause :: SetClause  ->
                 T_SetClause 
sem_SetClause (RowSetClause _ann _atts _vals )  =
    (sem_SetClause_RowSetClause _ann (sem_StringList _atts ) (sem_ExpressionList _vals ) )
sem_SetClause (SetClause _ann _att _val )  =
    (sem_SetClause_SetClause _ann _att (sem_Expression _val ) )
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
sem_SetClause_RowSetClause :: Annotation ->
                              T_StringList  ->
                              T_ExpressionList  ->
                              T_SetClause 
sem_SetClause_RowSetClause ann_ atts_ vals_  =
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
              -- "./TypeChecking.ag"(line 792, column 9)
              _rowSetError =
                  {-# LINE 792 "./TypeChecking.ag" #-}
                  let atts = _attsIstrings
                      types = getRowTypes _valsItypeList
                  in if length atts /= length types
                       then Just WrongNumberOfColumns
                       else Nothing
                  {-# LINE 6119 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 798, column 9)
              _lhsOpairs =
                  {-# LINE 798 "./TypeChecking.ag" #-}
                  zip _attsIstrings $ getRowTypes _valsItypeList
                  {-# LINE 6124 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowSetClause ann_ _attsIannotatedTree _valsIannotatedTree
                  {-# LINE 6129 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6134 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valsIenv
                  {-# LINE 6139 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOrowSetError =
                  {-# LINE 783 "./TypeChecking.ag" #-}
                  _rowSetError
                  {-# LINE 6144 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6149 "AstInternal.hs" #-}
              -- copy rule (chain)
              _valsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 6154 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIenv,_attsIstrings) =
                  (atts_ _attsOenv )
              ( _valsIannotatedTree,_valsIenv,_valsItypeList) =
                  (vals_ _valsOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOpairs,_lhsOrowSetError)))
sem_SetClause_SetClause :: Annotation ->
                           String ->
                           T_Expression  ->
                           T_SetClause 
sem_SetClause_SetClause ann_ att_ val_  =
    (\ _lhsIenv ->
         (let _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetError :: (Maybe TypeError)
              _lhsOannotatedTree :: SetClause
              _lhsOenv :: Environment
              _valOenv :: Environment
              _valIannotatedTree :: Expression
              _valIenv :: Environment
              _valIliftedColumnName :: String
              -- "./TypeChecking.ag"(line 789, column 9)
              _lhsOpairs =
                  {-# LINE 789 "./TypeChecking.ag" #-}
                  [(att_, getTypeAnnotation _valIannotatedTree)]
                  {-# LINE 6178 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 790, column 9)
              _lhsOrowSetError =
                  {-# LINE 790 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 6183 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SetClause ann_ att_ _valIannotatedTree
                  {-# LINE 6188 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6193 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valIenv
                  {-# LINE 6198 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6203 "AstInternal.hs" #-}
              ( _valIannotatedTree,_valIenv,_valIliftedColumnName) =
                  (val_ _valOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOpairs,_lhsOrowSetError)))
-- SetClauseList -----------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         pairs                : [(String,Type)]
         rowSetErrors         : [TypeError]
   alternatives:
      alternative Cons:
         child hd             : SetClause 
         child tl             : SetClauseList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 774, column 10)
              _lhsOpairs =
                  {-# LINE 774 "./TypeChecking.ag" #-}
                  _hdIpairs ++ _tlIpairs
                  {-# LINE 6267 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 775, column 10)
              _lhsOrowSetErrors =
                  {-# LINE 775 "./TypeChecking.ag" #-}
                  maybeToList _hdIrowSetError ++ _tlIrowSetErrors
                  {-# LINE 6272 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 6277 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6282 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 6287 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6292 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 6297 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 776, column 9)
              _lhsOpairs =
                  {-# LINE 776 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6314 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 777, column 9)
              _lhsOrowSetErrors =
                  {-# LINE 777 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6319 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6324 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6329 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6334 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOpairs,_lhsOrowSetErrors)))
-- Statement ---------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         envUpdates           : [EnvironmentUpdate]
   alternatives:
      alternative Assignment:
         child ann            : {Annotation}
         child target         : {String}
         child value          : Expression 
         visit 0:
            local annotatedTree : _
      alternative CaseStatement:
         child ann            : {Annotation}
         child val            : Expression 
         child cases          : ExpressionListStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
      alternative ContinueStatement:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
      alternative Copy:
         child ann            : {Annotation}
         child table          : {String}
         child targetCols     : StringList 
         child source         : CopySource 
         visit 0:
            local annotatedTree : _
      alternative CopyData:
         child ann            : {Annotation}
         child insData        : {String}
         visit 0:
            local annotatedTree : _
      alternative CreateDomain:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         child check          : MaybeBoolExpression 
         visit 0:
            local tpe         : _
            local backTree    : _
            local statementInfo : _
            local envUpdates  : _
            local annotatedTree : _
      alternative CreateFunction:
         child ann            : {Annotation}
         child lang           : Language 
         child name           : {String}
         child params         : ParamDefList 
         child rettype        : TypeName 
         child bodyQuote      : {String}
         child body           : FnBody 
         child vol            : Volatility 
         visit 0:
            local tpe         : _
            local backTree    : _
            local statementInfo : _
            local envUpdates  : _
            local annotatedTree : _
      alternative CreateTable:
         child ann            : {Annotation}
         child name           : {String}
         child atts           : AttributeDefList 
         child cons           : ConstraintList 
         visit 0:
            local attrTypes   : _
            local tpe         : _
            local backTree    : _
            local statementInfo : _
            local envUpdates  : _
            local annotatedTree : _
      alternative CreateTableAs:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : SelectExpression 
         visit 0:
            local selType     : _
            local tpe         : _
            local backTree    : _
            local statementInfo : _
            local attrs       : _
            local envUpdates  : _
            local annotatedTree : _
      alternative CreateType:
         child ann            : {Annotation}
         child name           : {String}
         child atts           : TypeAttributeDefList 
         visit 0:
            local tpe         : _
            local backTree    : _
            local statementInfo : _
            local envUpdates  : _
            local annotatedTree : _
      alternative CreateView:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : SelectExpression 
         visit 0:
            local tpe         : _
            local backTree    : _
            local statementInfo : _
            local attrs       : _
            local envUpdates  : _
            local annotatedTree : _
      alternative Delete:
         child ann            : {Annotation}
         child table          : {String}
         child whr            : MaybeBoolExpression 
         child returning      : {Maybe SelectList}
         visit 0:
            local tpe         : _
            local statementInfo : _
            local backTree    : _
            local envUpdates  : _
            local annotatedTree : _
      alternative DropFunction:
         child ann            : {Annotation}
         child ifE            : IfExists 
         child sigs           : StringStringListPairList 
         child cascade        : Cascade 
         visit 0:
            local annotatedTree : _
      alternative DropSomething:
         child ann            : {Annotation}
         child dropType       : DropType 
         child ifE            : IfExists 
         child names          : StringList 
         child cascade        : Cascade 
         visit 0:
            local annotatedTree : _
      alternative Execute:
         child ann            : {Annotation}
         child expr           : Expression 
         visit 0:
            local annotatedTree : _
      alternative ExecuteInto:
         child ann            : {Annotation}
         child expr           : Expression 
         child targets        : StringList 
         visit 0:
            local annotatedTree : _
      alternative ForIntegerStatement:
         child ann            : {Annotation}
         child var            : {String}
         child from           : Expression 
         child to             : Expression 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
      alternative ForSelectStatement:
         child ann            : {Annotation}
         child var            : {String}
         child sel            : SelectExpression 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
      alternative If:
         child ann            : {Annotation}
         child cases          : ExpressionStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
      alternative Insert:
         child ann            : {Annotation}
         child table          : {String}
         child targetCols     : StringList 
         child insData        : SelectExpression 
         child returning      : {Maybe SelectList}
         visit 0:
            local columnStuff : _
            local tpe         : _
            local statementInfo : _
            local backTree    : _
            local envUpdates  : _
            local annotatedTree : _
      alternative NullStatement:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
      alternative Perform:
         child ann            : {Annotation}
         child expr           : Expression 
         visit 0:
            local annotatedTree : _
      alternative Raise:
         child ann            : {Annotation}
         child level          : RaiseType 
         child message        : {String}
         child args           : ExpressionList 
         visit 0:
            local annotatedTree : _
      alternative Return:
         child ann            : {Annotation}
         child value          : MaybeExpression 
         visit 0:
            local tpe         : _
            local backTree    : _
            local envUpdates  : _
            local statementInfo : _
            local annotatedTree : _
      alternative ReturnNext:
         child ann            : {Annotation}
         child expr           : Expression 
         visit 0:
            local annotatedTree : _
      alternative ReturnQuery:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local annotatedTree : _
      alternative SelectStatement:
         child ann            : {Annotation}
         child ex             : SelectExpression 
         visit 0:
            local tpe         : _
            local statementInfo : _
            local backTree    : _
            local envUpdates  : _
            local annotatedTree : _
      alternative Truncate:
         child ann            : {Annotation}
         child tables         : StringList 
         child restartIdentity : RestartIdentity 
         child cascade        : Cascade 
         visit 0:
            local annotatedTree : _
      alternative Update:
         child ann            : {Annotation}
         child table          : {String}
         child assigns        : SetClauseList 
         child whr            : MaybeBoolExpression 
         child returning      : {Maybe SelectList}
         visit 0:
            local tpe         : _
            local columnsConsistent : _
            local statementInfo : _
            local backTree    : _
            local envUpdates  : _
            local annotatedTree : _
      alternative WhileStatement:
         child ann            : {Annotation}
         child expr           : Expression 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
-}
data Statement  = Assignment (Annotation) (String) (Expression) 
                | CaseStatement (Annotation) (Expression) (ExpressionListStatementListPairList) (StatementList) 
                | ContinueStatement (Annotation) 
                | Copy (Annotation) (String) (StringList) (CopySource) 
                | CopyData (Annotation) (String) 
                | CreateDomain (Annotation) (String) (TypeName) (MaybeBoolExpression) 
                | CreateFunction (Annotation) (Language) (String) (ParamDefList) (TypeName) (String) (FnBody) (Volatility) 
                | CreateTable (Annotation) (String) (AttributeDefList) (ConstraintList) 
                | CreateTableAs (Annotation) (String) (SelectExpression) 
                | CreateType (Annotation) (String) (TypeAttributeDefList) 
                | CreateView (Annotation) (String) (SelectExpression) 
                | Delete (Annotation) (String) (MaybeBoolExpression) (Maybe SelectList) 
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
                | Update (Annotation) (String) (SetClauseList) (MaybeBoolExpression) (Maybe SelectList) 
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
    (sem_Statement_CreateDomain _ann _name (sem_TypeName _typ ) (sem_MaybeBoolExpression _check ) )
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
    (sem_Statement_Delete _ann _table (sem_MaybeBoolExpression _whr ) _returning )
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
    (sem_Statement_Update _ann _table (sem_SetClauseList _assigns ) (sem_MaybeBoolExpression _whr ) _returning )
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6710 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Assignment ann_ target_ _valueIannotatedTree
                  {-# LINE 6715 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6720 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valueIenv
                  {-# LINE 6725 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6730 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6760 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 366, column 24)
              _elsOenvUpdates =
                  {-# LINE 366 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6765 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CaseStatement ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 6770 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6775 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 6780 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6785 "AstInternal.hs" #-}
              -- copy rule (chain)
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valIenv
                  {-# LINE 6790 "AstInternal.hs" #-}
              -- copy rule (chain)
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 6795 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6814 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ContinueStatement ann_
                  {-# LINE 6819 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6824 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6829 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6852 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Copy ann_ table_ _targetColsIannotatedTree _sourceIannotatedTree
                  {-# LINE 6857 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6862 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sourceIenv
                  {-# LINE 6867 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetColsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6872 "AstInternal.hs" #-}
              -- copy rule (chain)
              _sourceOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _targetColsIenv
                  {-# LINE 6877 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6895 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 6900 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6905 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6910 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_CreateDomain :: Annotation ->
                              String ->
                              T_TypeName  ->
                              T_MaybeBoolExpression  ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ check_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _typOenv :: Environment
              _checkOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: Type
              _checkIannotatedTree :: MaybeBoolExpression
              _checkIenv :: Environment
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6937 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6942 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 943, column 9)
              _tpe =
                  {-# LINE 943 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 6947 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 944, column 9)
              _backTree =
                  {-# LINE 944 "./TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree _checkIannotatedTree
                  {-# LINE 6952 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 945, column 9)
              _statementInfo =
                  {-# LINE 945 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6957 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 946, column 9)
              _envUpdates =
                  {-# LINE 946 "./TypeChecking.ag" #-}
                  [EnvCreateDomain (ScalarType name_) _typInamedType]
                  {-# LINE 6962 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree _checkIannotatedTree
                  {-# LINE 6967 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _checkIenv
                  {-# LINE 6972 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6977 "AstInternal.hs" #-}
              -- copy rule (chain)
              _checkOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 6982 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
              ( _checkIannotatedTree,_checkIenv) =
                  (check_ _checkOenv )
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
              _paramsIparams :: ([(String, Type)])
              _rettypeIannotatedTree :: TypeName
              _rettypeIenv :: Environment
              _rettypeInamedType :: Type
              _bodyIannotatedTree :: FnBody
              _bodyIenv :: Environment
              _volIannotatedTree :: Volatility
              _volIenv :: Environment
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 7027 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7032 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 987, column 9)
              _tpe =
                  {-# LINE 987 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 7037 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 988, column 9)
              _backTree =
                  {-# LINE 988 "./TypeChecking.ag" #-}
                  CreateFunction ann_
                                 _langIannotatedTree
                                 name_
                                 _paramsIannotatedTree
                                 _rettypeIannotatedTree
                                 bodyQuote_
                                 _bodyIannotatedTree
                                 _volIannotatedTree
                  {-# LINE 7049 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 996, column 9)
              _statementInfo =
                  {-# LINE 996 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7054 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 997, column 9)
              _envUpdates =
                  {-# LINE 997 "./TypeChecking.ag" #-}
                  [EnvCreateFunction FunName name_ (map snd _paramsIparams) _rettypeInamedType]
                  {-# LINE 7059 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 999, column 9)
              _bodyOenv =
                  {-# LINE 999 "./TypeChecking.ag" #-}
                  fromRight _lhsIenv $
                  updateEnvironment _lhsIenv [EnvStackIDs [("", _paramsIparams)
                                                          ,(name_, _paramsIparams)]]
                  {-# LINE 7066 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateFunction ann_ _langIannotatedTree name_ _paramsIannotatedTree _rettypeIannotatedTree bodyQuote_ _bodyIannotatedTree _volIannotatedTree
                  {-# LINE 7071 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _volIenv
                  {-# LINE 7076 "AstInternal.hs" #-}
              -- copy rule (down)
              _langOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7081 "AstInternal.hs" #-}
              -- copy rule (chain)
              _paramsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _langIenv
                  {-# LINE 7086 "AstInternal.hs" #-}
              -- copy rule (chain)
              _rettypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _paramsIenv
                  {-# LINE 7091 "AstInternal.hs" #-}
              -- copy rule (chain)
              _volOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _bodyIenv
                  {-# LINE 7096 "AstInternal.hs" #-}
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
              _attsIattrs :: ([(String, Type)])
              _attsIenv :: Environment
              _consIannotatedTree :: ConstraintList
              _consIenv :: Environment
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 7133 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7138 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 857, column 9)
              _attrTypes =
                  {-# LINE 857 "./TypeChecking.ag" #-}
                  map snd _attsIattrs
                  {-# LINE 7143 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 858, column 9)
              _tpe =
                  {-# LINE 858 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 7148 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 859, column 9)
              _backTree =
                  {-# LINE 859 "./TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
                  {-# LINE 7153 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 860, column 9)
              _statementInfo =
                  {-# LINE 860 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7158 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 861, column 9)
              _envUpdates =
                  {-# LINE 861 "./TypeChecking.ag" #-}
                  [EnvCreateTable name_ _attsIattrs []]
                  {-# LINE 7163 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
                  {-# LINE 7168 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _consIenv
                  {-# LINE 7173 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7178 "AstInternal.hs" #-}
              -- copy rule (chain)
              _consOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 7183 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 865, column 9)
              _selType =
                  {-# LINE 865 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exprIannotatedTree
                  {-# LINE 7205 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 866, column 9)
              _tpe =
                  {-# LINE 866 "./TypeChecking.ag" #-}
                  Right _selType
                  {-# LINE 7210 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 867, column 9)
              _backTree =
                  {-# LINE 867 "./TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 7215 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 868, column 9)
              _statementInfo =
                  {-# LINE 868 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7220 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 869, column 9)
              _attrs =
                  {-# LINE 869 "./TypeChecking.ag" #-}
                  case _selType     of
                    UnnamedCompositeType c -> c
                    _-> []
                  {-# LINE 7227 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 872, column 9)
              _envUpdates =
                  {-# LINE 872 "./TypeChecking.ag" #-}
                  [EnvCreateTable name_ _attrs     []]
                  {-# LINE 7232 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 7237 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7242 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 7247 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOenvUpdates =
                  {-# LINE 331 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7252 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7257 "AstInternal.hs" #-}
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
              _attsIattrs :: ([(String, Type)])
              _attsIenv :: Environment
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 7282 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7287 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 924, column 9)
              _tpe =
                  {-# LINE 924 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 7292 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 926, column 9)
              _backTree =
                  {-# LINE 926 "./TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 7297 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 927, column 9)
              _statementInfo =
                  {-# LINE 927 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7302 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 928, column 9)
              _envUpdates =
                  {-# LINE 928 "./TypeChecking.ag" #-}
                  [EnvCreateComposite name_ _attsIattrs]
                  {-# LINE 7307 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 7312 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 7317 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7322 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 7346 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7351 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 882, column 9)
              _tpe =
                  {-# LINE 882 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _exprIannotatedTree] $ Right $ Pseudo Void
                  {-# LINE 7356 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 883, column 9)
              _backTree =
                  {-# LINE 883 "./TypeChecking.ag" #-}
                  CreateView ann_ name_ _exprIannotatedTree
                  {-# LINE 7361 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 884, column 9)
              _statementInfo =
                  {-# LINE 884 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7366 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 885, column 9)
              _attrs =
                  {-# LINE 885 "./TypeChecking.ag" #-}
                  case getTypeAnnotation _exprIannotatedTree of
                    SetOfType (UnnamedCompositeType c) -> c
                    _ -> []
                  {-# LINE 7373 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 888, column 9)
              _envUpdates =
                  {-# LINE 888 "./TypeChecking.ag" #-}
                  [EnvCreateView name_ _attrs    ]
                  {-# LINE 7378 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateView ann_ name_ _exprIannotatedTree
                  {-# LINE 7383 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 7388 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7393 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
sem_Statement_Delete :: Annotation ->
                        String ->
                        T_MaybeBoolExpression  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Delete ann_ table_ whr_ returning_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOenv :: Environment
              _whrOenv :: Environment
              _whrIannotatedTree :: MaybeBoolExpression
              _whrIenv :: Environment
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 7418 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7423 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 814, column 9)
              _tpe =
                  {-# LINE 814 "./TypeChecking.ag" #-}
                  case checkRelationExists _lhsIenv table_ of
                    Just e -> Left [e]
                    Nothing -> Right $ Pseudo Void
                  {-# LINE 7430 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 818, column 9)
              _statementInfo =
                  {-# LINE 818 "./TypeChecking.ag" #-}
                  [DeleteInfo table_]
                  {-# LINE 7435 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 819, column 9)
              _backTree =
                  {-# LINE 819 "./TypeChecking.ag" #-}
                  Delete ann_ table_ _whrIannotatedTree returning_
                  {-# LINE 7440 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 820, column 9)
              _envUpdates =
                  {-# LINE 820 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7445 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Delete ann_ table_ _whrIannotatedTree returning_
                  {-# LINE 7450 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _whrIenv
                  {-# LINE 7455 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7460 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7487 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  DropFunction ann_ _ifEIannotatedTree _sigsIannotatedTree _cascadeIannotatedTree
                  {-# LINE 7492 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7497 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _cascadeIenv
                  {-# LINE 7502 "AstInternal.hs" #-}
              -- copy rule (down)
              _ifEOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7507 "AstInternal.hs" #-}
              -- copy rule (chain)
              _sigsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _ifEIenv
                  {-# LINE 7512 "AstInternal.hs" #-}
              -- copy rule (chain)
              _cascadeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sigsIenv
                  {-# LINE 7517 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7553 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  DropSomething ann_ _dropTypeIannotatedTree _ifEIannotatedTree _namesIannotatedTree _cascadeIannotatedTree
                  {-# LINE 7558 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7563 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _cascadeIenv
                  {-# LINE 7568 "AstInternal.hs" #-}
              -- copy rule (down)
              _dropTypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7573 "AstInternal.hs" #-}
              -- copy rule (chain)
              _ifEOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _dropTypeIenv
                  {-# LINE 7578 "AstInternal.hs" #-}
              -- copy rule (chain)
              _namesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _ifEIenv
                  {-# LINE 7583 "AstInternal.hs" #-}
              -- copy rule (chain)
              _cascadeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _namesIenv
                  {-# LINE 7588 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7614 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Execute ann_ _exprIannotatedTree
                  {-# LINE 7619 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7624 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 7629 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7634 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7659 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIannotatedTree _targetsIannotatedTree
                  {-# LINE 7664 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7669 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _targetsIenv
                  {-# LINE 7674 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7679 "AstInternal.hs" #-}
              -- copy rule (chain)
              _targetsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 7684 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7718 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 369, column 10)
              _stsOenvUpdates =
                  {-# LINE 369 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7723 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ForIntegerStatement ann_ var_ _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 7728 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7733 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 7738 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7743 "AstInternal.hs" #-}
              -- copy rule (chain)
              _toOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fromIenv
                  {-# LINE 7748 "AstInternal.hs" #-}
              -- copy rule (chain)
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _toIenv
                  {-# LINE 7753 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7783 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 369, column 10)
              _stsOenvUpdates =
                  {-# LINE 369 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7788 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ForSelectStatement ann_ var_ _selIannotatedTree _stsIannotatedTree
                  {-# LINE 7793 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7798 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 7803 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7808 "AstInternal.hs" #-}
              -- copy rule (chain)
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 7813 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7840 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 366, column 24)
              _elsOenvUpdates =
                  {-# LINE 366 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7845 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  If ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 7850 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7855 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 7860 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7865 "AstInternal.hs" #-}
              -- copy rule (chain)
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 7870 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 7902 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7907 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 708, column 9)
              _columnStuff =
                  {-# LINE 708 "./TypeChecking.ag" #-}
                  checkColumnConsistency _lhsIenv
                                         table_
                                         _targetColsIstrings
                                         (getCAtts $ getTypeAnnotation _insDataIannotatedTree)
                  {-# LINE 7915 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 713, column 9)
              _tpe =
                  {-# LINE 713 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _insDataIannotatedTree] $ do
                    _columnStuff
                    Right $ Pseudo Void
                  {-# LINE 7922 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 717, column 9)
              _statementInfo =
                  {-# LINE 717 "./TypeChecking.ag" #-}
                  [InsertInfo table_ $ errorToTypeFailF UnnamedCompositeType _columnStuff    ]
                  {-# LINE 7927 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 719, column 9)
              _backTree =
                  {-# LINE 719 "./TypeChecking.ag" #-}
                  Insert ann_ table_ _targetColsIannotatedTree
                         _insDataIannotatedTree returning_
                  {-# LINE 7933 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 721, column 9)
              _envUpdates =
                  {-# LINE 721 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7938 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Insert ann_ table_ _targetColsIannotatedTree _insDataIannotatedTree returning_
                  {-# LINE 7943 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _insDataIenv
                  {-# LINE 7948 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetColsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7953 "AstInternal.hs" #-}
              -- copy rule (chain)
              _insDataOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _targetColsIenv
                  {-# LINE 7958 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7975 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 7980 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7985 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7990 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8008 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Perform ann_ _exprIannotatedTree
                  {-# LINE 8013 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8018 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 8023 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8028 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8053 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Raise ann_ _levelIannotatedTree message_ _argsIannotatedTree
                  {-# LINE 8058 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8063 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _argsIenv
                  {-# LINE 8068 "AstInternal.hs" #-}
              -- copy rule (down)
              _levelOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8073 "AstInternal.hs" #-}
              -- copy rule (chain)
              _argsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _levelIenv
                  {-# LINE 8078 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 8104 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 8109 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1064, column 9)
              _tpe =
                  {-# LINE 1064 "./TypeChecking.ag" #-}
                  checkTypes [fromMaybe typeBool _valueIexprType] $ Right $ Pseudo Void
                  {-# LINE 8114 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1065, column 9)
              _backTree =
                  {-# LINE 1065 "./TypeChecking.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 8119 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1066, column 9)
              _envUpdates =
                  {-# LINE 1066 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8124 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1067, column 9)
              _statementInfo =
                  {-# LINE 1067 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8129 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 8134 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valueIenv
                  {-# LINE 8139 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8144 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8164 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIannotatedTree
                  {-# LINE 8169 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8174 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 8179 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8184 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8203 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIannotatedTree
                  {-# LINE 8208 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8213 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 8218 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8223 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 8246 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 8251 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 383, column 9)
              _tpe =
                  {-# LINE 383 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _exIannotatedTree] $ Right $ Pseudo Void
                  {-# LINE 8256 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 384, column 9)
              _statementInfo =
                  {-# LINE 384 "./TypeChecking.ag" #-}
                  [SelectInfo $ getTypeAnnotation _exIannotatedTree]
                  {-# LINE 8261 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 385, column 9)
              _backTree =
                  {-# LINE 385 "./TypeChecking.ag" #-}
                  SelectStatement ann_ _exIannotatedTree
                  {-# LINE 8266 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 386, column 9)
              _envUpdates =
                  {-# LINE 386 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8271 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SelectStatement ann_ _exIannotatedTree
                  {-# LINE 8276 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exIenv
                  {-# LINE 8281 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8286 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8314 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Truncate ann_ _tablesIannotatedTree _restartIdentityIannotatedTree _cascadeIannotatedTree
                  {-# LINE 8319 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8324 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _cascadeIenv
                  {-# LINE 8329 "AstInternal.hs" #-}
              -- copy rule (down)
              _tablesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8334 "AstInternal.hs" #-}
              -- copy rule (chain)
              _restartIdentityOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tablesIenv
                  {-# LINE 8339 "AstInternal.hs" #-}
              -- copy rule (chain)
              _cascadeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _restartIdentityIenv
                  {-# LINE 8344 "AstInternal.hs" #-}
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
                        T_MaybeBoolExpression  ->
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
              _whrIannotatedTree :: MaybeBoolExpression
              _whrIenv :: Environment
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 8379 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 8384 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 746, column 9)
              _tpe =
                  {-# LINE 746 "./TypeChecking.ag" #-}
                  do
                  let re = checkRelationExists _lhsIenv table_
                  when (isJust re) $
                       Left [fromJust $ re]
                  chainTypeCheckFailed (map snd _assignsIpairs) $ do
                    _columnsConsistent
                    checkErrorList _assignsIrowSetErrors $ Pseudo Void
                  {-# LINE 8395 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 754, column 9)
              _columnsConsistent =
                  {-# LINE 754 "./TypeChecking.ag" #-}
                  checkColumnConsistency _lhsIenv table_ (map fst _assignsIpairs) _assignsIpairs
                  {-# LINE 8400 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 756, column 9)
              _statementInfo =
                  {-# LINE 756 "./TypeChecking.ag" #-}
                  [UpdateInfo table_ $ flip errorToTypeFailF _columnsConsistent     $
                                           \c -> let colNames = map fst _assignsIpairs
                                                 in UnnamedCompositeType $ map (\t -> (t,getType c t)) colNames]
                  where
                    getType cols t = fromJust $ lookup t cols
                  {-# LINE 8409 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 763, column 9)
              _backTree =
                  {-# LINE 763 "./TypeChecking.ag" #-}
                  Update ann_ table_ _assignsIannotatedTree _whrIannotatedTree returning_
                  {-# LINE 8414 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 764, column 9)
              _envUpdates =
                  {-# LINE 764 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8419 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Update ann_ table_ _assignsIannotatedTree _whrIannotatedTree returning_
                  {-# LINE 8424 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _whrIenv
                  {-# LINE 8429 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8434 "AstInternal.hs" #-}
              -- copy rule (chain)
              _whrOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _assignsIenv
                  {-# LINE 8439 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8467 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 369, column 10)
              _stsOenvUpdates =
                  {-# LINE 369 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8472 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  WhileStatement ann_ _exprIannotatedTree _stsIannotatedTree
                  {-# LINE 8477 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8482 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 8487 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8492 "AstInternal.hs" #-}
              -- copy rule (chain)
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 8497 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIenv,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              ( _stsIannotatedTree,_stsIenv,_stsIenvUpdates) =
                  (sts_ _stsOenv _stsOenvUpdates )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
-- StatementList -----------------------------------------------
{-
   visit 0:
      chained attributes:
         env                  : Environment
         envUpdates           : [EnvironmentUpdate]
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cons:
         child hd             : Statement 
         child tl             : StatementList 
         visit 0:
            local newEnv      : _
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 353, column 9)
              _newEnv =
                  {-# LINE 353 "./TypeChecking.ag" #-}
                  fromRight _lhsIenv $ updateEnvironment _lhsIenv _lhsIenvUpdates
                  {-# LINE 8563 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 354, column 9)
              _hdOenv =
                  {-# LINE 354 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 8568 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 355, column 9)
              _tlOenv =
                  {-# LINE 355 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 8573 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 356, column 9)
              _tlOenvUpdates =
                  {-# LINE 356 "./TypeChecking.ag" #-}
                  _hdIenvUpdates
                  {-# LINE 8578 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8583 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8588 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 8593 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenvUpdates =
                  {-# LINE 344 "./TypeChecking.ag" #-}
                  _tlIenvUpdates
                  {-# LINE 8598 "AstInternal.hs" #-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8615 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8620 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8625 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenvUpdates =
                  {-# LINE 344 "./TypeChecking.ag" #-}
                  _lhsIenvUpdates
                  {-# LINE 8630 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOenvUpdates)))
-- StringList --------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         strings              : [String]
   alternatives:
      alternative Cons:
         child hd             : {String}
         child tl             : StringList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 730, column 10)
              _lhsOstrings =
                  {-# LINE 730 "./TypeChecking.ag" #-}
                  hd_ : _tlIstrings
                  {-# LINE 8684 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) hd_ _tlIannotatedTree
                  {-# LINE 8689 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8694 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 8699 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8704 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIenv,_tlIstrings) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOstrings)))
sem_StringList_Nil :: T_StringList 
sem_StringList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOstrings :: ([String])
              _lhsOannotatedTree :: StringList
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 731, column 9)
              _lhsOstrings =
                  {-# LINE 731 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8718 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8723 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8728 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8733 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOstrings)))
-- StringStringListPair ----------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Tuple:
         child x1             : {String}
         child x2             : StringList 
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (x1_,_x2IannotatedTree)
                  {-# LINE 8782 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8787 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 8792 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8797 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2Ienv,_x2Istrings) =
                  (x2_ _x2Oenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- StringStringListPairList ------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cons:
         child hd             : StringStringListPair 
         child tl             : StringStringListPairList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8853 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8858 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 8863 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8868 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 8873 "AstInternal.hs" #-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8888 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8893 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8898 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- TableRef ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         idens                : [(String,([(String,Type)],[(String,Type)]))]
         joinIdens            : [String]
   alternatives:
      alternative JoinedTref:
         child ann            : {Annotation}
         child tbl            : TableRef 
         child nat            : Natural 
         child joinType       : JoinType 
         child tbl1           : TableRef 
         child onExpr         : OnExpr 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative SubTref:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         child alias          : {String}
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative Tref:
         child ann            : {Annotation}
         child tbl            : {String}
         visit 0:
            local tpe         : _
            local relType     : _
            local unwrappedRelType : _
            local backTree    : _
            local annotatedTree : _
      alternative TrefAlias:
         child ann            : {Annotation}
         child tbl            : {String}
         child alias          : {String}
         visit 0:
            local tpe         : _
            local relType     : _
            local unwrappedRelType : _
            local backTree    : _
            local annotatedTree : _
      alternative TrefFun:
         child ann            : {Annotation}
         child fn             : Expression 
         visit 0:
            local tpe         : _
            local alias       : _
            local backTree    : _
            local annotatedTree : _
      alternative TrefFunAlias:
         child ann            : {Annotation}
         child fn             : Expression 
         child alias          : {String}
         visit 0:
            local tpe         : _
            local alias       : _
            local backTree    : _
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 446, column 9)
              _lhsOannotatedTree =
                  {-# LINE 446 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9039 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 488, column 9)
              _tpe =
                  {-# LINE 488 "./TypeChecking.ag" #-}
                  checkTypes [tblt
                            ,tbl1t] $
                     case (_natIannotatedTree, _onExprIannotatedTree) of
                            (Natural, _) -> unionJoinList $
                                            commonFieldNames tblt tbl1t
                            (_,Just (JoinUsing _ s)) -> unionJoinList s
                            _ -> unionJoinList []
                  where
                    tblt = getTypeAnnotation _tblIannotatedTree
                    tbl1t = getTypeAnnotation _tbl1IannotatedTree
                    unionJoinList s =
                        combineTableTypesWithUsingList _lhsIenv s tblt tbl1t
                  {-# LINE 9055 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 501, column 9)
              _lhsOidens =
                  {-# LINE 501 "./TypeChecking.ag" #-}
                  _tblIidens ++ _tbl1Iidens
                  {-# LINE 9060 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 502, column 9)
              _lhsOjoinIdens =
                  {-# LINE 502 "./TypeChecking.ag" #-}
                  commonFieldNames (getTypeAnnotation _tblIannotatedTree)
                                   (getTypeAnnotation _tbl1IannotatedTree)
                  {-# LINE 9066 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 504, column 9)
              _backTree =
                  {-# LINE 504 "./TypeChecking.ag" #-}
                  JoinedTref ann_
                             _tblIannotatedTree
                             _natIannotatedTree
                             _joinTypeIannotatedTree
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
                  {-# LINE 9076 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  JoinedTref ann_ _tblIannotatedTree _natIannotatedTree _joinTypeIannotatedTree _tbl1IannotatedTree _onExprIannotatedTree
                  {-# LINE 9081 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onExprIenv
                  {-# LINE 9086 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9091 "AstInternal.hs" #-}
              -- copy rule (chain)
              _natOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tblIenv
                  {-# LINE 9096 "AstInternal.hs" #-}
              -- copy rule (chain)
              _joinTypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _natIenv
                  {-# LINE 9101 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tbl1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _joinTypeIenv
                  {-# LINE 9106 "AstInternal.hs" #-}
              -- copy rule (chain)
              _onExprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tbl1Ienv
                  {-# LINE 9111 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 446, column 9)
              _lhsOannotatedTree =
                  {-# LINE 446 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9143 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 452, column 15)
              _tpe =
                  {-# LINE 452 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _selIannotatedTree] <$>
                  unwrapSetOfWhenComposite $ getTypeAnnotation _selIannotatedTree
                  {-# LINE 9149 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 454, column 15)
              _backTree =
                  {-# LINE 454 "./TypeChecking.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 9154 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 455, column 15)
              _lhsOidens =
                  {-# LINE 455 "./TypeChecking.ag" #-}
                  [(alias_, (fromRight [] $ getTbCols _selIannotatedTree, []))]
                  {-# LINE 9159 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 456, column 15)
              _lhsOjoinIdens =
                  {-# LINE 456 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9164 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 9169 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 9174 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9179 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 446, column 9)
              _lhsOannotatedTree =
                  {-# LINE 446 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9199 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 458, column 9)
              _tpe =
                  {-# LINE 458 "./TypeChecking.ag" #-}
                  either Left (Right . fst) _relType
                  {-# LINE 9204 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 459, column 9)
              _lhsOjoinIdens =
                  {-# LINE 459 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9209 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 460, column 9)
              _relType =
                  {-# LINE 460 "./TypeChecking.ag" #-}
                  getRelationType _lhsIenv tbl_
                  {-# LINE 9214 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 461, column 9)
              _unwrappedRelType =
                  {-# LINE 461 "./TypeChecking.ag" #-}
                  fromRight ([],[]) $
                  do
                    lrt <- _relType
                    let (UnnamedCompositeType a,UnnamedCompositeType b) = lrt
                    return (a,b)
                  {-# LINE 9223 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 468, column 9)
              _lhsOidens =
                  {-# LINE 468 "./TypeChecking.ag" #-}
                  [(tbl_, _unwrappedRelType    )]
                  {-# LINE 9228 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 469, column 9)
              _backTree =
                  {-# LINE 469 "./TypeChecking.ag" #-}
                  Tref ann_ tbl_
                  {-# LINE 9233 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Tref ann_ tbl_
                  {-# LINE 9238 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9243 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 446, column 9)
              _lhsOannotatedTree =
                  {-# LINE 446 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9262 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 458, column 9)
              _tpe =
                  {-# LINE 458 "./TypeChecking.ag" #-}
                  either Left (Right . fst) _relType
                  {-# LINE 9267 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 459, column 9)
              _lhsOjoinIdens =
                  {-# LINE 459 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9272 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 460, column 9)
              _relType =
                  {-# LINE 460 "./TypeChecking.ag" #-}
                  getRelationType _lhsIenv tbl_
                  {-# LINE 9277 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 461, column 9)
              _unwrappedRelType =
                  {-# LINE 461 "./TypeChecking.ag" #-}
                  fromRight ([],[]) $
                  do
                    lrt <- _relType
                    let (UnnamedCompositeType a,UnnamedCompositeType b) = lrt
                    return (a,b)
                  {-# LINE 9286 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 471, column 9)
              _lhsOidens =
                  {-# LINE 471 "./TypeChecking.ag" #-}
                  [(alias_, _unwrappedRelType    )]
                  {-# LINE 9291 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 472, column 9)
              _backTree =
                  {-# LINE 472 "./TypeChecking.ag" #-}
                  TrefAlias ann_ tbl_ alias_
                  {-# LINE 9296 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TrefAlias ann_ tbl_ alias_
                  {-# LINE 9301 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9306 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 446, column 9)
              _lhsOannotatedTree =
                  {-# LINE 446 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9328 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 474, column 9)
              _tpe =
                  {-# LINE 474 "./TypeChecking.ag" #-}
                  getFnType _lhsIenv _alias _fnIannotatedTree
                  {-# LINE 9333 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 475, column 9)
              _lhsOjoinIdens =
                  {-# LINE 475 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9338 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 476, column 9)
              _lhsOidens =
                  {-# LINE 476 "./TypeChecking.ag" #-}
                  case getFunIdens
                            _lhsIenv _alias
                            _fnIannotatedTree of
                    Right (s, UnnamedCompositeType c) -> [(s,(c,[]))]
                    _ -> []
                  {-# LINE 9347 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 482, column 9)
              _alias =
                  {-# LINE 482 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 9352 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 483, column 9)
              _backTree =
                  {-# LINE 483 "./TypeChecking.ag" #-}
                  TrefFun ann_ _fnIannotatedTree
                  {-# LINE 9357 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TrefFun ann_ _fnIannotatedTree
                  {-# LINE 9362 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fnIenv
                  {-# LINE 9367 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9372 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 446, column 9)
              _lhsOannotatedTree =
                  {-# LINE 446 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9397 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 474, column 9)
              _tpe =
                  {-# LINE 474 "./TypeChecking.ag" #-}
                  getFnType _lhsIenv alias_ _fnIannotatedTree
                  {-# LINE 9402 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 475, column 9)
              _lhsOjoinIdens =
                  {-# LINE 475 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9407 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 476, column 9)
              _lhsOidens =
                  {-# LINE 476 "./TypeChecking.ag" #-}
                  case getFunIdens
                            _lhsIenv _alias
                            _fnIannotatedTree of
                    Right (s, UnnamedCompositeType c) -> [(s,(c,[]))]
                    _ -> []
                  {-# LINE 9416 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 485, column 9)
              _alias =
                  {-# LINE 485 "./TypeChecking.ag" #-}
                  alias_
                  {-# LINE 9421 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 486, column 9)
              _backTree =
                  {-# LINE 486 "./TypeChecking.ag" #-}
                  TrefFunAlias ann_ _fnIannotatedTree alias_
                  {-# LINE 9426 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TrefFunAlias ann_ _fnIannotatedTree _alias
                  {-# LINE 9431 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fnIenv
                  {-# LINE 9436 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9441 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIenv,_fnIliftedColumnName) =
                  (fn_ _fnOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
-- TypeAttributeDef --------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         attrName             : String
         namedType            : Type
   alternatives:
      alternative TypeAttDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
-}
data TypeAttributeDef  = TypeAttDef (Annotation) (String) (TypeName) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TypeAttributeDef :: TypeAttributeDef  ->
                        T_TypeAttributeDef 
sem_TypeAttributeDef (TypeAttDef _ann _name _typ )  =
    (sem_TypeAttributeDef_TypeAttDef _ann _name (sem_TypeName _typ ) )
-- semantic domain
type T_TypeAttributeDef  = Environment ->
                           ( TypeAttributeDef,String,Environment,Type)
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {env_Inh_TypeAttributeDef :: Environment}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef,attrName_Syn_TypeAttributeDef :: String,env_Syn_TypeAttributeDef :: Environment,namedType_Syn_TypeAttributeDef :: Type}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOenv,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_TypeAttributeDef _lhsOannotatedTree _lhsOattrName _lhsOenv _lhsOnamedType ))
sem_TypeAttributeDef_TypeAttDef :: Annotation ->
                                   String ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef ann_ name_ typ_  =
    (\ _lhsIenv ->
         (let _lhsOattrName :: String
              _lhsOnamedType :: Type
              _lhsOannotatedTree :: TypeAttributeDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: Type
              -- "./TypeChecking.ag"(line 908, column 9)
              _lhsOattrName =
                  {-# LINE 908 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 9499 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 909, column 9)
              _lhsOnamedType =
                  {-# LINE 909 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 9504 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIannotatedTree
                  {-# LINE 9509 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9514 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 9519 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9524 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOenv,_lhsOnamedType)))
-- TypeAttributeDefList ----------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         attrs                : [(String, Type)]
   alternatives:
      alternative Cons:
         child hd             : TypeAttributeDef 
         child tl             : TypeAttributeDefList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
type TypeAttributeDefList  = [(TypeAttributeDef)]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = Environment ->
                               ( TypeAttributeDefList,([(String, Type)]),Environment)
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {env_Inh_TypeAttributeDefList :: Environment}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,attrs_Syn_TypeAttributeDefList :: [(String, Type)],env_Syn_TypeAttributeDefList :: Environment}
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
         (let _lhsOattrs :: ([(String, Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOenv :: Environment
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: TypeAttributeDef
              _hdIattrName :: String
              _hdIenv :: Environment
              _hdInamedType :: Type
              _tlIannotatedTree :: TypeAttributeDefList
              _tlIattrs :: ([(String, Type)])
              _tlIenv :: Environment
              -- "./TypeChecking.ag"(line 918, column 12)
              _lhsOattrs =
                  {-# LINE 918 "./TypeChecking.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 9585 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 9590 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9595 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 9600 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9605 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 9610 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIattrName,_hdIenv,_hdInamedType) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIattrs,_tlIenv) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOenv)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOattrs :: ([(String, Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 919, column 11)
              _lhsOattrs =
                  {-# LINE 919 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9626 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9631 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9636 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9641 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOenv)))
-- TypeName ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         namedType            : Type
   alternatives:
      alternative ArrayTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative PrecTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         child prec           : {Integer}
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative SetOfTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
      alternative SimpleTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
-}
data TypeName  = ArrayTypeName (Annotation) (TypeName) 
               | PrecTypeName (Annotation) (String) (Integer) 
               | SetOfTypeName (Annotation) (TypeName) 
               | SimpleTypeName (Annotation) (String) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TypeName :: TypeName  ->
                T_TypeName 
sem_TypeName (ArrayTypeName _ann _typ )  =
    (sem_TypeName_ArrayTypeName _ann (sem_TypeName _typ ) )
sem_TypeName (PrecTypeName _ann _tn _prec )  =
    (sem_TypeName_PrecTypeName _ann _tn _prec )
sem_TypeName (SetOfTypeName _ann _typ )  =
    (sem_TypeName_SetOfTypeName _ann (sem_TypeName _typ ) )
sem_TypeName (SimpleTypeName _ann _tn )  =
    (sem_TypeName_SimpleTypeName _ann _tn )
-- semantic domain
type T_TypeName  = Environment ->
                   ( TypeName,Environment,Type)
data Inh_TypeName  = Inh_TypeName {env_Inh_TypeName :: Environment}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName,env_Syn_TypeName :: Environment,namedType_Syn_TypeName :: Type}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_TypeName _lhsOannotatedTree _lhsOenv _lhsOnamedType ))
sem_TypeName_ArrayTypeName :: Annotation ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName ann_ typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: Type
              _lhsOannotatedTree :: TypeName
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: Type
              -- "./TypeChecking.ag"(line 135, column 10)
              _lhsOnamedType =
                  {-# LINE 135 "./TypeChecking.ag" #-}
                  errorToTypeFail _tpe
                  {-# LINE 9726 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOannotatedTree =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  setAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 9733 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 146, column 9)
              _tpe =
                  {-# LINE 146 "./TypeChecking.ag" #-}
                  checkTypes [_typInamedType] $ Right $ ArrayType _typInamedType
                  {-# LINE 9738 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 147, column 9)
              _backTree =
                  {-# LINE 147 "./TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 9743 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 9748 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 9753 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9758 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType)))
sem_TypeName_PrecTypeName :: Annotation ->
                             String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName ann_ tn_ prec_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: Type
              _lhsOannotatedTree :: TypeName
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 135, column 10)
              _lhsOnamedType =
                  {-# LINE 135 "./TypeChecking.ag" #-}
                  errorToTypeFail _tpe
                  {-# LINE 9775 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOannotatedTree =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  setAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 9782 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 152, column 9)
              _tpe =
                  {-# LINE 152 "./TypeChecking.ag" #-}
                  Right TypeCheckFailed
                  {-# LINE 9787 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 153, column 9)
              _backTree =
                  {-# LINE 153 "./TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 9792 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 9797 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9802 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType)))
sem_TypeName_SetOfTypeName :: Annotation ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName ann_ typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: Type
              _lhsOannotatedTree :: TypeName
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: Type
              -- "./TypeChecking.ag"(line 135, column 10)
              _lhsOnamedType =
                  {-# LINE 135 "./TypeChecking.ag" #-}
                  errorToTypeFail _tpe
                  {-# LINE 9820 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOannotatedTree =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  setAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 9827 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 149, column 9)
              _tpe =
                  {-# LINE 149 "./TypeChecking.ag" #-}
                  checkTypes [_typInamedType] $ Right $ SetOfType _typInamedType
                  {-# LINE 9832 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 150, column 9)
              _backTree =
                  {-# LINE 150 "./TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 9837 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 9842 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 9847 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9852 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType)))
sem_TypeName_SimpleTypeName :: Annotation ->
                               String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName ann_ tn_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: Type
              _lhsOannotatedTree :: TypeName
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 135, column 10)
              _lhsOnamedType =
                  {-# LINE 135 "./TypeChecking.ag" #-}
                  errorToTypeFail _tpe
                  {-# LINE 9868 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOannotatedTree =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  setAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 9875 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 143, column 9)
              _tpe =
                  {-# LINE 143 "./TypeChecking.ag" #-}
                  envLookupType _lhsIenv $ canonicalizeTypeName tn_
                  {-# LINE 9880 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 144, column 9)
              _backTree =
                  {-# LINE 144 "./TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 9885 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 9890 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9895 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOnamedType)))
-- VarDef ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         def                  : (String,Type)
   alternatives:
      alternative VarDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         child value          : {Maybe Expression}
         visit 0:
            local annotatedTree : _
-}
data VarDef  = VarDef (Annotation) (String) (TypeName) (Maybe Expression) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_VarDef :: VarDef  ->
              T_VarDef 
sem_VarDef (VarDef _ann _name _typ _value )  =
    (sem_VarDef_VarDef _ann _name (sem_TypeName _typ ) _value )
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
sem_VarDef_VarDef :: Annotation ->
                     String ->
                     T_TypeName  ->
                     (Maybe Expression) ->
                     T_VarDef 
sem_VarDef_VarDef ann_ name_ typ_ value_  =
    (\ _lhsIenv ->
         (let _lhsOdef :: ((String,Type))
              _lhsOannotatedTree :: VarDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: Type
              -- "./TypeChecking.ag"(line 1020, column 14)
              _lhsOdef =
                  {-# LINE 1020 "./TypeChecking.ag" #-}
                  (name_, _typInamedType)
                  {-# LINE 9951 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIannotatedTree value_
                  {-# LINE 9956 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9961 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 9966 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9971 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOdef,_lhsOenv)))
-- VarDefList --------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         defs                 : [(String,Type)]
   alternatives:
      alternative Cons:
         child hd             : VarDef 
         child tl             : VarDefList 
         visit 0:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
-}
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
              -- "./TypeChecking.ag"(line 1023, column 12)
              _lhsOdefs =
                  {-# LINE 1023 "./TypeChecking.ag" #-}
                  _hdIdef : _tlIdefs
                  {-# LINE 10031 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 10036 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10041 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 10046 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10051 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 10056 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 1024, column 11)
              _lhsOdefs =
                  {-# LINE 1024 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 10072 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 10077 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10082 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10087 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOenv)))
-- Volatility --------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Immutable:
         visit 0:
            local annotatedTree : _
      alternative Stable:
         visit 0:
            local annotatedTree : _
      alternative Volatile:
         visit 0:
            local annotatedTree : _
-}
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Immutable
                  {-# LINE 10141 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10146 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10151 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Volatility_Stable :: T_Volatility 
sem_Volatility_Stable  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Volatility
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Stable
                  {-# LINE 10162 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10167 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10172 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Volatility_Volatile :: T_Volatility 
sem_Volatility_Volatile  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Volatility
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Volatile
                  {-# LINE 10183 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10188 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10193 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))