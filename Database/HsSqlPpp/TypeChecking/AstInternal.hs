

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

{-# LINE 396 "./TypeChecking.ag" #-}

checkExpressionBool :: Maybe Expression -> Either [TypeError] Type
checkExpressionBool whr = do
  let ty = fromMaybe typeBool $ fmap getTypeAnnotation whr
  when (ty `notElem` [typeBool, TypeCheckFailed]) $
       Left [ExpressionMustBeBool]
  return ty
{-# LINE 140 "AstInternal.hs" #-}

{-# LINE 443 "./TypeChecking.ag" #-}

getTbCols c = unwrapSetOfComposite (getTypeAnnotation c)
{-# LINE 145 "AstInternal.hs" #-}

{-# LINE 521 "./TypeChecking.ag" #-}


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
{-# LINE 173 "AstInternal.hs" #-}

{-# LINE 580 "./TypeChecking.ag" #-}


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
{-# LINE 196 "AstInternal.hs" #-}

{-# LINE 675 "./TypeChecking.ag" #-}

fixedValue :: a -> a -> a -> a
fixedValue a _ _ = a
{-# LINE 202 "AstInternal.hs" #-}

{-# LINE 709 "./TypeChecking.ag" #-}

getCAtts t =
    case t of
      SetOfType (UnnamedCompositeType t) -> t
      _ -> []
{-# LINE 210 "AstInternal.hs" #-}

{-# LINE 750 "./TypeChecking.ag" #-}


{-# LINE 215 "AstInternal.hs" #-}

{-# LINE 811 "./TypeChecking.ag" #-}

getRowTypes :: [Type] -> [Type]
getRowTypes [RowCtor ts] = ts
getRowTypes ts = ts
{-# LINE 222 "AstInternal.hs" #-}
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
         child check          : {Maybe Expression}
         child cons           : RowConstraintList 
         visit 0:
            local annotatedTree : _
-}
data AttributeDef  = AttributeDef (Annotation) (String) (TypeName) (Maybe Expression) (RowConstraintList) 
                   deriving ( Data,Eq,Show,Typeable)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _ann _name _typ _check _cons )  =
    (sem_AttributeDef_AttributeDef _ann _name (sem_TypeName _typ ) _check (sem_RowConstraintList _cons ) )
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
                                 (Maybe Expression) ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef ann_ name_ typ_ check_ cons_  =
    (\ _lhsIenv ->
         (let _lhsOattrName :: String
              _lhsOnamedType :: Type
              _lhsOannotatedTree :: AttributeDef
              _lhsOenv :: Environment
              _typOenv :: Environment
              _consOenv :: Environment
              _typIannotatedTree :: TypeName
              _typIenv :: Environment
              _typInamedType :: Type
              _consIannotatedTree :: RowConstraintList
              _consIenv :: Environment
              -- "./TypeChecking.ag"(line 855, column 7)
              _lhsOattrName =
                  {-# LINE 855 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 284 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 856, column 7)
              _lhsOnamedType =
                  {-# LINE 856 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 289 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIannotatedTree check_ _consIannotatedTree
                  {-# LINE 294 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 299 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _consIenv
                  {-# LINE 304 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 309 "AstInternal.hs" #-}
              -- copy rule (chain)
              _consOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 314 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIenv,_typInamedType) =
                  (typ_ _typOenv )
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
              -- "./TypeChecking.ag"(line 865, column 12)
              _lhsOattrs =
                  {-# LINE 865 "./TypeChecking.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 377 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 382 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 387 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 392 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 397 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 402 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 866, column 11)
              _lhsOattrs =
                  {-# LINE 866 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 418 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 423 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 428 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 433 "AstInternal.hs" #-}
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
                  {-# LINE 481 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 486 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 491 "AstInternal.hs" #-}
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
                  {-# LINE 502 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 507 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 512 "AstInternal.hs" #-}
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
                  {-# LINE 567 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 572 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 577 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 582 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 587 "AstInternal.hs" #-}
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
                  {-# LINE 602 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 607 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 612 "AstInternal.hs" #-}
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
                  {-# LINE 664 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 669 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 674 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 679 "AstInternal.hs" #-}
              -- copy rule (chain)
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x1Ienv
                  {-# LINE 684 "AstInternal.hs" #-}
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
                  {-# LINE 742 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 747 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 752 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 757 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 762 "AstInternal.hs" #-}
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
                  {-# LINE 777 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 782 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 787 "AstInternal.hs" #-}
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
                  {-# LINE 847 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 852 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 857 "AstInternal.hs" #-}
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
                  {-# LINE 868 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 873 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 878 "AstInternal.hs" #-}
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
                  {-# LINE 889 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 894 "AstInternal.hs" #-}
              -- copy rule (chain)
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  UnionAll
                  {-# LINE 910 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 915 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 920 "AstInternal.hs" #-}
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
                  {-# LINE 998 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1003 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _expressionIenv
                  {-# LINE 1008 "AstInternal.hs" #-}
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1013 "AstInternal.hs" #-}
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
                  {-# LINE 1032 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1037 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 1042 "AstInternal.hs" #-}
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1047 "AstInternal.hs" #-}
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
                  {-# LINE 1080 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1085 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onDeleteIenv
                  {-# LINE 1090 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1095 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tableAttsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 1100 "AstInternal.hs" #-}
              -- copy rule (chain)
              _onUpdateOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tableAttsIenv
                  {-# LINE 1105 "AstInternal.hs" #-}
              -- copy rule (chain)
              _onDeleteOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onUpdateIenv
                  {-# LINE 1110 "AstInternal.hs" #-}
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
                  {-# LINE 1135 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1140 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 1145 "AstInternal.hs" #-}
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1150 "AstInternal.hs" #-}
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
                  {-# LINE 1206 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1211 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 1216 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1221 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 1226 "AstInternal.hs" #-}
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
                  {-# LINE 1241 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1246 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1251 "AstInternal.hs" #-}
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
                  {-# LINE 1301 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1306 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1311 "AstInternal.hs" #-}
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
                  {-# LINE 1322 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1327 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1332 "AstInternal.hs" #-}
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
                  {-# LINE 1380 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1385 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1390 "AstInternal.hs" #-}
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
                  {-# LINE 1401 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1406 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1411 "AstInternal.hs" #-}
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
                  {-# LINE 1459 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1464 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1469 "AstInternal.hs" #-}
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
                  {-# LINE 1480 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1485 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1490 "AstInternal.hs" #-}
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
                  {-# LINE 1550 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1555 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1560 "AstInternal.hs" #-}
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
                  {-# LINE 1571 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1576 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1581 "AstInternal.hs" #-}
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
                  {-# LINE 1592 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1597 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1602 "AstInternal.hs" #-}
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
                  {-# LINE 1613 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1618 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1623 "AstInternal.hs" #-}
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
                  {-# LINE 1826 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 95, column 9)
              _backTree =
                  {-# LINE 95 "./TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 1831 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 103, column 19)
              _tpe =
                  {-# LINE 103 "./TypeChecking.ag" #-}
                  Right typeBool
                  {-# LINE 1836 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1841 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 1846 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1851 "AstInternal.hs" #-}
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
                  {-# LINE 1876 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 181, column 9)
              _whenTypes =
                  {-# LINE 181 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
                  {-# LINE 1882 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 183, column 9)
              _thenTypes =
                  {-# LINE 183 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
                  {-# LINE 1889 "AstInternal.hs" #-}
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
                  {-# LINE 1900 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 197, column 9)
              _backTree =
                  {-# LINE 197 "./TypeChecking.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1905 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1910 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1915 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 1920 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1925 "AstInternal.hs" #-}
              -- copy rule (chain)
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 1930 "AstInternal.hs" #-}
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
                  {-# LINE 1964 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 181, column 9)
              _whenTypes =
                  {-# LINE 181 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
                  {-# LINE 1970 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 183, column 9)
              _thenTypes =
                  {-# LINE 183 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
                  {-# LINE 1977 "AstInternal.hs" #-}
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
                  {-# LINE 1989 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 211, column 9)
              _backTree =
                  {-# LINE 211 "./TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1994 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  _valueIliftedColumnName
                  {-# LINE 1999 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 2004 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 2009 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2014 "AstInternal.hs" #-}
              -- copy rule (chain)
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valueIenv
                  {-# LINE 2019 "AstInternal.hs" #-}
              -- copy rule (chain)
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 2024 "AstInternal.hs" #-}
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
                  {-# LINE 2056 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 115, column 12)
              _tpe =
                  {-# LINE 115 "./TypeChecking.ag" #-}
                  Right $ _tnInamedType
                  {-# LINE 2061 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 116, column 12)
              _backTree =
                  {-# LINE 116 "./TypeChecking.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 2066 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 691, column 10)
              _lhsOliftedColumnName =
                  {-# LINE 691 "./TypeChecking.ag" #-}
                  case _tnIannotatedTree of
                    SimpleTypeName _ tn -> tn
                    _ -> ""
                  {-# LINE 2073 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 2078 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tnIenv
                  {-# LINE 2083 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2088 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 2093 "AstInternal.hs" #-}
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
                  {-# LINE 2117 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 227, column 9)
              _tpe =
                  {-# LINE 227 "./TypeChecking.ag" #-}
                  Right typeBool
                  {-# LINE 2122 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 228, column 9)
              _backTree =
                  {-# LINE 228 "./TypeChecking.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 2127 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2132 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 2137 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 2142 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2147 "AstInternal.hs" #-}
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
                  {-# LINE 2166 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 93, column 9)
              _backTree =
                  {-# LINE 93 "./TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 2171 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 102, column 17)
              _tpe =
                  {-# LINE 102 "./TypeChecking.ag" #-}
                  Right typeNumeric
                  {-# LINE 2176 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2181 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 2186 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2191 "AstInternal.hs" #-}
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
                  {-# LINE 2213 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 160, column 9)
              _tpe =
                  {-# LINE 160 "./TypeChecking.ag" #-}
                  checkTypes _argsItypeList $
                    typeCheckFunCall
                      _lhsIenv
                      funName_
                      _argsItypeList
                  {-# LINE 2222 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 165, column 9)
              _backTree =
                  {-# LINE 165 "./TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 2227 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 687, column 13)
              _lhsOliftedColumnName =
                  {-# LINE 687 "./TypeChecking.ag" #-}
                  if isOperatorName funName_
                     then ""
                     else funName_
                  {-# LINE 2234 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 2239 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _argsIenv
                  {-# LINE 2244 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2249 "AstInternal.hs" #-}
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
                  {-# LINE 2268 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 221, column 9)
              _tpe =
                  {-# LINE 221 "./TypeChecking.ag" #-}
                  let (correlationName,iden) = splitIdentifier i_
                  in envLookupID _lhsIenv correlationName iden
                  {-# LINE 2274 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 223, column 9)
              _backTree =
                  {-# LINE 223 "./TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 2279 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 686, column 16)
              _lhsOliftedColumnName =
                  {-# LINE 686 "./TypeChecking.ag" #-}
                  i_
                  {-# LINE 2284 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 2289 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2294 "AstInternal.hs" #-}
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
                  {-# LINE 2321 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 255, column 9)
              _tpe =
                  {-# LINE 255 "./TypeChecking.ag" #-}
                  do
                    lt <- _listIlistType
                    ty <- resolveResultSetType
                            _lhsIenv
                            [getTypeAnnotation _exprIannotatedTree, lt]
                    return typeBool
                  {-# LINE 2331 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 261, column 9)
              _backTree =
                  {-# LINE 261 "./TypeChecking.ag" #-}
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                  {-# LINE 2336 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  _exprIliftedColumnName
                  {-# LINE 2341 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                  {-# LINE 2346 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _listIenv
                  {-# LINE 2351 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2356 "AstInternal.hs" #-}
              -- copy rule (chain)
              _listOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 2361 "AstInternal.hs" #-}
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
                  {-# LINE 2382 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 89, column 9)
              _backTree =
                  {-# LINE 89 "./TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 2387 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 100, column 19)
              _tpe =
                  {-# LINE 100 "./TypeChecking.ag" #-}
                  Right typeInt
                  {-# LINE 2392 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2397 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 2402 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2407 "AstInternal.hs" #-}
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
                  {-# LINE 2423 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 97, column 9)
              _backTree =
                  {-# LINE 97 "./TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 2428 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 105, column 16)
              _tpe =
                  {-# LINE 105 "./TypeChecking.ag" #-}
                  Right UnknownStringLit
                  {-# LINE 2433 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2438 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 2443 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2448 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOliftedColumnName)))
sem_Expression_PositionalArg :: Annotation ->
                                Integer ->
                                T_Expression 
sem_Expression_PositionalArg ann_ p_  =
    (\ _lhsIenv ->
         (let _lhsOliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOenv :: Environment
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2462 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 2467 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2472 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2477 "AstInternal.hs" #-}
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
                  {-# LINE 2497 "AstInternal.hs" #-}
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
                  {-# LINE 2509 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 248, column 9)
              _backTree =
                  {-# LINE 248 "./TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 2514 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2519 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 2524 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 2529 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2534 "AstInternal.hs" #-}
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
                  {-# LINE 2554 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 91, column 9)
              _backTree =
                  {-# LINE 91 "./TypeChecking.ag" #-}
                  StringLit ann_ quote_ value_
                  {-# LINE 2559 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 101, column 18)
              _tpe =
                  {-# LINE 101 "./TypeChecking.ag" #-}
                  Right UnknownStringLit
                  {-# LINE 2564 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2569 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  StringLit ann_ quote_ value_
                  {-# LINE 2574 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2579 "AstInternal.hs" #-}
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
              -- use rule "./TypeChecking.ag"(line 672, column 22)
              _lhsOliftedColumnName =
                  {-# LINE 672 "./TypeChecking.ag" #-}
                  _fnIliftedColumnName
                  {-# LINE 2611 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree _dirIannotatedTree
                  {-# LINE 2616 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2621 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _dirIenv
                  {-# LINE 2626 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2631 "AstInternal.hs" #-}
              -- copy rule (chain)
              _partitionByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fnIenv
                  {-# LINE 2636 "AstInternal.hs" #-}
              -- copy rule (chain)
              _orderByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _partitionByIenv
                  {-# LINE 2641 "AstInternal.hs" #-}
              -- copy rule (chain)
              _dirOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _orderByIenv
                  {-# LINE 2646 "AstInternal.hs" #-}
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
                  {-# LINE 2712 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2717 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2722 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 2727 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2732 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 2737 "AstInternal.hs" #-}
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
                  {-# LINE 2753 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2758 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2763 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2768 "AstInternal.hs" #-}
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
                  {-# LINE 2826 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2831 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2836 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 2841 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2846 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 2851 "AstInternal.hs" #-}
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
                  {-# LINE 2867 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2872 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2877 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2882 "AstInternal.hs" #-}
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
                  {-# LINE 2936 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 2941 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2946 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 2951 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2956 "AstInternal.hs" #-}
              -- copy rule (chain)
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x1Ienv
                  {-# LINE 2961 "AstInternal.hs" #-}
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
                  {-# LINE 3019 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3024 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 3029 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3034 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 3039 "AstInternal.hs" #-}
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
                  {-# LINE 3054 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3059 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3064 "AstInternal.hs" #-}
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
                  {-# LINE 3112 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3117 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 3122 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3127 "AstInternal.hs" #-}
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
                  {-# LINE 3183 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 3188 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3193 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 3198 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3203 "AstInternal.hs" #-}
              -- copy rule (chain)
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x1Ienv
                  {-# LINE 3208 "AstInternal.hs" #-}
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
                  {-# LINE 3266 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3271 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 3276 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3281 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 3286 "AstInternal.hs" #-}
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
                  {-# LINE 3301 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3306 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3311 "AstInternal.hs" #-}
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
                  {-# LINE 3376 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1018, column 9)
              _stsOenv =
                  {-# LINE 1018 "./TypeChecking.ag" #-}
                  fromRight _lhsIenv $ updateEnvironment _lhsIenv [EnvStackIDs [("", _varsIdefs)]]
                  {-# LINE 3381 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _varsIannotatedTree _stsIannotatedTree
                  {-# LINE 3386 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3391 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 3396 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3401 "AstInternal.hs" #-}
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
                  {-# LINE 3423 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIannotatedTree
                  {-# LINE 3428 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3433 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 3438 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3443 "AstInternal.hs" #-}
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
                  {-# LINE 3493 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3498 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3503 "AstInternal.hs" #-}
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
                  {-# LINE 3514 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3519 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3524 "AstInternal.hs" #-}
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
                  {-# LINE 3586 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  InList ann_ _exprsIannotatedTree
                  {-# LINE 3591 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3596 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprsIenv
                  {-# LINE 3601 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3606 "AstInternal.hs" #-}
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
                  {-# LINE 3633 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  InSelect ann_ _selIannotatedTree
                  {-# LINE 3638 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3643 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 3648 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3653 "AstInternal.hs" #-}
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
                  {-# LINE 3713 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3718 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _expressionIenv
                  {-# LINE 3723 "AstInternal.hs" #-}
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3728 "AstInternal.hs" #-}
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
                  {-# LINE 3747 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3752 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 3757 "AstInternal.hs" #-}
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3762 "AstInternal.hs" #-}
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
                  {-# LINE 3830 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3835 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3840 "AstInternal.hs" #-}
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
                  {-# LINE 3851 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3856 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3861 "AstInternal.hs" #-}
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
                  {-# LINE 3872 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3877 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3882 "AstInternal.hs" #-}
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
                  {-# LINE 3893 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3898 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3903 "AstInternal.hs" #-}
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
                  {-# LINE 3914 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3919 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3924 "AstInternal.hs" #-}
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
                  {-# LINE 3972 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3977 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3982 "AstInternal.hs" #-}
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
                  {-# LINE 3993 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3998 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4003 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- MTableRef ---------------------------------------------------
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 4060 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4065 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 4070 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOidens =
                  {-# LINE 450 "./TypeChecking.ag" #-}
                  _justIidens
                  {-# LINE 4075 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOjoinIdens =
                  {-# LINE 451 "./TypeChecking.ag" #-}
                  _justIjoinIdens
                  {-# LINE 4080 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4085 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 550, column 9)
              _lhsOidens =
                  {-# LINE 550 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4100 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 551, column 9)
              _lhsOjoinIdens =
                  {-# LINE 551 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4105 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 4110 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4115 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4120 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOidens,_lhsOjoinIdens)))
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
              -- "./TypeChecking.ag"(line 1062, column 12)
              _lhsOexprType =
                  {-# LINE 1062 "./TypeChecking.ag" #-}
                  Just $ getTypeAnnotation _justIannotatedTree
                  {-# LINE 4174 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 4179 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4184 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 4189 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4194 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv,_justIliftedColumnName) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOexprType)))
sem_MaybeExpression_Nothing :: T_MaybeExpression 
sem_MaybeExpression_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOexprType :: (Maybe Type)
              _lhsOannotatedTree :: MaybeExpression
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 1063, column 15)
              _lhsOexprType =
                  {-# LINE 1063 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 4208 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 4213 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4218 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4223 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOexprType)))
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
                  {-# LINE 4271 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4276 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4281 "AstInternal.hs" #-}
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
                  {-# LINE 4292 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4297 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4302 "AstInternal.hs" #-}
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
                  {-# LINE 4353 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4358 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 4363 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4368 "AstInternal.hs" #-}
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
                  {-# LINE 4381 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4386 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4391 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 988, column 9)
              _lhsOnamedType =
                  {-# LINE 988 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 4455 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 990, column 9)
              _lhsOparamName =
                  {-# LINE 990 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 4460 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIannotatedTree
                  {-# LINE 4465 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4470 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 4475 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4480 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 988, column 9)
              _lhsOnamedType =
                  {-# LINE 988 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 4501 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 992, column 9)
              _lhsOparamName =
                  {-# LINE 992 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 4506 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIannotatedTree
                  {-# LINE 4511 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4516 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 4521 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4526 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 996, column 13)
              _lhsOparams =
                  {-# LINE 996 "./TypeChecking.ag" #-}
                  ((_hdIparamName, _hdInamedType) : _tlIparams)
                  {-# LINE 4587 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 4592 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4597 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 4602 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4607 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 4612 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 995, column 12)
              _lhsOparams =
                  {-# LINE 995 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4628 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4633 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4638 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4643 "AstInternal.hs" #-}
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
                  {-# LINE 4697 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4702 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4707 "AstInternal.hs" #-}
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
                  {-# LINE 4718 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4723 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4728 "AstInternal.hs" #-}
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
                  {-# LINE 4739 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4744 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4749 "AstInternal.hs" #-}
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
                  {-# LINE 4797 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4802 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4807 "AstInternal.hs" #-}
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
                  {-# LINE 4818 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4823 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4828 "AstInternal.hs" #-}
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
                  {-# LINE 4877 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Root _statementsIannotatedTree
                  {-# LINE 4882 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4887 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _statementsIenv
                  {-# LINE 4892 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4897 "AstInternal.hs" #-}
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
                  {-# LINE 4983 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4988 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4993 "AstInternal.hs" #-}
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
                  {-# LINE 5005 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5010 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5015 "AstInternal.hs" #-}
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
                  {-# LINE 5032 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5037 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _expressionIenv
                  {-# LINE 5042 "AstInternal.hs" #-}
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5047 "AstInternal.hs" #-}
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
                  {-# LINE 5061 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5066 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5071 "AstInternal.hs" #-}
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
                  {-# LINE 5093 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5098 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onDeleteIenv
                  {-# LINE 5103 "AstInternal.hs" #-}
              -- copy rule (down)
              _onUpdateOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5108 "AstInternal.hs" #-}
              -- copy rule (chain)
              _onDeleteOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onUpdateIenv
                  {-# LINE 5113 "AstInternal.hs" #-}
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
                  {-# LINE 5129 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5134 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5139 "AstInternal.hs" #-}
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
                  {-# LINE 5193 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5198 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 5203 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5208 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 5213 "AstInternal.hs" #-}
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
                  {-# LINE 5228 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5233 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5238 "AstInternal.hs" #-}
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
         child selTref        : MTableRef 
         child selWhere       : Where 
         child selGroupBy     : ExpressionList 
         child selHaving      : MaybeExpression 
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
                       | Select (Annotation) (Distinct) (SelectList) (MTableRef) (Where) (ExpressionList) (MaybeExpression) (ExpressionList) (Direction) (MaybeExpression) (MaybeExpression) 
                       | Values (Annotation) (ExpressionListList) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectExpression :: SelectExpression  ->
                        T_SelectExpression 
sem_SelectExpression (CombineSelect _ann _ctype _sel1 _sel2 )  =
    (sem_SelectExpression_CombineSelect _ann (sem_CombineType _ctype ) (sem_SelectExpression _sel1 ) (sem_SelectExpression _sel2 ) )
sem_SelectExpression (Select _ann _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selDir _selLimit _selOffset )  =
    (sem_SelectExpression_Select _ann (sem_Distinct _selDistinct ) (sem_SelectList _selSelectList ) (sem_MTableRef _selTref ) (sem_Where _selWhere ) (sem_ExpressionList _selGroupBy ) (sem_MaybeExpression _selHaving ) (sem_ExpressionList _selOrderBy ) (sem_Direction _selDir ) (sem_MaybeExpression _selLimit ) (sem_MaybeExpression _selOffset ) )
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
                  {-# LINE 5332 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 434, column 9)
              _tpe =
                  {-# LINE 434 "./TypeChecking.ag" #-}
                  let sel1t = getTypeAnnotation _sel1IannotatedTree
                      sel2t = getTypeAnnotation _sel2IannotatedTree
                  in checkTypes [sel1t, sel2t] $
                        typeCheckCombineSelect _lhsIenv sel1t sel2t
                  {-# LINE 5340 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 439, column 9)
              _backTree =
                  {-# LINE 439 "./TypeChecking.ag" #-}
                  CombineSelect ann_ _ctypeIannotatedTree
                                _sel1IannotatedTree
                                _sel2IannotatedTree
                  {-# LINE 5347 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CombineSelect ann_ _ctypeIannotatedTree _sel1IannotatedTree _sel2IannotatedTree
                  {-# LINE 5352 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sel2Ienv
                  {-# LINE 5357 "AstInternal.hs" #-}
              -- copy rule (down)
              _ctypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5362 "AstInternal.hs" #-}
              -- copy rule (chain)
              _sel1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _ctypeIenv
                  {-# LINE 5367 "AstInternal.hs" #-}
              -- copy rule (chain)
              _sel2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sel1Ienv
                  {-# LINE 5372 "AstInternal.hs" #-}
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
                               T_MaybeExpression  ->
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
              _selTrefIannotatedTree :: MTableRef
              _selTrefIenv :: Environment
              _selTrefIidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _selTrefIjoinIdens :: ([String])
              _selWhereIannotatedTree :: Where
              _selWhereIenv :: Environment
              _selGroupByIannotatedTree :: ExpressionList
              _selGroupByIenv :: Environment
              _selGroupByItypeList :: ([Type])
              _selHavingIannotatedTree :: MaybeExpression
              _selHavingIenv :: Environment
              _selHavingIexprType :: (Maybe Type)
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
                  {-# LINE 5441 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 412, column 9)
              _tpe =
                  {-# LINE 412 "./TypeChecking.ag" #-}
                  do
                  whereType <- checkExpressionBool _selWhereIannotatedTree
                  let trefType = fromMaybe typeBool $ fmap getTypeAnnotation
                                                           _selTrefIannotatedTree
                      slType = _selSelectListIlistType
                  chainTypeCheckFailed [trefType, whereType, slType] $
                    Right $ case slType of
                              UnnamedCompositeType [(_,Pseudo Void)] -> Pseudo Void
                              _ -> SetOfType slType
                  {-# LINE 5454 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 422, column 9)
              _backTree =
                  {-# LINE 422 "./TypeChecking.ag" #-}
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
                  {-# LINE 5469 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 631, column 10)
              _newEnv =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  case updateEnvironment _lhsIenv
                        (convertToNewStyleUpdates _selTrefIidens _selTrefIjoinIdens) of
                    Left x -> error $ show x
                    Right e -> e
                  {-# LINE 5477 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 635, column 10)
              _selSelectListOenv =
                  {-# LINE 635 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 5482 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 636, column 10)
              _selWhereOenv =
                  {-# LINE 636 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 5487 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Select ann_ _selDistinctIannotatedTree _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selDirIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
                  {-# LINE 5492 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selOffsetIenv
                  {-# LINE 5497 "AstInternal.hs" #-}
              -- copy rule (down)
              _selDistinctOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5502 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selTrefOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selSelectListIenv
                  {-# LINE 5507 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selGroupByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selWhereIenv
                  {-# LINE 5512 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selHavingOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selGroupByIenv
                  {-# LINE 5517 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selOrderByOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selHavingIenv
                  {-# LINE 5522 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selDirOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selOrderByIenv
                  {-# LINE 5527 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selLimitOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selDirIenv
                  {-# LINE 5532 "AstInternal.hs" #-}
              -- copy rule (chain)
              _selOffsetOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selLimitIenv
                  {-# LINE 5537 "AstInternal.hs" #-}
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
              ( _selHavingIannotatedTree,_selHavingIenv,_selHavingIexprType) =
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
                  {-# LINE 5577 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 407, column 9)
              _tpe =
                  {-# LINE 407 "./TypeChecking.ag" #-}
                  typeCheckValuesExpr
                              _lhsIenv
                              _vllItypeListList
                  {-# LINE 5584 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 410, column 9)
              _backTree =
                  {-# LINE 410 "./TypeChecking.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 5589 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 5594 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _vllIenv
                  {-# LINE 5599 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5604 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 571, column 9)
              _lhsOitemType =
                  {-# LINE 571 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exIannotatedTree
                  {-# LINE 5670 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 576, column 9)
              _annotatedTree =
                  {-# LINE 576 "./TypeChecking.ag" #-}
                  SelExp ann_ $ fixStar _exIannotatedTree
                  {-# LINE 5675 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 697, column 14)
              _lhsOcolumnName =
                  {-# LINE 697 "./TypeChecking.ag" #-}
                  case _exIliftedColumnName of
                    "" -> "?column?"
                    s -> s
                  {-# LINE 5682 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5687 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exIenv
                  {-# LINE 5692 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5697 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 571, column 9)
              _lhsOitemType =
                  {-# LINE 571 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exIannotatedTree
                  {-# LINE 5719 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 578, column 9)
              _backTree =
                  {-# LINE 578 "./TypeChecking.ag" #-}
                  SelectItem ann_ (fixStar _exIannotatedTree) name_
                  {-# LINE 5724 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 700, column 18)
              _lhsOcolumnName =
                  {-# LINE 700 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 5729 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SelectItem ann_ _exIannotatedTree name_
                  {-# LINE 5734 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5739 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exIenv
                  {-# LINE 5744 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5749 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 560, column 12)
              _lhsOlistType =
                  {-# LINE 560 "./TypeChecking.ag" #-}
                  doSelectItemListTpe _lhsIenv _hdIcolumnName _hdIitemType _tlIlistType
                  {-# LINE 5810 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5815 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5820 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 5825 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5830 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 5835 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 561, column 11)
              _lhsOlistType =
                  {-# LINE 561 "./TypeChecking.ag" #-}
                  UnnamedCompositeType []
                  {-# LINE 5851 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5856 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5861 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5866 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 606, column 9)
              _lhsOlistType =
                  {-# LINE 606 "./TypeChecking.ag" #-}
                  _itemsIlistType
                  {-# LINE 5924 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SelectList ann_ _itemsIannotatedTree _stringListIannotatedTree
                  {-# LINE 5929 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5934 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stringListIenv
                  {-# LINE 5939 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5944 "AstInternal.hs" #-}
              -- copy rule (chain)
              _stringListOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _itemsIenv
                  {-# LINE 5949 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 803, column 9)
              _rowSetError =
                  {-# LINE 803 "./TypeChecking.ag" #-}
                  let atts = _attsIstrings
                      types = getRowTypes _valsItypeList
                  in if length atts /= length types
                       then Just WrongNumberOfColumns
                       else Nothing
                  {-# LINE 6027 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 809, column 9)
              _lhsOpairs =
                  {-# LINE 809 "./TypeChecking.ag" #-}
                  zip _attsIstrings $ getRowTypes _valsItypeList
                  {-# LINE 6032 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  RowSetClause ann_ _attsIannotatedTree _valsIannotatedTree
                  {-# LINE 6037 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6042 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valsIenv
                  {-# LINE 6047 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOrowSetError =
                  {-# LINE 794 "./TypeChecking.ag" #-}
                  _rowSetError
                  {-# LINE 6052 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6057 "AstInternal.hs" #-}
              -- copy rule (chain)
              _valsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 6062 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 800, column 9)
              _lhsOpairs =
                  {-# LINE 800 "./TypeChecking.ag" #-}
                  [(att_, getTypeAnnotation _valIannotatedTree)]
                  {-# LINE 6086 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 801, column 9)
              _lhsOrowSetError =
                  {-# LINE 801 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 6091 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SetClause ann_ att_ _valIannotatedTree
                  {-# LINE 6096 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6101 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valIenv
                  {-# LINE 6106 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6111 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 785, column 10)
              _lhsOpairs =
                  {-# LINE 785 "./TypeChecking.ag" #-}
                  _hdIpairs ++ _tlIpairs
                  {-# LINE 6175 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 786, column 10)
              _lhsOrowSetErrors =
                  {-# LINE 786 "./TypeChecking.ag" #-}
                  maybeToList _hdIrowSetError ++ _tlIrowSetErrors
                  {-# LINE 6180 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 6185 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6190 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 6195 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6200 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 6205 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 787, column 9)
              _lhsOpairs =
                  {-# LINE 787 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6222 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 788, column 9)
              _lhsOrowSetErrors =
                  {-# LINE 788 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6227 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6232 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6237 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6242 "AstInternal.hs" #-}
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
         child check          : {Maybe Expression}
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
         child whr            : Where 
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
         child whr            : Where 
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
              -- "./TypeChecking.ag"(line 339, column 9)
              _lhsOenvUpdates =
                  {-# LINE 339 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6618 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Assignment ann_ target_ _valueIannotatedTree
                  {-# LINE 6623 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6628 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valueIenv
                  {-# LINE 6633 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6638 "AstInternal.hs" #-}
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
                  {-# LINE 6668 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 366, column 24)
              _elsOenvUpdates =
                  {-# LINE 366 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6673 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CaseStatement ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 6678 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6683 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 6688 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6693 "AstInternal.hs" #-}
              -- copy rule (chain)
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valIenv
                  {-# LINE 6698 "AstInternal.hs" #-}
              -- copy rule (chain)
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 6703 "AstInternal.hs" #-}
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
                  {-# LINE 6722 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ContinueStatement ann_
                  {-# LINE 6727 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6732 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6737 "AstInternal.hs" #-}
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
                  {-# LINE 6760 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Copy ann_ table_ _targetColsIannotatedTree _sourceIannotatedTree
                  {-# LINE 6765 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6770 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sourceIenv
                  {-# LINE 6775 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetColsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6780 "AstInternal.hs" #-}
              -- copy rule (chain)
              _sourceOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _targetColsIenv
                  {-# LINE 6785 "AstInternal.hs" #-}
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
                  {-# LINE 6803 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 6808 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6813 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6818 "AstInternal.hs" #-}
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
              _typInamedType :: Type
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6842 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6847 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 956, column 9)
              _tpe =
                  {-# LINE 956 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 6852 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 957, column 9)
              _backTree =
                  {-# LINE 957 "./TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree check_
                  {-# LINE 6857 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 958, column 9)
              _statementInfo =
                  {-# LINE 958 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6862 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 959, column 9)
              _envUpdates =
                  {-# LINE 959 "./TypeChecking.ag" #-}
                  [EnvCreateDomain (ScalarType name_) _typInamedType]
                  {-# LINE 6867 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree check_
                  {-# LINE 6872 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 6877 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6882 "AstInternal.hs" #-}
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
                  {-# LINE 6925 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6930 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1000, column 9)
              _tpe =
                  {-# LINE 1000 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 6935 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1001, column 9)
              _backTree =
                  {-# LINE 1001 "./TypeChecking.ag" #-}
                  CreateFunction ann_
                                 _langIannotatedTree
                                 name_
                                 _paramsIannotatedTree
                                 _rettypeIannotatedTree
                                 bodyQuote_
                                 _bodyIannotatedTree
                                 _volIannotatedTree
                  {-# LINE 6947 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1009, column 9)
              _statementInfo =
                  {-# LINE 1009 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6952 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1010, column 9)
              _envUpdates =
                  {-# LINE 1010 "./TypeChecking.ag" #-}
                  [EnvCreateFunction FunName name_ (map snd _paramsIparams) _rettypeInamedType]
                  {-# LINE 6957 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1012, column 9)
              _bodyOenv =
                  {-# LINE 1012 "./TypeChecking.ag" #-}
                  fromRight _lhsIenv $
                  updateEnvironment _lhsIenv [EnvStackIDs [("", _paramsIparams)
                                                          ,(name_, _paramsIparams)]]
                  {-# LINE 6964 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateFunction ann_ _langIannotatedTree name_ _paramsIannotatedTree _rettypeIannotatedTree bodyQuote_ _bodyIannotatedTree _volIannotatedTree
                  {-# LINE 6969 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _volIenv
                  {-# LINE 6974 "AstInternal.hs" #-}
              -- copy rule (down)
              _langOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6979 "AstInternal.hs" #-}
              -- copy rule (chain)
              _paramsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _langIenv
                  {-# LINE 6984 "AstInternal.hs" #-}
              -- copy rule (chain)
              _rettypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _paramsIenv
                  {-# LINE 6989 "AstInternal.hs" #-}
              -- copy rule (chain)
              _volOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _bodyIenv
                  {-# LINE 6994 "AstInternal.hs" #-}
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
                  {-# LINE 7031 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7036 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 870, column 9)
              _attrTypes =
                  {-# LINE 870 "./TypeChecking.ag" #-}
                  map snd _attsIattrs
                  {-# LINE 7041 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 871, column 9)
              _tpe =
                  {-# LINE 871 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 7046 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 872, column 9)
              _backTree =
                  {-# LINE 872 "./TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
                  {-# LINE 7051 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 873, column 9)
              _statementInfo =
                  {-# LINE 873 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7056 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 874, column 9)
              _envUpdates =
                  {-# LINE 874 "./TypeChecking.ag" #-}
                  [EnvCreateTable name_ _attsIattrs []]
                  {-# LINE 7061 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
                  {-# LINE 7066 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _consIenv
                  {-# LINE 7071 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7076 "AstInternal.hs" #-}
              -- copy rule (chain)
              _consOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 7081 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 878, column 9)
              _selType =
                  {-# LINE 878 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exprIannotatedTree
                  {-# LINE 7103 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 879, column 9)
              _tpe =
                  {-# LINE 879 "./TypeChecking.ag" #-}
                  Right _selType
                  {-# LINE 7108 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 880, column 9)
              _backTree =
                  {-# LINE 880 "./TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 7113 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 881, column 9)
              _statementInfo =
                  {-# LINE 881 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7118 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 882, column 9)
              _attrs =
                  {-# LINE 882 "./TypeChecking.ag" #-}
                  case _selType     of
                    UnnamedCompositeType c -> c
                    _-> []
                  {-# LINE 7125 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 885, column 9)
              _envUpdates =
                  {-# LINE 885 "./TypeChecking.ag" #-}
                  [EnvCreateTable name_ _attrs     []]
                  {-# LINE 7130 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 7135 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7140 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 7145 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOenvUpdates =
                  {-# LINE 331 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7150 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7155 "AstInternal.hs" #-}
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
                  {-# LINE 7180 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7185 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 937, column 9)
              _tpe =
                  {-# LINE 937 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 7190 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 939, column 9)
              _backTree =
                  {-# LINE 939 "./TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 7195 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 940, column 9)
              _statementInfo =
                  {-# LINE 940 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7200 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 941, column 9)
              _envUpdates =
                  {-# LINE 941 "./TypeChecking.ag" #-}
                  [EnvCreateComposite name_ _attsIattrs]
                  {-# LINE 7205 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 7210 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _attsIenv
                  {-# LINE 7215 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7220 "AstInternal.hs" #-}
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
                  {-# LINE 7244 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7249 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 895, column 9)
              _tpe =
                  {-# LINE 895 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _exprIannotatedTree] $ Right $ Pseudo Void
                  {-# LINE 7254 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 896, column 9)
              _backTree =
                  {-# LINE 896 "./TypeChecking.ag" #-}
                  CreateView ann_ name_ _exprIannotatedTree
                  {-# LINE 7259 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 897, column 9)
              _statementInfo =
                  {-# LINE 897 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7264 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 898, column 9)
              _attrs =
                  {-# LINE 898 "./TypeChecking.ag" #-}
                  case getTypeAnnotation _exprIannotatedTree of
                    SetOfType (UnnamedCompositeType c) -> c
                    _ -> []
                  {-# LINE 7271 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 901, column 9)
              _envUpdates =
                  {-# LINE 901 "./TypeChecking.ag" #-}
                  [EnvCreateView name_ _attrs    ]
                  {-# LINE 7276 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  CreateView ann_ name_ _exprIannotatedTree
                  {-# LINE 7281 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 7286 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7291 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 7316 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7321 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 825, column 9)
              _tpe =
                  {-# LINE 825 "./TypeChecking.ag" #-}
                  case checkRelationExists _lhsIenv table_ of
                    Just e -> Left [e]
                    Nothing -> do
                      whereType <- checkExpressionBool _whrIannotatedTree
                      return $ Pseudo Void
                  {-# LINE 7330 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 831, column 9)
              _statementInfo =
                  {-# LINE 831 "./TypeChecking.ag" #-}
                  [DeleteInfo table_]
                  {-# LINE 7335 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 832, column 9)
              _backTree =
                  {-# LINE 832 "./TypeChecking.ag" #-}
                  Delete ann_ table_ _whrIannotatedTree returning_
                  {-# LINE 7340 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 833, column 9)
              _envUpdates =
                  {-# LINE 833 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7345 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Delete ann_ table_ _whrIannotatedTree returning_
                  {-# LINE 7350 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _whrIenv
                  {-# LINE 7355 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7360 "AstInternal.hs" #-}
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
                  {-# LINE 7387 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  DropFunction ann_ _ifEIannotatedTree _sigsIannotatedTree _cascadeIannotatedTree
                  {-# LINE 7392 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7397 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _cascadeIenv
                  {-# LINE 7402 "AstInternal.hs" #-}
              -- copy rule (down)
              _ifEOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7407 "AstInternal.hs" #-}
              -- copy rule (chain)
              _sigsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _ifEIenv
                  {-# LINE 7412 "AstInternal.hs" #-}
              -- copy rule (chain)
              _cascadeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _sigsIenv
                  {-# LINE 7417 "AstInternal.hs" #-}
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
                  {-# LINE 7453 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  DropSomething ann_ _dropTypeIannotatedTree _ifEIannotatedTree _namesIannotatedTree _cascadeIannotatedTree
                  {-# LINE 7458 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7463 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _cascadeIenv
                  {-# LINE 7468 "AstInternal.hs" #-}
              -- copy rule (down)
              _dropTypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7473 "AstInternal.hs" #-}
              -- copy rule (chain)
              _ifEOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _dropTypeIenv
                  {-# LINE 7478 "AstInternal.hs" #-}
              -- copy rule (chain)
              _namesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _ifEIenv
                  {-# LINE 7483 "AstInternal.hs" #-}
              -- copy rule (chain)
              _cascadeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _namesIenv
                  {-# LINE 7488 "AstInternal.hs" #-}
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
                  {-# LINE 7514 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Execute ann_ _exprIannotatedTree
                  {-# LINE 7519 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7524 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 7529 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7534 "AstInternal.hs" #-}
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
                  {-# LINE 7559 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIannotatedTree _targetsIannotatedTree
                  {-# LINE 7564 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7569 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _targetsIenv
                  {-# LINE 7574 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7579 "AstInternal.hs" #-}
              -- copy rule (chain)
              _targetsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 7584 "AstInternal.hs" #-}
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
                  {-# LINE 7618 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 369, column 10)
              _stsOenvUpdates =
                  {-# LINE 369 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7623 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ForIntegerStatement ann_ var_ _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 7628 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7633 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 7638 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7643 "AstInternal.hs" #-}
              -- copy rule (chain)
              _toOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fromIenv
                  {-# LINE 7648 "AstInternal.hs" #-}
              -- copy rule (chain)
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _toIenv
                  {-# LINE 7653 "AstInternal.hs" #-}
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
                  {-# LINE 7683 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 369, column 10)
              _stsOenvUpdates =
                  {-# LINE 369 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7688 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ForSelectStatement ann_ var_ _selIannotatedTree _stsIannotatedTree
                  {-# LINE 7693 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7698 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 7703 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7708 "AstInternal.hs" #-}
              -- copy rule (chain)
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 7713 "AstInternal.hs" #-}
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
                  {-# LINE 7740 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 366, column 24)
              _elsOenvUpdates =
                  {-# LINE 366 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7745 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  If ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 7750 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7755 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _elsIenv
                  {-# LINE 7760 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7765 "AstInternal.hs" #-}
              -- copy rule (chain)
              _elsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _casesIenv
                  {-# LINE 7770 "AstInternal.hs" #-}
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
                  {-# LINE 7802 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7807 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 718, column 9)
              _columnStuff =
                  {-# LINE 718 "./TypeChecking.ag" #-}
                  checkColumnConsistency _lhsIenv
                                         table_
                                         _targetColsIstrings
                                         (getCAtts $ getTypeAnnotation _insDataIannotatedTree)
                  {-# LINE 7815 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 723, column 9)
              _tpe =
                  {-# LINE 723 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _insDataIannotatedTree] $ do
                    _columnStuff
                    Right $ Pseudo Void
                  {-# LINE 7822 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 727, column 9)
              _statementInfo =
                  {-# LINE 727 "./TypeChecking.ag" #-}
                  [InsertInfo table_ $ errorToTypeFailF UnnamedCompositeType _columnStuff    ]
                  {-# LINE 7827 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 729, column 9)
              _backTree =
                  {-# LINE 729 "./TypeChecking.ag" #-}
                  Insert ann_ table_ _targetColsIannotatedTree
                         _insDataIannotatedTree returning_
                  {-# LINE 7833 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 731, column 9)
              _envUpdates =
                  {-# LINE 731 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7838 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Insert ann_ table_ _targetColsIannotatedTree _insDataIannotatedTree returning_
                  {-# LINE 7843 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _insDataIenv
                  {-# LINE 7848 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetColsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7853 "AstInternal.hs" #-}
              -- copy rule (chain)
              _insDataOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _targetColsIenv
                  {-# LINE 7858 "AstInternal.hs" #-}
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
                  {-# LINE 7875 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 7880 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7885 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7890 "AstInternal.hs" #-}
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
                  {-# LINE 7908 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Perform ann_ _exprIannotatedTree
                  {-# LINE 7913 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7918 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 7923 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7928 "AstInternal.hs" #-}
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
                  {-# LINE 7953 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Raise ann_ _levelIannotatedTree message_ _argsIannotatedTree
                  {-# LINE 7958 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7963 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _argsIenv
                  {-# LINE 7968 "AstInternal.hs" #-}
              -- copy rule (down)
              _levelOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7973 "AstInternal.hs" #-}
              -- copy rule (chain)
              _argsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _levelIenv
                  {-# LINE 7978 "AstInternal.hs" #-}
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
                  {-# LINE 8004 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 8009 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1049, column 9)
              _tpe =
                  {-# LINE 1049 "./TypeChecking.ag" #-}
                  checkTypes [fromMaybe typeBool _valueIexprType] $ Right $ Pseudo Void
                  {-# LINE 8014 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1050, column 9)
              _backTree =
                  {-# LINE 1050 "./TypeChecking.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 8019 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1051, column 9)
              _envUpdates =
                  {-# LINE 1051 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8024 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 1052, column 9)
              _statementInfo =
                  {-# LINE 1052 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8029 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 8034 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _valueIenv
                  {-# LINE 8039 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8044 "AstInternal.hs" #-}
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
                  {-# LINE 8064 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIannotatedTree
                  {-# LINE 8069 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8074 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 8079 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8084 "AstInternal.hs" #-}
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
                  {-# LINE 8103 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIannotatedTree
                  {-# LINE 8108 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8113 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 8118 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8123 "AstInternal.hs" #-}
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
                  {-# LINE 8146 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 8151 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 383, column 9)
              _tpe =
                  {-# LINE 383 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _exIannotatedTree] $ Right $ Pseudo Void
                  {-# LINE 8156 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 384, column 9)
              _statementInfo =
                  {-# LINE 384 "./TypeChecking.ag" #-}
                  [SelectInfo $ getTypeAnnotation _exIannotatedTree]
                  {-# LINE 8161 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 385, column 9)
              _backTree =
                  {-# LINE 385 "./TypeChecking.ag" #-}
                  SelectStatement ann_ _exIannotatedTree
                  {-# LINE 8166 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 386, column 9)
              _envUpdates =
                  {-# LINE 386 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8171 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SelectStatement ann_ _exIannotatedTree
                  {-# LINE 8176 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exIenv
                  {-# LINE 8181 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8186 "AstInternal.hs" #-}
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
                  {-# LINE 8214 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Truncate ann_ _tablesIannotatedTree _restartIdentityIannotatedTree _cascadeIannotatedTree
                  {-# LINE 8219 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8224 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _cascadeIenv
                  {-# LINE 8229 "AstInternal.hs" #-}
              -- copy rule (down)
              _tablesOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8234 "AstInternal.hs" #-}
              -- copy rule (chain)
              _restartIdentityOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tablesIenv
                  {-# LINE 8239 "AstInternal.hs" #-}
              -- copy rule (chain)
              _cascadeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _restartIdentityIenv
                  {-# LINE 8244 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 321, column 9)
              _lhsOannotatedTree =
                  {-# LINE 321 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 8279 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 326, column 9)
              _lhsOenvUpdates =
                  {-# LINE 326 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 8284 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 756, column 9)
              _tpe =
                  {-# LINE 756 "./TypeChecking.ag" #-}
                  do
                  let re = checkRelationExists _lhsIenv table_
                  when (isJust re) $
                       Left [fromJust $ re]
                  whereType <- checkExpressionBool _whrIannotatedTree
                  chainTypeCheckFailed (whereType:map snd _assignsIpairs) $ do
                    _columnsConsistent
                    checkErrorList _assignsIrowSetErrors $ Pseudo Void
                  {-# LINE 8296 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 765, column 9)
              _columnsConsistent =
                  {-# LINE 765 "./TypeChecking.ag" #-}
                  checkColumnConsistency _lhsIenv table_ (map fst _assignsIpairs) _assignsIpairs
                  {-# LINE 8301 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 767, column 9)
              _statementInfo =
                  {-# LINE 767 "./TypeChecking.ag" #-}
                  [UpdateInfo table_ $ flip errorToTypeFailF _columnsConsistent     $
                                           \c -> let colNames = map fst _assignsIpairs
                                                 in UnnamedCompositeType $ map (\t -> (t,getType c t)) colNames]
                  where
                    getType cols t = fromJust $ lookup t cols
                  {-# LINE 8310 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 774, column 9)
              _backTree =
                  {-# LINE 774 "./TypeChecking.ag" #-}
                  Update ann_ table_ _assignsIannotatedTree _whrIannotatedTree returning_
                  {-# LINE 8315 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 775, column 9)
              _envUpdates =
                  {-# LINE 775 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8320 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Update ann_ table_ _assignsIannotatedTree _whrIannotatedTree returning_
                  {-# LINE 8325 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _whrIenv
                  {-# LINE 8330 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8335 "AstInternal.hs" #-}
              -- copy rule (chain)
              _whrOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _assignsIenv
                  {-# LINE 8340 "AstInternal.hs" #-}
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
                  {-# LINE 8368 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 369, column 10)
              _stsOenvUpdates =
                  {-# LINE 369 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8373 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  WhileStatement ann_ _exprIannotatedTree _stsIannotatedTree
                  {-# LINE 8378 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8383 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _stsIenv
                  {-# LINE 8388 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8393 "AstInternal.hs" #-}
              -- copy rule (chain)
              _stsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _exprIenv
                  {-# LINE 8398 "AstInternal.hs" #-}
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
                  {-# LINE 8464 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 354, column 9)
              _hdOenv =
                  {-# LINE 354 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 8469 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 355, column 9)
              _tlOenv =
                  {-# LINE 355 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 8474 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 356, column 9)
              _tlOenvUpdates =
                  {-# LINE 356 "./TypeChecking.ag" #-}
                  _hdIenvUpdates
                  {-# LINE 8479 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8484 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8489 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 8494 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenvUpdates =
                  {-# LINE 344 "./TypeChecking.ag" #-}
                  _tlIenvUpdates
                  {-# LINE 8499 "AstInternal.hs" #-}
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
                  {-# LINE 8516 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8521 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8526 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenvUpdates =
                  {-# LINE 344 "./TypeChecking.ag" #-}
                  _lhsIenvUpdates
                  {-# LINE 8531 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 740, column 10)
              _lhsOstrings =
                  {-# LINE 740 "./TypeChecking.ag" #-}
                  hd_ : _tlIstrings
                  {-# LINE 8585 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) hd_ _tlIannotatedTree
                  {-# LINE 8590 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8595 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 8600 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8605 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIenv,_tlIstrings) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOenv,_lhsOstrings)))
sem_StringList_Nil :: T_StringList 
sem_StringList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOstrings :: ([String])
              _lhsOannotatedTree :: StringList
              _lhsOenv :: Environment
              -- "./TypeChecking.ag"(line 741, column 9)
              _lhsOstrings =
                  {-# LINE 741 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8619 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8624 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8629 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8634 "AstInternal.hs" #-}
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
                  {-# LINE 8683 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8688 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _x2Ienv
                  {-# LINE 8693 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8698 "AstInternal.hs" #-}
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
                  {-# LINE 8754 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8759 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 8764 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8769 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 8774 "AstInternal.hs" #-}
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
                  {-# LINE 8789 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8794 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8799 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 456, column 9)
              _lhsOannotatedTree =
                  {-# LINE 456 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 8940 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 498, column 9)
              _tpe =
                  {-# LINE 498 "./TypeChecking.ag" #-}
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
                  {-# LINE 8956 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 511, column 9)
              _lhsOidens =
                  {-# LINE 511 "./TypeChecking.ag" #-}
                  _tblIidens ++ _tbl1Iidens
                  {-# LINE 8961 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 512, column 9)
              _lhsOjoinIdens =
                  {-# LINE 512 "./TypeChecking.ag" #-}
                  commonFieldNames (getTypeAnnotation _tblIannotatedTree)
                                   (getTypeAnnotation _tbl1IannotatedTree)
                  {-# LINE 8967 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 514, column 9)
              _backTree =
                  {-# LINE 514 "./TypeChecking.ag" #-}
                  JoinedTref ann_
                             _tblIannotatedTree
                             _natIannotatedTree
                             _joinTypeIannotatedTree
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
                  {-# LINE 8977 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  JoinedTref ann_ _tblIannotatedTree _natIannotatedTree _joinTypeIannotatedTree _tbl1IannotatedTree _onExprIannotatedTree
                  {-# LINE 8982 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _onExprIenv
                  {-# LINE 8987 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8992 "AstInternal.hs" #-}
              -- copy rule (chain)
              _natOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tblIenv
                  {-# LINE 8997 "AstInternal.hs" #-}
              -- copy rule (chain)
              _joinTypeOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _natIenv
                  {-# LINE 9002 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tbl1Oenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _joinTypeIenv
                  {-# LINE 9007 "AstInternal.hs" #-}
              -- copy rule (chain)
              _onExprOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tbl1Ienv
                  {-# LINE 9012 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 456, column 9)
              _lhsOannotatedTree =
                  {-# LINE 456 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9044 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 462, column 15)
              _tpe =
                  {-# LINE 462 "./TypeChecking.ag" #-}
                  checkTypes [getTypeAnnotation _selIannotatedTree] <$>
                  unwrapSetOfWhenComposite $ getTypeAnnotation _selIannotatedTree
                  {-# LINE 9050 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 464, column 15)
              _backTree =
                  {-# LINE 464 "./TypeChecking.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 9055 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 465, column 15)
              _lhsOidens =
                  {-# LINE 465 "./TypeChecking.ag" #-}
                  [(alias_, (fromRight [] $ getTbCols _selIannotatedTree, []))]
                  {-# LINE 9060 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 466, column 15)
              _lhsOjoinIdens =
                  {-# LINE 466 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9065 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 9070 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _selIenv
                  {-# LINE 9075 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9080 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 456, column 9)
              _lhsOannotatedTree =
                  {-# LINE 456 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9100 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 468, column 9)
              _tpe =
                  {-# LINE 468 "./TypeChecking.ag" #-}
                  either Left (Right . fst) _relType
                  {-# LINE 9105 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 469, column 9)
              _lhsOjoinIdens =
                  {-# LINE 469 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9110 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 470, column 9)
              _relType =
                  {-# LINE 470 "./TypeChecking.ag" #-}
                  getRelationType _lhsIenv tbl_
                  {-# LINE 9115 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 471, column 9)
              _unwrappedRelType =
                  {-# LINE 471 "./TypeChecking.ag" #-}
                  fromRight ([],[]) $
                  do
                    lrt <- _relType
                    let (UnnamedCompositeType a,UnnamedCompositeType b) = lrt
                    return (a,b)
                  {-# LINE 9124 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 478, column 9)
              _lhsOidens =
                  {-# LINE 478 "./TypeChecking.ag" #-}
                  [(tbl_, _unwrappedRelType    )]
                  {-# LINE 9129 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 479, column 9)
              _backTree =
                  {-# LINE 479 "./TypeChecking.ag" #-}
                  Tref ann_ tbl_
                  {-# LINE 9134 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Tref ann_ tbl_
                  {-# LINE 9139 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9144 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 456, column 9)
              _lhsOannotatedTree =
                  {-# LINE 456 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9163 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 468, column 9)
              _tpe =
                  {-# LINE 468 "./TypeChecking.ag" #-}
                  either Left (Right . fst) _relType
                  {-# LINE 9168 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 469, column 9)
              _lhsOjoinIdens =
                  {-# LINE 469 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9173 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 470, column 9)
              _relType =
                  {-# LINE 470 "./TypeChecking.ag" #-}
                  getRelationType _lhsIenv tbl_
                  {-# LINE 9178 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 471, column 9)
              _unwrappedRelType =
                  {-# LINE 471 "./TypeChecking.ag" #-}
                  fromRight ([],[]) $
                  do
                    lrt <- _relType
                    let (UnnamedCompositeType a,UnnamedCompositeType b) = lrt
                    return (a,b)
                  {-# LINE 9187 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 481, column 9)
              _lhsOidens =
                  {-# LINE 481 "./TypeChecking.ag" #-}
                  [(alias_, _unwrappedRelType    )]
                  {-# LINE 9192 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 482, column 9)
              _backTree =
                  {-# LINE 482 "./TypeChecking.ag" #-}
                  TrefAlias ann_ tbl_ alias_
                  {-# LINE 9197 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TrefAlias ann_ tbl_ alias_
                  {-# LINE 9202 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9207 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 456, column 9)
              _lhsOannotatedTree =
                  {-# LINE 456 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9229 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 484, column 9)
              _tpe =
                  {-# LINE 484 "./TypeChecking.ag" #-}
                  getFnType _lhsIenv _alias _fnIannotatedTree
                  {-# LINE 9234 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 485, column 9)
              _lhsOjoinIdens =
                  {-# LINE 485 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9239 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 486, column 9)
              _lhsOidens =
                  {-# LINE 486 "./TypeChecking.ag" #-}
                  case getFunIdens
                            _lhsIenv _alias
                            _fnIannotatedTree of
                    Right (s, UnnamedCompositeType c) -> [(s,(c,[]))]
                    _ -> []
                  {-# LINE 9248 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 492, column 9)
              _alias =
                  {-# LINE 492 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 9253 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 493, column 9)
              _backTree =
                  {-# LINE 493 "./TypeChecking.ag" #-}
                  TrefFun ann_ _fnIannotatedTree
                  {-# LINE 9258 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TrefFun ann_ _fnIannotatedTree
                  {-# LINE 9263 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fnIenv
                  {-# LINE 9268 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9273 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 456, column 9)
              _lhsOannotatedTree =
                  {-# LINE 456 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 9298 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 484, column 9)
              _tpe =
                  {-# LINE 484 "./TypeChecking.ag" #-}
                  getFnType _lhsIenv alias_ _fnIannotatedTree
                  {-# LINE 9303 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 485, column 9)
              _lhsOjoinIdens =
                  {-# LINE 485 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9308 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 486, column 9)
              _lhsOidens =
                  {-# LINE 486 "./TypeChecking.ag" #-}
                  case getFunIdens
                            _lhsIenv _alias
                            _fnIannotatedTree of
                    Right (s, UnnamedCompositeType c) -> [(s,(c,[]))]
                    _ -> []
                  {-# LINE 9317 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 495, column 9)
              _alias =
                  {-# LINE 495 "./TypeChecking.ag" #-}
                  alias_
                  {-# LINE 9322 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 496, column 9)
              _backTree =
                  {-# LINE 496 "./TypeChecking.ag" #-}
                  TrefFunAlias ann_ _fnIannotatedTree alias_
                  {-# LINE 9327 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TrefFunAlias ann_ _fnIannotatedTree _alias
                  {-# LINE 9332 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _fnIenv
                  {-# LINE 9337 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9342 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 921, column 9)
              _lhsOattrName =
                  {-# LINE 921 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 9400 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 922, column 9)
              _lhsOnamedType =
                  {-# LINE 922 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 9405 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIannotatedTree
                  {-# LINE 9410 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9415 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 9420 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9425 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 931, column 12)
              _lhsOattrs =
                  {-# LINE 931 "./TypeChecking.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 9486 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 9491 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9496 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 9501 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9506 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 9511 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 932, column 11)
              _lhsOattrs =
                  {-# LINE 932 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9527 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9532 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9537 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9542 "AstInternal.hs" #-}
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
                  {-# LINE 9627 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOannotatedTree =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  setAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 9634 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 146, column 9)
              _tpe =
                  {-# LINE 146 "./TypeChecking.ag" #-}
                  checkTypes [_typInamedType] $ Right $ ArrayType _typInamedType
                  {-# LINE 9639 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 147, column 9)
              _backTree =
                  {-# LINE 147 "./TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 9644 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 9649 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 9654 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9659 "AstInternal.hs" #-}
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
                  {-# LINE 9676 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOannotatedTree =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  setAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 9683 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 152, column 9)
              _tpe =
                  {-# LINE 152 "./TypeChecking.ag" #-}
                  Right TypeCheckFailed
                  {-# LINE 9688 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 153, column 9)
              _backTree =
                  {-# LINE 153 "./TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 9693 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 9698 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9703 "AstInternal.hs" #-}
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
                  {-# LINE 9721 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOannotatedTree =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  setAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 9728 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 149, column 9)
              _tpe =
                  {-# LINE 149 "./TypeChecking.ag" #-}
                  checkTypes [_typInamedType] $ Right $ SetOfType _typInamedType
                  {-# LINE 9733 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 150, column 9)
              _backTree =
                  {-# LINE 150 "./TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 9738 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 9743 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 9748 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9753 "AstInternal.hs" #-}
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
                  {-# LINE 9769 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOannotatedTree =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  setAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 9776 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 143, column 9)
              _tpe =
                  {-# LINE 143 "./TypeChecking.ag" #-}
                  envLookupType _lhsIenv $ canonicalizeTypeName tn_
                  {-# LINE 9781 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 144, column 9)
              _backTree =
                  {-# LINE 144 "./TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 9786 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 9791 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9796 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 1033, column 14)
              _lhsOdef =
                  {-# LINE 1033 "./TypeChecking.ag" #-}
                  (name_, _typInamedType)
                  {-# LINE 9852 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIannotatedTree value_
                  {-# LINE 9857 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9862 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _typIenv
                  {-# LINE 9867 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9872 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 1036, column 12)
              _lhsOdefs =
                  {-# LINE 1036 "./TypeChecking.ag" #-}
                  _hdIdef : _tlIdefs
                  {-# LINE 9932 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 9937 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9942 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _tlIenv
                  {-# LINE 9947 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9952 "AstInternal.hs" #-}
              -- copy rule (chain)
              _tlOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _hdIenv
                  {-# LINE 9957 "AstInternal.hs" #-}
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
              -- "./TypeChecking.ag"(line 1037, column 11)
              _lhsOdefs =
                  {-# LINE 1037 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9973 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 9978 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9983 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9988 "AstInternal.hs" #-}
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
                  {-# LINE 10042 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10047 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10052 "AstInternal.hs" #-}
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
                  {-# LINE 10063 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10068 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10073 "AstInternal.hs" #-}
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
                  {-# LINE 10084 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10089 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10094 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))
-- Where -------------------------------------------------------
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
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 10146 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10151 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _justIenv
                  {-# LINE 10156 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10161 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIenv,_justIliftedColumnName) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOenv)))
sem_Where_Nothing :: T_Where 
sem_Where_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Where
              _lhsOenv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 10174 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 53 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10179 "AstInternal.hs" #-}
              -- copy rule (chain)
              _lhsOenv =
                  {-# LINE 52 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10184 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenv)))