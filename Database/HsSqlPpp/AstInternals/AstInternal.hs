

-- UUAGC 0.9.10 (AstInternal.ag)
module Database.HsSqlPpp.AstInternals.AstInternal(
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
   ,annotateAstsEnv
   ,annotateAstEnvEnv
) where

import Data.Maybe
import Data.List
import Debug.Trace
import Control.Monad.Error
import Control.Arrow
import Data.Either
import Control.Applicative
import Data.Generics

import Database.HsSqlPpp.AstInternals.TypeType
import Database.HsSqlPpp.AstInternals.TypeConversion
import Database.HsSqlPpp.AstInternals.TypeCheckingH
import Database.HsSqlPpp.AstInternals.AstAnnotation
import Database.HsSqlPpp.AstInternals.EnvironmentInternal
import Database.HsSqlPpp.AstInternals.DefaultTemplate1Environment
import Database.HsSqlPpp.Utils

{-# LINE 603 "AstInternal.ag" #-}

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

-- | Type check multiple asts, allowing type checking references in later
--   files to definitions in earlier files.
annotateAstsEnv :: Environment -> [StatementList] -> [StatementList]
annotateAstsEnv env sts =
    annInt env sts []
    where
      annInt e (s:ss) ress =
          let (e1,res) = annotateAstEnvEnv e s
          in annInt e1 ss (res:ress)
      annInt _ [] ress = reverse ress

-- | Type check an ast, and return the resultant Environment as well
--   as the annotated ast.
annotateAstEnvEnv :: Environment -> StatementList -> (Environment,StatementList)
annotateAstEnvEnv env sts =
    let t = sem_Root (Root sts)
        ta = wrap_Root t Inh_Root {env_Inh_Root = env}
        tl = annotatedTree_Syn_Root ta
        env1 = producedEnv_Syn_Root ta
   in case tl of
         Root r -> (env1,r)

-- | Testing utility, mainly used to check an expression for type errors
-- or to get its type.
annotateExpression :: Environment -> Expression -> Expression
annotateExpression env ex =
    let t = sem_ExpressionRoot (ExpressionRoot ex)
        rt = (annotatedTree_Syn_ExpressionRoot
              (wrap_ExpressionRoot t Inh_ExpressionRoot {env_Inh_ExpressionRoot = env}))
    in case rt of
         ExpressionRoot e -> e

{-# LINE 141 "AstInternal.hs" #-}

{-# LINE 68 "./TypeChecking.ag" #-}

annTypesAndErrors :: Data a => a -> Type -> [TypeError]
                  -> Maybe [AnnotationElement] -> a
annTypesAndErrors item nt errs add =
    updateAnnotation modifier item
    where
      modifier = (([TypeAnnotation nt] ++ fromMaybe [] add ++
       map TypeErrorA errs) ++)

{-# LINE 153 "AstInternal.hs" #-}

{-# LINE 421 "./TypeChecking.ag" #-}

getTbCols c = unwrapSetOfComposite (getTypeAnnotation c)
{-# LINE 158 "AstInternal.hs" #-}

{-# LINE 496 "./TypeChecking.ag" #-}


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
{-# LINE 186 "AstInternal.hs" #-}

{-# LINE 547 "./TypeChecking.ag" #-}


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
{-# LINE 209 "AstInternal.hs" #-}

{-# LINE 633 "./TypeChecking.ag" #-}

fixedValue :: a -> a -> a -> a
fixedValue a _ _ = a
{-# LINE 215 "AstInternal.hs" #-}

{-# LINE 667 "./TypeChecking.ag" #-}

getCAtts t =
    case t of
      SetOfType (UnnamedCompositeType t) -> t
      _ -> []
{-# LINE 223 "AstInternal.hs" #-}

{-# LINE 746 "./TypeChecking.ag" #-}

getRowTypes :: [Type] -> [Type]
getRowTypes [RowCtor ts] = ts
getRowTypes ts = ts
{-# LINE 230 "AstInternal.hs" #-}
-- AttributeDef ------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                       ( AttributeDef,String,Type)
data Inh_AttributeDef  = Inh_AttributeDef {env_Inh_AttributeDef :: Environment}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef,attrName_Syn_AttributeDef :: String,namedType_Syn_AttributeDef :: Type}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_AttributeDef _lhsOannotatedTree _lhsOattrName _lhsOnamedType ))
sem_AttributeDef_AttributeDef :: Annotation ->
                                 String ->
                                 T_TypeName  ->
                                 T_MaybeExpression  ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef ann_ name_ typ_ def_ cons_  =
    (\ _lhsIenv ->
         (let _consOenv :: Environment
              _defOenv :: Environment
              _typOenv :: Environment
              _consIannotatedTree :: RowConstraintList
              _defIannotatedTree :: MaybeExpression
              _defIexprType :: (Maybe Type)
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: AttributeDef
              _lhsOattrName :: String
              _lhsOnamedType :: Type
              -- copy rule (down)
              _consOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 292 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 297 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 302 "AstInternal.hs" #-}
              ( _consIannotatedTree) =
                  (cons_ _consOenv )
              ( _defIannotatedTree,_defIexprType) =
                  (def_ _defOenv )
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
                  {-# LINE 313 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 318 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 784, column 7)
              _lhsOattrName =
                  {-# LINE 784 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 323 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 785, column 7)
              _lhsOnamedType =
                  {-# LINE 785 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 328 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType)))
-- AttributeDefList --------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                           ( AttributeDefList,([(String, Type)]))
data Inh_AttributeDefList  = Inh_AttributeDefList {env_Inh_AttributeDefList :: Environment}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList,attrs_Syn_AttributeDefList :: [(String, Type)]}
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrs) =
             (sem _lhsIenv )
     in  (Syn_AttributeDefList _lhsOannotatedTree _lhsOattrs ))
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: AttributeDefList
              _tlIattrs :: ([(String, Type)])
              _hdIannotatedTree :: AttributeDef
              _hdIattrName :: String
              _hdInamedType :: Type
              _lhsOannotatedTree :: AttributeDefList
              _lhsOattrs :: ([(String, Type)])
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 384 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 389 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIattrs) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree,_hdIattrName,_hdInamedType) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 398 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 403 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 790, column 12)
              _lhsOattrs =
                  {-# LINE 790 "./TypeChecking.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 408 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: AttributeDefList
              _lhsOattrs :: ([(String, Type)])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 419 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 424 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 791, column 11)
              _lhsOattrs =
                  {-# LINE 791 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 429 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs)))
-- Cascade -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                  ( Cascade)
data Inh_Cascade  = Inh_Cascade {env_Inh_Cascade :: Environment}
data Syn_Cascade  = Syn_Cascade {annotatedTree_Syn_Cascade :: Cascade}
wrap_Cascade :: T_Cascade  ->
                Inh_Cascade  ->
                Syn_Cascade 
wrap_Cascade sem (Inh_Cascade _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_Cascade _lhsOannotatedTree ))
sem_Cascade_Cascade :: T_Cascade 
sem_Cascade_Cascade  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Cascade
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Cascade
                  {-# LINE 476 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 481 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Cascade_Restrict :: T_Cascade 
sem_Cascade_Restrict  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Cascade
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Restrict
                  {-# LINE 491 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 496 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- CaseExpressionList ------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                             ( CaseExpressionList)
data Inh_CaseExpressionList  = Inh_CaseExpressionList {env_Inh_CaseExpressionList :: Environment}
data Syn_CaseExpressionList  = Syn_CaseExpressionList {annotatedTree_Syn_CaseExpressionList :: CaseExpressionList}
wrap_CaseExpressionList :: T_CaseExpressionList  ->
                           Inh_CaseExpressionList  ->
                           Syn_CaseExpressionList 
wrap_CaseExpressionList sem (Inh_CaseExpressionList _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_CaseExpressionList _lhsOannotatedTree ))
sem_CaseExpressionList_Cons :: T_Expression  ->
                               T_CaseExpressionList  ->
                               T_CaseExpressionList 
sem_CaseExpressionList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: CaseExpressionList
              _hdIannotatedTree :: Expression
              _hdIliftedColumnName :: String
              _lhsOannotatedTree :: CaseExpressionList
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 548 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 553 "AstInternal.hs" #-}
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree,_hdIliftedColumnName) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 562 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 567 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CaseExpressionList_Nil :: T_CaseExpressionList 
sem_CaseExpressionList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CaseExpressionList
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 577 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 582 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- CaseExpressionListExpressionPair ----------------------------
{-
   visit 0:
      inherited attribute:
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
                                           ( CaseExpressionListExpressionPair)
data Inh_CaseExpressionListExpressionPair  = Inh_CaseExpressionListExpressionPair {env_Inh_CaseExpressionListExpressionPair :: Environment}
data Syn_CaseExpressionListExpressionPair  = Syn_CaseExpressionListExpressionPair {annotatedTree_Syn_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair}
wrap_CaseExpressionListExpressionPair :: T_CaseExpressionListExpressionPair  ->
                                         Inh_CaseExpressionListExpressionPair  ->
                                         Syn_CaseExpressionListExpressionPair 
wrap_CaseExpressionListExpressionPair sem (Inh_CaseExpressionListExpressionPair _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_CaseExpressionListExpressionPair _lhsOannotatedTree ))
sem_CaseExpressionListExpressionPair_Tuple :: T_CaseExpressionList  ->
                                              T_Expression  ->
                                              T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair_Tuple x1_ x2_  =
    (\ _lhsIenv ->
         (let _x2Oenv :: Environment
              _x1Oenv :: Environment
              _x2IannotatedTree :: Expression
              _x2IliftedColumnName :: String
              _x1IannotatedTree :: CaseExpressionList
              _lhsOannotatedTree :: CaseExpressionListExpressionPair
              -- copy rule (down)
              _x2Oenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 631 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 636 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2IliftedColumnName) =
                  (x2_ _x2Oenv )
              ( _x1IannotatedTree) =
                  (x1_ _x1Oenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 645 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 650 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- CaseExpressionListExpressionPairList ------------------------
{-
   visit 0:
      inherited attribute:
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
                                               ( CaseExpressionListExpressionPairList)
data Inh_CaseExpressionListExpressionPairList  = Inh_CaseExpressionListExpressionPairList {env_Inh_CaseExpressionListExpressionPairList :: Environment}
data Syn_CaseExpressionListExpressionPairList  = Syn_CaseExpressionListExpressionPairList {annotatedTree_Syn_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList}
wrap_CaseExpressionListExpressionPairList :: T_CaseExpressionListExpressionPairList  ->
                                             Inh_CaseExpressionListExpressionPairList  ->
                                             Syn_CaseExpressionListExpressionPairList 
wrap_CaseExpressionListExpressionPairList sem (Inh_CaseExpressionListExpressionPairList _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_CaseExpressionListExpressionPairList _lhsOannotatedTree ))
sem_CaseExpressionListExpressionPairList_Cons :: T_CaseExpressionListExpressionPair  ->
                                                 T_CaseExpressionListExpressionPairList  ->
                                                 T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: CaseExpressionListExpressionPairList
              _hdIannotatedTree :: CaseExpressionListExpressionPair
              _lhsOannotatedTree :: CaseExpressionListExpressionPairList
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 701 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 706 "AstInternal.hs" #-}
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 715 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 720 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CaseExpressionListExpressionPairList_Nil :: T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CaseExpressionListExpressionPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 730 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 735 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- CombineType -------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                      ( CombineType)
data Inh_CombineType  = Inh_CombineType {env_Inh_CombineType :: Environment}
data Syn_CombineType  = Syn_CombineType {annotatedTree_Syn_CombineType :: CombineType}
wrap_CombineType :: T_CombineType  ->
                    Inh_CombineType  ->
                    Syn_CombineType 
wrap_CombineType sem (Inh_CombineType _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_CombineType _lhsOannotatedTree ))
sem_CombineType_Except :: T_CombineType 
sem_CombineType_Except  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Except
                  {-# LINE 794 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 799 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CombineType_Intersect :: T_CombineType 
sem_CombineType_Intersect  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Intersect
                  {-# LINE 809 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 814 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CombineType_Union :: T_CombineType 
sem_CombineType_Union  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Union
                  {-# LINE 824 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 829 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CombineType_UnionAll :: T_CombineType 
sem_CombineType_UnionAll  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  UnionAll
                  {-# LINE 839 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 844 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- Constraint --------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                     ( Constraint)
data Inh_Constraint  = Inh_Constraint {env_Inh_Constraint :: Environment}
data Syn_Constraint  = Syn_Constraint {annotatedTree_Syn_Constraint :: Constraint}
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_Constraint _lhsOannotatedTree ))
sem_Constraint_CheckConstraint :: Annotation ->
                                  T_Expression  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint ann_ expression_  =
    (\ _lhsIenv ->
         (let _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIliftedColumnName :: String
              _lhsOannotatedTree :: Constraint
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 920 "AstInternal.hs" #-}
              ( _expressionIannotatedTree,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  CheckConstraint ann_ _expressionIannotatedTree
                  {-# LINE 927 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 932 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Constraint_PrimaryKeyConstraint :: Annotation ->
                                       T_StringList  ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint ann_ stringList_  =
    (\ _lhsIenv ->
         (let _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _lhsOannotatedTree :: Constraint
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 947 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_ _stringListOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  PrimaryKeyConstraint ann_ _stringListIannotatedTree
                  {-# LINE 954 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 959 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Constraint_ReferenceConstraint :: Annotation ->
                                      T_StringList  ->
                                      String ->
                                      T_StringList  ->
                                      T_Cascade  ->
                                      T_Cascade  ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint ann_ atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIenv ->
         (let _onDeleteOenv :: Environment
              _onDeleteIannotatedTree :: Cascade
              _onUpdateOenv :: Environment
              _onUpdateIannotatedTree :: Cascade
              _tableAttsOenv :: Environment
              _tableAttsIannotatedTree :: StringList
              _tableAttsIstrings :: ([String])
              _attsOenv :: Environment
              _attsIannotatedTree :: StringList
              _attsIstrings :: ([String])
              _lhsOannotatedTree :: Constraint
              -- copy rule (down)
              _onDeleteOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 985 "AstInternal.hs" #-}
              ( _onDeleteIannotatedTree) =
                  (onDelete_ _onDeleteOenv )
              -- copy rule (down)
              _onUpdateOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 992 "AstInternal.hs" #-}
              ( _onUpdateIannotatedTree) =
                  (onUpdate_ _onUpdateOenv )
              -- copy rule (down)
              _tableAttsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 999 "AstInternal.hs" #-}
              ( _tableAttsIannotatedTree,_tableAttsIstrings) =
                  (tableAtts_ _tableAttsOenv )
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1006 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIstrings) =
                  (atts_ _attsOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ReferenceConstraint ann_ _attsIannotatedTree table_ _tableAttsIannotatedTree _onUpdateIannotatedTree _onDeleteIannotatedTree
                  {-# LINE 1013 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1018 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Constraint_UniqueConstraint :: Annotation ->
                                   T_StringList  ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint ann_ stringList_  =
    (\ _lhsIenv ->
         (let _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _lhsOannotatedTree :: Constraint
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1033 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_ _stringListOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  UniqueConstraint ann_ _stringListIannotatedTree
                  {-# LINE 1040 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1045 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- ConstraintList ----------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                         ( ConstraintList)
data Inh_ConstraintList  = Inh_ConstraintList {env_Inh_ConstraintList :: Environment}
data Syn_ConstraintList  = Syn_ConstraintList {annotatedTree_Syn_ConstraintList :: ConstraintList}
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_ConstraintList _lhsOannotatedTree ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: ConstraintList
              _hdIannotatedTree :: Constraint
              _lhsOannotatedTree :: ConstraintList
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1096 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1101 "AstInternal.hs" #-}
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1110 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1115 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 1125 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1130 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- CopySource --------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                     ( CopySource)
data Inh_CopySource  = Inh_CopySource {env_Inh_CopySource :: Environment}
data Syn_CopySource  = Syn_CopySource {annotatedTree_Syn_CopySource :: CopySource}
wrap_CopySource :: T_CopySource  ->
                   Inh_CopySource  ->
                   Syn_CopySource 
wrap_CopySource sem (Inh_CopySource _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_CopySource _lhsOannotatedTree ))
sem_CopySource_CopyFilename :: String ->
                               T_CopySource 
sem_CopySource_CopyFilename string_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CopySource
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  CopyFilename string_
                  {-# LINE 1179 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1184 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CopySource_Stdin :: T_CopySource 
sem_CopySource_Stdin  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CopySource
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Stdin
                  {-# LINE 1194 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1199 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- Direction ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                    ( Direction)
data Inh_Direction  = Inh_Direction {env_Inh_Direction :: Environment}
data Syn_Direction  = Syn_Direction {annotatedTree_Syn_Direction :: Direction}
wrap_Direction :: T_Direction  ->
                  Inh_Direction  ->
                  Syn_Direction 
wrap_Direction sem (Inh_Direction _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_Direction _lhsOannotatedTree ))
sem_Direction_Asc :: T_Direction 
sem_Direction_Asc  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Direction
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Asc
                  {-# LINE 1246 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1251 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Direction_Desc :: T_Direction 
sem_Direction_Desc  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Direction
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Desc
                  {-# LINE 1261 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1266 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- Distinct ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                   ( Distinct)
data Inh_Distinct  = Inh_Distinct {env_Inh_Distinct :: Environment}
data Syn_Distinct  = Syn_Distinct {annotatedTree_Syn_Distinct :: Distinct}
wrap_Distinct :: T_Distinct  ->
                 Inh_Distinct  ->
                 Syn_Distinct 
wrap_Distinct sem (Inh_Distinct _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_Distinct _lhsOannotatedTree ))
sem_Distinct_Distinct :: T_Distinct 
sem_Distinct_Distinct  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Distinct
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Distinct
                  {-# LINE 1313 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1318 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Distinct_Dupes :: T_Distinct 
sem_Distinct_Dupes  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Distinct
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Dupes
                  {-# LINE 1328 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1333 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- DropType ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                   ( DropType)
data Inh_DropType  = Inh_DropType {env_Inh_DropType :: Environment}
data Syn_DropType  = Syn_DropType {annotatedTree_Syn_DropType :: DropType}
wrap_DropType :: T_DropType  ->
                 Inh_DropType  ->
                 Syn_DropType 
wrap_DropType sem (Inh_DropType _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_DropType _lhsOannotatedTree ))
sem_DropType_Domain :: T_DropType 
sem_DropType_Domain  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Domain
                  {-# LINE 1392 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1397 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_DropType_Table :: T_DropType 
sem_DropType_Table  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Table
                  {-# LINE 1407 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1412 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_DropType_Type :: T_DropType 
sem_DropType_Type  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Type
                  {-# LINE 1422 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1427 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_DropType_View :: T_DropType 
sem_DropType_View  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  View
                  {-# LINE 1437 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1442 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         liftedColumnName     : String
   alternatives:
      alternative BooleanLit:
         child ann            : {Annotation}
         child b              : {Bool}
         visit 0:
            local tpe         : _
            local backTree    : _
      alternative Case:
         child ann            : {Annotation}
         child cases          : CaseExpressionListExpressionPairList 
         child els            : MaybeExpression 
         visit 0:
            local backTree    : _
            local thenTypes   : _
            local whenTypes   : _
            local tpe         : _
      alternative CaseSimple:
         child ann            : {Annotation}
         child value          : Expression 
         child cases          : CaseExpressionListExpressionPairList 
         child els            : MaybeExpression 
         visit 0:
            local backTree    : _
            local thenTypes   : _
            local whenTypes   : _
            local tpe         : _
      alternative Cast:
         child ann            : {Annotation}
         child expr           : Expression 
         child tn             : TypeName 
         visit 0:
            local backTree    : _
            local tpe         : _
      alternative Exists:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local backTree    : _
            local tpe         : _
      alternative FloatLit:
         child ann            : {Annotation}
         child d              : {Double}
         visit 0:
            local tpe         : _
            local backTree    : _
      alternative FunCall:
         child ann            : {Annotation}
         child funName        : {String}
         child args           : ExpressionList 
         visit 0:
            local backTree    : _
            local tpe         : _
      alternative Identifier:
         child ann            : {Annotation}
         child i              : {String}
         visit 0:
            local backTree    : _
            local tpe         : _
      alternative InPredicate:
         child ann            : {Annotation}
         child expr           : Expression 
         child i              : {Bool}
         child list           : InList 
         visit 0:
            local backTree    : _
            local tpe         : _
      alternative IntegerLit:
         child ann            : {Annotation}
         child i              : {Integer}
         visit 0:
            local tpe         : _
            local backTree    : _
      alternative NullLit:
         child ann            : {Annotation}
         visit 0:
            local tpe         : _
            local backTree    : _
      alternative PositionalArg:
         child ann            : {Annotation}
         child p              : {Integer}
         visit 0:
            local annotatedTree : _
      alternative ScalarSubQuery:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local backTree    : _
            local tpe         : _
      alternative StringLit:
         child ann            : {Annotation}
         child quote          : {String}
         child value          : {String}
         visit 0:
            local tpe         : _
            local backTree    : _
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
                     ( Expression,String)
data Inh_Expression  = Inh_Expression {env_Inh_Expression :: Environment}
data Syn_Expression  = Syn_Expression {annotatedTree_Syn_Expression :: Expression,liftedColumnName_Syn_Expression :: String}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression sem (Inh_Expression _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOliftedColumnName) =
             (sem _lhsIenv )
     in  (Syn_Expression _lhsOannotatedTree _lhsOliftedColumnName ))
sem_Expression_BooleanLit :: Annotation ->
                             Bool ->
                             T_Expression 
sem_Expression_BooleanLit ann_ b_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking.ag"(line 108, column 19)
              _tpe =
                  {-# LINE 108 "./TypeChecking.ag" #-}
                  Right typeBool
                  {-# LINE 1628 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 100, column 9)
              _backTree =
                  {-# LINE 100 "./TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 1633 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1641 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1646 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Case :: Annotation ->
                       T_CaseExpressionListExpressionPairList  ->
                       T_MaybeExpression  ->
                       T_Expression 
sem_Expression_Case ann_ cases_ els_  =
    (\ _lhsIenv ->
         (let _elsOenv :: Environment
              _casesOenv :: Environment
              _elsIannotatedTree :: MaybeExpression
              _elsIexprType :: (Maybe Type)
              _casesIannotatedTree :: CaseExpressionListExpressionPairList
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _elsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1665 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1670 "AstInternal.hs" #-}
              ( _elsIannotatedTree,_elsIexprType) =
                  (els_ _elsOenv )
              ( _casesIannotatedTree) =
                  (cases_ _casesOenv )
              -- "./TypeChecking.ag"(line 198, column 9)
              _backTree =
                  {-# LINE 198 "./TypeChecking.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1679 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 184, column 9)
              _thenTypes =
                  {-# LINE 184 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
                  {-# LINE 1686 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 182, column 9)
              _whenTypes =
                  {-# LINE 182 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
                  {-# LINE 1692 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 190, column 9)
              _tpe =
                  {-# LINE 190 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed _whenTypes     $ do
                     when (any (/= typeBool) _whenTypes    ) $
                       Left [WrongTypes typeBool _whenTypes    ]
                     chainTypeCheckFailed _thenTypes     $
                              resolveResultSetType
                                _lhsIenv
                                _thenTypes
                  {-# LINE 1703 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1711 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1716 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_CaseSimple :: Annotation ->
                             T_Expression  ->
                             T_CaseExpressionListExpressionPairList  ->
                             T_MaybeExpression  ->
                             T_Expression 
sem_Expression_CaseSimple ann_ value_ cases_ els_  =
    (\ _lhsIenv ->
         (let _elsOenv :: Environment
              _casesOenv :: Environment
              _valueOenv :: Environment
              _elsIannotatedTree :: MaybeExpression
              _elsIexprType :: (Maybe Type)
              _casesIannotatedTree :: CaseExpressionListExpressionPairList
              _valueIannotatedTree :: Expression
              _valueIliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _elsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1739 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1744 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1749 "AstInternal.hs" #-}
              ( _elsIannotatedTree,_elsIexprType) =
                  (els_ _elsOenv )
              ( _casesIannotatedTree) =
                  (cases_ _casesOenv )
              ( _valueIannotatedTree,_valueIliftedColumnName) =
                  (value_ _valueOenv )
              -- "./TypeChecking.ag"(line 212, column 9)
              _backTree =
                  {-# LINE 212 "./TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 1760 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 184, column 9)
              _thenTypes =
                  {-# LINE 184 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
                  {-# LINE 1767 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 182, column 9)
              _whenTypes =
                  {-# LINE 182 "./TypeChecking.ag" #-}
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
                  {-# LINE 1773 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 203, column 9)
              _tpe =
                  {-# LINE 203 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed _whenTypes     $ do
                  checkWhenTypes <- resolveResultSetType
                                         _lhsIenv
                                         (getTypeAnnotation _valueIannotatedTree: _whenTypes    )
                  chainTypeCheckFailed _thenTypes     $
                             resolveResultSetType
                                      _lhsIenv
                                      _thenTypes
                  {-# LINE 1785 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1793 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  _valueIliftedColumnName
                  {-# LINE 1798 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Cast :: Annotation ->
                       T_Expression  ->
                       T_TypeName  ->
                       T_Expression 
sem_Expression_Cast ann_ expr_ tn_  =
    (\ _lhsIenv ->
         (let _tnOenv :: Environment
              _exprOenv :: Environment
              _tnIannotatedTree :: TypeName
              _tnInamedType :: Type
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _tnOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1818 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1823 "AstInternal.hs" #-}
              ( _tnIannotatedTree,_tnInamedType) =
                  (tn_ _tnOenv )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              -- "./TypeChecking.ag"(line 121, column 12)
              _backTree =
                  {-# LINE 121 "./TypeChecking.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 1832 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 120, column 12)
              _tpe =
                  {-# LINE 120 "./TypeChecking.ag" #-}
                  Right $ _tnInamedType
                  {-# LINE 1837 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1845 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 649, column 10)
              _lhsOliftedColumnName =
                  {-# LINE 649 "./TypeChecking.ag" #-}
                  case _tnIannotatedTree of
                    SimpleTypeName _ tn -> tn
                    _ -> ""
                  {-# LINE 1852 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Exists :: Annotation ->
                         T_SelectExpression  ->
                         T_Expression 
sem_Expression_Exists ann_ sel_  =
    (\ _lhsIenv ->
         (let _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1867 "AstInternal.hs" #-}
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
              -- "./TypeChecking.ag"(line 229, column 9)
              _backTree =
                  {-# LINE 229 "./TypeChecking.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 1874 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 228, column 9)
              _tpe =
                  {-# LINE 228 "./TypeChecking.ag" #-}
                  Right typeBool
                  {-# LINE 1879 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1887 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1892 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_FloatLit :: Annotation ->
                           Double ->
                           T_Expression 
sem_Expression_FloatLit ann_ d_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking.ag"(line 107, column 17)
              _tpe =
                  {-# LINE 107 "./TypeChecking.ag" #-}
                  Right typeNumeric
                  {-# LINE 1905 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 98, column 9)
              _backTree =
                  {-# LINE 98 "./TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 1910 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1918 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 1923 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_FunCall :: Annotation ->
                          String ->
                          T_ExpressionList  ->
                          T_Expression 
sem_Expression_FunCall ann_ funName_ args_  =
    (\ _lhsIenv ->
         (let _argsOenv :: Environment
              _argsIannotatedTree :: ExpressionList
              _argsItypeList :: ([Type])
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _argsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1940 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsItypeList) =
                  (args_ _argsOenv )
              -- "./TypeChecking.ag"(line 166, column 9)
              _backTree =
                  {-# LINE 166 "./TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 1947 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 161, column 9)
              _tpe =
                  {-# LINE 161 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed _argsItypeList $
                    typeCheckFunCall
                      _lhsIenv
                      funName_
                      _argsItypeList
                  {-# LINE 1956 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1964 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 645, column 13)
              _lhsOliftedColumnName =
                  {-# LINE 645 "./TypeChecking.ag" #-}
                  if isOperatorName funName_
                     then ""
                     else funName_
                  {-# LINE 1971 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Identifier :: Annotation ->
                             String ->
                             T_Expression 
sem_Expression_Identifier ann_ i_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking.ag"(line 224, column 9)
              _backTree =
                  {-# LINE 224 "./TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 1984 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 222, column 9)
              _tpe =
                  {-# LINE 222 "./TypeChecking.ag" #-}
                  let (correlationName,iden) = splitIdentifier i_
                  in envLookupID _lhsIenv correlationName iden
                  {-# LINE 1990 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 1998 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 644, column 16)
              _lhsOliftedColumnName =
                  {-# LINE 644 "./TypeChecking.ag" #-}
                  i_
                  {-# LINE 2003 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_InPredicate :: Annotation ->
                              T_Expression  ->
                              Bool ->
                              T_InList  ->
                              T_Expression 
sem_Expression_InPredicate ann_ expr_ i_ list_  =
    (\ _lhsIenv ->
         (let _listOenv :: Environment
              _exprOenv :: Environment
              _listIannotatedTree :: InList
              _listIlistType :: (Either [TypeError] Type)
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _listOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2024 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2029 "AstInternal.hs" #-}
              ( _listIannotatedTree,_listIlistType) =
                  (list_ _listOenv )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              -- "./TypeChecking.ag"(line 262, column 9)
              _backTree =
                  {-# LINE 262 "./TypeChecking.ag" #-}
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                  {-# LINE 2038 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 256, column 9)
              _tpe =
                  {-# LINE 256 "./TypeChecking.ag" #-}
                  do
                    lt <- _listIlistType
                    ty <- resolveResultSetType
                            _lhsIenv
                            [getTypeAnnotation _exprIannotatedTree, lt]
                    return typeBool
                  {-# LINE 2048 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2056 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  _exprIliftedColumnName
                  {-# LINE 2061 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_IntegerLit :: Annotation ->
                             Integer ->
                             T_Expression 
sem_Expression_IntegerLit ann_ i_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking.ag"(line 105, column 19)
              _tpe =
                  {-# LINE 105 "./TypeChecking.ag" #-}
                  Right typeInt
                  {-# LINE 2074 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 94, column 9)
              _backTree =
                  {-# LINE 94 "./TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 2079 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2087 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2092 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_NullLit :: Annotation ->
                          T_Expression 
sem_Expression_NullLit ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking.ag"(line 110, column 16)
              _tpe =
                  {-# LINE 110 "./TypeChecking.ag" #-}
                  Right UnknownStringLit
                  {-# LINE 2104 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 102, column 9)
              _backTree =
                  {-# LINE 102 "./TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 2109 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2117 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2122 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_PositionalArg :: Annotation ->
                                Integer ->
                                T_Expression 
sem_Expression_PositionalArg ann_ p_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 2135 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2140 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2145 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_ScalarSubQuery :: Annotation ->
                                 T_SelectExpression  ->
                                 T_Expression 
sem_Expression_ScalarSubQuery ann_ sel_  =
    (\ _lhsIenv ->
         (let _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2160 "AstInternal.hs" #-}
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
              -- "./TypeChecking.ag"(line 249, column 9)
              _backTree =
                  {-# LINE 249 "./TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 2167 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 240, column 9)
              _tpe =
                  {-# LINE 240 "./TypeChecking.ag" #-}
                  let selType = getTypeAnnotation _selIannotatedTree
                  in chainTypeCheckFailed [selType]
                       $ do
                         f <- map snd <$> unwrapSetOfComposite selType
                         case length f of
                              0 -> Left [InternalError "no columns in scalar subquery?"]
                              1 -> Right $ head f
                              _ -> Right $ RowCtor f
                  {-# LINE 2179 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2187 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2192 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_StringLit :: Annotation ->
                            String ->
                            String ->
                            T_Expression 
sem_Expression_StringLit ann_ quote_ value_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking.ag"(line 106, column 18)
              _tpe =
                  {-# LINE 106 "./TypeChecking.ag" #-}
                  Right UnknownStringLit
                  {-# LINE 2206 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 96, column 9)
              _backTree =
                  {-# LINE 96 "./TypeChecking.ag" #-}
                  StringLit ann_ quote_ value_
                  {-# LINE 2211 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 83, column 9)
              _lhsOannotatedTree =
                  {-# LINE 83 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2219 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 2224 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_WindowFn :: Annotation ->
                           T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList  ->
                           T_Direction  ->
                           T_Expression 
sem_Expression_WindowFn ann_ fn_ partitionBy_ orderBy_ dir_  =
    (\ _lhsIenv ->
         (let _orderByOenv :: Environment
              _partitionByOenv :: Environment
              _fnOenv :: Environment
              _dirOenv :: Environment
              _dirIannotatedTree :: Direction
              _orderByIannotatedTree :: ExpressionList
              _orderByItypeList :: ([Type])
              _partitionByIannotatedTree :: ExpressionList
              _partitionByItypeList :: ([Type])
              _fnIannotatedTree :: Expression
              _fnIliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _orderByOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2251 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2256 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2261 "AstInternal.hs" #-}
              -- copy rule (down)
              _dirOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2266 "AstInternal.hs" #-}
              ( _dirIannotatedTree) =
                  (dir_ _dirOenv )
              ( _orderByIannotatedTree,_orderByItypeList) =
                  (orderBy_ _orderByOenv )
              ( _partitionByIannotatedTree,_partitionByItypeList) =
                  (partitionBy_ _partitionByOenv )
              ( _fnIannotatedTree,_fnIliftedColumnName) =
                  (fn_ _fnOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree _dirIannotatedTree
                  {-# LINE 2279 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2284 "AstInternal.hs" #-}
              -- use rule "./TypeChecking.ag"(line 631, column 37)
              _lhsOliftedColumnName =
                  {-# LINE 631 "./TypeChecking.ag" #-}
                  _fnIliftedColumnName
                  {-# LINE 2289 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
-- ExpressionList ----------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                         ( ExpressionList,([Type]))
data Inh_ExpressionList  = Inh_ExpressionList {env_Inh_ExpressionList :: Environment}
data Syn_ExpressionList  = Syn_ExpressionList {annotatedTree_Syn_ExpressionList :: ExpressionList,typeList_Syn_ExpressionList :: [Type]}
wrap_ExpressionList :: T_ExpressionList  ->
                       Inh_ExpressionList  ->
                       Syn_ExpressionList 
wrap_ExpressionList sem (Inh_ExpressionList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOtypeList) =
             (sem _lhsIenv )
     in  (Syn_ExpressionList _lhsOannotatedTree _lhsOtypeList ))
sem_ExpressionList_Cons :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList 
sem_ExpressionList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: ExpressionList
              _tlItypeList :: ([Type])
              _hdIannotatedTree :: Expression
              _hdIliftedColumnName :: String
              _lhsOannotatedTree :: ExpressionList
              _lhsOtypeList :: ([Type])
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2344 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2349 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlItypeList) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree,_hdIliftedColumnName) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2358 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2363 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 289, column 12)
              _lhsOtypeList =
                  {-# LINE 289 "./TypeChecking.ag" #-}
                  getTypeAnnotation _hdIannotatedTree : _tlItypeList
                  {-# LINE 2368 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOtypeList)))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionList
              _lhsOtypeList :: ([Type])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2379 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2384 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 290, column 11)
              _lhsOtypeList =
                  {-# LINE 290 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2389 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOtypeList)))
-- ExpressionListList ------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                             ( ExpressionListList,([[Type]]))
data Inh_ExpressionListList  = Inh_ExpressionListList {env_Inh_ExpressionListList :: Environment}
data Syn_ExpressionListList  = Syn_ExpressionListList {annotatedTree_Syn_ExpressionListList :: ExpressionListList,typeListList_Syn_ExpressionListList :: [[Type]]}
wrap_ExpressionListList :: T_ExpressionListList  ->
                           Inh_ExpressionListList  ->
                           Syn_ExpressionListList 
wrap_ExpressionListList sem (Inh_ExpressionListList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOtypeListList) =
             (sem _lhsIenv )
     in  (Syn_ExpressionListList _lhsOannotatedTree _lhsOtypeListList ))
sem_ExpressionListList_Cons :: T_ExpressionList  ->
                               T_ExpressionListList  ->
                               T_ExpressionListList 
sem_ExpressionListList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: ExpressionListList
              _tlItypeListList :: ([[Type]])
              _hdIannotatedTree :: ExpressionList
              _hdItypeList :: ([Type])
              _lhsOannotatedTree :: ExpressionListList
              _lhsOtypeListList :: ([[Type]])
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2444 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2449 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlItypeListList) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree,_hdItypeList) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2458 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2463 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 296, column 12)
              _lhsOtypeListList =
                  {-# LINE 296 "./TypeChecking.ag" #-}
                  _hdItypeList : _tlItypeListList
                  {-# LINE 2468 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOtypeListList)))
sem_ExpressionListList_Nil :: T_ExpressionListList 
sem_ExpressionListList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionListList
              _lhsOtypeListList :: ([[Type]])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2479 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2484 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 297, column 11)
              _lhsOtypeListList =
                  {-# LINE 297 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2489 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOtypeListList)))
-- ExpressionListStatementListPair -----------------------------
{-
   visit 0:
      inherited attribute:
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
                                          ( ExpressionListStatementListPair)
data Inh_ExpressionListStatementListPair  = Inh_ExpressionListStatementListPair {env_Inh_ExpressionListStatementListPair :: Environment}
data Syn_ExpressionListStatementListPair  = Syn_ExpressionListStatementListPair {annotatedTree_Syn_ExpressionListStatementListPair :: ExpressionListStatementListPair}
wrap_ExpressionListStatementListPair :: T_ExpressionListStatementListPair  ->
                                        Inh_ExpressionListStatementListPair  ->
                                        Syn_ExpressionListStatementListPair 
wrap_ExpressionListStatementListPair sem (Inh_ExpressionListStatementListPair _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_ExpressionListStatementListPair _lhsOannotatedTree ))
sem_ExpressionListStatementListPair_Tuple :: T_ExpressionList  ->
                                             T_StatementList  ->
                                             T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIenv ->
         (let _x2Oenv :: Environment
              _x1Oenv :: Environment
              _x2OenvUpdates :: ([EnvironmentUpdate])
              _x2IannotatedTree :: StatementList
              _x2IproducedEnv :: Environment
              _x1IannotatedTree :: ExpressionList
              _x1ItypeList :: ([Type])
              _lhsOannotatedTree :: ExpressionListStatementListPair
              -- copy rule (down)
              _x2Oenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2540 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2545 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 346, column 13)
              _x2OenvUpdates =
                  {-# LINE 346 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2550 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2IproducedEnv) =
                  (x2_ _x2Oenv _x2OenvUpdates )
              ( _x1IannotatedTree,_x1ItypeList) =
                  (x1_ _x1Oenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 2559 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2564 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- ExpressionListStatementListPairList -------------------------
{-
   visit 0:
      inherited attribute:
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
                                              ( ExpressionListStatementListPairList)
data Inh_ExpressionListStatementListPairList  = Inh_ExpressionListStatementListPairList {env_Inh_ExpressionListStatementListPairList :: Environment}
data Syn_ExpressionListStatementListPairList  = Syn_ExpressionListStatementListPairList {annotatedTree_Syn_ExpressionListStatementListPairList :: ExpressionListStatementListPairList}
wrap_ExpressionListStatementListPairList :: T_ExpressionListStatementListPairList  ->
                                            Inh_ExpressionListStatementListPairList  ->
                                            Syn_ExpressionListStatementListPairList 
wrap_ExpressionListStatementListPairList sem (Inh_ExpressionListStatementListPairList _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_ExpressionListStatementListPairList _lhsOannotatedTree ))
sem_ExpressionListStatementListPairList_Cons :: T_ExpressionListStatementListPair  ->
                                                T_ExpressionListStatementListPairList  ->
                                                T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: ExpressionListStatementListPairList
              _hdIannotatedTree :: ExpressionListStatementListPair
              _lhsOannotatedTree :: ExpressionListStatementListPairList
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2615 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2620 "AstInternal.hs" #-}
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2629 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2634 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_ExpressionListStatementListPairList_Nil :: T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionListStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2644 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2649 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- ExpressionRoot ----------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                         ( ExpressionRoot)
data Inh_ExpressionRoot  = Inh_ExpressionRoot {env_Inh_ExpressionRoot :: Environment}
data Syn_ExpressionRoot  = Syn_ExpressionRoot {annotatedTree_Syn_ExpressionRoot :: ExpressionRoot}
wrap_ExpressionRoot :: T_ExpressionRoot  ->
                       Inh_ExpressionRoot  ->
                       Syn_ExpressionRoot 
wrap_ExpressionRoot sem (Inh_ExpressionRoot _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_ExpressionRoot _lhsOannotatedTree ))
sem_ExpressionRoot_ExpressionRoot :: T_Expression  ->
                                     T_ExpressionRoot 
sem_ExpressionRoot_ExpressionRoot expr_  =
    (\ _lhsIenv ->
         (let _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: ExpressionRoot
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2695 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ExpressionRoot _exprIannotatedTree
                  {-# LINE 2702 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2707 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- ExpressionStatementListPair ---------------------------------
{-
   visit 0:
      inherited attribute:
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
                                      ( ExpressionStatementListPair)
data Inh_ExpressionStatementListPair  = Inh_ExpressionStatementListPair {env_Inh_ExpressionStatementListPair :: Environment}
data Syn_ExpressionStatementListPair  = Syn_ExpressionStatementListPair {annotatedTree_Syn_ExpressionStatementListPair :: ExpressionStatementListPair}
wrap_ExpressionStatementListPair :: T_ExpressionStatementListPair  ->
                                    Inh_ExpressionStatementListPair  ->
                                    Syn_ExpressionStatementListPair 
wrap_ExpressionStatementListPair sem (Inh_ExpressionStatementListPair _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_ExpressionStatementListPair _lhsOannotatedTree ))
sem_ExpressionStatementListPair_Tuple :: T_Expression  ->
                                         T_StatementList  ->
                                         T_ExpressionStatementListPair 
sem_ExpressionStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIenv ->
         (let _x2Oenv :: Environment
              _x1Oenv :: Environment
              _x2OenvUpdates :: ([EnvironmentUpdate])
              _x2IannotatedTree :: StatementList
              _x2IproducedEnv :: Environment
              _x1IannotatedTree :: Expression
              _x1IliftedColumnName :: String
              _lhsOannotatedTree :: ExpressionStatementListPair
              -- copy rule (down)
              _x2Oenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2758 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2763 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 348, column 13)
              _x2OenvUpdates =
                  {-# LINE 348 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2768 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2IproducedEnv) =
                  (x2_ _x2Oenv _x2OenvUpdates )
              ( _x1IannotatedTree,_x1IliftedColumnName) =
                  (x1_ _x1Oenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 2777 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2782 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- ExpressionStatementListPairList -----------------------------
{-
   visit 0:
      inherited attribute:
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
                                          ( ExpressionStatementListPairList)
data Inh_ExpressionStatementListPairList  = Inh_ExpressionStatementListPairList {env_Inh_ExpressionStatementListPairList :: Environment}
data Syn_ExpressionStatementListPairList  = Syn_ExpressionStatementListPairList {annotatedTree_Syn_ExpressionStatementListPairList :: ExpressionStatementListPairList}
wrap_ExpressionStatementListPairList :: T_ExpressionStatementListPairList  ->
                                        Inh_ExpressionStatementListPairList  ->
                                        Syn_ExpressionStatementListPairList 
wrap_ExpressionStatementListPairList sem (Inh_ExpressionStatementListPairList _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_ExpressionStatementListPairList _lhsOannotatedTree ))
sem_ExpressionStatementListPairList_Cons :: T_ExpressionStatementListPair  ->
                                            T_ExpressionStatementListPairList  ->
                                            T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: ExpressionStatementListPairList
              _hdIannotatedTree :: ExpressionStatementListPair
              _lhsOannotatedTree :: ExpressionStatementListPairList
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2833 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2838 "AstInternal.hs" #-}
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2847 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2852 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_ExpressionStatementListPairList_Nil :: T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2862 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2867 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- FnBody ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                 ( FnBody)
data Inh_FnBody  = Inh_FnBody {env_Inh_FnBody :: Environment}
data Syn_FnBody  = Syn_FnBody {annotatedTree_Syn_FnBody :: FnBody}
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_FnBody _lhsOannotatedTree ))
sem_FnBody_PlpgsqlFnBody :: Annotation ->
                            T_VarDefList  ->
                            T_StatementList  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody ann_ vars_ sts_  =
    (\ _lhsIenv ->
         (let _varsOenv :: Environment
              _varsIannotatedTree :: VarDefList
              _varsIdefs :: ([(String,Type)])
              _stsOenv :: Environment
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsIannotatedTree :: StatementList
              _stsIproducedEnv :: Environment
              _lhsOannotatedTree :: FnBody
              -- copy rule (down)
              _varsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2929 "AstInternal.hs" #-}
              ( _varsIannotatedTree,_varsIdefs) =
                  (vars_ _varsOenv )
              -- "./TypeChecking.ag"(line 938, column 9)
              _stsOenv =
                  {-# LINE 938 "./TypeChecking.ag" #-}
                  fromRight _lhsIenv $ updateEnvironment _lhsIenv [EnvStackIDs [("", _varsIdefs)]]
                  {-# LINE 2936 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 350, column 31)
              _stsOenvUpdates =
                  {-# LINE 350 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2941 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIproducedEnv) =
                  (sts_ _stsOenv _stsOenvUpdates )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _varsIannotatedTree _stsIannotatedTree
                  {-# LINE 2948 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2953 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_FnBody_SqlFnBody :: Annotation ->
                        T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody ann_ sts_  =
    (\ _lhsIenv ->
         (let _stsOenv :: Environment
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsIannotatedTree :: StatementList
              _stsIproducedEnv :: Environment
              _lhsOannotatedTree :: FnBody
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2969 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 350, column 31)
              _stsOenvUpdates =
                  {-# LINE 350 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 2974 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIproducedEnv) =
                  (sts_ _stsOenv _stsOenvUpdates )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIannotatedTree
                  {-# LINE 2981 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2986 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- IfExists ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                   ( IfExists)
data Inh_IfExists  = Inh_IfExists {env_Inh_IfExists :: Environment}
data Syn_IfExists  = Syn_IfExists {annotatedTree_Syn_IfExists :: IfExists}
wrap_IfExists :: T_IfExists  ->
                 Inh_IfExists  ->
                 Syn_IfExists 
wrap_IfExists sem (Inh_IfExists _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_IfExists _lhsOannotatedTree ))
sem_IfExists_IfExists :: T_IfExists 
sem_IfExists_IfExists  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: IfExists
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  IfExists
                  {-# LINE 3033 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3038 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_IfExists_Require :: T_IfExists 
sem_IfExists_Require  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: IfExists
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Require
                  {-# LINE 3048 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3053 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- InList ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                 ( InList,(Either [TypeError] Type))
data Inh_InList  = Inh_InList {env_Inh_InList :: Environment}
data Syn_InList  = Syn_InList {annotatedTree_Syn_InList :: InList,listType_Syn_InList :: Either [TypeError] Type}
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOlistType) =
             (sem _lhsIenv )
     in  (Syn_InList _lhsOannotatedTree _lhsOlistType ))
sem_InList_InList :: Annotation ->
                     T_ExpressionList  ->
                     T_InList 
sem_InList_InList ann_ exprs_  =
    (\ _lhsIenv ->
         (let _exprsOenv :: Environment
              _exprsIannotatedTree :: ExpressionList
              _exprsItypeList :: ([Type])
              _lhsOannotatedTree :: InList
              _lhsOlistType :: (Either [TypeError] Type)
              -- copy rule (down)
              _exprsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3111 "AstInternal.hs" #-}
              ( _exprsIannotatedTree,_exprsItypeList) =
                  (exprs_ _exprsOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  InList ann_ _exprsIannotatedTree
                  {-# LINE 3118 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3123 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 269, column 9)
              _lhsOlistType =
                  {-# LINE 269 "./TypeChecking.ag" #-}
                  resolveResultSetType
                    _lhsIenv
                    _exprsItypeList
                  {-# LINE 3130 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType)))
sem_InList_InSelect :: Annotation ->
                       T_SelectExpression  ->
                       T_InList 
sem_InList_InSelect ann_ sel_  =
    (\ _lhsIenv ->
         (let _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _lhsOannotatedTree :: InList
              _lhsOlistType :: (Either [TypeError] Type)
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3145 "AstInternal.hs" #-}
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  InSelect ann_ _selIannotatedTree
                  {-# LINE 3152 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3157 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 273, column 9)
              _lhsOlistType =
                  {-# LINE 273 "./TypeChecking.ag" #-}
                  do
                    attrs <-  map snd <$> (unwrapSetOfComposite $
                                let a = getTypeAnnotation _selIannotatedTree
                                in                                      a)
                    typ <- case length attrs of
                                 0 -> Left [InternalError "got subquery with no columns? in inselect"]
                                 1 -> Right $ head attrs
                                 _ -> Right $ RowCtor attrs
                    chainTypeCheckFailed attrs $ Right typ
                  {-# LINE 3170 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType)))
-- JoinExpression ----------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                         ( JoinExpression)
data Inh_JoinExpression  = Inh_JoinExpression {env_Inh_JoinExpression :: Environment}
data Syn_JoinExpression  = Syn_JoinExpression {annotatedTree_Syn_JoinExpression :: JoinExpression}
wrap_JoinExpression :: T_JoinExpression  ->
                       Inh_JoinExpression  ->
                       Syn_JoinExpression 
wrap_JoinExpression sem (Inh_JoinExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_JoinExpression _lhsOannotatedTree ))
sem_JoinExpression_JoinOn :: Annotation ->
                             T_Expression  ->
                             T_JoinExpression 
sem_JoinExpression_JoinOn ann_ expression_  =
    (\ _lhsIenv ->
         (let _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIliftedColumnName :: String
              _lhsOannotatedTree :: JoinExpression
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3226 "AstInternal.hs" #-}
              ( _expressionIannotatedTree,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  JoinOn ann_ _expressionIannotatedTree
                  {-# LINE 3233 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3238 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_JoinExpression_JoinUsing :: Annotation ->
                                T_StringList  ->
                                T_JoinExpression 
sem_JoinExpression_JoinUsing ann_ stringList_  =
    (\ _lhsIenv ->
         (let _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _lhsOannotatedTree :: JoinExpression
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3253 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_ _stringListOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  JoinUsing ann_ _stringListIannotatedTree
                  {-# LINE 3260 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3265 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- JoinType ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                   ( JoinType)
data Inh_JoinType  = Inh_JoinType {env_Inh_JoinType :: Environment}
data Syn_JoinType  = Syn_JoinType {annotatedTree_Syn_JoinType :: JoinType}
wrap_JoinType :: T_JoinType  ->
                 Inh_JoinType  ->
                 Syn_JoinType 
wrap_JoinType sem (Inh_JoinType _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_JoinType _lhsOannotatedTree ))
sem_JoinType_Cross :: T_JoinType 
sem_JoinType_Cross  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Cross
                  {-# LINE 3330 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3335 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_JoinType_FullOuter :: T_JoinType 
sem_JoinType_FullOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  FullOuter
                  {-# LINE 3345 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3350 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_JoinType_Inner :: T_JoinType 
sem_JoinType_Inner  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Inner
                  {-# LINE 3360 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3365 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_JoinType_LeftOuter :: T_JoinType 
sem_JoinType_LeftOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  LeftOuter
                  {-# LINE 3375 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3380 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_JoinType_RightOuter :: T_JoinType 
sem_JoinType_RightOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  RightOuter
                  {-# LINE 3390 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3395 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- Language ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                   ( Language)
data Inh_Language  = Inh_Language {env_Inh_Language :: Environment}
data Syn_Language  = Syn_Language {annotatedTree_Syn_Language :: Language}
wrap_Language :: T_Language  ->
                 Inh_Language  ->
                 Syn_Language 
wrap_Language sem (Inh_Language _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_Language _lhsOannotatedTree ))
sem_Language_Plpgsql :: T_Language 
sem_Language_Plpgsql  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Language
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Plpgsql
                  {-# LINE 3442 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3447 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Language_Sql :: T_Language 
sem_Language_Sql  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Language
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Sql
                  {-# LINE 3457 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3462 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- MaybeBoolExpression -----------------------------------------
{-
   visit 0:
      inherited attribute:
         env                  : Environment
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Just:
         child just           : Expression 
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
                              ( MaybeBoolExpression)
data Inh_MaybeBoolExpression  = Inh_MaybeBoolExpression {env_Inh_MaybeBoolExpression :: Environment}
data Syn_MaybeBoolExpression  = Syn_MaybeBoolExpression {annotatedTree_Syn_MaybeBoolExpression :: MaybeBoolExpression}
wrap_MaybeBoolExpression :: T_MaybeBoolExpression  ->
                            Inh_MaybeBoolExpression  ->
                            Syn_MaybeBoolExpression 
wrap_MaybeBoolExpression sem (Inh_MaybeBoolExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_MaybeBoolExpression _lhsOannotatedTree ))
sem_MaybeBoolExpression_Just :: T_Expression  ->
                                T_MaybeBoolExpression 
sem_MaybeBoolExpression_Just just_  =
    (\ _lhsIenv ->
         (let _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIliftedColumnName :: String
              _lhsOannotatedTree :: MaybeBoolExpression
              -- copy rule (down)
              _justOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3510 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIliftedColumnName) =
                  (just_ _justOenv )
              -- "./TypeChecking.ag"(line 960, column 9)
              _lhsOannotatedTree =
                  {-# LINE 960 "./TypeChecking.ag" #-}
                  if getTypeAnnotation _justIannotatedTree `notElem` [typeBool, TypeCheckFailed]
                    then Just $ updateAnnotation ((TypeErrorA ExpressionMustBeBool) :)
                                  _justIannotatedTree
                    else Just $ _justIannotatedTree
                  {-# LINE 3520 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_MaybeBoolExpression_Nothing :: T_MaybeBoolExpression 
sem_MaybeBoolExpression_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MaybeBoolExpression
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3530 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3535 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- MaybeExpression ---------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                          ( MaybeExpression,(Maybe Type))
data Inh_MaybeExpression  = Inh_MaybeExpression {env_Inh_MaybeExpression :: Environment}
data Syn_MaybeExpression  = Syn_MaybeExpression {annotatedTree_Syn_MaybeExpression :: MaybeExpression,exprType_Syn_MaybeExpression :: Maybe Type}
wrap_MaybeExpression :: T_MaybeExpression  ->
                        Inh_MaybeExpression  ->
                        Syn_MaybeExpression 
wrap_MaybeExpression sem (Inh_MaybeExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOexprType) =
             (sem _lhsIenv )
     in  (Syn_MaybeExpression _lhsOannotatedTree _lhsOexprType ))
sem_MaybeExpression_Just :: T_Expression  ->
                            T_MaybeExpression 
sem_MaybeExpression_Just just_  =
    (\ _lhsIenv ->
         (let _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIliftedColumnName :: String
              _lhsOannotatedTree :: MaybeExpression
              _lhsOexprType :: (Maybe Type)
              -- copy rule (down)
              _justOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3587 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIliftedColumnName) =
                  (just_ _justOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3594 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3599 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 969, column 12)
              _lhsOexprType =
                  {-# LINE 969 "./TypeChecking.ag" #-}
                  Just $ getTypeAnnotation _justIannotatedTree
                  {-# LINE 3604 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOexprType)))
sem_MaybeExpression_Nothing :: T_MaybeExpression 
sem_MaybeExpression_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MaybeExpression
              _lhsOexprType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3615 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3620 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 970, column 15)
              _lhsOexprType =
                  {-# LINE 970 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3625 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOexprType)))
-- MaybeTableRef -----------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                        ( MaybeTableRef,([(String,([(String,Type)],[(String,Type)]))]),([String]))
data Inh_MaybeTableRef  = Inh_MaybeTableRef {env_Inh_MaybeTableRef :: Environment}
data Syn_MaybeTableRef  = Syn_MaybeTableRef {annotatedTree_Syn_MaybeTableRef :: MaybeTableRef,idens_Syn_MaybeTableRef :: [(String,([(String,Type)],[(String,Type)]))],joinIdens_Syn_MaybeTableRef :: [String]}
wrap_MaybeTableRef :: T_MaybeTableRef  ->
                      Inh_MaybeTableRef  ->
                      Syn_MaybeTableRef 
wrap_MaybeTableRef sem (Inh_MaybeTableRef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens) =
             (sem _lhsIenv )
     in  (Syn_MaybeTableRef _lhsOannotatedTree _lhsOidens _lhsOjoinIdens ))
sem_MaybeTableRef_Just :: T_TableRef  ->
                          T_MaybeTableRef 
sem_MaybeTableRef_Just just_  =
    (\ _lhsIenv ->
         (let _justOenv :: Environment
              _justIannotatedTree :: TableRef
              _justIidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _justIjoinIdens :: ([String])
              _lhsOannotatedTree :: MaybeTableRef
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              -- copy rule (down)
              _justOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3680 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIidens,_justIjoinIdens) =
                  (just_ _justOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3687 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3692 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOidens =
                  {-# LINE 425 "./TypeChecking.ag" #-}
                  _justIidens
                  {-# LINE 3697 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOjoinIdens =
                  {-# LINE 426 "./TypeChecking.ag" #-}
                  _justIjoinIdens
                  {-# LINE 3702 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_MaybeTableRef_Nothing :: T_MaybeTableRef 
sem_MaybeTableRef_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MaybeTableRef
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3714 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3719 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 525, column 9)
              _lhsOidens =
                  {-# LINE 525 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3724 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 526, column 9)
              _lhsOjoinIdens =
                  {-# LINE 526 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 3729 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
-- Natural -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                  ( Natural)
data Inh_Natural  = Inh_Natural {env_Inh_Natural :: Environment}
data Syn_Natural  = Syn_Natural {annotatedTree_Syn_Natural :: Natural}
wrap_Natural :: T_Natural  ->
                Inh_Natural  ->
                Syn_Natural 
wrap_Natural sem (Inh_Natural _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_Natural _lhsOannotatedTree ))
sem_Natural_Natural :: T_Natural 
sem_Natural_Natural  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Natural
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Natural
                  {-# LINE 3776 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3781 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Natural_Unnatural :: T_Natural 
sem_Natural_Unnatural  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Natural
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Unnatural
                  {-# LINE 3791 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3796 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- OnExpr ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                 ( OnExpr)
data Inh_OnExpr  = Inh_OnExpr {env_Inh_OnExpr :: Environment}
data Syn_OnExpr  = Syn_OnExpr {annotatedTree_Syn_OnExpr :: OnExpr}
wrap_OnExpr :: T_OnExpr  ->
               Inh_OnExpr  ->
               Syn_OnExpr 
wrap_OnExpr sem (Inh_OnExpr _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_OnExpr _lhsOannotatedTree ))
sem_OnExpr_Just :: T_JoinExpression  ->
                   T_OnExpr 
sem_OnExpr_Just just_  =
    (\ _lhsIenv ->
         (let _justOenv :: Environment
              _justIannotatedTree :: JoinExpression
              _lhsOannotatedTree :: OnExpr
              -- copy rule (down)
              _justOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3845 "AstInternal.hs" #-}
              ( _justIannotatedTree) =
                  (just_ _justOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3852 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3857 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: OnExpr
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3867 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3872 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- ParamDef ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                   ( ParamDef,Type,String)
data Inh_ParamDef  = Inh_ParamDef {env_Inh_ParamDef :: Environment}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef,namedType_Syn_ParamDef :: Type,paramName_Syn_ParamDef :: String}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOnamedType,_lhsOparamName) =
             (sem _lhsIenv )
     in  (Syn_ParamDef _lhsOannotatedTree _lhsOnamedType _lhsOparamName ))
sem_ParamDef_ParamDef :: Annotation ->
                         String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef ann_ name_ typ_  =
    (\ _lhsIenv ->
         (let _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: ParamDef
              _lhsOnamedType :: Type
              _lhsOparamName :: String
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3934 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIannotatedTree
                  {-# LINE 3941 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3946 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 908, column 9)
              _lhsOnamedType =
                  {-# LINE 908 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 3951 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 910, column 9)
              _lhsOparamName =
                  {-# LINE 910 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 3956 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOparamName)))
sem_ParamDef_ParamDefTp :: Annotation ->
                           T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp ann_ typ_  =
    (\ _lhsIenv ->
         (let _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: ParamDef
              _lhsOnamedType :: Type
              _lhsOparamName :: String
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3973 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIannotatedTree
                  {-# LINE 3980 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3985 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 908, column 9)
              _lhsOnamedType =
                  {-# LINE 908 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 3990 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 912, column 9)
              _lhsOparamName =
                  {-# LINE 912 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 3995 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOparamName)))
-- ParamDefList ------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                       ( ParamDefList,([(String, Type)]))
data Inh_ParamDefList  = Inh_ParamDefList {env_Inh_ParamDefList :: Environment}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList,params_Syn_ParamDefList :: [(String, Type)]}
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOparams) =
             (sem _lhsIenv )
     in  (Syn_ParamDefList _lhsOannotatedTree _lhsOparams ))
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: ParamDefList
              _tlIparams :: ([(String, Type)])
              _hdIannotatedTree :: ParamDef
              _hdInamedType :: Type
              _hdIparamName :: String
              _lhsOannotatedTree :: ParamDefList
              _lhsOparams :: ([(String, Type)])
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4051 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4056 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIparams) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree,_hdInamedType,_hdIparamName) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 4065 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4070 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 916, column 13)
              _lhsOparams =
                  {-# LINE 916 "./TypeChecking.ag" #-}
                  ((_hdIparamName, _hdInamedType) : _tlIparams)
                  {-# LINE 4075 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOparams)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ParamDefList
              _lhsOparams :: ([(String, Type)])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4086 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4091 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 915, column 12)
              _lhsOparams =
                  {-# LINE 915 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4096 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOparams)))
-- RaiseType ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                    ( RaiseType)
data Inh_RaiseType  = Inh_RaiseType {env_Inh_RaiseType :: Environment}
data Syn_RaiseType  = Syn_RaiseType {annotatedTree_Syn_RaiseType :: RaiseType}
wrap_RaiseType :: T_RaiseType  ->
                  Inh_RaiseType  ->
                  Syn_RaiseType 
wrap_RaiseType sem (Inh_RaiseType _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_RaiseType _lhsOannotatedTree ))
sem_RaiseType_RError :: T_RaiseType 
sem_RaiseType_RError  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RaiseType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  RError
                  {-# LINE 4149 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4154 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_RaiseType_RException :: T_RaiseType 
sem_RaiseType_RException  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RaiseType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  RException
                  {-# LINE 4164 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4169 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_RaiseType_RNotice :: T_RaiseType 
sem_RaiseType_RNotice  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RaiseType
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  RNotice
                  {-# LINE 4179 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4184 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- RestartIdentity ---------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                          ( RestartIdentity)
data Inh_RestartIdentity  = Inh_RestartIdentity {env_Inh_RestartIdentity :: Environment}
data Syn_RestartIdentity  = Syn_RestartIdentity {annotatedTree_Syn_RestartIdentity :: RestartIdentity}
wrap_RestartIdentity :: T_RestartIdentity  ->
                        Inh_RestartIdentity  ->
                        Syn_RestartIdentity 
wrap_RestartIdentity sem (Inh_RestartIdentity _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_RestartIdentity _lhsOannotatedTree ))
sem_RestartIdentity_ContinueIdentity :: T_RestartIdentity 
sem_RestartIdentity_ContinueIdentity  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RestartIdentity
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ContinueIdentity
                  {-# LINE 4231 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4236 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_RestartIdentity_RestartIdentity :: T_RestartIdentity 
sem_RestartIdentity_RestartIdentity  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RestartIdentity
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  RestartIdentity
                  {-# LINE 4246 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4251 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- Root --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         producedEnv          : Environment
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
data Syn_Root  = Syn_Root {annotatedTree_Syn_Root :: Root,producedEnv_Syn_Root :: Environment}
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOproducedEnv) =
             (sem _lhsIenv )
     in  (Syn_Root _lhsOannotatedTree _lhsOproducedEnv ))
sem_Root_Root :: T_StatementList  ->
                 T_Root 
sem_Root_Root statements_  =
    (\ _lhsIenv ->
         (let _statementsOenv :: Environment
              _statementsOenvUpdates :: ([EnvironmentUpdate])
              _statementsIannotatedTree :: StatementList
              _statementsIproducedEnv :: Environment
              _lhsOannotatedTree :: Root
              _lhsOproducedEnv :: Environment
              -- copy rule (down)
              _statementsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4300 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 330, column 12)
              _statementsOenvUpdates =
                  {-# LINE 330 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4305 "AstInternal.hs" #-}
              ( _statementsIannotatedTree,_statementsIproducedEnv) =
                  (statements_ _statementsOenv _statementsOenvUpdates )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Root _statementsIannotatedTree
                  {-# LINE 4312 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4317 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOproducedEnv =
                  {-# LINE 59 "./TypeChecking.ag" #-}
                  _statementsIproducedEnv
                  {-# LINE 4322 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOproducedEnv)))
-- RowConstraint -----------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                        ( RowConstraint)
data Inh_RowConstraint  = Inh_RowConstraint {env_Inh_RowConstraint :: Environment}
data Syn_RowConstraint  = Syn_RowConstraint {annotatedTree_Syn_RowConstraint :: RowConstraint}
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_RowConstraint _lhsOannotatedTree ))
sem_RowConstraint_NotNullConstraint :: Annotation ->
                                       T_RowConstraint 
sem_RowConstraint_NotNullConstraint ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  NotNullConstraint ann_
                  {-# LINE 4405 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4410 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_RowConstraint_NullConstraint :: Annotation ->
                                    T_RowConstraint 
sem_RowConstraint_NullConstraint ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  NullConstraint ann_
                  {-# LINE 4421 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4426 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_RowConstraint_RowCheckConstraint :: Annotation ->
                                        T_Expression  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint ann_ expression_  =
    (\ _lhsIenv ->
         (let _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIliftedColumnName :: String
              _lhsOannotatedTree :: RowConstraint
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4441 "AstInternal.hs" #-}
              ( _expressionIannotatedTree,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  RowCheckConstraint ann_ _expressionIannotatedTree
                  {-# LINE 4448 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4453 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_RowConstraint_RowPrimaryKeyConstraint :: Annotation ->
                                             T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint ann_
                  {-# LINE 4464 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4469 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_RowConstraint_RowReferenceConstraint :: Annotation ->
                                            String ->
                                            (Maybe String) ->
                                            T_Cascade  ->
                                            T_Cascade  ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint ann_ table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIenv ->
         (let _onDeleteOenv :: Environment
              _onDeleteIannotatedTree :: Cascade
              _onUpdateOenv :: Environment
              _onUpdateIannotatedTree :: Cascade
              _lhsOannotatedTree :: RowConstraint
              -- copy rule (down)
              _onDeleteOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4488 "AstInternal.hs" #-}
              ( _onDeleteIannotatedTree) =
                  (onDelete_ _onDeleteOenv )
              -- copy rule (down)
              _onUpdateOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4495 "AstInternal.hs" #-}
              ( _onUpdateIannotatedTree) =
                  (onUpdate_ _onUpdateOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  RowReferenceConstraint ann_ table_ att_ _onUpdateIannotatedTree _onDeleteIannotatedTree
                  {-# LINE 4502 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4507 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_RowConstraint_RowUniqueConstraint :: Annotation ->
                                         T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  RowUniqueConstraint ann_
                  {-# LINE 4518 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4523 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- RowConstraintList -------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                            ( RowConstraintList)
data Inh_RowConstraintList  = Inh_RowConstraintList {env_Inh_RowConstraintList :: Environment}
data Syn_RowConstraintList  = Syn_RowConstraintList {annotatedTree_Syn_RowConstraintList :: RowConstraintList}
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_RowConstraintList _lhsOannotatedTree ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: RowConstraintList
              _hdIannotatedTree :: RowConstraint
              _lhsOannotatedTree :: RowConstraintList
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4574 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4579 "AstInternal.hs" #-}
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 4588 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4593 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 4603 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4608 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- SelectExpression --------------------------------------------
{-
   visit 0:
      inherited attribute:
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
            local backTree    : _
            local tpe         : _
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
            local newEnv      : _
            local backTree    : _
            local tpe         : _
      alternative Values:
         child ann            : {Annotation}
         child vll            : ExpressionListList 
         visit 0:
            local backTree    : _
            local tpe         : _
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
                           ( SelectExpression)
data Inh_SelectExpression  = Inh_SelectExpression {env_Inh_SelectExpression :: Environment}
data Syn_SelectExpression  = Syn_SelectExpression {annotatedTree_Syn_SelectExpression :: SelectExpression}
wrap_SelectExpression :: T_SelectExpression  ->
                         Inh_SelectExpression  ->
                         Syn_SelectExpression 
wrap_SelectExpression sem (Inh_SelectExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_SelectExpression _lhsOannotatedTree ))
sem_SelectExpression_CombineSelect :: Annotation ->
                                      T_CombineType  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression 
sem_SelectExpression_CombineSelect ann_ ctype_ sel1_ sel2_  =
    (\ _lhsIenv ->
         (let _sel2Oenv :: Environment
              _sel1Oenv :: Environment
              _sel2IannotatedTree :: SelectExpression
              _sel1IannotatedTree :: SelectExpression
              _ctypeOenv :: Environment
              _ctypeIannotatedTree :: CombineType
              _lhsOannotatedTree :: SelectExpression
              -- copy rule (down)
              _sel2Oenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4692 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Oenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4697 "AstInternal.hs" #-}
              ( _sel2IannotatedTree) =
                  (sel2_ _sel2Oenv )
              ( _sel1IannotatedTree) =
                  (sel1_ _sel1Oenv )
              -- copy rule (down)
              _ctypeOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4706 "AstInternal.hs" #-}
              ( _ctypeIannotatedTree) =
                  (ctype_ _ctypeOenv )
              -- "./TypeChecking.ag"(line 417, column 9)
              _backTree =
                  {-# LINE 417 "./TypeChecking.ag" #-}
                  CombineSelect ann_ _ctypeIannotatedTree
                                _sel1IannotatedTree
                                _sel2IannotatedTree
                  {-# LINE 4715 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 412, column 9)
              _tpe =
                  {-# LINE 412 "./TypeChecking.ag" #-}
                  let sel1t = getTypeAnnotation _sel1IannotatedTree
                      sel2t = getTypeAnnotation _sel2IannotatedTree
                  in chainTypeCheckFailed [sel1t, sel2t] $
                        typeCheckCombineSelect _lhsIenv sel1t sel2t
                  {-# LINE 4723 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 379, column 9)
              _lhsOannotatedTree =
                  {-# LINE 379 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 4731 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
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
         (let _selOffsetOenv :: Environment
              _selLimitOenv :: Environment
              _selOrderByOenv :: Environment
              _selHavingOenv :: Environment
              _selGroupByOenv :: Environment
              _selTrefOenv :: Environment
              _selTrefIannotatedTree :: MaybeTableRef
              _selTrefIidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _selTrefIjoinIdens :: ([String])
              _selWhereOenv :: Environment
              _selSelectListOenv :: Environment
              _selOffsetIannotatedTree :: MaybeExpression
              _selOffsetIexprType :: (Maybe Type)
              _selLimitIannotatedTree :: MaybeExpression
              _selLimitIexprType :: (Maybe Type)
              _selDirOenv :: Environment
              _selDirIannotatedTree :: Direction
              _selOrderByIannotatedTree :: ExpressionList
              _selOrderByItypeList :: ([Type])
              _selHavingIannotatedTree :: MaybeBoolExpression
              _selGroupByIannotatedTree :: ExpressionList
              _selGroupByItypeList :: ([Type])
              _selWhereIannotatedTree :: MaybeBoolExpression
              _selSelectListIannotatedTree :: SelectList
              _selSelectListIlistType :: Type
              _selDistinctOenv :: Environment
              _selDistinctIannotatedTree :: Distinct
              _lhsOannotatedTree :: SelectExpression
              -- copy rule (down)
              _selOffsetOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4779 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4784 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOrderByOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4789 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4794 "AstInternal.hs" #-}
              -- copy rule (down)
              _selGroupByOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4799 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4804 "AstInternal.hs" #-}
              ( _selTrefIannotatedTree,_selTrefIidens,_selTrefIjoinIdens) =
                  (selTref_ _selTrefOenv )
              -- "./TypeChecking.ag"(line 597, column 10)
              _newEnv =
                  {-# LINE 597 "./TypeChecking.ag" #-}
                  case updateEnvironment _lhsIenv
                        (convertToNewStyleUpdates _selTrefIidens _selTrefIjoinIdens) of
                    Left x -> error $ show x
                    Right e -> e
                  {-# LINE 4814 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 602, column 10)
              _selWhereOenv =
                  {-# LINE 602 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 4819 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 601, column 10)
              _selSelectListOenv =
                  {-# LINE 601 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 4824 "AstInternal.hs" #-}
              ( _selOffsetIannotatedTree,_selOffsetIexprType) =
                  (selOffset_ _selOffsetOenv )
              ( _selLimitIannotatedTree,_selLimitIexprType) =
                  (selLimit_ _selLimitOenv )
              -- copy rule (down)
              _selDirOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4833 "AstInternal.hs" #-}
              ( _selDirIannotatedTree) =
                  (selDir_ _selDirOenv )
              ( _selOrderByIannotatedTree,_selOrderByItypeList) =
                  (selOrderBy_ _selOrderByOenv )
              ( _selHavingIannotatedTree) =
                  (selHaving_ _selHavingOenv )
              ( _selGroupByIannotatedTree,_selGroupByItypeList) =
                  (selGroupBy_ _selGroupByOenv )
              ( _selWhereIannotatedTree) =
                  (selWhere_ _selWhereOenv )
              ( _selSelectListIannotatedTree,_selSelectListIlistType) =
                  (selSelectList_ _selSelectListOenv )
              -- copy rule (down)
              _selDistinctOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4850 "AstInternal.hs" #-}
              ( _selDistinctIannotatedTree) =
                  (selDistinct_ _selDistinctOenv )
              -- "./TypeChecking.ag"(line 400, column 9)
              _backTree =
                  {-# LINE 400 "./TypeChecking.ag" #-}
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
                  {-# LINE 4867 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 391, column 9)
              _tpe =
                  {-# LINE 391 "./TypeChecking.ag" #-}
                  do
                  let trefType = fromMaybe typeBool $ fmap getTypeAnnotation
                                                           _selTrefIannotatedTree
                      slType = _selSelectListIlistType
                  chainTypeCheckFailed [trefType, slType] $
                    Right $ case slType of
                              UnnamedCompositeType [(_,Pseudo Void)] -> Pseudo Void
                              _ -> SetOfType slType
                  {-# LINE 4879 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 379, column 9)
              _lhsOannotatedTree =
                  {-# LINE 379 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 4887 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_SelectExpression_Values :: Annotation ->
                               T_ExpressionListList  ->
                               T_SelectExpression 
sem_SelectExpression_Values ann_ vll_  =
    (\ _lhsIenv ->
         (let _vllOenv :: Environment
              _vllIannotatedTree :: ExpressionListList
              _vllItypeListList :: ([[Type]])
              _lhsOannotatedTree :: SelectExpression
              -- copy rule (down)
              _vllOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4902 "AstInternal.hs" #-}
              ( _vllIannotatedTree,_vllItypeListList) =
                  (vll_ _vllOenv )
              -- "./TypeChecking.ag"(line 389, column 9)
              _backTree =
                  {-# LINE 389 "./TypeChecking.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 4909 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 386, column 9)
              _tpe =
                  {-# LINE 386 "./TypeChecking.ag" #-}
                  typeCheckValuesExpr
                              _lhsIenv
                              _vllItypeListList
                  {-# LINE 4916 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 379, column 9)
              _lhsOannotatedTree =
                  {-# LINE 379 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 4924 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- SelectItem --------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                     ( SelectItem,String,Type)
data Inh_SelectItem  = Inh_SelectItem {env_Inh_SelectItem :: Environment}
data Syn_SelectItem  = Syn_SelectItem {annotatedTree_Syn_SelectItem :: SelectItem,columnName_Syn_SelectItem :: String,itemType_Syn_SelectItem :: Type}
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOitemType) =
             (sem _lhsIenv )
     in  (Syn_SelectItem _lhsOannotatedTree _lhsOcolumnName _lhsOitemType ))
sem_SelectItem_SelExp :: Annotation ->
                         T_Expression  ->
                         T_SelectItem 
sem_SelectItem_SelExp ann_ ex_  =
    (\ _lhsIenv ->
         (let _exOenv :: Environment
              _exIannotatedTree :: Expression
              _exIliftedColumnName :: String
              _lhsOannotatedTree :: SelectItem
              _lhsOcolumnName :: String
              _lhsOitemType :: Type
              -- copy rule (down)
              _exOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4985 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIliftedColumnName) =
                  (ex_ _exOenv )
              -- "./TypeChecking.ag"(line 543, column 9)
              _annotatedTree =
                  {-# LINE 543 "./TypeChecking.ag" #-}
                  SelExp ann_ $ fixStar _exIannotatedTree
                  {-# LINE 4992 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4997 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 655, column 14)
              _lhsOcolumnName =
                  {-# LINE 655 "./TypeChecking.ag" #-}
                  case _exIliftedColumnName of
                    "" -> "?column?"
                    s -> s
                  {-# LINE 5004 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 538, column 9)
              _lhsOitemType =
                  {-# LINE 538 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exIannotatedTree
                  {-# LINE 5009 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOitemType)))
sem_SelectItem_SelectItem :: Annotation ->
                             T_Expression  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem ann_ ex_ name_  =
    (\ _lhsIenv ->
         (let _exOenv :: Environment
              _exIannotatedTree :: Expression
              _exIliftedColumnName :: String
              _lhsOannotatedTree :: SelectItem
              _lhsOcolumnName :: String
              _lhsOitemType :: Type
              -- copy rule (down)
              _exOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5027 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIliftedColumnName) =
                  (ex_ _exOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  SelectItem ann_ _exIannotatedTree name_
                  {-# LINE 5034 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5039 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 658, column 18)
              _lhsOcolumnName =
                  {-# LINE 658 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 5044 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 538, column 9)
              _lhsOitemType =
                  {-# LINE 538 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exIannotatedTree
                  {-# LINE 5049 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOitemType)))
-- SelectItemList ----------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                         ( SelectItemList,Type)
data Inh_SelectItemList  = Inh_SelectItemList {env_Inh_SelectItemList :: Environment}
data Syn_SelectItemList  = Syn_SelectItemList {annotatedTree_Syn_SelectItemList :: SelectItemList,listType_Syn_SelectItemList :: Type}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOlistType) =
             (sem _lhsIenv )
     in  (Syn_SelectItemList _lhsOannotatedTree _lhsOlistType ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: SelectItemList
              _tlIlistType :: Type
              _hdIannotatedTree :: SelectItem
              _hdIcolumnName :: String
              _hdIitemType :: Type
              _lhsOannotatedTree :: SelectItemList
              _lhsOlistType :: Type
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5105 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5110 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIlistType) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree,_hdIcolumnName,_hdIitemType) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5119 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5124 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 531, column 12)
              _lhsOlistType =
                  {-# LINE 531 "./TypeChecking.ag" #-}
                  doSelectItemListTpe _lhsIenv _hdIcolumnName _hdIitemType _tlIlistType
                  {-# LINE 5129 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: SelectItemList
              _lhsOlistType :: Type
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5140 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5145 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 532, column 11)
              _lhsOlistType =
                  {-# LINE 532 "./TypeChecking.ag" #-}
                  UnnamedCompositeType []
                  {-# LINE 5150 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType)))
-- SelectList --------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                     ( SelectList,Type)
data Inh_SelectList  = Inh_SelectList {env_Inh_SelectList :: Environment}
data Syn_SelectList  = Syn_SelectList {annotatedTree_Syn_SelectList :: SelectList,listType_Syn_SelectList :: Type}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOlistType) =
             (sem _lhsIenv )
     in  (Syn_SelectList _lhsOannotatedTree _lhsOlistType ))
sem_SelectList_SelectList :: Annotation ->
                             T_SelectItemList  ->
                             T_StringList  ->
                             T_SelectList 
sem_SelectList_SelectList ann_ items_ stringList_  =
    (\ _lhsIenv ->
         (let _itemsOenv :: Environment
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _itemsIannotatedTree :: SelectItemList
              _itemsIlistType :: Type
              _lhsOannotatedTree :: SelectList
              _lhsOlistType :: Type
              -- copy rule (down)
              _itemsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5205 "AstInternal.hs" #-}
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5210 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_ _stringListOenv )
              ( _itemsIannotatedTree,_itemsIlistType) =
                  (items_ _itemsOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  SelectList ann_ _itemsIannotatedTree _stringListIannotatedTree
                  {-# LINE 5219 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5224 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 571, column 9)
              _lhsOlistType =
                  {-# LINE 571 "./TypeChecking.ag" #-}
                  _itemsIlistType
                  {-# LINE 5229 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType)))
-- SetClause ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
            local annotatedTree : _
            local rowSetError : _
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
                    ( SetClause,([(String,Type)]),(Maybe TypeError))
data Inh_SetClause  = Inh_SetClause {env_Inh_SetClause :: Environment}
data Syn_SetClause  = Syn_SetClause {annotatedTree_Syn_SetClause :: SetClause,pairs_Syn_SetClause :: [(String,Type)],rowSetError_Syn_SetClause :: Maybe TypeError}
wrap_SetClause :: T_SetClause  ->
                  Inh_SetClause  ->
                  Syn_SetClause 
wrap_SetClause sem (Inh_SetClause _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOpairs,_lhsOrowSetError) =
             (sem _lhsIenv )
     in  (Syn_SetClause _lhsOannotatedTree _lhsOpairs _lhsOrowSetError ))
sem_SetClause_RowSetClause :: Annotation ->
                              T_StringList  ->
                              T_ExpressionList  ->
                              T_SetClause 
sem_SetClause_RowSetClause ann_ atts_ vals_  =
    (\ _lhsIenv ->
         (let _valsOenv :: Environment
              _valsIannotatedTree :: ExpressionList
              _valsItypeList :: ([Type])
              _attsOenv :: Environment
              _attsIannotatedTree :: StringList
              _attsIstrings :: ([String])
              _lhsOannotatedTree :: SetClause
              _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetError :: (Maybe TypeError)
              -- copy rule (down)
              _valsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5296 "AstInternal.hs" #-}
              ( _valsIannotatedTree,_valsItypeList) =
                  (vals_ _valsOenv )
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5303 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIstrings) =
                  (atts_ _attsOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  RowSetClause ann_ _attsIannotatedTree _valsIannotatedTree
                  {-# LINE 5310 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5315 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 744, column 9)
              _lhsOpairs =
                  {-# LINE 744 "./TypeChecking.ag" #-}
                  zip _attsIstrings $ getRowTypes _valsItypeList
                  {-# LINE 5320 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 738, column 9)
              _rowSetError =
                  {-# LINE 738 "./TypeChecking.ag" #-}
                  let atts = _attsIstrings
                      types = getRowTypes _valsItypeList
                  in if length atts /= length types
                       then Just WrongNumberOfColumns
                       else Nothing
                  {-# LINE 5329 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOrowSetError =
                  {-# LINE 731 "./TypeChecking.ag" #-}
                  _rowSetError
                  {-# LINE 5334 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOpairs,_lhsOrowSetError)))
sem_SetClause_SetClause :: Annotation ->
                           String ->
                           T_Expression  ->
                           T_SetClause 
sem_SetClause_SetClause ann_ att_ val_  =
    (\ _lhsIenv ->
         (let _valOenv :: Environment
              _valIannotatedTree :: Expression
              _valIliftedColumnName :: String
              _lhsOannotatedTree :: SetClause
              _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetError :: (Maybe TypeError)
              -- copy rule (down)
              _valOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5352 "AstInternal.hs" #-}
              ( _valIannotatedTree,_valIliftedColumnName) =
                  (val_ _valOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  SetClause ann_ att_ _valIannotatedTree
                  {-# LINE 5359 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5364 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 735, column 9)
              _lhsOpairs =
                  {-# LINE 735 "./TypeChecking.ag" #-}
                  [(att_, getTypeAnnotation _valIannotatedTree)]
                  {-# LINE 5369 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 736, column 9)
              _lhsOrowSetError =
                  {-# LINE 736 "./TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 5374 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOpairs,_lhsOrowSetError)))
-- SetClauseList -----------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                        ( SetClauseList,([(String,Type)]),([TypeError]))
data Inh_SetClauseList  = Inh_SetClauseList {env_Inh_SetClauseList :: Environment}
data Syn_SetClauseList  = Syn_SetClauseList {annotatedTree_Syn_SetClauseList :: SetClauseList,pairs_Syn_SetClauseList :: [(String,Type)],rowSetErrors_Syn_SetClauseList :: [TypeError]}
wrap_SetClauseList :: T_SetClauseList  ->
                      Inh_SetClauseList  ->
                      Syn_SetClauseList 
wrap_SetClauseList sem (Inh_SetClauseList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOpairs,_lhsOrowSetErrors) =
             (sem _lhsIenv )
     in  (Syn_SetClauseList _lhsOannotatedTree _lhsOpairs _lhsOrowSetErrors ))
sem_SetClauseList_Cons :: T_SetClause  ->
                          T_SetClauseList  ->
                          T_SetClauseList 
sem_SetClauseList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: SetClauseList
              _tlIpairs :: ([(String,Type)])
              _tlIrowSetErrors :: ([TypeError])
              _hdIannotatedTree :: SetClause
              _hdIpairs :: ([(String,Type)])
              _hdIrowSetError :: (Maybe TypeError)
              _lhsOannotatedTree :: SetClauseList
              _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetErrors :: ([TypeError])
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5433 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5438 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIpairs,_tlIrowSetErrors) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree,_hdIpairs,_hdIrowSetError) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5447 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5452 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 725, column 10)
              _lhsOpairs =
                  {-# LINE 725 "./TypeChecking.ag" #-}
                  _hdIpairs ++ _tlIpairs
                  {-# LINE 5457 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 726, column 10)
              _lhsOrowSetErrors =
                  {-# LINE 726 "./TypeChecking.ag" #-}
                  maybeToList _hdIrowSetError ++ _tlIrowSetErrors
                  {-# LINE 5462 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOpairs,_lhsOrowSetErrors)))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: SetClauseList
              _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetErrors :: ([TypeError])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5474 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5479 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 727, column 9)
              _lhsOpairs =
                  {-# LINE 727 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5484 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 728, column 9)
              _lhsOrowSetErrors =
                  {-# LINE 728 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5489 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOpairs,_lhsOrowSetErrors)))
-- Statement ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
            local envUpdates  : _
            local statementInfo : _
            local backTree    : _
            local tpe         : _
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
            local envUpdates  : _
            local statementInfo : _
            local backTree    : _
            local tpe         : _
      alternative CreateTable:
         child ann            : {Annotation}
         child name           : {String}
         child atts           : AttributeDefList 
         child cons           : ConstraintList 
         visit 0:
            local envUpdates  : _
            local statementInfo : _
            local backTree    : _
            local tpe         : _
      alternative CreateTableAs:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : SelectExpression 
         visit 0:
            local annotatedTree : _
            local selType     : _
            local attrs       : _
            local envUpdates  : _
      alternative CreateType:
         child ann            : {Annotation}
         child name           : {String}
         child atts           : TypeAttributeDefList 
         visit 0:
            local envUpdates  : _
            local statementInfo : _
            local backTree    : _
            local tpe         : _
      alternative CreateView:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : SelectExpression 
         visit 0:
            local attrs       : _
            local envUpdates  : _
            local statementInfo : _
            local backTree    : _
            local tpe         : _
      alternative Delete:
         child ann            : {Annotation}
         child table          : {String}
         child whr            : MaybeBoolExpression 
         child returning      : {Maybe SelectList}
         visit 0:
            local envUpdates  : _
            local backTree    : _
            local statementInfo : _
            local tpe         : _
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
            local envUpdates  : _
            local backTree    : _
            local columnStuff : _
            local statementInfo : _
            local tpe         : _
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
            local statementInfo : _
            local envUpdates  : _
            local backTree    : _
            local tpe         : _
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
            local envUpdates  : _
            local backTree    : _
            local statementInfo : _
            local tpe         : _
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
            local envUpdates  : _
            local backTree    : _
            local columnsConsistent : _
            local statementInfo : _
            local tpe         : _
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
                    ( Statement,([EnvironmentUpdate]))
data Inh_Statement  = Inh_Statement {env_Inh_Statement :: Environment}
data Syn_Statement  = Syn_Statement {annotatedTree_Syn_Statement :: Statement,envUpdates_Syn_Statement :: [EnvironmentUpdate]}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOenvUpdates) =
             (sem _lhsIenv )
     in  (Syn_Statement _lhsOannotatedTree _lhsOenvUpdates ))
sem_Statement_Assignment :: Annotation ->
                            String ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIenv ->
         (let _valueOenv :: Environment
              _valueIannotatedTree :: Expression
              _valueIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5849 "AstInternal.hs" #-}
              ( _valueIannotatedTree,_valueIliftedColumnName) =
                  (value_ _valueOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Assignment ann_ target_ _valueIannotatedTree
                  {-# LINE 5856 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5861 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5866 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_CaseStatement :: Annotation ->
                               T_Expression  ->
                               T_ExpressionListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement ann_ val_ cases_ els_  =
    (\ _lhsIenv ->
         (let _elsOenv :: Environment
              _casesOenv :: Environment
              _valOenv :: Environment
              _elsOenvUpdates :: ([EnvironmentUpdate])
              _elsIannotatedTree :: StatementList
              _elsIproducedEnv :: Environment
              _casesIannotatedTree :: ExpressionListStatementListPairList
              _valIannotatedTree :: Expression
              _valIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _elsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5890 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5895 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5900 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 352, column 24)
              _elsOenvUpdates =
                  {-# LINE 352 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5905 "AstInternal.hs" #-}
              ( _elsIannotatedTree,_elsIproducedEnv) =
                  (els_ _elsOenv _elsOenvUpdates )
              ( _casesIannotatedTree) =
                  (cases_ _casesOenv )
              ( _valIannotatedTree,_valIliftedColumnName) =
                  (val_ _valOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  CaseStatement ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 5916 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5921 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5926 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_ContinueStatement :: Annotation ->
                                   T_Statement 
sem_Statement_ContinueStatement ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ContinueStatement ann_
                  {-# LINE 5938 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5943 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5948 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_Copy :: Annotation ->
                      String ->
                      T_StringList  ->
                      T_CopySource  ->
                      T_Statement 
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIenv ->
         (let _sourceOenv :: Environment
              _sourceIannotatedTree :: CopySource
              _targetColsOenv :: Environment
              _targetColsIannotatedTree :: StringList
              _targetColsIstrings :: ([String])
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _sourceOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5968 "AstInternal.hs" #-}
              ( _sourceIannotatedTree) =
                  (source_ _sourceOenv )
              -- copy rule (down)
              _targetColsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5975 "AstInternal.hs" #-}
              ( _targetColsIannotatedTree,_targetColsIstrings) =
                  (targetCols_ _targetColsOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Copy ann_ table_ _targetColsIannotatedTree _sourceIannotatedTree
                  {-# LINE 5982 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5987 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 5992 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_CopyData :: Annotation ->
                          String ->
                          T_Statement 
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 6005 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6010 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6015 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_CreateDomain :: Annotation ->
                              String ->
                              T_TypeName  ->
                              T_MaybeBoolExpression  ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ check_  =
    (\ _lhsIenv ->
         (let _checkOenv :: Environment
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _checkIannotatedTree :: MaybeBoolExpression
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _checkOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6035 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6040 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
              -- "./TypeChecking.ag"(line 870, column 9)
              _envUpdates =
                  {-# LINE 870 "./TypeChecking.ag" #-}
                  [EnvCreateDomain (ScalarType name_) _typInamedType]
                  {-# LINE 6047 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 869, column 9)
              _statementInfo =
                  {-# LINE 869 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6052 "AstInternal.hs" #-}
              ( _checkIannotatedTree) =
                  (check_ _checkOenv )
              -- "./TypeChecking.ag"(line 868, column 9)
              _backTree =
                  {-# LINE 868 "./TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree _checkIannotatedTree
                  {-# LINE 6059 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 867, column 9)
              _tpe =
                  {-# LINE 867 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 6064 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 310, column 9)
              _lhsOannotatedTree =
                  {-# LINE 310 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6073 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 315, column 9)
              _lhsOenvUpdates =
                  {-# LINE 315 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6078 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
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
         (let _rettypeOenv :: Environment
              _paramsOenv :: Environment
              _paramsIannotatedTree :: ParamDefList
              _paramsIparams :: ([(String, Type)])
              _bodyOenv :: Environment
              _rettypeIannotatedTree :: TypeName
              _rettypeInamedType :: Type
              _volOenv :: Environment
              _volIannotatedTree :: Volatility
              _bodyIannotatedTree :: FnBody
              _langOenv :: Environment
              _langIannotatedTree :: Language
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _rettypeOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6109 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6114 "AstInternal.hs" #-}
              ( _paramsIannotatedTree,_paramsIparams) =
                  (params_ _paramsOenv )
              -- "./TypeChecking.ag"(line 932, column 9)
              _bodyOenv =
                  {-# LINE 932 "./TypeChecking.ag" #-}
                  fromRight _lhsIenv $
                  updateEnvironment _lhsIenv [EnvStackIDs [("", _paramsIparams)
                                                          ,(name_, _paramsIparams)]]
                  {-# LINE 6123 "AstInternal.hs" #-}
              ( _rettypeIannotatedTree,_rettypeInamedType) =
                  (rettype_ _rettypeOenv )
              -- "./TypeChecking.ag"(line 930, column 9)
              _envUpdates =
                  {-# LINE 930 "./TypeChecking.ag" #-}
                  [EnvCreateFunction FunName name_ (map snd _paramsIparams) _rettypeInamedType]
                  {-# LINE 6130 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 929, column 9)
              _statementInfo =
                  {-# LINE 929 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6135 "AstInternal.hs" #-}
              -- copy rule (down)
              _volOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6140 "AstInternal.hs" #-}
              ( _volIannotatedTree) =
                  (vol_ _volOenv )
              ( _bodyIannotatedTree) =
                  (body_ _bodyOenv )
              -- copy rule (down)
              _langOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6149 "AstInternal.hs" #-}
              ( _langIannotatedTree) =
                  (lang_ _langOenv )
              -- "./TypeChecking.ag"(line 921, column 9)
              _backTree =
                  {-# LINE 921 "./TypeChecking.ag" #-}
                  CreateFunction ann_
                                 _langIannotatedTree
                                 name_
                                 _paramsIannotatedTree
                                 _rettypeIannotatedTree
                                 bodyQuote_
                                 _bodyIannotatedTree
                                 _volIannotatedTree
                  {-# LINE 6163 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 920, column 9)
              _tpe =
                  {-# LINE 920 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 6168 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 310, column 9)
              _lhsOannotatedTree =
                  {-# LINE 310 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6177 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 315, column 9)
              _lhsOenvUpdates =
                  {-# LINE 315 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6182 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_CreateTable :: Annotation ->
                             String ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIenv ->
         (let _consOenv :: Environment
              _attsOenv :: Environment
              _attsIannotatedTree :: AttributeDefList
              _attsIattrs :: ([(String, Type)])
              _consIannotatedTree :: ConstraintList
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _consOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6202 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6207 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIattrs) =
                  (atts_ _attsOenv )
              -- "./TypeChecking.ag"(line 799, column 9)
              _envUpdates =
                  {-# LINE 799 "./TypeChecking.ag" #-}
                  [EnvCreateTable name_ _attsIattrs []]
                  {-# LINE 6214 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 798, column 9)
              _statementInfo =
                  {-# LINE 798 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6219 "AstInternal.hs" #-}
              ( _consIannotatedTree) =
                  (cons_ _consOenv )
              -- "./TypeChecking.ag"(line 797, column 9)
              _backTree =
                  {-# LINE 797 "./TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
                  {-# LINE 6226 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 796, column 9)
              _tpe =
                  {-# LINE 796 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 6231 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 310, column 9)
              _lhsOannotatedTree =
                  {-# LINE 310 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6240 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 315, column 9)
              _lhsOenvUpdates =
                  {-# LINE 315 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6245 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_CreateTableAs :: Annotation ->
                               String ->
                               T_SelectExpression  ->
                               T_Statement 
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIenv ->
         (let _exprOenv :: Environment
              _exprIannotatedTree :: SelectExpression
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6261 "AstInternal.hs" #-}
              ( _exprIannotatedTree) =
                  (expr_ _exprOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 6268 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6273 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 803, column 9)
              _selType =
                  {-# LINE 803 "./TypeChecking.ag" #-}
                  getTypeAnnotation _exprIannotatedTree
                  {-# LINE 6278 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 807, column 9)
              _attrs =
                  {-# LINE 807 "./TypeChecking.ag" #-}
                  case _selType     of
                    UnnamedCompositeType c -> c
                    _-> []
                  {-# LINE 6285 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 810, column 9)
              _envUpdates =
                  {-# LINE 810 "./TypeChecking.ag" #-}
                  [EnvCreateTable name_ _attrs     []]
                  {-# LINE 6290 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOenvUpdates =
                  {-# LINE 317 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6295 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_CreateType :: Annotation ->
                            String ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIenv ->
         (let _attsOenv :: Environment
              _attsIannotatedTree :: TypeAttributeDefList
              _attsIattrs :: ([(String, Type)])
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6312 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIattrs) =
                  (atts_ _attsOenv )
              -- "./TypeChecking.ag"(line 855, column 9)
              _envUpdates =
                  {-# LINE 855 "./TypeChecking.ag" #-}
                  [EnvCreateComposite name_ _attsIattrs]
                  {-# LINE 6319 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 854, column 9)
              _statementInfo =
                  {-# LINE 854 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6324 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 853, column 9)
              _backTree =
                  {-# LINE 853 "./TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 6329 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 852, column 9)
              _tpe =
                  {-# LINE 852 "./TypeChecking.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 6334 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 310, column 9)
              _lhsOannotatedTree =
                  {-# LINE 310 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6343 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 315, column 9)
              _lhsOenvUpdates =
                  {-# LINE 315 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6348 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_CreateView :: Annotation ->
                            String ->
                            T_SelectExpression  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ expr_  =
    (\ _lhsIenv ->
         (let _exprOenv :: Environment
              _exprIannotatedTree :: SelectExpression
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6364 "AstInternal.hs" #-}
              ( _exprIannotatedTree) =
                  (expr_ _exprOenv )
              -- "./TypeChecking.ag"(line 823, column 9)
              _attrs =
                  {-# LINE 823 "./TypeChecking.ag" #-}
                  case getTypeAnnotation _exprIannotatedTree of
                    SetOfType (UnnamedCompositeType c) -> c
                    _ -> []
                  {-# LINE 6373 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 826, column 9)
              _envUpdates =
                  {-# LINE 826 "./TypeChecking.ag" #-}
                  [EnvCreateView name_ _attrs    ]
                  {-# LINE 6378 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 822, column 9)
              _statementInfo =
                  {-# LINE 822 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6383 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 821, column 9)
              _backTree =
                  {-# LINE 821 "./TypeChecking.ag" #-}
                  CreateView ann_ name_ _exprIannotatedTree
                  {-# LINE 6388 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 820, column 9)
              _tpe =
                  {-# LINE 820 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed [getTypeAnnotation _exprIannotatedTree] $ Right $ Pseudo Void
                  {-# LINE 6393 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 310, column 9)
              _lhsOannotatedTree =
                  {-# LINE 310 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6402 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 315, column 9)
              _lhsOenvUpdates =
                  {-# LINE 315 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6407 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_Delete :: Annotation ->
                        String ->
                        T_MaybeBoolExpression  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Delete ann_ table_ whr_ returning_  =
    (\ _lhsIenv ->
         (let _whrOenv :: Environment
              _whrIannotatedTree :: MaybeBoolExpression
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _whrOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6424 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 766, column 9)
              _envUpdates =
                  {-# LINE 766 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6429 "AstInternal.hs" #-}
              ( _whrIannotatedTree) =
                  (whr_ _whrOenv )
              -- "./TypeChecking.ag"(line 765, column 9)
              _backTree =
                  {-# LINE 765 "./TypeChecking.ag" #-}
                  Delete ann_ table_ _whrIannotatedTree returning_
                  {-# LINE 6436 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 764, column 9)
              _statementInfo =
                  {-# LINE 764 "./TypeChecking.ag" #-}
                  [DeleteInfo table_]
                  {-# LINE 6441 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 760, column 9)
              _tpe =
                  {-# LINE 760 "./TypeChecking.ag" #-}
                  case checkRelationExists _lhsIenv table_ of
                    Just e -> Left [e]
                    Nothing -> Right $ Pseudo Void
                  {-# LINE 6448 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 310, column 9)
              _lhsOannotatedTree =
                  {-# LINE 310 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6457 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 315, column 9)
              _lhsOenvUpdates =
                  {-# LINE 315 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6462 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_DropFunction :: Annotation ->
                              T_IfExists  ->
                              T_StringStringListPairList  ->
                              T_Cascade  ->
                              T_Statement 
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIenv ->
         (let _cascadeOenv :: Environment
              _cascadeIannotatedTree :: Cascade
              _sigsOenv :: Environment
              _sigsIannotatedTree :: StringStringListPairList
              _ifEOenv :: Environment
              _ifEIannotatedTree :: IfExists
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _cascadeOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6483 "AstInternal.hs" #-}
              ( _cascadeIannotatedTree) =
                  (cascade_ _cascadeOenv )
              -- copy rule (down)
              _sigsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6490 "AstInternal.hs" #-}
              ( _sigsIannotatedTree) =
                  (sigs_ _sigsOenv )
              -- copy rule (down)
              _ifEOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6497 "AstInternal.hs" #-}
              ( _ifEIannotatedTree) =
                  (ifE_ _ifEOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  DropFunction ann_ _ifEIannotatedTree _sigsIannotatedTree _cascadeIannotatedTree
                  {-# LINE 6504 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6509 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6514 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_DropSomething :: Annotation ->
                               T_DropType  ->
                               T_IfExists  ->
                               T_StringList  ->
                               T_Cascade  ->
                               T_Statement 
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIenv ->
         (let _cascadeOenv :: Environment
              _cascadeIannotatedTree :: Cascade
              _namesOenv :: Environment
              _namesIannotatedTree :: StringList
              _namesIstrings :: ([String])
              _ifEOenv :: Environment
              _ifEIannotatedTree :: IfExists
              _dropTypeOenv :: Environment
              _dropTypeIannotatedTree :: DropType
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _cascadeOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6539 "AstInternal.hs" #-}
              ( _cascadeIannotatedTree) =
                  (cascade_ _cascadeOenv )
              -- copy rule (down)
              _namesOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6546 "AstInternal.hs" #-}
              ( _namesIannotatedTree,_namesIstrings) =
                  (names_ _namesOenv )
              -- copy rule (down)
              _ifEOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6553 "AstInternal.hs" #-}
              ( _ifEIannotatedTree) =
                  (ifE_ _ifEOenv )
              -- copy rule (down)
              _dropTypeOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6560 "AstInternal.hs" #-}
              ( _dropTypeIannotatedTree) =
                  (dropType_ _dropTypeOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  DropSomething ann_ _dropTypeIannotatedTree _ifEIannotatedTree _namesIannotatedTree _cascadeIannotatedTree
                  {-# LINE 6567 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6572 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6577 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_Execute :: Annotation ->
                         T_Expression  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIenv ->
         (let _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6593 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Execute ann_ _exprIannotatedTree
                  {-# LINE 6600 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6605 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6610 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_ExecuteInto :: Annotation ->
                             T_Expression  ->
                             T_StringList  ->
                             T_Statement 
sem_Statement_ExecuteInto ann_ expr_ targets_  =
    (\ _lhsIenv ->
         (let _exprOenv :: Environment
              _targetsOenv :: Environment
              _targetsIannotatedTree :: StringList
              _targetsIstrings :: ([String])
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6630 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6635 "AstInternal.hs" #-}
              ( _targetsIannotatedTree,_targetsIstrings) =
                  (targets_ _targetsOenv )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIannotatedTree _targetsIannotatedTree
                  {-# LINE 6644 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6649 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6654 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_ForIntegerStatement :: Annotation ->
                                     String ->
                                     T_Expression  ->
                                     T_Expression  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement ann_ var_ from_ to_ sts_  =
    (\ _lhsIenv ->
         (let _stsOenv :: Environment
              _toOenv :: Environment
              _fromOenv :: Environment
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsIannotatedTree :: StatementList
              _stsIproducedEnv :: Environment
              _toIannotatedTree :: Expression
              _toIliftedColumnName :: String
              _fromIannotatedTree :: Expression
              _fromIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6680 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6685 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6690 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 355, column 10)
              _stsOenvUpdates =
                  {-# LINE 355 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6695 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIproducedEnv) =
                  (sts_ _stsOenv _stsOenvUpdates )
              ( _toIannotatedTree,_toIliftedColumnName) =
                  (to_ _toOenv )
              ( _fromIannotatedTree,_fromIliftedColumnName) =
                  (from_ _fromOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ForIntegerStatement ann_ var_ _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 6706 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6711 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6716 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_ForSelectStatement :: Annotation ->
                                    String ->
                                    T_SelectExpression  ->
                                    T_StatementList  ->
                                    T_Statement 
sem_Statement_ForSelectStatement ann_ var_ sel_ sts_  =
    (\ _lhsIenv ->
         (let _stsOenv :: Environment
              _selOenv :: Environment
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsIannotatedTree :: StatementList
              _stsIproducedEnv :: Environment
              _selIannotatedTree :: SelectExpression
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6737 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6742 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 355, column 10)
              _stsOenvUpdates =
                  {-# LINE 355 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6747 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIproducedEnv) =
                  (sts_ _stsOenv _stsOenvUpdates )
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ForSelectStatement ann_ var_ _selIannotatedTree _stsIannotatedTree
                  {-# LINE 6756 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6761 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6766 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_If :: Annotation ->
                    T_ExpressionStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIenv ->
         (let _elsOenv :: Environment
              _casesOenv :: Environment
              _elsOenvUpdates :: ([EnvironmentUpdate])
              _elsIannotatedTree :: StatementList
              _elsIproducedEnv :: Environment
              _casesIannotatedTree :: ExpressionStatementListPairList
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _elsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6786 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6791 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 352, column 24)
              _elsOenvUpdates =
                  {-# LINE 352 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6796 "AstInternal.hs" #-}
              ( _elsIannotatedTree,_elsIproducedEnv) =
                  (els_ _elsOenv _elsOenvUpdates )
              ( _casesIannotatedTree) =
                  (cases_ _casesOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  If ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 6805 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6810 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6815 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_Insert :: Annotation ->
                        String ->
                        T_StringList  ->
                        T_SelectExpression  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIenv ->
         (let _insDataOenv :: Environment
              _insDataIannotatedTree :: SelectExpression
              _targetColsOenv :: Environment
              _targetColsIannotatedTree :: StringList
              _targetColsIstrings :: ([String])
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _insDataOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6836 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 689, column 9)
              _envUpdates =
                  {-# LINE 689 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6841 "AstInternal.hs" #-}
              ( _insDataIannotatedTree) =
                  (insData_ _insDataOenv )
              -- copy rule (down)
              _targetColsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6848 "AstInternal.hs" #-}
              ( _targetColsIannotatedTree,_targetColsIstrings) =
                  (targetCols_ _targetColsOenv )
              -- "./TypeChecking.ag"(line 687, column 9)
              _backTree =
                  {-# LINE 687 "./TypeChecking.ag" #-}
                  Insert ann_ table_ _targetColsIannotatedTree
                         _insDataIannotatedTree returning_
                  {-# LINE 6856 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 676, column 9)
              _columnStuff =
                  {-# LINE 676 "./TypeChecking.ag" #-}
                  checkColumnConsistency _lhsIenv
                                         table_
                                         _targetColsIstrings
                                         (getCAtts $ getTypeAnnotation _insDataIannotatedTree)
                  {-# LINE 6864 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 685, column 9)
              _statementInfo =
                  {-# LINE 685 "./TypeChecking.ag" #-}
                  [InsertInfo table_ $ errorToTypeFailF UnnamedCompositeType _columnStuff    ]
                  {-# LINE 6869 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 681, column 9)
              _tpe =
                  {-# LINE 681 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed [getTypeAnnotation _insDataIannotatedTree] $ do
                    _columnStuff
                    Right $ Pseudo Void
                  {-# LINE 6876 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 310, column 9)
              _lhsOannotatedTree =
                  {-# LINE 310 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 6885 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 315, column 9)
              _lhsOenvUpdates =
                  {-# LINE 315 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 6890 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_NullStatement :: Annotation ->
                               T_Statement 
sem_Statement_NullStatement ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 6902 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6907 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6912 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_Perform :: Annotation ->
                         T_Expression  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIenv ->
         (let _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6928 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Perform ann_ _exprIannotatedTree
                  {-# LINE 6935 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6940 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6945 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_Raise :: Annotation ->
                       T_RaiseType  ->
                       String ->
                       T_ExpressionList  ->
                       T_Statement 
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIenv ->
         (let _argsOenv :: Environment
              _argsIannotatedTree :: ExpressionList
              _argsItypeList :: ([Type])
              _levelOenv :: Environment
              _levelIannotatedTree :: RaiseType
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _argsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6965 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsItypeList) =
                  (args_ _argsOenv )
              -- copy rule (down)
              _levelOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6972 "AstInternal.hs" #-}
              ( _levelIannotatedTree) =
                  (level_ _levelOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Raise ann_ _levelIannotatedTree message_ _argsIannotatedTree
                  {-# LINE 6979 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6984 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 6989 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_Return :: Annotation ->
                        T_MaybeExpression  ->
                        T_Statement 
sem_Statement_Return ann_ value_  =
    (\ _lhsIenv ->
         (let _valueOenv :: Environment
              _valueIannotatedTree :: MaybeExpression
              _valueIexprType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7005 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 987, column 9)
              _statementInfo =
                  {-# LINE 987 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7010 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 986, column 9)
              _envUpdates =
                  {-# LINE 986 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7015 "AstInternal.hs" #-}
              ( _valueIannotatedTree,_valueIexprType) =
                  (value_ _valueOenv )
              -- "./TypeChecking.ag"(line 985, column 9)
              _backTree =
                  {-# LINE 985 "./TypeChecking.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 7022 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 984, column 9)
              _tpe =
                  {-# LINE 984 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed [fromMaybe typeBool _valueIexprType] $ Right $ Pseudo Void
                  {-# LINE 7027 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 310, column 9)
              _lhsOannotatedTree =
                  {-# LINE 310 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 7036 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 315, column 9)
              _lhsOenvUpdates =
                  {-# LINE 315 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7041 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_ReturnNext :: Annotation ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIenv ->
         (let _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7057 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIannotatedTree
                  {-# LINE 7064 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7069 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7074 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_ReturnQuery :: Annotation ->
                             T_SelectExpression  ->
                             T_Statement 
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIenv ->
         (let _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7089 "AstInternal.hs" #-}
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIannotatedTree
                  {-# LINE 7096 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7101 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7106 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_SelectStatement :: Annotation ->
                                 T_SelectExpression  ->
                                 T_Statement 
sem_Statement_SelectStatement ann_ ex_  =
    (\ _lhsIenv ->
         (let _exOenv :: Environment
              _exIannotatedTree :: SelectExpression
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _exOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7121 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 374, column 9)
              _envUpdates =
                  {-# LINE 374 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7126 "AstInternal.hs" #-}
              ( _exIannotatedTree) =
                  (ex_ _exOenv )
              -- "./TypeChecking.ag"(line 373, column 9)
              _backTree =
                  {-# LINE 373 "./TypeChecking.ag" #-}
                  SelectStatement ann_ _exIannotatedTree
                  {-# LINE 7133 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 372, column 9)
              _statementInfo =
                  {-# LINE 372 "./TypeChecking.ag" #-}
                  [SelectInfo $ getTypeAnnotation _exIannotatedTree]
                  {-# LINE 7138 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 371, column 9)
              _tpe =
                  {-# LINE 371 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed [getTypeAnnotation _exIannotatedTree] $ Right $ Pseudo Void
                  {-# LINE 7143 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 310, column 9)
              _lhsOannotatedTree =
                  {-# LINE 310 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 7152 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 315, column 9)
              _lhsOenvUpdates =
                  {-# LINE 315 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7157 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_Truncate :: Annotation ->
                          T_StringList  ->
                          T_RestartIdentity  ->
                          T_Cascade  ->
                          T_Statement 
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIenv ->
         (let _cascadeOenv :: Environment
              _cascadeIannotatedTree :: Cascade
              _restartIdentityOenv :: Environment
              _restartIdentityIannotatedTree :: RestartIdentity
              _tablesOenv :: Environment
              _tablesIannotatedTree :: StringList
              _tablesIstrings :: ([String])
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _cascadeOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7179 "AstInternal.hs" #-}
              ( _cascadeIannotatedTree) =
                  (cascade_ _cascadeOenv )
              -- copy rule (down)
              _restartIdentityOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7186 "AstInternal.hs" #-}
              ( _restartIdentityIannotatedTree) =
                  (restartIdentity_ _restartIdentityOenv )
              -- copy rule (down)
              _tablesOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7193 "AstInternal.hs" #-}
              ( _tablesIannotatedTree,_tablesIstrings) =
                  (tables_ _tablesOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Truncate ann_ _tablesIannotatedTree _restartIdentityIannotatedTree _cascadeIannotatedTree
                  {-# LINE 7200 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7205 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7210 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_Update :: Annotation ->
                        String ->
                        T_SetClauseList  ->
                        T_MaybeBoolExpression  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Update ann_ table_ assigns_ whr_ returning_  =
    (\ _lhsIenv ->
         (let _whrOenv :: Environment
              _assignsOenv :: Environment
              _whrIannotatedTree :: MaybeBoolExpression
              _assignsIannotatedTree :: SetClauseList
              _assignsIpairs :: ([(String,Type)])
              _assignsIrowSetErrors :: ([TypeError])
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _whrOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7232 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7237 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 719, column 9)
              _envUpdates =
                  {-# LINE 719 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7242 "AstInternal.hs" #-}
              ( _whrIannotatedTree) =
                  (whr_ _whrOenv )
              ( _assignsIannotatedTree,_assignsIpairs,_assignsIrowSetErrors) =
                  (assigns_ _assignsOenv )
              -- "./TypeChecking.ag"(line 718, column 9)
              _backTree =
                  {-# LINE 718 "./TypeChecking.ag" #-}
                  Update ann_ table_ _assignsIannotatedTree _whrIannotatedTree returning_
                  {-# LINE 7251 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 714, column 9)
              _columnsConsistent =
                  {-# LINE 714 "./TypeChecking.ag" #-}
                  checkColumnConsistency _lhsIenv table_ (map fst _assignsIpairs) _assignsIpairs
                  {-# LINE 7256 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 716, column 9)
              _statementInfo =
                  {-# LINE 716 "./TypeChecking.ag" #-}
                  [UpdateInfo table_ $ errorToTypeFailF UnnamedCompositeType _columnsConsistent    ]
                  {-# LINE 7261 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 706, column 9)
              _tpe =
                  {-# LINE 706 "./TypeChecking.ag" #-}
                  do
                  let re = checkRelationExists _lhsIenv table_
                  when (isJust re) $
                       Left [fromJust $ re]
                  chainTypeCheckFailed (map snd _assignsIpairs) $ do
                    _columnsConsistent
                    checkErrorList _assignsIrowSetErrors $ Pseudo Void
                  {-# LINE 7272 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 310, column 9)
              _lhsOannotatedTree =
                  {-# LINE 310 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 7281 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 315, column 9)
              _lhsOenvUpdates =
                  {-# LINE 315 "./TypeChecking.ag" #-}
                  _envUpdates
                  {-# LINE 7286 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
sem_Statement_WhileStatement :: Annotation ->
                                T_Expression  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement ann_ expr_ sts_  =
    (\ _lhsIenv ->
         (let _stsOenv :: Environment
              _exprOenv :: Environment
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsIannotatedTree :: StatementList
              _stsIproducedEnv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7307 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7312 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 355, column 10)
              _stsOenvUpdates =
                  {-# LINE 355 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7317 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIproducedEnv) =
                  (sts_ _stsOenv _stsOenvUpdates )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  WhileStatement ann_ _exprIannotatedTree _stsIannotatedTree
                  {-# LINE 7326 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7331 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 324, column 9)
              _lhsOenvUpdates =
                  {-# LINE 324 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7336 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOenvUpdates)))
-- StatementList -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         envUpdates           : [EnvironmentUpdate]
      synthesized attributes:
         annotatedTree        : SELF 
         producedEnv          : Environment
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
                        ( StatementList,Environment)
data Inh_StatementList  = Inh_StatementList {env_Inh_StatementList :: Environment,envUpdates_Inh_StatementList :: [EnvironmentUpdate]}
data Syn_StatementList  = Syn_StatementList {annotatedTree_Syn_StatementList :: StatementList,producedEnv_Syn_StatementList :: Environment}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIenv _lhsIenvUpdates )  =
    (let ( _lhsOannotatedTree,_lhsOproducedEnv) =
             (sem _lhsIenv _lhsIenvUpdates )
     in  (Syn_StatementList _lhsOannotatedTree _lhsOproducedEnv ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIenvUpdates ->
         (let _hdOenv :: Environment
              _hdIannotatedTree :: Statement
              _hdIenvUpdates :: ([EnvironmentUpdate])
              _tlOenvUpdates :: ([EnvironmentUpdate])
              _tlOenv :: Environment
              _tlIannotatedTree :: StatementList
              _tlIproducedEnv :: Environment
              _lhsOannotatedTree :: StatementList
              _lhsOproducedEnv :: Environment
              -- "./TypeChecking.ag"(line 334, column 9)
              _newEnv =
                  {-# LINE 334 "./TypeChecking.ag" #-}
                  fromRight _lhsIenv $ updateEnvironment _lhsIenv _lhsIenvUpdates
                  {-# LINE 7396 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 335, column 9)
              _hdOenv =
                  {-# LINE 335 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 7401 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIenvUpdates) =
                  (hd_ _hdOenv )
              -- "./TypeChecking.ag"(line 340, column 9)
              _tlOenvUpdates =
                  {-# LINE 340 "./TypeChecking.ag" #-}
                  _hdIenvUpdates
                  {-# LINE 7408 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 336, column 9)
              _tlOenv =
                  {-# LINE 336 "./TypeChecking.ag" #-}
                  _newEnv
                  {-# LINE 7413 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIproducedEnv) =
                  (tl_ _tlOenv _tlOenvUpdates )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 7420 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7425 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 337, column 9)
              _lhsOproducedEnv =
                  {-# LINE 337 "./TypeChecking.ag" #-}
                  case _tlIannotatedTree of
                   [] -> _newEnv
                   _ -> _tlIproducedEnv
                  {-# LINE 7432 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOproducedEnv)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIenv
       _lhsIenvUpdates ->
         (let _lhsOannotatedTree :: StatementList
              _lhsOproducedEnv :: Environment
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7444 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7449 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 342, column 9)
              _lhsOproducedEnv =
                  {-# LINE 342 "./TypeChecking.ag" #-}
                  emptyEnvironment
                  {-# LINE 7454 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOproducedEnv)))
-- StringList --------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                     ( StringList,([String]))
data Inh_StringList  = Inh_StringList {env_Inh_StringList :: Environment}
data Syn_StringList  = Syn_StringList {annotatedTree_Syn_StringList :: StringList,strings_Syn_StringList :: [String]}
wrap_StringList :: T_StringList  ->
                   Inh_StringList  ->
                   Syn_StringList 
wrap_StringList sem (Inh_StringList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOstrings) =
             (sem _lhsIenv )
     in  (Syn_StringList _lhsOannotatedTree _lhsOstrings ))
sem_StringList_Cons :: String ->
                       T_StringList  ->
                       T_StringList 
sem_StringList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _tlIannotatedTree :: StringList
              _tlIstrings :: ([String])
              _lhsOannotatedTree :: StringList
              _lhsOstrings :: ([String])
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7506 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIstrings) =
                  (tl_ _tlOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) hd_ _tlIannotatedTree
                  {-# LINE 7513 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7518 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 694, column 10)
              _lhsOstrings =
                  {-# LINE 694 "./TypeChecking.ag" #-}
                  hd_ : _tlIstrings
                  {-# LINE 7523 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOstrings)))
sem_StringList_Nil :: T_StringList 
sem_StringList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: StringList
              _lhsOstrings :: ([String])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7534 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7539 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 695, column 9)
              _lhsOstrings =
                  {-# LINE 695 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7544 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOstrings)))
-- StringStringListPair ----------------------------------------
{-
   visit 0:
      inherited attribute:
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
                               ( StringStringListPair)
data Inh_StringStringListPair  = Inh_StringStringListPair {env_Inh_StringStringListPair :: Environment}
data Syn_StringStringListPair  = Syn_StringStringListPair {annotatedTree_Syn_StringStringListPair :: StringStringListPair}
wrap_StringStringListPair :: T_StringStringListPair  ->
                             Inh_StringStringListPair  ->
                             Syn_StringStringListPair 
wrap_StringStringListPair sem (Inh_StringStringListPair _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_StringStringListPair _lhsOannotatedTree ))
sem_StringStringListPair_Tuple :: String ->
                                  T_StringList  ->
                                  T_StringStringListPair 
sem_StringStringListPair_Tuple x1_ x2_  =
    (\ _lhsIenv ->
         (let _x2Oenv :: Environment
              _x2IannotatedTree :: StringList
              _x2Istrings :: ([String])
              _lhsOannotatedTree :: StringStringListPair
              -- copy rule (down)
              _x2Oenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7591 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2Istrings) =
                  (x2_ _x2Oenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (x1_,_x2IannotatedTree)
                  {-# LINE 7598 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7603 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- StringStringListPairList ------------------------------------
{-
   visit 0:
      inherited attribute:
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
                                   ( StringStringListPairList)
data Inh_StringStringListPairList  = Inh_StringStringListPairList {env_Inh_StringStringListPairList :: Environment}
data Syn_StringStringListPairList  = Syn_StringStringListPairList {annotatedTree_Syn_StringStringListPairList :: StringStringListPairList}
wrap_StringStringListPairList :: T_StringStringListPairList  ->
                                 Inh_StringStringListPairList  ->
                                 Syn_StringStringListPairList 
wrap_StringStringListPairList sem (Inh_StringStringListPairList _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_StringStringListPairList _lhsOannotatedTree ))
sem_StringStringListPairList_Cons :: T_StringStringListPair  ->
                                     T_StringStringListPairList  ->
                                     T_StringStringListPairList 
sem_StringStringListPairList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _tlIannotatedTree :: StringStringListPairList
              _hdOenv :: Environment
              _hdIannotatedTree :: StringStringListPair
              _lhsOannotatedTree :: StringStringListPairList
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7654 "AstInternal.hs" #-}
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7661 "AstInternal.hs" #-}
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 7668 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7673 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_StringStringListPairList_Nil :: T_StringStringListPairList 
sem_StringStringListPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: StringStringListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7683 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7688 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- TableRef ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
            local backTree    : _
            local tpe         : _
      alternative SubTref:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         child alias          : {String}
         visit 0:
            local backTree    : _
            local tpe         : _
      alternative Tref:
         child ann            : {Annotation}
         child tbl            : {String}
         visit 0:
            local backTree    : _
            local relType     : _
            local tpe         : _
            local unwrappedRelType : _
      alternative TrefAlias:
         child ann            : {Annotation}
         child tbl            : {String}
         child alias          : {String}
         visit 0:
            local backTree    : _
            local relType     : _
            local tpe         : _
            local unwrappedRelType : _
      alternative TrefFun:
         child ann            : {Annotation}
         child fn             : Expression 
         visit 0:
            local backTree    : _
            local alias1      : _
            local tpe         : _
      alternative TrefFunAlias:
         child ann            : {Annotation}
         child fn             : Expression 
         child alias          : {String}
         visit 0:
            local backTree    : _
            local alias1      : _
            local tpe         : _
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
                   ( TableRef,([(String,([(String,Type)],[(String,Type)]))]),([String]))
data Inh_TableRef  = Inh_TableRef {env_Inh_TableRef :: Environment}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef,idens_Syn_TableRef :: [(String,([(String,Type)],[(String,Type)]))],joinIdens_Syn_TableRef :: [String]}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens) =
             (sem _lhsIenv )
     in  (Syn_TableRef _lhsOannotatedTree _lhsOidens _lhsOjoinIdens ))
sem_TableRef_JoinedTref :: Annotation ->
                           T_TableRef  ->
                           T_Natural  ->
                           T_JoinType  ->
                           T_TableRef  ->
                           T_OnExpr  ->
                           T_TableRef 
sem_TableRef_JoinedTref ann_ tbl_ nat_ joinType_ tbl1_ onExpr_  =
    (\ _lhsIenv ->
         (let _onExprOenv :: Environment
              _tbl1Oenv :: Environment
              _tblOenv :: Environment
              _onExprIannotatedTree :: OnExpr
              _tbl1IannotatedTree :: TableRef
              _tbl1Iidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _tbl1IjoinIdens :: ([String])
              _joinTypeOenv :: Environment
              _joinTypeIannotatedTree :: JoinType
              _natOenv :: Environment
              _natIannotatedTree :: Natural
              _tblIannotatedTree :: TableRef
              _tblIidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _tblIjoinIdens :: ([String])
              _lhsOannotatedTree :: TableRef
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              -- copy rule (down)
              _onExprOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7814 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1Oenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7819 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7824 "AstInternal.hs" #-}
              ( _onExprIannotatedTree) =
                  (onExpr_ _onExprOenv )
              ( _tbl1IannotatedTree,_tbl1Iidens,_tbl1IjoinIdens) =
                  (tbl1_ _tbl1Oenv )
              -- copy rule (down)
              _joinTypeOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7833 "AstInternal.hs" #-}
              ( _joinTypeIannotatedTree) =
                  (joinType_ _joinTypeOenv )
              -- copy rule (down)
              _natOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7840 "AstInternal.hs" #-}
              ( _natIannotatedTree) =
                  (nat_ _natOenv )
              ( _tblIannotatedTree,_tblIidens,_tblIjoinIdens) =
                  (tbl_ _tblOenv )
              -- "./TypeChecking.ag"(line 489, column 9)
              _backTree =
                  {-# LINE 489 "./TypeChecking.ag" #-}
                  JoinedTref ann_
                             _tblIannotatedTree
                             _natIannotatedTree
                             _joinTypeIannotatedTree
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
                  {-# LINE 7854 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 473, column 9)
              _tpe =
                  {-# LINE 473 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed [tblt
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
                  {-# LINE 7870 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 430, column 9)
              _lhsOannotatedTree =
                  {-# LINE 430 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7878 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 486, column 9)
              _lhsOidens =
                  {-# LINE 486 "./TypeChecking.ag" #-}
                  _tblIidens ++ _tbl1Iidens
                  {-# LINE 7883 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 487, column 9)
              _lhsOjoinIdens =
                  {-# LINE 487 "./TypeChecking.ag" #-}
                  commonFieldNames (getTypeAnnotation _tblIannotatedTree)
                                   (getTypeAnnotation _tbl1IannotatedTree)
                  {-# LINE 7889 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_SubTref :: Annotation ->
                        T_SelectExpression  ->
                        String ->
                        T_TableRef 
sem_TableRef_SubTref ann_ sel_ alias_  =
    (\ _lhsIenv ->
         (let _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _lhsOannotatedTree :: TableRef
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7906 "AstInternal.hs" #-}
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
              -- "./TypeChecking.ag"(line 439, column 15)
              _backTree =
                  {-# LINE 439 "./TypeChecking.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 7913 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 437, column 15)
              _tpe =
                  {-# LINE 437 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed [getTypeAnnotation _selIannotatedTree] <$>
                  unwrapSetOfWhenComposite $ getTypeAnnotation _selIannotatedTree
                  {-# LINE 7919 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 430, column 9)
              _lhsOannotatedTree =
                  {-# LINE 430 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7927 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 440, column 15)
              _lhsOidens =
                  {-# LINE 440 "./TypeChecking.ag" #-}
                  [(alias_, (fromRight [] $ getTbCols _selIannotatedTree, []))]
                  {-# LINE 7932 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 441, column 15)
              _lhsOjoinIdens =
                  {-# LINE 441 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7937 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_Tref :: Annotation ->
                     String ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              -- "./TypeChecking.ag"(line 454, column 9)
              _backTree =
                  {-# LINE 454 "./TypeChecking.ag" #-}
                  Tref ann_ tbl_
                  {-# LINE 7951 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 445, column 9)
              _relType =
                  {-# LINE 445 "./TypeChecking.ag" #-}
                  getRelationType _lhsIenv tbl_
                  {-# LINE 7956 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 443, column 9)
              _tpe =
                  {-# LINE 443 "./TypeChecking.ag" #-}
                  either Left (Right . fst) _relType
                  {-# LINE 7961 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 430, column 9)
              _lhsOannotatedTree =
                  {-# LINE 430 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7969 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 446, column 9)
              _unwrappedRelType =
                  {-# LINE 446 "./TypeChecking.ag" #-}
                  fromRight ([],[]) $
                  do
                    lrt <- _relType
                    let (UnnamedCompositeType a,UnnamedCompositeType b) = lrt
                    return (a,b)
                  {-# LINE 7978 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 453, column 9)
              _lhsOidens =
                  {-# LINE 453 "./TypeChecking.ag" #-}
                  [(tbl_, _unwrappedRelType    )]
                  {-# LINE 7983 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 444, column 9)
              _lhsOjoinIdens =
                  {-# LINE 444 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 7988 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_TrefAlias :: Annotation ->
                          String ->
                          String ->
                          T_TableRef 
sem_TableRef_TrefAlias ann_ tbl_ alias_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              -- "./TypeChecking.ag"(line 457, column 9)
              _backTree =
                  {-# LINE 457 "./TypeChecking.ag" #-}
                  TrefAlias ann_ tbl_ alias_
                  {-# LINE 8003 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 445, column 9)
              _relType =
                  {-# LINE 445 "./TypeChecking.ag" #-}
                  getRelationType _lhsIenv tbl_
                  {-# LINE 8008 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 443, column 9)
              _tpe =
                  {-# LINE 443 "./TypeChecking.ag" #-}
                  either Left (Right . fst) _relType
                  {-# LINE 8013 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 430, column 9)
              _lhsOannotatedTree =
                  {-# LINE 430 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 8021 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 446, column 9)
              _unwrappedRelType =
                  {-# LINE 446 "./TypeChecking.ag" #-}
                  fromRight ([],[]) $
                  do
                    lrt <- _relType
                    let (UnnamedCompositeType a,UnnamedCompositeType b) = lrt
                    return (a,b)
                  {-# LINE 8030 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 456, column 9)
              _lhsOidens =
                  {-# LINE 456 "./TypeChecking.ag" #-}
                  [(alias_, _unwrappedRelType    )]
                  {-# LINE 8035 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 444, column 9)
              _lhsOjoinIdens =
                  {-# LINE 444 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8040 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_TrefFun :: Annotation ->
                        T_Expression  ->
                        T_TableRef 
sem_TableRef_TrefFun ann_ fn_  =
    (\ _lhsIenv ->
         (let _fnOenv :: Environment
              _fnIannotatedTree :: Expression
              _fnIliftedColumnName :: String
              _lhsOannotatedTree :: TableRef
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8057 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIliftedColumnName) =
                  (fn_ _fnOenv )
              -- "./TypeChecking.ag"(line 468, column 9)
              _backTree =
                  {-# LINE 468 "./TypeChecking.ag" #-}
                  TrefFun ann_ _fnIannotatedTree
                  {-# LINE 8064 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 467, column 9)
              _alias1 =
                  {-# LINE 467 "./TypeChecking.ag" #-}
                  ""
                  {-# LINE 8069 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 459, column 9)
              _tpe =
                  {-# LINE 459 "./TypeChecking.ag" #-}
                  getFnType _lhsIenv _alias1     _fnIannotatedTree
                  {-# LINE 8074 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 430, column 9)
              _lhsOannotatedTree =
                  {-# LINE 430 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 8082 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 461, column 9)
              _lhsOidens =
                  {-# LINE 461 "./TypeChecking.ag" #-}
                  case getFunIdens
                            _lhsIenv _alias1
                            _fnIannotatedTree of
                    Right (s, UnnamedCompositeType c) -> [(s,(c,[]))]
                    _ -> []
                  {-# LINE 8091 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 460, column 9)
              _lhsOjoinIdens =
                  {-# LINE 460 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8096 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_TrefFunAlias :: Annotation ->
                             T_Expression  ->
                             String ->
                             T_TableRef 
sem_TableRef_TrefFunAlias ann_ fn_ alias_  =
    (\ _lhsIenv ->
         (let _fnOenv :: Environment
              _fnIannotatedTree :: Expression
              _fnIliftedColumnName :: String
              _lhsOannotatedTree :: TableRef
              _lhsOidens :: ([(String,([(String,Type)],[(String,Type)]))])
              _lhsOjoinIdens :: ([String])
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8114 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIliftedColumnName) =
                  (fn_ _fnOenv )
              -- "./TypeChecking.ag"(line 471, column 9)
              _backTree =
                  {-# LINE 471 "./TypeChecking.ag" #-}
                  TrefFunAlias ann_ _fnIannotatedTree alias_
                  {-# LINE 8121 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 470, column 9)
              _alias1 =
                  {-# LINE 470 "./TypeChecking.ag" #-}
                  alias_
                  {-# LINE 8126 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 459, column 9)
              _tpe =
                  {-# LINE 459 "./TypeChecking.ag" #-}
                  getFnType _lhsIenv _alias1     _fnIannotatedTree
                  {-# LINE 8131 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 430, column 9)
              _lhsOannotatedTree =
                  {-# LINE 430 "./TypeChecking.ag" #-}
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 8139 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 461, column 9)
              _lhsOidens =
                  {-# LINE 461 "./TypeChecking.ag" #-}
                  case getFunIdens
                            _lhsIenv _alias1
                            _fnIannotatedTree of
                    Right (s, UnnamedCompositeType c) -> [(s,(c,[]))]
                    _ -> []
                  {-# LINE 8148 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 460, column 9)
              _lhsOjoinIdens =
                  {-# LINE 460 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8153 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
-- TypeAttributeDef --------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                           ( TypeAttributeDef,String,Type)
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {env_Inh_TypeAttributeDef :: Environment}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef,attrName_Syn_TypeAttributeDef :: String,namedType_Syn_TypeAttributeDef :: Type}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_TypeAttributeDef _lhsOannotatedTree _lhsOattrName _lhsOnamedType ))
sem_TypeAttributeDef_TypeAttDef :: Annotation ->
                                   String ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef ann_ name_ typ_  =
    (\ _lhsIenv ->
         (let _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: TypeAttributeDef
              _lhsOattrName :: String
              _lhsOnamedType :: Type
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8207 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIannotatedTree
                  {-# LINE 8214 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8219 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 841, column 9)
              _lhsOattrName =
                  {-# LINE 841 "./TypeChecking.ag" #-}
                  name_
                  {-# LINE 8224 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 842, column 9)
              _lhsOnamedType =
                  {-# LINE 842 "./TypeChecking.ag" #-}
                  _typInamedType
                  {-# LINE 8229 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType)))
-- TypeAttributeDefList ----------------------------------------
{-
   visit 0:
      inherited attribute:
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
                               ( TypeAttributeDefList,([(String, Type)]))
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {env_Inh_TypeAttributeDefList :: Environment}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,attrs_Syn_TypeAttributeDefList :: [(String, Type)]}
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrs) =
             (sem _lhsIenv )
     in  (Syn_TypeAttributeDefList _lhsOannotatedTree _lhsOattrs ))
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: TypeAttributeDefList
              _tlIattrs :: ([(String, Type)])
              _hdIannotatedTree :: TypeAttributeDef
              _hdIattrName :: String
              _hdInamedType :: Type
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOattrs :: ([(String, Type)])
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8285 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8290 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIattrs) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree,_hdIattrName,_hdInamedType) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8299 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8304 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 847, column 12)
              _lhsOattrs =
                  {-# LINE 847 "./TypeChecking.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 8309 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOattrs :: ([(String, Type)])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8320 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8325 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 848, column 11)
              _lhsOattrs =
                  {-# LINE 848 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8330 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs)))
-- TypeName ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         env                  : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         namedType            : Type
   alternatives:
      alternative ArrayTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local backTree    : _
            local tpe         : _
      alternative PrecTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         child prec           : {Integer}
         visit 0:
            local backTree    : _
            local tpe         : _
      alternative SetOfTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local backTree    : _
            local tpe         : _
      alternative SimpleTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         visit 0:
            local backTree    : _
            local tpe         : _
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
                   ( TypeName,Type)
data Inh_TypeName  = Inh_TypeName {env_Inh_TypeName :: Environment}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName,namedType_Syn_TypeName :: Type}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_TypeName _lhsOannotatedTree _lhsOnamedType ))
sem_TypeName_ArrayTypeName :: Annotation ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName ann_ typ_  =
    (\ _lhsIenv ->
         (let _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: TypeName
              _lhsOnamedType :: Type
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8409 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
              -- "./TypeChecking.ag"(line 148, column 9)
              _backTree =
                  {-# LINE 148 "./TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 8416 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 147, column 9)
              _tpe =
                  {-# LINE 147 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed [_typInamedType] $ Right $ ArrayType _typInamedType
                  {-# LINE 8421 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 137, column 10)
              _lhsOannotatedTree =
                  {-# LINE 137 "./TypeChecking.ag" #-}
                  updateAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 8428 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOnamedType =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  errorToTypeFail _tpe
                  {-# LINE 8433 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
sem_TypeName_PrecTypeName :: Annotation ->
                             String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName ann_ tn_ prec_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TypeName
              _lhsOnamedType :: Type
              -- "./TypeChecking.ag"(line 154, column 9)
              _backTree =
                  {-# LINE 154 "./TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 8447 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 153, column 9)
              _tpe =
                  {-# LINE 153 "./TypeChecking.ag" #-}
                  Right TypeCheckFailed
                  {-# LINE 8452 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 137, column 10)
              _lhsOannotatedTree =
                  {-# LINE 137 "./TypeChecking.ag" #-}
                  updateAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 8459 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOnamedType =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  errorToTypeFail _tpe
                  {-# LINE 8464 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
sem_TypeName_SetOfTypeName :: Annotation ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName ann_ typ_  =
    (\ _lhsIenv ->
         (let _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: TypeName
              _lhsOnamedType :: Type
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8480 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
              -- "./TypeChecking.ag"(line 151, column 9)
              _backTree =
                  {-# LINE 151 "./TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 8487 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 150, column 9)
              _tpe =
                  {-# LINE 150 "./TypeChecking.ag" #-}
                  chainTypeCheckFailed [_typInamedType] $ Right $ SetOfType _typInamedType
                  {-# LINE 8492 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 137, column 10)
              _lhsOannotatedTree =
                  {-# LINE 137 "./TypeChecking.ag" #-}
                  updateAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 8499 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOnamedType =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  errorToTypeFail _tpe
                  {-# LINE 8504 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
sem_TypeName_SimpleTypeName :: Annotation ->
                               String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName ann_ tn_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TypeName
              _lhsOnamedType :: Type
              -- "./TypeChecking.ag"(line 145, column 9)
              _backTree =
                  {-# LINE 145 "./TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 8517 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 144, column 9)
              _tpe =
                  {-# LINE 144 "./TypeChecking.ag" #-}
                  envLookupType _lhsIenv $ canonicalizeTypeName tn_
                  {-# LINE 8522 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 137, column 10)
              _lhsOannotatedTree =
                  {-# LINE 137 "./TypeChecking.ag" #-}
                  updateAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 8529 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 136, column 10)
              _lhsOnamedType =
                  {-# LINE 136 "./TypeChecking.ag" #-}
                  errorToTypeFail _tpe
                  {-# LINE 8534 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
-- VarDef ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                 ( VarDef,((String,Type)))
data Inh_VarDef  = Inh_VarDef {env_Inh_VarDef :: Environment}
data Syn_VarDef  = Syn_VarDef {annotatedTree_Syn_VarDef :: VarDef,def_Syn_VarDef :: (String,Type)}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOdef) =
             (sem _lhsIenv )
     in  (Syn_VarDef _lhsOannotatedTree _lhsOdef ))
sem_VarDef_VarDef :: Annotation ->
                     String ->
                     T_TypeName  ->
                     (Maybe Expression) ->
                     T_VarDef 
sem_VarDef_VarDef ann_ name_ typ_ value_  =
    (\ _lhsIenv ->
         (let _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: VarDef
              _lhsOdef :: ((String,Type))
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8588 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIannotatedTree value_
                  {-# LINE 8595 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8600 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 945, column 14)
              _lhsOdef =
                  {-# LINE 945 "./TypeChecking.ag" #-}
                  (name_, _typInamedType)
                  {-# LINE 8605 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdef)))
-- VarDefList --------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                     ( VarDefList,([(String,Type)]))
data Inh_VarDefList  = Inh_VarDefList {env_Inh_VarDefList :: Environment}
data Syn_VarDefList  = Syn_VarDefList {annotatedTree_Syn_VarDefList :: VarDefList,defs_Syn_VarDefList :: [(String,Type)]}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOdefs) =
             (sem _lhsIenv )
     in  (Syn_VarDefList _lhsOannotatedTree _lhsOdefs ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlIannotatedTree :: VarDefList
              _tlIdefs :: ([(String,Type)])
              _hdIannotatedTree :: VarDef
              _hdIdef :: ((String,Type))
              _lhsOannotatedTree :: VarDefList
              _lhsOdefs :: ([(String,Type)])
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8660 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8665 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIdefs) =
                  (tl_ _tlOenv )
              ( _hdIannotatedTree,_hdIdef) =
                  (hd_ _hdOenv )
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8674 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8679 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 948, column 12)
              _lhsOdefs =
                  {-# LINE 948 "./TypeChecking.ag" #-}
                  _hdIdef : _tlIdefs
                  {-# LINE 8684 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdefs)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: VarDefList
              _lhsOdefs :: ([(String,Type)])
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8695 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8700 "AstInternal.hs" #-}
              -- "./TypeChecking.ag"(line 949, column 11)
              _lhsOdefs =
                  {-# LINE 949 "./TypeChecking.ag" #-}
                  []
                  {-# LINE 8705 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdefs)))
-- Volatility --------------------------------------------------
{-
   visit 0:
      inherited attribute:
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
                     ( Volatility)
data Inh_Volatility  = Inh_Volatility {env_Inh_Volatility :: Environment}
data Syn_Volatility  = Syn_Volatility {annotatedTree_Syn_Volatility :: Volatility}
wrap_Volatility :: T_Volatility  ->
                   Inh_Volatility  ->
                   Syn_Volatility 
wrap_Volatility sem (Inh_Volatility _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_Volatility _lhsOannotatedTree ))
sem_Volatility_Immutable :: T_Volatility 
sem_Volatility_Immutable  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Volatility
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Immutable
                  {-# LINE 8758 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8763 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Volatility_Stable :: T_Volatility 
sem_Volatility_Stable  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Volatility
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Stable
                  {-# LINE 8773 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8778 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Volatility_Volatile :: T_Volatility 
sem_Volatility_Volatile  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Volatility
              -- self rule
              _annotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  Volatile
                  {-# LINE 8788 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 56 "./TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8793 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))