

-- UUAGC 0.9.10 (Ast.ag)
module Ast(
    -- exports
    MySourcePos

    --ast nodes
   ,Statement (..)
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
   ,OperatorType (..)
   ,getOperatorType
   ,InList (..)
   ,StatementList
   --checking stuff
   ,Message (..)
   ,MessageStuff (..)
   --types
   ,Type (..)
   ,PseudoType (..)
   ,TypeErrorInfo (..)
   ,Scope(..)
   ,defaultScope
   ,emptyScope
   --fns
   ,checkAst
   ,getExpressionType
   ,getStatementsType
   ,getStatementsTypeScope
   ,resetSps
   ,resetSp
   ,resetSp'
   ,resetSps'
   ,nsp
   ,typeSmallInt
   ,typeBigInt
   ,typeInt
   ,typeNumeric
   ,typeFloat4
   ,typeFloat8
   ,typeVarChar
   ,typeChar
   ,typeBool

) where

import Data.Maybe
import Data.List
import Debug.Trace
import Control.Monad.Error
import Control.Arrow

import TypeType
import AstUtils
import TypeConversion
import TypeCheckingH
import Scope
import DefaultScope


checkAst :: StatementList -> [Message]
checkAst sts = let t = sem_Root (Root sts)
               in (messages_Syn_Root (wrap_Root t Inh_Root {scope_Inh_Root = defaultScope}))
{-
================================================================================

= Types

These are the utility functions which clients use to typecheck sql.


-}

getExpressionType :: Scope -> Expression -> Type
getExpressionType scope ex =
    let t = sem_ExpressionRoot (ExpressionRoot ex)
    in (nodeType_Syn_ExpressionRoot
        (wrap_ExpressionRoot t Inh_ExpressionRoot {scope_Inh_ExpressionRoot = combineScopes defaultScope scope}))


getStatementsType :: StatementList -> [Type]
getStatementsType = getStatementsTypeScope emptyScope

getStatementsTypeScope :: Scope -> StatementList -> [Type]
getStatementsTypeScope scope st =
    let t = sem_Root (Root st)
        tl = (nodeType_Syn_Root
              (wrap_Root t Inh_Root {scope_Inh_Root = combineScopes defaultScope scope}))
    in unwrapTypeList tl

--hack job, often not interested in the source positions when testing
--the asts produced, so this function will reset all the source
--positions to empty ("", 0, 0) so we can compare them for equality, etc.
--without having to get the positions correct.

resetSps :: [Statement] -> [Statement]
resetSps = map resetSp

resetSp :: Statement -> Statement
resetSp (CreateFunction l n p r bq b v) = CreateFunction l n p r bq
                                              (case b of
                                                SqlFnBody stss -> SqlFnBody (map resetSp' stss)
                                                PlpgsqlFnBody vd stss -> PlpgsqlFnBody vd (map resetSp' stss))
                                            v
resetSp (ForSelectStatement v s stss) = ForSelectStatement v s (map resetSp' stss)
resetSp (ForIntegerStatement v f t stss) = ForIntegerStatement v f t (map resetSp' stss)
resetSp (CaseStatement v cs els) = CaseStatement v (map (second (map resetSp')) cs) (map resetSp' els)
resetSp (If cs els) = If (map (second (map resetSp')) cs) (map resetSp' els)
resetSp a = a

resetSp' :: SourcePosStatement -> SourcePosStatement
resetSp' (_,st) = (nsp,resetSp st)

resetSps' :: StatementList -> StatementList
resetSps' = map resetSp'

nsp :: MySourcePos
nsp = ("", 0,0)


setUnknown :: Type -> Type -> Type
setUnknown _ _ = UnknownType

appendTypeList :: Type -> Type -> Type
appendTypeList t1 (TypeList ts) = TypeList (t1:ts)
appendTypeList t1 t2 = TypeList [t1,t2]


-- i think this should be alright, an identifier referenced in an
-- expression can only have zero or one dot in it.

splitIdentifier :: String -> (String,String)
splitIdentifier s = let (a,b) = span (/= '.') s
                    in if b == ""
                         then ("", a)
                         else (a,tail b)


--returns the type of the relation, and the system columns also
getRelationType :: Scope -> MySourcePos -> String -> (Type,Type)
getRelationType scope sp tbl =
          case getAttrs scope [TableComposite, ViewComposite] tbl of
            Just ((_,_,a@(UnnamedCompositeType _))
                 ,(_,_,s@(UnnamedCompositeType _)) ) -> (a,s)
            _ -> (TypeError sp (UnrecognisedRelation tbl), TypeList [])

getFnType :: Scope -> MySourcePos -> String -> Expression -> Type -> Type
getFnType scope sp alias fnVal fnType =
    checkErrors [fnType] $ snd $ getFunIdens scope sp alias fnVal fnType

getFunIdens :: Scope -> MySourcePos -> String -> Expression -> Type -> (String, Type)
getFunIdens scope sp alias fnVal fnType =
   case fnVal of
       FunCall f _ ->
           let correlationName = if alias /= ""
                                   then alias
                                   else f
           in (correlationName, case fnType of
                SetOfType (CompositeType t) -> getCompositeType t
                SetOfType x -> UnnamedCompositeType [(correlationName,x)]
                y -> UnnamedCompositeType [(correlationName,y)])
       x -> ("", TypeError sp (ContextError "FunCall"))
   where
     getCompositeType t =
                    case getAttrs scope [Composite
                                              ,TableComposite
                                              ,ViewComposite] t of
                      Just ((_,_,a@(UnnamedCompositeType _)), _) -> a
                      _ -> UnnamedCompositeType []

commonFieldNames t1 t2 =
    intersect (fn t1) (fn t2)
    where
      fn (UnnamedCompositeType s) = map fst s
      fn _ = []

both :: (a->b) -> (a,a) -> (b,b)
both fn (x,y) = (fn x, fn y)



fixedValue :: a -> a -> a -> a
fixedValue a _ _ = a
-- AttributeDef ------------------------------------------------
data AttributeDef  = AttributeDef (String) (TypeName) (Maybe Expression) (RowConstraintList) 
                   deriving ( Eq,Show)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _name _typ _check _cons )  =
    (sem_AttributeDef_AttributeDef _name (sem_TypeName _typ ) _check (sem_RowConstraintList _cons ) )
-- semantic domain
type T_AttributeDef  = Bool ->
                       Scope ->
                       MySourcePos ->
                       ( AttributeDef,([Message]),Type)
data Inh_AttributeDef  = Inh_AttributeDef {inLoop_Inh_AttributeDef :: Bool,scope_Inh_AttributeDef :: Scope,sourcePos_Inh_AttributeDef :: MySourcePos}
data Syn_AttributeDef  = Syn_AttributeDef {actualValue_Syn_AttributeDef :: AttributeDef,messages_Syn_AttributeDef :: [Message],nodeType_Syn_AttributeDef :: Type}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_AttributeDef _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_AttributeDef_AttributeDef :: String ->
                                 T_TypeName  ->
                                 (Maybe Expression) ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef name_ typ_ check_ cons_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: AttributeDef
              _typOinLoop :: Bool
              _typOscope :: Scope
              _typOsourcePos :: MySourcePos
              _consOinLoop :: Bool
              _consOscope :: Scope
              _consOsourcePos :: MySourcePos
              _typIactualValue :: TypeName
              _typImessages :: ([Message])
              _typInodeType :: Type
              _consIactualValue :: RowConstraintList
              _consImessages :: ([Message])
              _consInodeType :: Type
              _lhsOmessages =
                  _typImessages ++ _consImessages
              _lhsOnodeType =
                  _typInodeType `setUnknown` _consInodeType
              _actualValue =
                  AttributeDef name_ _typIactualValue check_ _consIactualValue
              _lhsOactualValue =
                  _actualValue
              _typOinLoop =
                  _lhsIinLoop
              _typOscope =
                  _lhsIscope
              _typOsourcePos =
                  _lhsIsourcePos
              _consOinLoop =
                  _lhsIinLoop
              _consOscope =
                  _lhsIscope
              _consOsourcePos =
                  _lhsIsourcePos
              ( _typIactualValue,_typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOscope _typOsourcePos )
              ( _consIactualValue,_consImessages,_consInodeType) =
                  (cons_ _consOinLoop _consOscope _consOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- AttributeDefList --------------------------------------------
type AttributeDefList  = [(AttributeDef)]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = Bool ->
                           Scope ->
                           MySourcePos ->
                           ( AttributeDefList,([Message]),Type)
data Inh_AttributeDefList  = Inh_AttributeDefList {inLoop_Inh_AttributeDefList :: Bool,scope_Inh_AttributeDefList :: Scope,sourcePos_Inh_AttributeDefList :: MySourcePos}
data Syn_AttributeDefList  = Syn_AttributeDefList {actualValue_Syn_AttributeDefList :: AttributeDefList,messages_Syn_AttributeDefList :: [Message],nodeType_Syn_AttributeDefList :: Type}
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_AttributeDefList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: AttributeDefList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: AttributeDef
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: AttributeDefList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: AttributeDefList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_Cascade  = Bool ->
                  Scope ->
                  MySourcePos ->
                  ( Cascade,([Message]),Type)
data Inh_Cascade  = Inh_Cascade {inLoop_Inh_Cascade :: Bool,scope_Inh_Cascade :: Scope,sourcePos_Inh_Cascade :: MySourcePos}
data Syn_Cascade  = Syn_Cascade {actualValue_Syn_Cascade :: Cascade,messages_Syn_Cascade :: [Message],nodeType_Syn_Cascade :: Type}
wrap_Cascade :: T_Cascade  ->
                Inh_Cascade  ->
                Syn_Cascade 
wrap_Cascade sem (Inh_Cascade _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_Cascade _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_Cascade_Cascade :: T_Cascade 
sem_Cascade_Cascade  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Cascade
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Cascade
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Cascade_Restrict :: T_Cascade 
sem_Cascade_Restrict  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Cascade
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Restrict
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- CaseExpressionList ------------------------------------------
type CaseExpressionList  = [(Expression)]
-- cata
sem_CaseExpressionList :: CaseExpressionList  ->
                          T_CaseExpressionList 
sem_CaseExpressionList list  =
    (Prelude.foldr sem_CaseExpressionList_Cons sem_CaseExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_CaseExpressionList  = Bool ->
                             Scope ->
                             MySourcePos ->
                             ( CaseExpressionList,([Message]),Type)
data Inh_CaseExpressionList  = Inh_CaseExpressionList {inLoop_Inh_CaseExpressionList :: Bool,scope_Inh_CaseExpressionList :: Scope,sourcePos_Inh_CaseExpressionList :: MySourcePos}
data Syn_CaseExpressionList  = Syn_CaseExpressionList {actualValue_Syn_CaseExpressionList :: CaseExpressionList,messages_Syn_CaseExpressionList :: [Message],nodeType_Syn_CaseExpressionList :: Type}
wrap_CaseExpressionList :: T_CaseExpressionList  ->
                           Inh_CaseExpressionList  ->
                           Syn_CaseExpressionList 
wrap_CaseExpressionList sem (Inh_CaseExpressionList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_CaseExpressionList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_CaseExpressionList_Cons :: T_Expression  ->
                               T_CaseExpressionList  ->
                               T_CaseExpressionList 
sem_CaseExpressionList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CaseExpressionList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: Expression
              _hdIliftedColumnName :: String
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: CaseExpressionList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdIliftedColumnName,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_CaseExpressionList_Nil :: T_CaseExpressionList 
sem_CaseExpressionList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CaseExpressionList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- CaseExpressionListExpressionPair ----------------------------
type CaseExpressionListExpressionPair  = ( (CaseExpressionList),(Expression))
-- cata
sem_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair  ->
                                        T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair ( x1,x2)  =
    (sem_CaseExpressionListExpressionPair_Tuple (sem_CaseExpressionList x1 ) (sem_Expression x2 ) )
-- semantic domain
type T_CaseExpressionListExpressionPair  = Bool ->
                                           Scope ->
                                           MySourcePos ->
                                           ( CaseExpressionListExpressionPair,([Message]),Type)
data Inh_CaseExpressionListExpressionPair  = Inh_CaseExpressionListExpressionPair {inLoop_Inh_CaseExpressionListExpressionPair :: Bool,scope_Inh_CaseExpressionListExpressionPair :: Scope,sourcePos_Inh_CaseExpressionListExpressionPair :: MySourcePos}
data Syn_CaseExpressionListExpressionPair  = Syn_CaseExpressionListExpressionPair {actualValue_Syn_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair,messages_Syn_CaseExpressionListExpressionPair :: [Message],nodeType_Syn_CaseExpressionListExpressionPair :: Type}
wrap_CaseExpressionListExpressionPair :: T_CaseExpressionListExpressionPair  ->
                                         Inh_CaseExpressionListExpressionPair  ->
                                         Syn_CaseExpressionListExpressionPair 
wrap_CaseExpressionListExpressionPair sem (Inh_CaseExpressionListExpressionPair _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_CaseExpressionListExpressionPair _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_CaseExpressionListExpressionPair_Tuple :: T_CaseExpressionList  ->
                                              T_Expression  ->
                                              T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CaseExpressionListExpressionPair
              _x1OinLoop :: Bool
              _x1Oscope :: Scope
              _x1OsourcePos :: MySourcePos
              _x2OinLoop :: Bool
              _x2Oscope :: Scope
              _x2OsourcePos :: MySourcePos
              _x1IactualValue :: CaseExpressionList
              _x1Imessages :: ([Message])
              _x1InodeType :: Type
              _x2IactualValue :: Expression
              _x2IliftedColumnName :: String
              _x2Imessages :: ([Message])
              _x2InodeType :: Type
              _lhsOmessages =
                  _x1Imessages ++ _x2Imessages
              _lhsOnodeType =
                  _x1InodeType `appendTypeList` _x2InodeType
              _actualValue =
                  (_x1IactualValue,_x2IactualValue)
              _lhsOactualValue =
                  _actualValue
              _x1OinLoop =
                  _lhsIinLoop
              _x1Oscope =
                  _lhsIscope
              _x1OsourcePos =
                  _lhsIsourcePos
              _x2OinLoop =
                  _lhsIinLoop
              _x2Oscope =
                  _lhsIscope
              _x2OsourcePos =
                  _lhsIsourcePos
              ( _x1IactualValue,_x1Imessages,_x1InodeType) =
                  (x1_ _x1OinLoop _x1Oscope _x1OsourcePos )
              ( _x2IactualValue,_x2IliftedColumnName,_x2Imessages,_x2InodeType) =
                  (x2_ _x2OinLoop _x2Oscope _x2OsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- CaseExpressionListExpressionPairList ------------------------
type CaseExpressionListExpressionPairList  = [(CaseExpressionListExpressionPair)]
-- cata
sem_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList  ->
                                            T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList list  =
    (Prelude.foldr sem_CaseExpressionListExpressionPairList_Cons sem_CaseExpressionListExpressionPairList_Nil (Prelude.map sem_CaseExpressionListExpressionPair list) )
-- semantic domain
type T_CaseExpressionListExpressionPairList  = Bool ->
                                               Scope ->
                                               MySourcePos ->
                                               ( CaseExpressionListExpressionPairList,([Message]),Type)
data Inh_CaseExpressionListExpressionPairList  = Inh_CaseExpressionListExpressionPairList {inLoop_Inh_CaseExpressionListExpressionPairList :: Bool,scope_Inh_CaseExpressionListExpressionPairList :: Scope,sourcePos_Inh_CaseExpressionListExpressionPairList :: MySourcePos}
data Syn_CaseExpressionListExpressionPairList  = Syn_CaseExpressionListExpressionPairList {actualValue_Syn_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList,messages_Syn_CaseExpressionListExpressionPairList :: [Message],nodeType_Syn_CaseExpressionListExpressionPairList :: Type}
wrap_CaseExpressionListExpressionPairList :: T_CaseExpressionListExpressionPairList  ->
                                             Inh_CaseExpressionListExpressionPairList  ->
                                             Syn_CaseExpressionListExpressionPairList 
wrap_CaseExpressionListExpressionPairList sem (Inh_CaseExpressionListExpressionPairList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_CaseExpressionListExpressionPairList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_CaseExpressionListExpressionPairList_Cons :: T_CaseExpressionListExpressionPair  ->
                                                 T_CaseExpressionListExpressionPairList  ->
                                                 T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CaseExpressionListExpressionPairList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: CaseExpressionListExpressionPair
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: CaseExpressionListExpressionPairList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_CaseExpressionListExpressionPairList_Nil :: T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CaseExpressionListExpressionPairList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_CombineType  = Bool ->
                      Scope ->
                      MySourcePos ->
                      ( CombineType,([Message]),Type)
data Inh_CombineType  = Inh_CombineType {inLoop_Inh_CombineType :: Bool,scope_Inh_CombineType :: Scope,sourcePos_Inh_CombineType :: MySourcePos}
data Syn_CombineType  = Syn_CombineType {actualValue_Syn_CombineType :: CombineType,messages_Syn_CombineType :: [Message],nodeType_Syn_CombineType :: Type}
wrap_CombineType :: T_CombineType  ->
                    Inh_CombineType  ->
                    Syn_CombineType 
wrap_CombineType sem (Inh_CombineType _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_CombineType _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_CombineType_Except :: T_CombineType 
sem_CombineType_Except  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CombineType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Except
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_CombineType_Intersect :: T_CombineType 
sem_CombineType_Intersect  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CombineType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Intersect
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_CombineType_Union :: T_CombineType 
sem_CombineType_Union  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CombineType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Union
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_CombineType_UnionAll :: T_CombineType 
sem_CombineType_UnionAll  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CombineType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  UnionAll
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_Constraint  = Bool ->
                     Scope ->
                     MySourcePos ->
                     ( Constraint,([Message]),Type)
data Inh_Constraint  = Inh_Constraint {inLoop_Inh_Constraint :: Bool,scope_Inh_Constraint :: Scope,sourcePos_Inh_Constraint :: MySourcePos}
data Syn_Constraint  = Syn_Constraint {actualValue_Syn_Constraint :: Constraint,messages_Syn_Constraint :: [Message],nodeType_Syn_Constraint :: Type}
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_Constraint _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_Constraint_CheckConstraint :: T_Expression  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint expression_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Constraint
              _expressionOinLoop :: Bool
              _expressionOscope :: Scope
              _expressionOsourcePos :: MySourcePos
              _expressionIactualValue :: Expression
              _expressionIliftedColumnName :: String
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _actualValue =
                  CheckConstraint _expressionIactualValue
              _lhsOactualValue =
                  _actualValue
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOscope =
                  _lhsIscope
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionIactualValue,_expressionIliftedColumnName,_expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOscope _expressionOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Constraint_PrimaryKeyConstraint :: T_StringList  ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint stringList_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Constraint
              _stringListOinLoop :: Bool
              _stringListOscope :: Scope
              _stringListOsourcePos :: MySourcePos
              _stringListIactualValue :: StringList
              _stringListImessages :: ([Message])
              _stringListInodeType :: Type
              _lhsOmessages =
                  _stringListImessages
              _lhsOnodeType =
                  _stringListInodeType
              _actualValue =
                  PrimaryKeyConstraint _stringListIactualValue
              _lhsOactualValue =
                  _actualValue
              _stringListOinLoop =
                  _lhsIinLoop
              _stringListOscope =
                  _lhsIscope
              _stringListOsourcePos =
                  _lhsIsourcePos
              ( _stringListIactualValue,_stringListImessages,_stringListInodeType) =
                  (stringList_ _stringListOinLoop _stringListOscope _stringListOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Constraint_ReferenceConstraint :: T_StringList  ->
                                      String ->
                                      T_StringList  ->
                                      T_Cascade  ->
                                      T_Cascade  ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Constraint
              _attsOinLoop :: Bool
              _attsOscope :: Scope
              _attsOsourcePos :: MySourcePos
              _tableAttsOinLoop :: Bool
              _tableAttsOscope :: Scope
              _tableAttsOsourcePos :: MySourcePos
              _onUpdateOinLoop :: Bool
              _onUpdateOscope :: Scope
              _onUpdateOsourcePos :: MySourcePos
              _onDeleteOinLoop :: Bool
              _onDeleteOscope :: Scope
              _onDeleteOsourcePos :: MySourcePos
              _attsIactualValue :: StringList
              _attsImessages :: ([Message])
              _attsInodeType :: Type
              _tableAttsIactualValue :: StringList
              _tableAttsImessages :: ([Message])
              _tableAttsInodeType :: Type
              _onUpdateIactualValue :: Cascade
              _onUpdateImessages :: ([Message])
              _onUpdateInodeType :: Type
              _onDeleteIactualValue :: Cascade
              _onDeleteImessages :: ([Message])
              _onDeleteInodeType :: Type
              _lhsOmessages =
                  _attsImessages ++ _tableAttsImessages ++ _onUpdateImessages ++ _onDeleteImessages
              _lhsOnodeType =
                  _attsInodeType `setUnknown` _tableAttsInodeType `setUnknown` _onUpdateInodeType `setUnknown` _onDeleteInodeType
              _actualValue =
                  ReferenceConstraint _attsIactualValue table_ _tableAttsIactualValue _onUpdateIactualValue _onDeleteIactualValue
              _lhsOactualValue =
                  _actualValue
              _attsOinLoop =
                  _lhsIinLoop
              _attsOscope =
                  _lhsIscope
              _attsOsourcePos =
                  _lhsIsourcePos
              _tableAttsOinLoop =
                  _lhsIinLoop
              _tableAttsOscope =
                  _lhsIscope
              _tableAttsOsourcePos =
                  _lhsIsourcePos
              _onUpdateOinLoop =
                  _lhsIinLoop
              _onUpdateOscope =
                  _lhsIscope
              _onUpdateOsourcePos =
                  _lhsIsourcePos
              _onDeleteOinLoop =
                  _lhsIinLoop
              _onDeleteOscope =
                  _lhsIscope
              _onDeleteOsourcePos =
                  _lhsIsourcePos
              ( _attsIactualValue,_attsImessages,_attsInodeType) =
                  (atts_ _attsOinLoop _attsOscope _attsOsourcePos )
              ( _tableAttsIactualValue,_tableAttsImessages,_tableAttsInodeType) =
                  (tableAtts_ _tableAttsOinLoop _tableAttsOscope _tableAttsOsourcePos )
              ( _onUpdateIactualValue,_onUpdateImessages,_onUpdateInodeType) =
                  (onUpdate_ _onUpdateOinLoop _onUpdateOscope _onUpdateOsourcePos )
              ( _onDeleteIactualValue,_onDeleteImessages,_onDeleteInodeType) =
                  (onDelete_ _onDeleteOinLoop _onDeleteOscope _onDeleteOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Constraint_UniqueConstraint :: T_StringList  ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint stringList_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Constraint
              _stringListOinLoop :: Bool
              _stringListOscope :: Scope
              _stringListOsourcePos :: MySourcePos
              _stringListIactualValue :: StringList
              _stringListImessages :: ([Message])
              _stringListInodeType :: Type
              _lhsOmessages =
                  _stringListImessages
              _lhsOnodeType =
                  _stringListInodeType
              _actualValue =
                  UniqueConstraint _stringListIactualValue
              _lhsOactualValue =
                  _actualValue
              _stringListOinLoop =
                  _lhsIinLoop
              _stringListOscope =
                  _lhsIscope
              _stringListOsourcePos =
                  _lhsIsourcePos
              ( _stringListIactualValue,_stringListImessages,_stringListInodeType) =
                  (stringList_ _stringListOinLoop _stringListOscope _stringListOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- ConstraintList ----------------------------------------------
type ConstraintList  = [(Constraint)]
-- cata
sem_ConstraintList :: ConstraintList  ->
                      T_ConstraintList 
sem_ConstraintList list  =
    (Prelude.foldr sem_ConstraintList_Cons sem_ConstraintList_Nil (Prelude.map sem_Constraint list) )
-- semantic domain
type T_ConstraintList  = Bool ->
                         Scope ->
                         MySourcePos ->
                         ( ConstraintList,([Message]),Type)
data Inh_ConstraintList  = Inh_ConstraintList {inLoop_Inh_ConstraintList :: Bool,scope_Inh_ConstraintList :: Scope,sourcePos_Inh_ConstraintList :: MySourcePos}
data Syn_ConstraintList  = Syn_ConstraintList {actualValue_Syn_ConstraintList :: ConstraintList,messages_Syn_ConstraintList :: [Message],nodeType_Syn_ConstraintList :: Type}
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_ConstraintList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ConstraintList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: Constraint
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: ConstraintList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ConstraintList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_CopySource  = Bool ->
                     Scope ->
                     MySourcePos ->
                     ( CopySource,([Message]),Type)
data Inh_CopySource  = Inh_CopySource {inLoop_Inh_CopySource :: Bool,scope_Inh_CopySource :: Scope,sourcePos_Inh_CopySource :: MySourcePos}
data Syn_CopySource  = Syn_CopySource {actualValue_Syn_CopySource :: CopySource,messages_Syn_CopySource :: [Message],nodeType_Syn_CopySource :: Type}
wrap_CopySource :: T_CopySource  ->
                   Inh_CopySource  ->
                   Syn_CopySource 
wrap_CopySource sem (Inh_CopySource _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_CopySource _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_CopySource_CopyFilename :: String ->
                               T_CopySource 
sem_CopySource_CopyFilename string_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CopySource
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  CopyFilename string_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_CopySource_Stdin :: T_CopySource 
sem_CopySource_Stdin  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: CopySource
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Stdin
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_Direction  = Bool ->
                    Scope ->
                    MySourcePos ->
                    ( Direction,([Message]),Type)
data Inh_Direction  = Inh_Direction {inLoop_Inh_Direction :: Bool,scope_Inh_Direction :: Scope,sourcePos_Inh_Direction :: MySourcePos}
data Syn_Direction  = Syn_Direction {actualValue_Syn_Direction :: Direction,messages_Syn_Direction :: [Message],nodeType_Syn_Direction :: Type}
wrap_Direction :: T_Direction  ->
                  Inh_Direction  ->
                  Syn_Direction 
wrap_Direction sem (Inh_Direction _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_Direction _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_Direction_Asc :: T_Direction 
sem_Direction_Asc  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Direction
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Asc
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Direction_Desc :: T_Direction 
sem_Direction_Desc  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Direction
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Desc
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_Distinct  = Bool ->
                   Scope ->
                   MySourcePos ->
                   ( Distinct,([Message]),Type)
data Inh_Distinct  = Inh_Distinct {inLoop_Inh_Distinct :: Bool,scope_Inh_Distinct :: Scope,sourcePos_Inh_Distinct :: MySourcePos}
data Syn_Distinct  = Syn_Distinct {actualValue_Syn_Distinct :: Distinct,messages_Syn_Distinct :: [Message],nodeType_Syn_Distinct :: Type}
wrap_Distinct :: T_Distinct  ->
                 Inh_Distinct  ->
                 Syn_Distinct 
wrap_Distinct sem (Inh_Distinct _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_Distinct _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_Distinct_Distinct :: T_Distinct 
sem_Distinct_Distinct  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Distinct
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Distinct
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Distinct_Dupes :: T_Distinct 
sem_Distinct_Dupes  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Distinct
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Dupes
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_DropType  = Bool ->
                   Scope ->
                   MySourcePos ->
                   ( DropType,([Message]),Type)
data Inh_DropType  = Inh_DropType {inLoop_Inh_DropType :: Bool,scope_Inh_DropType :: Scope,sourcePos_Inh_DropType :: MySourcePos}
data Syn_DropType  = Syn_DropType {actualValue_Syn_DropType :: DropType,messages_Syn_DropType :: [Message],nodeType_Syn_DropType :: Type}
wrap_DropType :: T_DropType  ->
                 Inh_DropType  ->
                 Syn_DropType 
wrap_DropType sem (Inh_DropType _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_DropType _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_DropType_Domain :: T_DropType 
sem_DropType_Domain  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: DropType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Domain
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_DropType_Table :: T_DropType 
sem_DropType_Table  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: DropType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Table
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_DropType_Type :: T_DropType 
sem_DropType_Type  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: DropType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Type
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_DropType_View :: T_DropType 
sem_DropType_View  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: DropType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  View
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- Expression --------------------------------------------------
data Expression  = BooleanLit (Bool) 
                 | Case (CaseExpressionListExpressionPairList) (MaybeExpression) 
                 | CaseSimple (Expression) (CaseExpressionListExpressionPairList) (MaybeExpression) 
                 | Cast (Expression) (TypeName) 
                 | Exists (SelectExpression) 
                 | FloatLit (Double) 
                 | FunCall (String) (ExpressionList) 
                 | Identifier (String) 
                 | InPredicate (Expression) (Bool) (InList) 
                 | IntegerLit (Integer) 
                 | NullLit 
                 | PositionalArg (Integer) 
                 | ScalarSubQuery (SelectExpression) 
                 | StringLit (String) (String) 
                 | WindowFn (Expression) (ExpressionList) (ExpressionList) (Direction) 
                 deriving ( Eq,Show)
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (BooleanLit _bool )  =
    (sem_Expression_BooleanLit _bool )
sem_Expression (Case _cases _els )  =
    (sem_Expression_Case (sem_CaseExpressionListExpressionPairList _cases ) (sem_MaybeExpression _els ) )
sem_Expression (CaseSimple _value _cases _els )  =
    (sem_Expression_CaseSimple (sem_Expression _value ) (sem_CaseExpressionListExpressionPairList _cases ) (sem_MaybeExpression _els ) )
sem_Expression (Cast _expr _tn )  =
    (sem_Expression_Cast (sem_Expression _expr ) (sem_TypeName _tn ) )
sem_Expression (Exists _sel )  =
    (sem_Expression_Exists (sem_SelectExpression _sel ) )
sem_Expression (FloatLit _double )  =
    (sem_Expression_FloatLit _double )
sem_Expression (FunCall _funName _args )  =
    (sem_Expression_FunCall _funName (sem_ExpressionList _args ) )
sem_Expression (Identifier _i )  =
    (sem_Expression_Identifier _i )
sem_Expression (InPredicate _expr _i _list )  =
    (sem_Expression_InPredicate (sem_Expression _expr ) _i (sem_InList _list ) )
sem_Expression (IntegerLit _integer )  =
    (sem_Expression_IntegerLit _integer )
sem_Expression (NullLit )  =
    (sem_Expression_NullLit )
sem_Expression (PositionalArg _integer )  =
    (sem_Expression_PositionalArg _integer )
sem_Expression (ScalarSubQuery _sel )  =
    (sem_Expression_ScalarSubQuery (sem_SelectExpression _sel ) )
sem_Expression (StringLit _quote _value )  =
    (sem_Expression_StringLit _quote _value )
sem_Expression (WindowFn _fn _partitionBy _orderBy _dir )  =
    (sem_Expression_WindowFn (sem_Expression _fn ) (sem_ExpressionList _partitionBy ) (sem_ExpressionList _orderBy ) (sem_Direction _dir ) )
-- semantic domain
type T_Expression  = Bool ->
                     Scope ->
                     MySourcePos ->
                     ( Expression,String,([Message]),Type)
data Inh_Expression  = Inh_Expression {inLoop_Inh_Expression :: Bool,scope_Inh_Expression :: Scope,sourcePos_Inh_Expression :: MySourcePos}
data Syn_Expression  = Syn_Expression {actualValue_Syn_Expression :: Expression,liftedColumnName_Syn_Expression :: String,messages_Syn_Expression :: [Message],nodeType_Syn_Expression :: Type}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression sem (Inh_Expression _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_Expression _lhsOactualValue _lhsOliftedColumnName _lhsOmessages _lhsOnodeType ))
sem_Expression_BooleanLit :: Bool ->
                             T_Expression 
sem_Expression_BooleanLit bool_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _lhsOnodeType =
                  typeBool
              _lhsOliftedColumnName =
                  ""
              _lhsOmessages =
                  []
              _actualValue =
                  BooleanLit bool_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_Case :: T_CaseExpressionListExpressionPairList  ->
                       T_MaybeExpression  ->
                       T_Expression 
sem_Expression_Case cases_ els_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _casesOinLoop :: Bool
              _casesOscope :: Scope
              _casesOsourcePos :: MySourcePos
              _elsOinLoop :: Bool
              _elsOscope :: Scope
              _elsOsourcePos :: MySourcePos
              _casesIactualValue :: CaseExpressionListExpressionPairList
              _casesImessages :: ([Message])
              _casesInodeType :: Type
              _elsIactualValue :: MaybeExpression
              _elsImessages :: ([Message])
              _elsInodeType :: Type
              _lhsOnodeType =
                  let elseThen =
                          case _elsInodeType of
                            TypeList [] -> []
                            t -> [t]
                      unwrappedLists = map unwrapTypeList $ unwrapTypeList _casesInodeType
                      whenTypes :: [Type]
                      whenTypes = concat $ map unwrapTypeList $ map head unwrappedLists
                      thenTypes :: [Type]
                      thenTypes = map (head . tail) unwrappedLists ++ elseThen
                      whensAllBool :: Type
                      whensAllBool = if any (/= typeBool) whenTypes
                                       then TypeError _lhsIsourcePos
                                                (WrongTypes typeBool whenTypes)
                                       else TypeList []
                  in checkErrors (whenTypes ++ thenTypes ++ [whensAllBool]) $
                       resolveResultSetType
                         _lhsIscope
                         _lhsIsourcePos
                         thenTypes
              _lhsOliftedColumnName =
                  ""
              _lhsOmessages =
                  _casesImessages ++ _elsImessages
              _actualValue =
                  Case _casesIactualValue _elsIactualValue
              _lhsOactualValue =
                  _actualValue
              _casesOinLoop =
                  _lhsIinLoop
              _casesOscope =
                  _lhsIscope
              _casesOsourcePos =
                  _lhsIsourcePos
              _elsOinLoop =
                  _lhsIinLoop
              _elsOscope =
                  _lhsIscope
              _elsOsourcePos =
                  _lhsIsourcePos
              ( _casesIactualValue,_casesImessages,_casesInodeType) =
                  (cases_ _casesOinLoop _casesOscope _casesOsourcePos )
              ( _elsIactualValue,_elsImessages,_elsInodeType) =
                  (els_ _elsOinLoop _elsOscope _elsOsourcePos )
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_CaseSimple :: T_Expression  ->
                             T_CaseExpressionListExpressionPairList  ->
                             T_MaybeExpression  ->
                             T_Expression 
sem_Expression_CaseSimple value_ cases_ els_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _valueOinLoop :: Bool
              _valueOscope :: Scope
              _valueOsourcePos :: MySourcePos
              _casesOinLoop :: Bool
              _casesOscope :: Scope
              _casesOsourcePos :: MySourcePos
              _elsOinLoop :: Bool
              _elsOscope :: Scope
              _elsOsourcePos :: MySourcePos
              _valueIactualValue :: Expression
              _valueIliftedColumnName :: String
              _valueImessages :: ([Message])
              _valueInodeType :: Type
              _casesIactualValue :: CaseExpressionListExpressionPairList
              _casesImessages :: ([Message])
              _casesInodeType :: Type
              _elsIactualValue :: MaybeExpression
              _elsImessages :: ([Message])
              _elsInodeType :: Type
              _lhsOnodeType =
                  let elseThen =
                          case _elsInodeType of
                            TypeList [] -> []
                            t -> [t]
                      unwrappedLists = map unwrapTypeList $ unwrapTypeList _casesInodeType
                      whenTypes :: [Type]
                      whenTypes = concat $ map unwrapTypeList $ map head unwrappedLists
                      thenTypes :: [Type]
                      thenTypes = map (head . tail) unwrappedLists ++ elseThen
                      checkWhenTypes = resolveResultSetType
                                         _lhsIscope
                                         _lhsIsourcePos
                                         (_valueInodeType:whenTypes)
                  in checkErrors (whenTypes ++ thenTypes ++ [checkWhenTypes]) $
                       resolveResultSetType
                         _lhsIscope
                         _lhsIsourcePos
                         thenTypes
              _lhsOliftedColumnName =
                  _valueIliftedColumnName
              _lhsOmessages =
                  _valueImessages ++ _casesImessages ++ _elsImessages
              _actualValue =
                  CaseSimple _valueIactualValue _casesIactualValue _elsIactualValue
              _lhsOactualValue =
                  _actualValue
              _valueOinLoop =
                  _lhsIinLoop
              _valueOscope =
                  _lhsIscope
              _valueOsourcePos =
                  _lhsIsourcePos
              _casesOinLoop =
                  _lhsIinLoop
              _casesOscope =
                  _lhsIscope
              _casesOsourcePos =
                  _lhsIsourcePos
              _elsOinLoop =
                  _lhsIinLoop
              _elsOscope =
                  _lhsIscope
              _elsOsourcePos =
                  _lhsIsourcePos
              ( _valueIactualValue,_valueIliftedColumnName,_valueImessages,_valueInodeType) =
                  (value_ _valueOinLoop _valueOscope _valueOsourcePos )
              ( _casesIactualValue,_casesImessages,_casesInodeType) =
                  (cases_ _casesOinLoop _casesOscope _casesOsourcePos )
              ( _elsIactualValue,_elsImessages,_elsInodeType) =
                  (els_ _elsOinLoop _elsOscope _elsOsourcePos )
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_Cast :: T_Expression  ->
                       T_TypeName  ->
                       T_Expression 
sem_Expression_Cast expr_ tn_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _exprOinLoop :: Bool
              _exprOscope :: Scope
              _exprOsourcePos :: MySourcePos
              _tnOinLoop :: Bool
              _tnOscope :: Scope
              _tnOsourcePos :: MySourcePos
              _exprIactualValue :: Expression
              _exprIliftedColumnName :: String
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _tnIactualValue :: TypeName
              _tnImessages :: ([Message])
              _tnInodeType :: Type
              _lhsOnodeType =
                  checkErrors [_exprInodeType]
                    _tnInodeType
              _lhsOliftedColumnName =
                  case _tnIactualValue of
                    SimpleTypeName tn -> tn
                    _ -> ""
              _lhsOmessages =
                  _exprImessages ++ _tnImessages
              _actualValue =
                  Cast _exprIactualValue _tnIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprOinLoop =
                  _lhsIinLoop
              _exprOscope =
                  _lhsIscope
              _exprOsourcePos =
                  _lhsIsourcePos
              _tnOinLoop =
                  _lhsIinLoop
              _tnOscope =
                  _lhsIscope
              _tnOsourcePos =
                  _lhsIsourcePos
              ( _exprIactualValue,_exprIliftedColumnName,_exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOscope _exprOsourcePos )
              ( _tnIactualValue,_tnImessages,_tnInodeType) =
                  (tn_ _tnOinLoop _tnOscope _tnOsourcePos )
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_Exists :: T_SelectExpression  ->
                         T_Expression 
sem_Expression_Exists sel_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _selOinLoop :: Bool
              _selOscope :: Scope
              _selOsourcePos :: MySourcePos
              _selIactualValue :: SelectExpression
              _selImessages :: ([Message])
              _selInodeType :: Type
              _lhsOnodeType =
                  checkErrors [_selInodeType] typeBool
              _lhsOliftedColumnName =
                  ""
              _lhsOmessages =
                  _selImessages
              _actualValue =
                  Exists _selIactualValue
              _lhsOactualValue =
                  _actualValue
              _selOinLoop =
                  _lhsIinLoop
              _selOscope =
                  _lhsIscope
              _selOsourcePos =
                  _lhsIsourcePos
              ( _selIactualValue,_selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOscope _selOsourcePos )
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_FloatLit :: Double ->
                           T_Expression 
sem_Expression_FloatLit double_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _lhsOnodeType =
                  typeNumeric
              _lhsOliftedColumnName =
                  ""
              _lhsOmessages =
                  []
              _actualValue =
                  FloatLit double_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_FunCall :: String ->
                          T_ExpressionList  ->
                          T_Expression 
sem_Expression_FunCall funName_ args_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _argsOinLoop :: Bool
              _argsOscope :: Scope
              _argsOsourcePos :: MySourcePos
              _argsIactualValue :: ExpressionList
              _argsImessages :: ([Message])
              _argsInodeType :: Type
              _lhsOnodeType =
                  checkErrors [_argsInodeType] $
                    typeCheckFunCall
                      _lhsIscope
                      _lhsIsourcePos
                      funName_
                      _argsInodeType
              _lhsOliftedColumnName =
                  if isOperator funName_
                     then ""
                     else funName_
              _lhsOmessages =
                  _argsImessages
              _actualValue =
                  FunCall funName_ _argsIactualValue
              _lhsOactualValue =
                  _actualValue
              _argsOinLoop =
                  _lhsIinLoop
              _argsOscope =
                  _lhsIscope
              _argsOsourcePos =
                  _lhsIsourcePos
              ( _argsIactualValue,_argsImessages,_argsInodeType) =
                  (args_ _argsOinLoop _argsOscope _argsOsourcePos )
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_Identifier :: String ->
                             T_Expression 
sem_Expression_Identifier i_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _lhsOnodeType =
                  let (correlationName,iden) = splitIdentifier i_
                  in scopeLookupID _lhsIscope _lhsIsourcePos correlationName iden
              _lhsOliftedColumnName =
                  i_
              _lhsOmessages =
                  []
              _actualValue =
                  Identifier i_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_InPredicate :: T_Expression  ->
                              Bool ->
                              T_InList  ->
                              T_Expression 
sem_Expression_InPredicate expr_ i_ list_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _exprOinLoop :: Bool
              _exprOscope :: Scope
              _exprOsourcePos :: MySourcePos
              _listOinLoop :: Bool
              _listOscope :: Scope
              _listOsourcePos :: MySourcePos
              _exprIactualValue :: Expression
              _exprIliftedColumnName :: String
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _listIactualValue :: InList
              _listImessages :: ([Message])
              _listInodeType :: Type
              _lhsOnodeType =
                  let er = resolveResultSetType
                               _lhsIscope
                               _lhsIsourcePos
                               [_exprInodeType, _listInodeType]
                  in checkErrors [er] typeBool
              _lhsOliftedColumnName =
                  _exprIliftedColumnName
              _lhsOmessages =
                  _exprImessages ++ _listImessages
              _actualValue =
                  InPredicate _exprIactualValue i_ _listIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprOinLoop =
                  _lhsIinLoop
              _exprOscope =
                  _lhsIscope
              _exprOsourcePos =
                  _lhsIsourcePos
              _listOinLoop =
                  _lhsIinLoop
              _listOscope =
                  _lhsIscope
              _listOsourcePos =
                  _lhsIsourcePos
              ( _exprIactualValue,_exprIliftedColumnName,_exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOscope _exprOsourcePos )
              ( _listIactualValue,_listImessages,_listInodeType) =
                  (list_ _listOinLoop _listOscope _listOsourcePos )
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_IntegerLit :: Integer ->
                             T_Expression 
sem_Expression_IntegerLit integer_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _lhsOnodeType =
                  typeInt
              _lhsOliftedColumnName =
                  ""
              _lhsOmessages =
                  []
              _actualValue =
                  IntegerLit integer_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_NullLit :: T_Expression 
sem_Expression_NullLit  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _lhsOnodeType =
                  UnknownStringLit
              _lhsOliftedColumnName =
                  ""
              _lhsOmessages =
                  []
              _actualValue =
                  NullLit
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_PositionalArg :: Integer ->
                                T_Expression 
sem_Expression_PositionalArg integer_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Expression
              _lhsOliftedColumnName =
                  ""
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  PositionalArg integer_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_ScalarSubQuery :: T_SelectExpression  ->
                                 T_Expression 
sem_Expression_ScalarSubQuery sel_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _selOinLoop :: Bool
              _selOscope :: Scope
              _selOsourcePos :: MySourcePos
              _selIactualValue :: SelectExpression
              _selImessages :: ([Message])
              _selInodeType :: Type
              _lhsOnodeType =
                  let f = map snd $ unwrapComposite $ unwrapSetOf _selInodeType
                  in case length f of
                     0 -> error "internal error: no columns in scalar subquery?"
                     1 -> head f
                     _ -> UnknownType
              _lhsOliftedColumnName =
                  ""
              _lhsOmessages =
                  _selImessages
              _actualValue =
                  ScalarSubQuery _selIactualValue
              _lhsOactualValue =
                  _actualValue
              _selOinLoop =
                  _lhsIinLoop
              _selOscope =
                  _lhsIscope
              _selOsourcePos =
                  _lhsIsourcePos
              ( _selIactualValue,_selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOscope _selOsourcePos )
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_StringLit :: String ->
                            String ->
                            T_Expression 
sem_Expression_StringLit quote_ value_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Expression
              _lhsOnodeType =
                  UnknownStringLit
              _lhsOliftedColumnName =
                  ""
              _lhsOmessages =
                  []
              _actualValue =
                  StringLit quote_ value_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
sem_Expression_WindowFn :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList  ->
                           T_Direction  ->
                           T_Expression 
sem_Expression_WindowFn fn_ partitionBy_ orderBy_ dir_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOliftedColumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Expression
              _fnOinLoop :: Bool
              _fnOscope :: Scope
              _fnOsourcePos :: MySourcePos
              _partitionByOinLoop :: Bool
              _partitionByOscope :: Scope
              _partitionByOsourcePos :: MySourcePos
              _orderByOinLoop :: Bool
              _orderByOscope :: Scope
              _orderByOsourcePos :: MySourcePos
              _dirOinLoop :: Bool
              _dirOscope :: Scope
              _dirOsourcePos :: MySourcePos
              _fnIactualValue :: Expression
              _fnIliftedColumnName :: String
              _fnImessages :: ([Message])
              _fnInodeType :: Type
              _partitionByIactualValue :: ExpressionList
              _partitionByImessages :: ([Message])
              _partitionByInodeType :: Type
              _orderByIactualValue :: ExpressionList
              _orderByImessages :: ([Message])
              _orderByInodeType :: Type
              _dirIactualValue :: Direction
              _dirImessages :: ([Message])
              _dirInodeType :: Type
              _lhsOliftedColumnName =
                  _fnIliftedColumnName
              _lhsOmessages =
                  _fnImessages ++ _partitionByImessages ++ _orderByImessages ++ _dirImessages
              _lhsOnodeType =
                  _fnInodeType `setUnknown` _partitionByInodeType `setUnknown` _orderByInodeType `setUnknown` _dirInodeType
              _actualValue =
                  WindowFn _fnIactualValue _partitionByIactualValue _orderByIactualValue _dirIactualValue
              _lhsOactualValue =
                  _actualValue
              _fnOinLoop =
                  _lhsIinLoop
              _fnOscope =
                  _lhsIscope
              _fnOsourcePos =
                  _lhsIsourcePos
              _partitionByOinLoop =
                  _lhsIinLoop
              _partitionByOscope =
                  _lhsIscope
              _partitionByOsourcePos =
                  _lhsIsourcePos
              _orderByOinLoop =
                  _lhsIinLoop
              _orderByOscope =
                  _lhsIscope
              _orderByOsourcePos =
                  _lhsIsourcePos
              _dirOinLoop =
                  _lhsIinLoop
              _dirOscope =
                  _lhsIscope
              _dirOsourcePos =
                  _lhsIsourcePos
              ( _fnIactualValue,_fnIliftedColumnName,_fnImessages,_fnInodeType) =
                  (fn_ _fnOinLoop _fnOscope _fnOsourcePos )
              ( _partitionByIactualValue,_partitionByImessages,_partitionByInodeType) =
                  (partitionBy_ _partitionByOinLoop _partitionByOscope _partitionByOsourcePos )
              ( _orderByIactualValue,_orderByImessages,_orderByInodeType) =
                  (orderBy_ _orderByOinLoop _orderByOscope _orderByOsourcePos )
              ( _dirIactualValue,_dirImessages,_dirInodeType) =
                  (dir_ _dirOinLoop _dirOscope _dirOsourcePos )
          in  ( _lhsOactualValue,_lhsOliftedColumnName,_lhsOmessages,_lhsOnodeType)))
-- ExpressionList ----------------------------------------------
type ExpressionList  = [(Expression)]
-- cata
sem_ExpressionList :: ExpressionList  ->
                      T_ExpressionList 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_ExpressionList  = Bool ->
                         Scope ->
                         MySourcePos ->
                         ( ExpressionList,([Message]),Type)
data Inh_ExpressionList  = Inh_ExpressionList {inLoop_Inh_ExpressionList :: Bool,scope_Inh_ExpressionList :: Scope,sourcePos_Inh_ExpressionList :: MySourcePos}
data Syn_ExpressionList  = Syn_ExpressionList {actualValue_Syn_ExpressionList :: ExpressionList,messages_Syn_ExpressionList :: [Message],nodeType_Syn_ExpressionList :: Type}
wrap_ExpressionList :: T_ExpressionList  ->
                       Inh_ExpressionList  ->
                       Syn_ExpressionList 
wrap_ExpressionList sem (Inh_ExpressionList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_ExpressionList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_ExpressionList_Cons :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList 
sem_ExpressionList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: Expression
              _hdIliftedColumnName :: String
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: ExpressionList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdIliftedColumnName,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- ExpressionListList ------------------------------------------
type ExpressionListList  = [(ExpressionList)]
-- cata
sem_ExpressionListList :: ExpressionListList  ->
                          T_ExpressionListList 
sem_ExpressionListList list  =
    (Prelude.foldr sem_ExpressionListList_Cons sem_ExpressionListList_Nil (Prelude.map sem_ExpressionList list) )
-- semantic domain
type T_ExpressionListList  = Bool ->
                             Scope ->
                             MySourcePos ->
                             ( ExpressionListList,([Message]),Type)
data Inh_ExpressionListList  = Inh_ExpressionListList {inLoop_Inh_ExpressionListList :: Bool,scope_Inh_ExpressionListList :: Scope,sourcePos_Inh_ExpressionListList :: MySourcePos}
data Syn_ExpressionListList  = Syn_ExpressionListList {actualValue_Syn_ExpressionListList :: ExpressionListList,messages_Syn_ExpressionListList :: [Message],nodeType_Syn_ExpressionListList :: Type}
wrap_ExpressionListList :: T_ExpressionListList  ->
                           Inh_ExpressionListList  ->
                           Syn_ExpressionListList 
wrap_ExpressionListList sem (Inh_ExpressionListList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_ExpressionListList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_ExpressionListList_Cons :: T_ExpressionList  ->
                               T_ExpressionListList  ->
                               T_ExpressionListList 
sem_ExpressionListList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionListList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: ExpressionList
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: ExpressionListList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_ExpressionListList_Nil :: T_ExpressionListList 
sem_ExpressionListList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionListList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- ExpressionListStatementListPair -----------------------------
type ExpressionListStatementListPair  = ( (ExpressionList),(StatementList))
-- cata
sem_ExpressionListStatementListPair :: ExpressionListStatementListPair  ->
                                       T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair ( x1,x2)  =
    (sem_ExpressionListStatementListPair_Tuple (sem_ExpressionList x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionListStatementListPair  = Bool ->
                                          Scope ->
                                          MySourcePos ->
                                          ( ExpressionListStatementListPair,([Message]),Type)
data Inh_ExpressionListStatementListPair  = Inh_ExpressionListStatementListPair {inLoop_Inh_ExpressionListStatementListPair :: Bool,scope_Inh_ExpressionListStatementListPair :: Scope,sourcePos_Inh_ExpressionListStatementListPair :: MySourcePos}
data Syn_ExpressionListStatementListPair  = Syn_ExpressionListStatementListPair {actualValue_Syn_ExpressionListStatementListPair :: ExpressionListStatementListPair,messages_Syn_ExpressionListStatementListPair :: [Message],nodeType_Syn_ExpressionListStatementListPair :: Type}
wrap_ExpressionListStatementListPair :: T_ExpressionListStatementListPair  ->
                                        Inh_ExpressionListStatementListPair  ->
                                        Syn_ExpressionListStatementListPair 
wrap_ExpressionListStatementListPair sem (Inh_ExpressionListStatementListPair _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_ExpressionListStatementListPair _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_ExpressionListStatementListPair_Tuple :: T_ExpressionList  ->
                                             T_StatementList  ->
                                             T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionListStatementListPair
              _x1OinLoop :: Bool
              _x1Oscope :: Scope
              _x1OsourcePos :: MySourcePos
              _x2OinLoop :: Bool
              _x2Oscope :: Scope
              _x2OsourcePos :: MySourcePos
              _x1IactualValue :: ExpressionList
              _x1Imessages :: ([Message])
              _x1InodeType :: Type
              _x2IactualValue :: StatementList
              _x2Imessages :: ([Message])
              _x2InodeType :: Type
              _lhsOmessages =
                  _x1Imessages ++ _x2Imessages
              _lhsOnodeType =
                  _x1InodeType `setUnknown` _x2InodeType
              _actualValue =
                  (_x1IactualValue,_x2IactualValue)
              _lhsOactualValue =
                  _actualValue
              _x1OinLoop =
                  _lhsIinLoop
              _x1Oscope =
                  _lhsIscope
              _x1OsourcePos =
                  _lhsIsourcePos
              _x2OinLoop =
                  _lhsIinLoop
              _x2Oscope =
                  _lhsIscope
              _x2OsourcePos =
                  _lhsIsourcePos
              ( _x1IactualValue,_x1Imessages,_x1InodeType) =
                  (x1_ _x1OinLoop _x1Oscope _x1OsourcePos )
              ( _x2IactualValue,_x2Imessages,_x2InodeType) =
                  (x2_ _x2OinLoop _x2Oscope _x2OsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- ExpressionListStatementListPairList -------------------------
type ExpressionListStatementListPairList  = [(ExpressionListStatementListPair)]
-- cata
sem_ExpressionListStatementListPairList :: ExpressionListStatementListPairList  ->
                                           T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList list  =
    (Prelude.foldr sem_ExpressionListStatementListPairList_Cons sem_ExpressionListStatementListPairList_Nil (Prelude.map sem_ExpressionListStatementListPair list) )
-- semantic domain
type T_ExpressionListStatementListPairList  = Bool ->
                                              Scope ->
                                              MySourcePos ->
                                              ( ExpressionListStatementListPairList,([Message]),Type)
data Inh_ExpressionListStatementListPairList  = Inh_ExpressionListStatementListPairList {inLoop_Inh_ExpressionListStatementListPairList :: Bool,scope_Inh_ExpressionListStatementListPairList :: Scope,sourcePos_Inh_ExpressionListStatementListPairList :: MySourcePos}
data Syn_ExpressionListStatementListPairList  = Syn_ExpressionListStatementListPairList {actualValue_Syn_ExpressionListStatementListPairList :: ExpressionListStatementListPairList,messages_Syn_ExpressionListStatementListPairList :: [Message],nodeType_Syn_ExpressionListStatementListPairList :: Type}
wrap_ExpressionListStatementListPairList :: T_ExpressionListStatementListPairList  ->
                                            Inh_ExpressionListStatementListPairList  ->
                                            Syn_ExpressionListStatementListPairList 
wrap_ExpressionListStatementListPairList sem (Inh_ExpressionListStatementListPairList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_ExpressionListStatementListPairList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_ExpressionListStatementListPairList_Cons :: T_ExpressionListStatementListPair  ->
                                                T_ExpressionListStatementListPairList  ->
                                                T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionListStatementListPairList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: ExpressionListStatementListPair
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: ExpressionListStatementListPairList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_ExpressionListStatementListPairList_Nil :: T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionListStatementListPairList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
                         ( ExpressionRoot,([Message]),Type)
data Inh_ExpressionRoot  = Inh_ExpressionRoot {scope_Inh_ExpressionRoot :: Scope}
data Syn_ExpressionRoot  = Syn_ExpressionRoot {actualValue_Syn_ExpressionRoot :: ExpressionRoot,messages_Syn_ExpressionRoot :: [Message],nodeType_Syn_ExpressionRoot :: Type}
wrap_ExpressionRoot :: T_ExpressionRoot  ->
                       Inh_ExpressionRoot  ->
                       Syn_ExpressionRoot 
wrap_ExpressionRoot sem (Inh_ExpressionRoot _lhsIscope )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIscope )
     in  (Syn_ExpressionRoot _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_ExpressionRoot_ExpressionRoot :: T_Expression  ->
                                     T_ExpressionRoot 
sem_ExpressionRoot_ExpressionRoot expr_  =
    (\ _lhsIscope ->
         (let _exprOsourcePos :: MySourcePos
              _exprOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionRoot
              _exprOscope :: Scope
              _exprIactualValue :: Expression
              _exprIliftedColumnName :: String
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _exprOsourcePos =
                  ("",0,0)
              _exprOinLoop =
                  False
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _actualValue =
                  ExpressionRoot _exprIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprOscope =
                  _lhsIscope
              ( _exprIactualValue,_exprIliftedColumnName,_exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOscope _exprOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- ExpressionStatementListPair ---------------------------------
type ExpressionStatementListPair  = ( (Expression),(StatementList))
-- cata
sem_ExpressionStatementListPair :: ExpressionStatementListPair  ->
                                   T_ExpressionStatementListPair 
sem_ExpressionStatementListPair ( x1,x2)  =
    (sem_ExpressionStatementListPair_Tuple (sem_Expression x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionStatementListPair  = Bool ->
                                      Scope ->
                                      MySourcePos ->
                                      ( ExpressionStatementListPair,([Message]),Type)
data Inh_ExpressionStatementListPair  = Inh_ExpressionStatementListPair {inLoop_Inh_ExpressionStatementListPair :: Bool,scope_Inh_ExpressionStatementListPair :: Scope,sourcePos_Inh_ExpressionStatementListPair :: MySourcePos}
data Syn_ExpressionStatementListPair  = Syn_ExpressionStatementListPair {actualValue_Syn_ExpressionStatementListPair :: ExpressionStatementListPair,messages_Syn_ExpressionStatementListPair :: [Message],nodeType_Syn_ExpressionStatementListPair :: Type}
wrap_ExpressionStatementListPair :: T_ExpressionStatementListPair  ->
                                    Inh_ExpressionStatementListPair  ->
                                    Syn_ExpressionStatementListPair 
wrap_ExpressionStatementListPair sem (Inh_ExpressionStatementListPair _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_ExpressionStatementListPair _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_ExpressionStatementListPair_Tuple :: T_Expression  ->
                                         T_StatementList  ->
                                         T_ExpressionStatementListPair 
sem_ExpressionStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionStatementListPair
              _x1OinLoop :: Bool
              _x1Oscope :: Scope
              _x1OsourcePos :: MySourcePos
              _x2OinLoop :: Bool
              _x2Oscope :: Scope
              _x2OsourcePos :: MySourcePos
              _x1IactualValue :: Expression
              _x1IliftedColumnName :: String
              _x1Imessages :: ([Message])
              _x1InodeType :: Type
              _x2IactualValue :: StatementList
              _x2Imessages :: ([Message])
              _x2InodeType :: Type
              _lhsOmessages =
                  _x1Imessages ++ _x2Imessages
              _lhsOnodeType =
                  _x1InodeType `setUnknown` _x2InodeType
              _actualValue =
                  (_x1IactualValue,_x2IactualValue)
              _lhsOactualValue =
                  _actualValue
              _x1OinLoop =
                  _lhsIinLoop
              _x1Oscope =
                  _lhsIscope
              _x1OsourcePos =
                  _lhsIsourcePos
              _x2OinLoop =
                  _lhsIinLoop
              _x2Oscope =
                  _lhsIscope
              _x2OsourcePos =
                  _lhsIsourcePos
              ( _x1IactualValue,_x1IliftedColumnName,_x1Imessages,_x1InodeType) =
                  (x1_ _x1OinLoop _x1Oscope _x1OsourcePos )
              ( _x2IactualValue,_x2Imessages,_x2InodeType) =
                  (x2_ _x2OinLoop _x2Oscope _x2OsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- ExpressionStatementListPairList -----------------------------
type ExpressionStatementListPairList  = [(ExpressionStatementListPair)]
-- cata
sem_ExpressionStatementListPairList :: ExpressionStatementListPairList  ->
                                       T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList list  =
    (Prelude.foldr sem_ExpressionStatementListPairList_Cons sem_ExpressionStatementListPairList_Nil (Prelude.map sem_ExpressionStatementListPair list) )
-- semantic domain
type T_ExpressionStatementListPairList  = Bool ->
                                          Scope ->
                                          MySourcePos ->
                                          ( ExpressionStatementListPairList,([Message]),Type)
data Inh_ExpressionStatementListPairList  = Inh_ExpressionStatementListPairList {inLoop_Inh_ExpressionStatementListPairList :: Bool,scope_Inh_ExpressionStatementListPairList :: Scope,sourcePos_Inh_ExpressionStatementListPairList :: MySourcePos}
data Syn_ExpressionStatementListPairList  = Syn_ExpressionStatementListPairList {actualValue_Syn_ExpressionStatementListPairList :: ExpressionStatementListPairList,messages_Syn_ExpressionStatementListPairList :: [Message],nodeType_Syn_ExpressionStatementListPairList :: Type}
wrap_ExpressionStatementListPairList :: T_ExpressionStatementListPairList  ->
                                        Inh_ExpressionStatementListPairList  ->
                                        Syn_ExpressionStatementListPairList 
wrap_ExpressionStatementListPairList sem (Inh_ExpressionStatementListPairList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_ExpressionStatementListPairList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_ExpressionStatementListPairList_Cons :: T_ExpressionStatementListPair  ->
                                            T_ExpressionStatementListPairList  ->
                                            T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionStatementListPairList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: ExpressionStatementListPair
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: ExpressionStatementListPairList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_ExpressionStatementListPairList_Nil :: T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ExpressionStatementListPairList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_FnBody  = Bool ->
                 Scope ->
                 MySourcePos ->
                 ( FnBody,([Message]),Type)
data Inh_FnBody  = Inh_FnBody {inLoop_Inh_FnBody :: Bool,scope_Inh_FnBody :: Scope,sourcePos_Inh_FnBody :: MySourcePos}
data Syn_FnBody  = Syn_FnBody {actualValue_Syn_FnBody :: FnBody,messages_Syn_FnBody :: [Message],nodeType_Syn_FnBody :: Type}
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_FnBody _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_FnBody_PlpgsqlFnBody :: T_VarDefList  ->
                            T_StatementList  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody varDefList_ sts_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: FnBody
              _varDefListOinLoop :: Bool
              _varDefListOscope :: Scope
              _varDefListOsourcePos :: MySourcePos
              _stsOinLoop :: Bool
              _stsOscope :: Scope
              _stsOsourcePos :: MySourcePos
              _varDefListIactualValue :: VarDefList
              _varDefListImessages :: ([Message])
              _varDefListInodeType :: Type
              _stsIactualValue :: StatementList
              _stsImessages :: ([Message])
              _stsInodeType :: Type
              _lhsOmessages =
                  _varDefListImessages ++ _stsImessages
              _lhsOnodeType =
                  _varDefListInodeType `setUnknown` _stsInodeType
              _actualValue =
                  PlpgsqlFnBody _varDefListIactualValue _stsIactualValue
              _lhsOactualValue =
                  _actualValue
              _varDefListOinLoop =
                  _lhsIinLoop
              _varDefListOscope =
                  _lhsIscope
              _varDefListOsourcePos =
                  _lhsIsourcePos
              _stsOinLoop =
                  _lhsIinLoop
              _stsOscope =
                  _lhsIscope
              _stsOsourcePos =
                  _lhsIsourcePos
              ( _varDefListIactualValue,_varDefListImessages,_varDefListInodeType) =
                  (varDefList_ _varDefListOinLoop _varDefListOscope _varDefListOsourcePos )
              ( _stsIactualValue,_stsImessages,_stsInodeType) =
                  (sts_ _stsOinLoop _stsOscope _stsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_FnBody_SqlFnBody :: T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody sts_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: FnBody
              _stsOinLoop :: Bool
              _stsOscope :: Scope
              _stsOsourcePos :: MySourcePos
              _stsIactualValue :: StatementList
              _stsImessages :: ([Message])
              _stsInodeType :: Type
              _lhsOmessages =
                  _stsImessages
              _lhsOnodeType =
                  _stsInodeType
              _actualValue =
                  SqlFnBody _stsIactualValue
              _lhsOactualValue =
                  _actualValue
              _stsOinLoop =
                  _lhsIinLoop
              _stsOscope =
                  _lhsIscope
              _stsOsourcePos =
                  _lhsIsourcePos
              ( _stsIactualValue,_stsImessages,_stsInodeType) =
                  (sts_ _stsOinLoop _stsOscope _stsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_IfExists  = Bool ->
                   Scope ->
                   MySourcePos ->
                   ( IfExists,([Message]),Type)
data Inh_IfExists  = Inh_IfExists {inLoop_Inh_IfExists :: Bool,scope_Inh_IfExists :: Scope,sourcePos_Inh_IfExists :: MySourcePos}
data Syn_IfExists  = Syn_IfExists {actualValue_Syn_IfExists :: IfExists,messages_Syn_IfExists :: [Message],nodeType_Syn_IfExists :: Type}
wrap_IfExists :: T_IfExists  ->
                 Inh_IfExists  ->
                 Syn_IfExists 
wrap_IfExists sem (Inh_IfExists _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_IfExists _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_IfExists_IfExists :: T_IfExists 
sem_IfExists_IfExists  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: IfExists
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  IfExists
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_IfExists_Require :: T_IfExists 
sem_IfExists_Require  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: IfExists
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Require
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_InList  = Bool ->
                 Scope ->
                 MySourcePos ->
                 ( InList,([Message]),Type)
data Inh_InList  = Inh_InList {inLoop_Inh_InList :: Bool,scope_Inh_InList :: Scope,sourcePos_Inh_InList :: MySourcePos}
data Syn_InList  = Syn_InList {actualValue_Syn_InList :: InList,messages_Syn_InList :: [Message],nodeType_Syn_InList :: Type}
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_InList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_InList_InList :: T_ExpressionList  ->
                     T_InList 
sem_InList_InList exprs_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: InList
              _exprsOinLoop :: Bool
              _exprsOscope :: Scope
              _exprsOsourcePos :: MySourcePos
              _exprsIactualValue :: ExpressionList
              _exprsImessages :: ([Message])
              _exprsInodeType :: Type
              _lhsOnodeType =
                  resolveResultSetType
                    _lhsIscope
                    _lhsIsourcePos
                    $ unwrapTypeList _exprsInodeType
              _lhsOmessages =
                  _exprsImessages
              _actualValue =
                  InList _exprsIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprsOinLoop =
                  _lhsIinLoop
              _exprsOscope =
                  _lhsIscope
              _exprsOsourcePos =
                  _lhsIsourcePos
              ( _exprsIactualValue,_exprsImessages,_exprsInodeType) =
                  (exprs_ _exprsOinLoop _exprsOscope _exprsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_InList_InSelect :: T_SelectExpression  ->
                       T_InList 
sem_InList_InSelect sel_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: InList
              _selOinLoop :: Bool
              _selOscope :: Scope
              _selOsourcePos :: MySourcePos
              _selIactualValue :: SelectExpression
              _selImessages :: ([Message])
              _selInodeType :: Type
              _lhsOmessages =
                  _selImessages
              _lhsOnodeType =
                  _selInodeType
              _actualValue =
                  InSelect _selIactualValue
              _lhsOactualValue =
                  _actualValue
              _selOinLoop =
                  _lhsIinLoop
              _selOscope =
                  _lhsIscope
              _selOsourcePos =
                  _lhsIsourcePos
              ( _selIactualValue,_selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOscope _selOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_JoinExpression  = Bool ->
                         Scope ->
                         MySourcePos ->
                         ( JoinExpression,([Message]),Type)
data Inh_JoinExpression  = Inh_JoinExpression {inLoop_Inh_JoinExpression :: Bool,scope_Inh_JoinExpression :: Scope,sourcePos_Inh_JoinExpression :: MySourcePos}
data Syn_JoinExpression  = Syn_JoinExpression {actualValue_Syn_JoinExpression :: JoinExpression,messages_Syn_JoinExpression :: [Message],nodeType_Syn_JoinExpression :: Type}
wrap_JoinExpression :: T_JoinExpression  ->
                       Inh_JoinExpression  ->
                       Syn_JoinExpression 
wrap_JoinExpression sem (Inh_JoinExpression _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_JoinExpression _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_JoinExpression_JoinOn :: T_Expression  ->
                             T_JoinExpression 
sem_JoinExpression_JoinOn expression_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: JoinExpression
              _expressionOinLoop :: Bool
              _expressionOscope :: Scope
              _expressionOsourcePos :: MySourcePos
              _expressionIactualValue :: Expression
              _expressionIliftedColumnName :: String
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _actualValue =
                  JoinOn _expressionIactualValue
              _lhsOactualValue =
                  _actualValue
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOscope =
                  _lhsIscope
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionIactualValue,_expressionIliftedColumnName,_expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOscope _expressionOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_JoinExpression_JoinUsing :: T_StringList  ->
                                T_JoinExpression 
sem_JoinExpression_JoinUsing stringList_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: JoinExpression
              _stringListOinLoop :: Bool
              _stringListOscope :: Scope
              _stringListOsourcePos :: MySourcePos
              _stringListIactualValue :: StringList
              _stringListImessages :: ([Message])
              _stringListInodeType :: Type
              _lhsOmessages =
                  _stringListImessages
              _lhsOnodeType =
                  _stringListInodeType
              _actualValue =
                  JoinUsing _stringListIactualValue
              _lhsOactualValue =
                  _actualValue
              _stringListOinLoop =
                  _lhsIinLoop
              _stringListOscope =
                  _lhsIscope
              _stringListOsourcePos =
                  _lhsIsourcePos
              ( _stringListIactualValue,_stringListImessages,_stringListInodeType) =
                  (stringList_ _stringListOinLoop _stringListOscope _stringListOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_JoinType  = Bool ->
                   Scope ->
                   MySourcePos ->
                   ( JoinType,([Message]),Type)
data Inh_JoinType  = Inh_JoinType {inLoop_Inh_JoinType :: Bool,scope_Inh_JoinType :: Scope,sourcePos_Inh_JoinType :: MySourcePos}
data Syn_JoinType  = Syn_JoinType {actualValue_Syn_JoinType :: JoinType,messages_Syn_JoinType :: [Message],nodeType_Syn_JoinType :: Type}
wrap_JoinType :: T_JoinType  ->
                 Inh_JoinType  ->
                 Syn_JoinType 
wrap_JoinType sem (Inh_JoinType _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_JoinType _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_JoinType_Cross :: T_JoinType 
sem_JoinType_Cross  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: JoinType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Cross
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_JoinType_FullOuter :: T_JoinType 
sem_JoinType_FullOuter  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: JoinType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  FullOuter
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_JoinType_Inner :: T_JoinType 
sem_JoinType_Inner  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: JoinType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Inner
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_JoinType_LeftOuter :: T_JoinType 
sem_JoinType_LeftOuter  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: JoinType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  LeftOuter
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_JoinType_RightOuter :: T_JoinType 
sem_JoinType_RightOuter  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: JoinType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  RightOuter
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_Language  = Bool ->
                   Scope ->
                   MySourcePos ->
                   ( Language,([Message]),Type)
data Inh_Language  = Inh_Language {inLoop_Inh_Language :: Bool,scope_Inh_Language :: Scope,sourcePos_Inh_Language :: MySourcePos}
data Syn_Language  = Syn_Language {actualValue_Syn_Language :: Language,messages_Syn_Language :: [Message],nodeType_Syn_Language :: Type}
wrap_Language :: T_Language  ->
                 Inh_Language  ->
                 Syn_Language 
wrap_Language sem (Inh_Language _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_Language _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_Language_Plpgsql :: T_Language 
sem_Language_Plpgsql  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Language
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Plpgsql
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Language_Sql :: T_Language 
sem_Language_Sql  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Language
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Sql
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_MTableRef  = Bool ->
                    Scope ->
                    MySourcePos ->
                    ( MTableRef,([QualifiedScope]),([String]),([Message]),Type)
data Inh_MTableRef  = Inh_MTableRef {inLoop_Inh_MTableRef :: Bool,scope_Inh_MTableRef :: Scope,sourcePos_Inh_MTableRef :: MySourcePos}
data Syn_MTableRef  = Syn_MTableRef {actualValue_Syn_MTableRef :: MTableRef,idens_Syn_MTableRef :: [QualifiedScope],joinIdens_Syn_MTableRef :: [String],messages_Syn_MTableRef :: [Message],nodeType_Syn_MTableRef :: Type}
wrap_MTableRef :: T_MTableRef  ->
                  Inh_MTableRef  ->
                  Syn_MTableRef 
wrap_MTableRef sem (Inh_MTableRef _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOidens,_lhsOjoinIdens,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_MTableRef _lhsOactualValue _lhsOidens _lhsOjoinIdens _lhsOmessages _lhsOnodeType ))
sem_MTableRef_Just :: T_TableRef  ->
                      T_MTableRef 
sem_MTableRef_Just just_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: MTableRef
              _lhsOidens :: ([QualifiedScope])
              _lhsOjoinIdens :: ([String])
              _justOinLoop :: Bool
              _justOscope :: Scope
              _justOsourcePos :: MySourcePos
              _justIactualValue :: TableRef
              _justIidens :: ([QualifiedScope])
              _justIjoinIdens :: ([String])
              _justImessages :: ([Message])
              _justInodeType :: Type
              _lhsOnodeType =
                  _justInodeType
              _lhsOmessages =
                  _justImessages
              _actualValue =
                  Just _justIactualValue
              _lhsOactualValue =
                  _actualValue
              _lhsOidens =
                  _justIidens
              _lhsOjoinIdens =
                  _justIjoinIdens
              _justOinLoop =
                  _lhsIinLoop
              _justOscope =
                  _lhsIscope
              _justOsourcePos =
                  _lhsIsourcePos
              ( _justIactualValue,_justIidens,_justIjoinIdens,_justImessages,_justInodeType) =
                  (just_ _justOinLoop _justOscope _justOsourcePos )
          in  ( _lhsOactualValue,_lhsOidens,_lhsOjoinIdens,_lhsOmessages,_lhsOnodeType)))
sem_MTableRef_Nothing :: T_MTableRef 
sem_MTableRef_Nothing  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOidens :: ([QualifiedScope])
              _lhsOjoinIdens :: ([String])
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: MTableRef
              _lhsOnodeType =
                  TypeList []
              _lhsOidens =
                  []
              _lhsOjoinIdens =
                  []
              _lhsOmessages =
                  []
              _actualValue =
                  Nothing
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOidens,_lhsOjoinIdens,_lhsOmessages,_lhsOnodeType)))
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
type T_MaybeExpression  = Bool ->
                          Scope ->
                          MySourcePos ->
                          ( MaybeExpression,([Message]),Type)
data Inh_MaybeExpression  = Inh_MaybeExpression {inLoop_Inh_MaybeExpression :: Bool,scope_Inh_MaybeExpression :: Scope,sourcePos_Inh_MaybeExpression :: MySourcePos}
data Syn_MaybeExpression  = Syn_MaybeExpression {actualValue_Syn_MaybeExpression :: MaybeExpression,messages_Syn_MaybeExpression :: [Message],nodeType_Syn_MaybeExpression :: Type}
wrap_MaybeExpression :: T_MaybeExpression  ->
                        Inh_MaybeExpression  ->
                        Syn_MaybeExpression 
wrap_MaybeExpression sem (Inh_MaybeExpression _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_MaybeExpression _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_MaybeExpression_Just :: T_Expression  ->
                            T_MaybeExpression 
sem_MaybeExpression_Just just_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: MaybeExpression
              _justOinLoop :: Bool
              _justOscope :: Scope
              _justOsourcePos :: MySourcePos
              _justIactualValue :: Expression
              _justIliftedColumnName :: String
              _justImessages :: ([Message])
              _justInodeType :: Type
              _lhsOnodeType =
                  _justInodeType
              _lhsOmessages =
                  _justImessages
              _actualValue =
                  Just _justIactualValue
              _lhsOactualValue =
                  _actualValue
              _justOinLoop =
                  _lhsIinLoop
              _justOscope =
                  _lhsIscope
              _justOsourcePos =
                  _lhsIsourcePos
              ( _justIactualValue,_justIliftedColumnName,_justImessages,_justInodeType) =
                  (just_ _justOinLoop _justOscope _justOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_MaybeExpression_Nothing :: T_MaybeExpression 
sem_MaybeExpression_Nothing  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: MaybeExpression
              _lhsOnodeType =
                  TypeList []
              _lhsOmessages =
                  []
              _actualValue =
                  Nothing
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_Natural  = Bool ->
                  Scope ->
                  MySourcePos ->
                  ( Natural,([Message]),Type)
data Inh_Natural  = Inh_Natural {inLoop_Inh_Natural :: Bool,scope_Inh_Natural :: Scope,sourcePos_Inh_Natural :: MySourcePos}
data Syn_Natural  = Syn_Natural {actualValue_Syn_Natural :: Natural,messages_Syn_Natural :: [Message],nodeType_Syn_Natural :: Type}
wrap_Natural :: T_Natural  ->
                Inh_Natural  ->
                Syn_Natural 
wrap_Natural sem (Inh_Natural _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_Natural _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_Natural_Natural :: T_Natural 
sem_Natural_Natural  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Natural
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Natural
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Natural_Unnatural :: T_Natural 
sem_Natural_Unnatural  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Natural
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Unnatural
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_OnExpr  = Bool ->
                 Scope ->
                 MySourcePos ->
                 ( OnExpr,([Message]),Type)
data Inh_OnExpr  = Inh_OnExpr {inLoop_Inh_OnExpr :: Bool,scope_Inh_OnExpr :: Scope,sourcePos_Inh_OnExpr :: MySourcePos}
data Syn_OnExpr  = Syn_OnExpr {actualValue_Syn_OnExpr :: OnExpr,messages_Syn_OnExpr :: [Message],nodeType_Syn_OnExpr :: Type}
wrap_OnExpr :: T_OnExpr  ->
               Inh_OnExpr  ->
               Syn_OnExpr 
wrap_OnExpr sem (Inh_OnExpr _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_OnExpr _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_OnExpr_Just :: T_JoinExpression  ->
                   T_OnExpr 
sem_OnExpr_Just just_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: OnExpr
              _justOinLoop :: Bool
              _justOscope :: Scope
              _justOsourcePos :: MySourcePos
              _justIactualValue :: JoinExpression
              _justImessages :: ([Message])
              _justInodeType :: Type
              _lhsOmessages =
                  _justImessages
              _lhsOnodeType =
                  _justInodeType
              _actualValue =
                  Just _justIactualValue
              _lhsOactualValue =
                  _actualValue
              _justOinLoop =
                  _lhsIinLoop
              _justOscope =
                  _lhsIscope
              _justOsourcePos =
                  _lhsIsourcePos
              ( _justIactualValue,_justImessages,_justInodeType) =
                  (just_ _justOinLoop _justOscope _justOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: OnExpr
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Nothing
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- ParamDef ----------------------------------------------------
data ParamDef  = ParamDef (String) (TypeName) 
               | ParamDefTp (TypeName) 
               deriving ( Eq,Show)
-- cata
sem_ParamDef :: ParamDef  ->
                T_ParamDef 
sem_ParamDef (ParamDef _string _typeName )  =
    (sem_ParamDef_ParamDef _string (sem_TypeName _typeName ) )
sem_ParamDef (ParamDefTp _typeName )  =
    (sem_ParamDef_ParamDefTp (sem_TypeName _typeName ) )
-- semantic domain
type T_ParamDef  = Bool ->
                   Scope ->
                   MySourcePos ->
                   ( ParamDef,([Message]),Type)
data Inh_ParamDef  = Inh_ParamDef {inLoop_Inh_ParamDef :: Bool,scope_Inh_ParamDef :: Scope,sourcePos_Inh_ParamDef :: MySourcePos}
data Syn_ParamDef  = Syn_ParamDef {actualValue_Syn_ParamDef :: ParamDef,messages_Syn_ParamDef :: [Message],nodeType_Syn_ParamDef :: Type}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_ParamDef _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_ParamDef_ParamDef :: String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef string_ typeName_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ParamDef
              _typeNameOinLoop :: Bool
              _typeNameOscope :: Scope
              _typeNameOsourcePos :: MySourcePos
              _typeNameIactualValue :: TypeName
              _typeNameImessages :: ([Message])
              _typeNameInodeType :: Type
              _lhsOmessages =
                  _typeNameImessages
              _lhsOnodeType =
                  _typeNameInodeType
              _actualValue =
                  ParamDef string_ _typeNameIactualValue
              _lhsOactualValue =
                  _actualValue
              _typeNameOinLoop =
                  _lhsIinLoop
              _typeNameOscope =
                  _lhsIscope
              _typeNameOsourcePos =
                  _lhsIsourcePos
              ( _typeNameIactualValue,_typeNameImessages,_typeNameInodeType) =
                  (typeName_ _typeNameOinLoop _typeNameOscope _typeNameOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_ParamDef_ParamDefTp :: T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp typeName_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ParamDef
              _typeNameOinLoop :: Bool
              _typeNameOscope :: Scope
              _typeNameOsourcePos :: MySourcePos
              _typeNameIactualValue :: TypeName
              _typeNameImessages :: ([Message])
              _typeNameInodeType :: Type
              _lhsOmessages =
                  _typeNameImessages
              _lhsOnodeType =
                  _typeNameInodeType
              _actualValue =
                  ParamDefTp _typeNameIactualValue
              _lhsOactualValue =
                  _actualValue
              _typeNameOinLoop =
                  _lhsIinLoop
              _typeNameOscope =
                  _lhsIscope
              _typeNameOsourcePos =
                  _lhsIsourcePos
              ( _typeNameIactualValue,_typeNameImessages,_typeNameInodeType) =
                  (typeName_ _typeNameOinLoop _typeNameOscope _typeNameOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- ParamDefList ------------------------------------------------
type ParamDefList  = [(ParamDef)]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = Bool ->
                       Scope ->
                       MySourcePos ->
                       ( ParamDefList,([Message]),Type)
data Inh_ParamDefList  = Inh_ParamDefList {inLoop_Inh_ParamDefList :: Bool,scope_Inh_ParamDefList :: Scope,sourcePos_Inh_ParamDefList :: MySourcePos}
data Syn_ParamDefList  = Syn_ParamDefList {actualValue_Syn_ParamDefList :: ParamDefList,messages_Syn_ParamDefList :: [Message],nodeType_Syn_ParamDefList :: Type}
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_ParamDefList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ParamDefList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: ParamDef
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: ParamDefList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: ParamDefList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_RaiseType  = Bool ->
                    Scope ->
                    MySourcePos ->
                    ( RaiseType,([Message]),Type)
data Inh_RaiseType  = Inh_RaiseType {inLoop_Inh_RaiseType :: Bool,scope_Inh_RaiseType :: Scope,sourcePos_Inh_RaiseType :: MySourcePos}
data Syn_RaiseType  = Syn_RaiseType {actualValue_Syn_RaiseType :: RaiseType,messages_Syn_RaiseType :: [Message],nodeType_Syn_RaiseType :: Type}
wrap_RaiseType :: T_RaiseType  ->
                  Inh_RaiseType  ->
                  Syn_RaiseType 
wrap_RaiseType sem (Inh_RaiseType _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_RaiseType _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_RaiseType_RError :: T_RaiseType 
sem_RaiseType_RError  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RaiseType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  RError
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_RaiseType_RException :: T_RaiseType 
sem_RaiseType_RException  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RaiseType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  RException
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_RaiseType_RNotice :: T_RaiseType 
sem_RaiseType_RNotice  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RaiseType
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  RNotice
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_RestartIdentity  = Bool ->
                          Scope ->
                          MySourcePos ->
                          ( RestartIdentity,([Message]),Type)
data Inh_RestartIdentity  = Inh_RestartIdentity {inLoop_Inh_RestartIdentity :: Bool,scope_Inh_RestartIdentity :: Scope,sourcePos_Inh_RestartIdentity :: MySourcePos}
data Syn_RestartIdentity  = Syn_RestartIdentity {actualValue_Syn_RestartIdentity :: RestartIdentity,messages_Syn_RestartIdentity :: [Message],nodeType_Syn_RestartIdentity :: Type}
wrap_RestartIdentity :: T_RestartIdentity  ->
                        Inh_RestartIdentity  ->
                        Syn_RestartIdentity 
wrap_RestartIdentity sem (Inh_RestartIdentity _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_RestartIdentity _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_RestartIdentity_ContinueIdentity :: T_RestartIdentity 
sem_RestartIdentity_ContinueIdentity  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RestartIdentity
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  ContinueIdentity
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_RestartIdentity_RestartIdentity :: T_RestartIdentity 
sem_RestartIdentity_RestartIdentity  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RestartIdentity
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  RestartIdentity
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
               ( Root,([Message]),Type)
data Inh_Root  = Inh_Root {scope_Inh_Root :: Scope}
data Syn_Root  = Syn_Root {actualValue_Syn_Root :: Root,messages_Syn_Root :: [Message],nodeType_Syn_Root :: Type}
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIscope )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIscope )
     in  (Syn_Root _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_Root_Root :: T_StatementList  ->
                 T_Root 
sem_Root_Root statements_  =
    (\ _lhsIscope ->
         (let _statementsOsourcePos :: MySourcePos
              _statementsOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Root
              _statementsOscope :: Scope
              _statementsIactualValue :: StatementList
              _statementsImessages :: ([Message])
              _statementsInodeType :: Type
              _statementsOsourcePos =
                  ("",0,0)
              _statementsOinLoop =
                  False
              _lhsOmessages =
                  _statementsImessages
              _lhsOnodeType =
                  _statementsInodeType
              _actualValue =
                  Root _statementsIactualValue
              _lhsOactualValue =
                  _actualValue
              _statementsOscope =
                  _lhsIscope
              ( _statementsIactualValue,_statementsImessages,_statementsInodeType) =
                  (statements_ _statementsOinLoop _statementsOscope _statementsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_RowConstraint  = Bool ->
                        Scope ->
                        MySourcePos ->
                        ( RowConstraint,([Message]),Type)
data Inh_RowConstraint  = Inh_RowConstraint {inLoop_Inh_RowConstraint :: Bool,scope_Inh_RowConstraint :: Scope,sourcePos_Inh_RowConstraint :: MySourcePos}
data Syn_RowConstraint  = Syn_RowConstraint {actualValue_Syn_RowConstraint :: RowConstraint,messages_Syn_RowConstraint :: [Message],nodeType_Syn_RowConstraint :: Type}
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_RowConstraint _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_RowConstraint_NotNullConstraint :: T_RowConstraint 
sem_RowConstraint_NotNullConstraint  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RowConstraint
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  NotNullConstraint
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_RowConstraint_NullConstraint :: T_RowConstraint 
sem_RowConstraint_NullConstraint  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RowConstraint
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  NullConstraint
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_RowConstraint_RowCheckConstraint :: T_Expression  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint expression_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RowConstraint
              _expressionOinLoop :: Bool
              _expressionOscope :: Scope
              _expressionOsourcePos :: MySourcePos
              _expressionIactualValue :: Expression
              _expressionIliftedColumnName :: String
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _actualValue =
                  RowCheckConstraint _expressionIactualValue
              _lhsOactualValue =
                  _actualValue
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOscope =
                  _lhsIscope
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionIactualValue,_expressionIliftedColumnName,_expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOscope _expressionOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_RowConstraint_RowPrimaryKeyConstraint :: T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RowConstraint
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  RowPrimaryKeyConstraint
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_RowConstraint_RowReferenceConstraint :: String ->
                                            (Maybe String) ->
                                            T_Cascade  ->
                                            T_Cascade  ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RowConstraint
              _onUpdateOinLoop :: Bool
              _onUpdateOscope :: Scope
              _onUpdateOsourcePos :: MySourcePos
              _onDeleteOinLoop :: Bool
              _onDeleteOscope :: Scope
              _onDeleteOsourcePos :: MySourcePos
              _onUpdateIactualValue :: Cascade
              _onUpdateImessages :: ([Message])
              _onUpdateInodeType :: Type
              _onDeleteIactualValue :: Cascade
              _onDeleteImessages :: ([Message])
              _onDeleteInodeType :: Type
              _lhsOmessages =
                  _onUpdateImessages ++ _onDeleteImessages
              _lhsOnodeType =
                  _onUpdateInodeType `setUnknown` _onDeleteInodeType
              _actualValue =
                  RowReferenceConstraint table_ att_ _onUpdateIactualValue _onDeleteIactualValue
              _lhsOactualValue =
                  _actualValue
              _onUpdateOinLoop =
                  _lhsIinLoop
              _onUpdateOscope =
                  _lhsIscope
              _onUpdateOsourcePos =
                  _lhsIsourcePos
              _onDeleteOinLoop =
                  _lhsIinLoop
              _onDeleteOscope =
                  _lhsIscope
              _onDeleteOsourcePos =
                  _lhsIsourcePos
              ( _onUpdateIactualValue,_onUpdateImessages,_onUpdateInodeType) =
                  (onUpdate_ _onUpdateOinLoop _onUpdateOscope _onUpdateOsourcePos )
              ( _onDeleteIactualValue,_onDeleteImessages,_onDeleteInodeType) =
                  (onDelete_ _onDeleteOinLoop _onDeleteOscope _onDeleteOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_RowConstraint_RowUniqueConstraint :: T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RowConstraint
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  RowUniqueConstraint
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- RowConstraintList -------------------------------------------
type RowConstraintList  = [(RowConstraint)]
-- cata
sem_RowConstraintList :: RowConstraintList  ->
                         T_RowConstraintList 
sem_RowConstraintList list  =
    (Prelude.foldr sem_RowConstraintList_Cons sem_RowConstraintList_Nil (Prelude.map sem_RowConstraint list) )
-- semantic domain
type T_RowConstraintList  = Bool ->
                            Scope ->
                            MySourcePos ->
                            ( RowConstraintList,([Message]),Type)
data Inh_RowConstraintList  = Inh_RowConstraintList {inLoop_Inh_RowConstraintList :: Bool,scope_Inh_RowConstraintList :: Scope,sourcePos_Inh_RowConstraintList :: MySourcePos}
data Syn_RowConstraintList  = Syn_RowConstraintList {actualValue_Syn_RowConstraintList :: RowConstraintList,messages_Syn_RowConstraintList :: [Message],nodeType_Syn_RowConstraintList :: Type}
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_RowConstraintList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RowConstraintList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: RowConstraint
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: RowConstraintList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: RowConstraintList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_SelectExpression  = Bool ->
                           Scope ->
                           MySourcePos ->
                           ( SelectExpression,([Message]),Type)
data Inh_SelectExpression  = Inh_SelectExpression {inLoop_Inh_SelectExpression :: Bool,scope_Inh_SelectExpression :: Scope,sourcePos_Inh_SelectExpression :: MySourcePos}
data Syn_SelectExpression  = Syn_SelectExpression {actualValue_Syn_SelectExpression :: SelectExpression,messages_Syn_SelectExpression :: [Message],nodeType_Syn_SelectExpression :: Type}
wrap_SelectExpression :: T_SelectExpression  ->
                         Inh_SelectExpression  ->
                         Syn_SelectExpression 
wrap_SelectExpression sem (Inh_SelectExpression _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_SelectExpression _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_SelectExpression_CombineSelect :: T_CombineType  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression 
sem_SelectExpression_CombineSelect ctype_ sel1_ sel2_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: SelectExpression
              _ctypeOinLoop :: Bool
              _ctypeOscope :: Scope
              _ctypeOsourcePos :: MySourcePos
              _sel1OinLoop :: Bool
              _sel1Oscope :: Scope
              _sel1OsourcePos :: MySourcePos
              _sel2OinLoop :: Bool
              _sel2Oscope :: Scope
              _sel2OsourcePos :: MySourcePos
              _ctypeIactualValue :: CombineType
              _ctypeImessages :: ([Message])
              _ctypeInodeType :: Type
              _sel1IactualValue :: SelectExpression
              _sel1Imessages :: ([Message])
              _sel1InodeType :: Type
              _sel2IactualValue :: SelectExpression
              _sel2Imessages :: ([Message])
              _sel2InodeType :: Type
              _lhsOnodeType =
                  checkErrors [_sel1InodeType,_sel2InodeType] $
                              typeCheckCombineSelect
                                        _lhsIscope
                                        _lhsIsourcePos
                                        _sel1InodeType
                                        _sel2InodeType
              _lhsOmessages =
                  _ctypeImessages ++ _sel1Imessages ++ _sel2Imessages
              _actualValue =
                  CombineSelect _ctypeIactualValue _sel1IactualValue _sel2IactualValue
              _lhsOactualValue =
                  _actualValue
              _ctypeOinLoop =
                  _lhsIinLoop
              _ctypeOscope =
                  _lhsIscope
              _ctypeOsourcePos =
                  _lhsIsourcePos
              _sel1OinLoop =
                  _lhsIinLoop
              _sel1Oscope =
                  _lhsIscope
              _sel1OsourcePos =
                  _lhsIsourcePos
              _sel2OinLoop =
                  _lhsIinLoop
              _sel2Oscope =
                  _lhsIscope
              _sel2OsourcePos =
                  _lhsIsourcePos
              ( _ctypeIactualValue,_ctypeImessages,_ctypeInodeType) =
                  (ctype_ _ctypeOinLoop _ctypeOscope _ctypeOsourcePos )
              ( _sel1IactualValue,_sel1Imessages,_sel1InodeType) =
                  (sel1_ _sel1OinLoop _sel1Oscope _sel1OsourcePos )
              ( _sel2IactualValue,_sel2Imessages,_sel2InodeType) =
                  (sel2_ _sel2OinLoop _sel2Oscope _sel2OsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _selSelectListOscope :: Scope
              _selWhereOscope :: Scope
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: SelectExpression
              _selDistinctOinLoop :: Bool
              _selDistinctOscope :: Scope
              _selDistinctOsourcePos :: MySourcePos
              _selSelectListOinLoop :: Bool
              _selSelectListOsourcePos :: MySourcePos
              _selTrefOinLoop :: Bool
              _selTrefOscope :: Scope
              _selTrefOsourcePos :: MySourcePos
              _selWhereOinLoop :: Bool
              _selWhereOsourcePos :: MySourcePos
              _selGroupByOinLoop :: Bool
              _selGroupByOscope :: Scope
              _selGroupByOsourcePos :: MySourcePos
              _selOrderByOinLoop :: Bool
              _selOrderByOscope :: Scope
              _selOrderByOsourcePos :: MySourcePos
              _selDirOinLoop :: Bool
              _selDirOscope :: Scope
              _selDirOsourcePos :: MySourcePos
              _selDistinctIactualValue :: Distinct
              _selDistinctImessages :: ([Message])
              _selDistinctInodeType :: Type
              _selSelectListIactualValue :: SelectList
              _selSelectListImessages :: ([Message])
              _selSelectListInodeType :: Type
              _selTrefIactualValue :: MTableRef
              _selTrefIidens :: ([QualifiedScope])
              _selTrefIjoinIdens :: ([String])
              _selTrefImessages :: ([Message])
              _selTrefInodeType :: Type
              _selWhereIactualValue :: Where
              _selWhereImessages :: ([Message])
              _selWhereInodeType :: Type
              _selGroupByIactualValue :: ExpressionList
              _selGroupByImessages :: ([Message])
              _selGroupByInodeType :: Type
              _selOrderByIactualValue :: ExpressionList
              _selOrderByImessages :: ([Message])
              _selOrderByInodeType :: Type
              _selDirIactualValue :: Direction
              _selDirImessages :: ([Message])
              _selDirInodeType :: Type
              _lhsOnodeType =
                  checkErrors
                    [_selTrefInodeType
                    ,_selSelectListInodeType
                    ,_selWhereInodeType]
                    (let t = _selSelectListInodeType
                     in if t == UnnamedCompositeType [("?column?",Pseudo Void)]
                        then Pseudo Void
                        else SetOfType _selSelectListInodeType)
              _selSelectListOscope =
                  scopeReplaceIds _lhsIscope _selTrefIidens _selTrefIjoinIdens
              _selWhereOscope =
                  scopeReplaceIds _lhsIscope _selTrefIidens _selTrefIjoinIdens
              _lhsOmessages =
                  _selDistinctImessages ++ _selSelectListImessages ++ _selTrefImessages ++ _selWhereImessages ++ _selGroupByImessages ++ _selOrderByImessages ++ _selDirImessages
              _actualValue =
                  Select _selDistinctIactualValue _selSelectListIactualValue _selTrefIactualValue _selWhereIactualValue _selGroupByIactualValue selHaving_ _selOrderByIactualValue _selDirIactualValue selLimit_ selOffset_
              _lhsOactualValue =
                  _actualValue
              _selDistinctOinLoop =
                  _lhsIinLoop
              _selDistinctOscope =
                  _lhsIscope
              _selDistinctOsourcePos =
                  _lhsIsourcePos
              _selSelectListOinLoop =
                  _lhsIinLoop
              _selSelectListOsourcePos =
                  _lhsIsourcePos
              _selTrefOinLoop =
                  _lhsIinLoop
              _selTrefOscope =
                  _lhsIscope
              _selTrefOsourcePos =
                  _lhsIsourcePos
              _selWhereOinLoop =
                  _lhsIinLoop
              _selWhereOsourcePos =
                  _lhsIsourcePos
              _selGroupByOinLoop =
                  _lhsIinLoop
              _selGroupByOscope =
                  _lhsIscope
              _selGroupByOsourcePos =
                  _lhsIsourcePos
              _selOrderByOinLoop =
                  _lhsIinLoop
              _selOrderByOscope =
                  _lhsIscope
              _selOrderByOsourcePos =
                  _lhsIsourcePos
              _selDirOinLoop =
                  _lhsIinLoop
              _selDirOscope =
                  _lhsIscope
              _selDirOsourcePos =
                  _lhsIsourcePos
              ( _selDistinctIactualValue,_selDistinctImessages,_selDistinctInodeType) =
                  (selDistinct_ _selDistinctOinLoop _selDistinctOscope _selDistinctOsourcePos )
              ( _selSelectListIactualValue,_selSelectListImessages,_selSelectListInodeType) =
                  (selSelectList_ _selSelectListOinLoop _selSelectListOscope _selSelectListOsourcePos )
              ( _selTrefIactualValue,_selTrefIidens,_selTrefIjoinIdens,_selTrefImessages,_selTrefInodeType) =
                  (selTref_ _selTrefOinLoop _selTrefOscope _selTrefOsourcePos )
              ( _selWhereIactualValue,_selWhereImessages,_selWhereInodeType) =
                  (selWhere_ _selWhereOinLoop _selWhereOscope _selWhereOsourcePos )
              ( _selGroupByIactualValue,_selGroupByImessages,_selGroupByInodeType) =
                  (selGroupBy_ _selGroupByOinLoop _selGroupByOscope _selGroupByOsourcePos )
              ( _selOrderByIactualValue,_selOrderByImessages,_selOrderByInodeType) =
                  (selOrderBy_ _selOrderByOinLoop _selOrderByOscope _selOrderByOsourcePos )
              ( _selDirIactualValue,_selDirImessages,_selDirInodeType) =
                  (selDir_ _selDirOinLoop _selDirOscope _selDirOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_SelectExpression_Values :: T_ExpressionListList  ->
                               T_SelectExpression 
sem_SelectExpression_Values vll_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: SelectExpression
              _vllOinLoop :: Bool
              _vllOscope :: Scope
              _vllOsourcePos :: MySourcePos
              _vllIactualValue :: ExpressionListList
              _vllImessages :: ([Message])
              _vllInodeType :: Type
              _lhsOnodeType =
                  checkErrors [_vllInodeType] $
                              typeCheckValuesExpr
                                        _lhsIscope
                                        _lhsIsourcePos
                                        _vllInodeType
              _lhsOmessages =
                  _vllImessages
              _actualValue =
                  Values _vllIactualValue
              _lhsOactualValue =
                  _actualValue
              _vllOinLoop =
                  _lhsIinLoop
              _vllOscope =
                  _lhsIscope
              _vllOsourcePos =
                  _lhsIsourcePos
              ( _vllIactualValue,_vllImessages,_vllInodeType) =
                  (vll_ _vllOinLoop _vllOscope _vllOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_SelectItem  = Bool ->
                     Scope ->
                     MySourcePos ->
                     ( SelectItem,String,([Message]),Type)
data Inh_SelectItem  = Inh_SelectItem {inLoop_Inh_SelectItem :: Bool,scope_Inh_SelectItem :: Scope,sourcePos_Inh_SelectItem :: MySourcePos}
data Syn_SelectItem  = Syn_SelectItem {actualValue_Syn_SelectItem :: SelectItem,columnName_Syn_SelectItem :: String,messages_Syn_SelectItem :: [Message],nodeType_Syn_SelectItem :: Type}
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOcolumnName,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_SelectItem _lhsOactualValue _lhsOcolumnName _lhsOmessages _lhsOnodeType ))
sem_SelectItem_SelExp :: T_Expression  ->
                         T_SelectItem 
sem_SelectItem_SelExp ex_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOcolumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: SelectItem
              _exOinLoop :: Bool
              _exOscope :: Scope
              _exOsourcePos :: MySourcePos
              _exIactualValue :: Expression
              _exIliftedColumnName :: String
              _exImessages :: ([Message])
              _exInodeType :: Type
              _lhsOnodeType =
                  _exInodeType
              _lhsOcolumnName =
                  case _exIliftedColumnName of
                    "" -> "?column?"
                    s -> s
              _lhsOmessages =
                  _exImessages
              _actualValue =
                  SelExp _exIactualValue
              _lhsOactualValue =
                  _actualValue
              _exOinLoop =
                  _lhsIinLoop
              _exOscope =
                  _lhsIscope
              _exOsourcePos =
                  _lhsIsourcePos
              ( _exIactualValue,_exIliftedColumnName,_exImessages,_exInodeType) =
                  (ex_ _exOinLoop _exOscope _exOsourcePos )
          in  ( _lhsOactualValue,_lhsOcolumnName,_lhsOmessages,_lhsOnodeType)))
sem_SelectItem_SelectItem :: T_Expression  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem ex_ name_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOcolumnName :: String
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: SelectItem
              _exOinLoop :: Bool
              _exOscope :: Scope
              _exOsourcePos :: MySourcePos
              _exIactualValue :: Expression
              _exIliftedColumnName :: String
              _exImessages :: ([Message])
              _exInodeType :: Type
              _lhsOnodeType =
                  _exInodeType
              _lhsOcolumnName =
                  name_
              _lhsOmessages =
                  _exImessages
              _actualValue =
                  SelectItem _exIactualValue name_
              _lhsOactualValue =
                  _actualValue
              _exOinLoop =
                  _lhsIinLoop
              _exOscope =
                  _lhsIscope
              _exOsourcePos =
                  _lhsIsourcePos
              ( _exIactualValue,_exIliftedColumnName,_exImessages,_exInodeType) =
                  (ex_ _exOinLoop _exOscope _exOsourcePos )
          in  ( _lhsOactualValue,_lhsOcolumnName,_lhsOmessages,_lhsOnodeType)))
-- SelectItemList ----------------------------------------------
type SelectItemList  = [(SelectItem)]
-- cata
sem_SelectItemList :: SelectItemList  ->
                      T_SelectItemList 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList  = Bool ->
                         Scope ->
                         MySourcePos ->
                         ( SelectItemList,([Message]),Type)
data Inh_SelectItemList  = Inh_SelectItemList {inLoop_Inh_SelectItemList :: Bool,scope_Inh_SelectItemList :: Scope,sourcePos_Inh_SelectItemList :: MySourcePos}
data Syn_SelectItemList  = Syn_SelectItemList {actualValue_Syn_SelectItemList :: SelectItemList,messages_Syn_SelectItemList :: [Message],nodeType_Syn_SelectItemList :: Type}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_SelectItemList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: SelectItemList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: SelectItem
              _hdIcolumnName :: String
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: SelectItemList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOnodeType =
                  foldr consComposite _tlInodeType
                     (let (correlationName,iden) = splitIdentifier _hdIcolumnName
                      in if iden == "*"
                          then scopeExpandStar _lhsIscope _lhsIsourcePos correlationName
                          else [(iden, _hdInodeType)])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdIcolumnName,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: SelectItemList
              _lhsOnodeType =
                  UnnamedCompositeType []
              _lhsOmessages =
                  []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- SelectList --------------------------------------------------
data SelectList  = SelectList (SelectItemList) (StringList) 
                 deriving ( Eq,Show)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _items _stringList )  =
    (sem_SelectList_SelectList (sem_SelectItemList _items ) (sem_StringList _stringList ) )
-- semantic domain
type T_SelectList  = Bool ->
                     Scope ->
                     MySourcePos ->
                     ( SelectList,([Message]),Type)
data Inh_SelectList  = Inh_SelectList {inLoop_Inh_SelectList :: Bool,scope_Inh_SelectList :: Scope,sourcePos_Inh_SelectList :: MySourcePos}
data Syn_SelectList  = Syn_SelectList {actualValue_Syn_SelectList :: SelectList,messages_Syn_SelectList :: [Message],nodeType_Syn_SelectList :: Type}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_SelectList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_SelectList_SelectList :: T_SelectItemList  ->
                             T_StringList  ->
                             T_SelectList 
sem_SelectList_SelectList items_ stringList_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: SelectList
              _itemsOinLoop :: Bool
              _itemsOscope :: Scope
              _itemsOsourcePos :: MySourcePos
              _stringListOinLoop :: Bool
              _stringListOscope :: Scope
              _stringListOsourcePos :: MySourcePos
              _itemsIactualValue :: SelectItemList
              _itemsImessages :: ([Message])
              _itemsInodeType :: Type
              _stringListIactualValue :: StringList
              _stringListImessages :: ([Message])
              _stringListInodeType :: Type
              _lhsOnodeType =
                  _itemsInodeType
              _lhsOmessages =
                  _itemsImessages ++ _stringListImessages
              _actualValue =
                  SelectList _itemsIactualValue _stringListIactualValue
              _lhsOactualValue =
                  _actualValue
              _itemsOinLoop =
                  _lhsIinLoop
              _itemsOscope =
                  _lhsIscope
              _itemsOsourcePos =
                  _lhsIsourcePos
              _stringListOinLoop =
                  _lhsIinLoop
              _stringListOscope =
                  _lhsIscope
              _stringListOsourcePos =
                  _lhsIsourcePos
              ( _itemsIactualValue,_itemsImessages,_itemsInodeType) =
                  (items_ _itemsOinLoop _itemsOscope _itemsOsourcePos )
              ( _stringListIactualValue,_stringListImessages,_stringListInodeType) =
                  (stringList_ _stringListOinLoop _stringListOscope _stringListOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- SetClause ---------------------------------------------------
data SetClause  = RowSetClause (StringList) (ExpressionList) 
                | SetClause (String) (Expression) 
                deriving ( Eq,Show)
-- cata
sem_SetClause :: SetClause  ->
                 T_SetClause 
sem_SetClause (RowSetClause _stringList _expressionList )  =
    (sem_SetClause_RowSetClause (sem_StringList _stringList ) (sem_ExpressionList _expressionList ) )
sem_SetClause (SetClause _string _expression )  =
    (sem_SetClause_SetClause _string (sem_Expression _expression ) )
-- semantic domain
type T_SetClause  = Bool ->
                    Scope ->
                    MySourcePos ->
                    ( SetClause,([Message]),Type)
data Inh_SetClause  = Inh_SetClause {inLoop_Inh_SetClause :: Bool,scope_Inh_SetClause :: Scope,sourcePos_Inh_SetClause :: MySourcePos}
data Syn_SetClause  = Syn_SetClause {actualValue_Syn_SetClause :: SetClause,messages_Syn_SetClause :: [Message],nodeType_Syn_SetClause :: Type}
wrap_SetClause :: T_SetClause  ->
                  Inh_SetClause  ->
                  Syn_SetClause 
wrap_SetClause sem (Inh_SetClause _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_SetClause _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_SetClause_RowSetClause :: T_StringList  ->
                              T_ExpressionList  ->
                              T_SetClause 
sem_SetClause_RowSetClause stringList_ expressionList_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: SetClause
              _stringListOinLoop :: Bool
              _stringListOscope :: Scope
              _stringListOsourcePos :: MySourcePos
              _expressionListOinLoop :: Bool
              _expressionListOscope :: Scope
              _expressionListOsourcePos :: MySourcePos
              _stringListIactualValue :: StringList
              _stringListImessages :: ([Message])
              _stringListInodeType :: Type
              _expressionListIactualValue :: ExpressionList
              _expressionListImessages :: ([Message])
              _expressionListInodeType :: Type
              _lhsOmessages =
                  _stringListImessages ++ _expressionListImessages
              _lhsOnodeType =
                  _stringListInodeType `setUnknown` _expressionListInodeType
              _actualValue =
                  RowSetClause _stringListIactualValue _expressionListIactualValue
              _lhsOactualValue =
                  _actualValue
              _stringListOinLoop =
                  _lhsIinLoop
              _stringListOscope =
                  _lhsIscope
              _stringListOsourcePos =
                  _lhsIsourcePos
              _expressionListOinLoop =
                  _lhsIinLoop
              _expressionListOscope =
                  _lhsIscope
              _expressionListOsourcePos =
                  _lhsIsourcePos
              ( _stringListIactualValue,_stringListImessages,_stringListInodeType) =
                  (stringList_ _stringListOinLoop _stringListOscope _stringListOsourcePos )
              ( _expressionListIactualValue,_expressionListImessages,_expressionListInodeType) =
                  (expressionList_ _expressionListOinLoop _expressionListOscope _expressionListOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_SetClause_SetClause :: String ->
                           T_Expression  ->
                           T_SetClause 
sem_SetClause_SetClause string_ expression_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: SetClause
              _expressionOinLoop :: Bool
              _expressionOscope :: Scope
              _expressionOsourcePos :: MySourcePos
              _expressionIactualValue :: Expression
              _expressionIliftedColumnName :: String
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _actualValue =
                  SetClause string_ _expressionIactualValue
              _lhsOactualValue =
                  _actualValue
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOscope =
                  _lhsIscope
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionIactualValue,_expressionIliftedColumnName,_expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOscope _expressionOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- SetClauseList -----------------------------------------------
type SetClauseList  = [(SetClause)]
-- cata
sem_SetClauseList :: SetClauseList  ->
                     T_SetClauseList 
sem_SetClauseList list  =
    (Prelude.foldr sem_SetClauseList_Cons sem_SetClauseList_Nil (Prelude.map sem_SetClause list) )
-- semantic domain
type T_SetClauseList  = Bool ->
                        Scope ->
                        MySourcePos ->
                        ( SetClauseList,([Message]),Type)
data Inh_SetClauseList  = Inh_SetClauseList {inLoop_Inh_SetClauseList :: Bool,scope_Inh_SetClauseList :: Scope,sourcePos_Inh_SetClauseList :: MySourcePos}
data Syn_SetClauseList  = Syn_SetClauseList {actualValue_Syn_SetClauseList :: SetClauseList,messages_Syn_SetClauseList :: [Message],nodeType_Syn_SetClauseList :: Type}
wrap_SetClauseList :: T_SetClauseList  ->
                      Inh_SetClauseList  ->
                      Syn_SetClauseList 
wrap_SetClauseList sem (Inh_SetClauseList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_SetClauseList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_SetClauseList_Cons :: T_SetClause  ->
                          T_SetClauseList  ->
                          T_SetClauseList 
sem_SetClauseList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: SetClauseList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: SetClause
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: SetClauseList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: SetClauseList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- SourcePosStatement ------------------------------------------
type SourcePosStatement  = ( (MySourcePos),(Statement))
-- cata
sem_SourcePosStatement :: SourcePosStatement  ->
                          T_SourcePosStatement 
sem_SourcePosStatement ( x1,x2)  =
    (sem_SourcePosStatement_Tuple x1 (sem_Statement x2 ) )
-- semantic domain
type T_SourcePosStatement  = Bool ->
                             Scope ->
                             MySourcePos ->
                             ( SourcePosStatement,([Message]),Type)
data Inh_SourcePosStatement  = Inh_SourcePosStatement {inLoop_Inh_SourcePosStatement :: Bool,scope_Inh_SourcePosStatement :: Scope,sourcePos_Inh_SourcePosStatement :: MySourcePos}
data Syn_SourcePosStatement  = Syn_SourcePosStatement {actualValue_Syn_SourcePosStatement :: SourcePosStatement,messages_Syn_SourcePosStatement :: [Message],nodeType_Syn_SourcePosStatement :: Type}
wrap_SourcePosStatement :: T_SourcePosStatement  ->
                           Inh_SourcePosStatement  ->
                           Syn_SourcePosStatement 
wrap_SourcePosStatement sem (Inh_SourcePosStatement _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_SourcePosStatement _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_SourcePosStatement_Tuple :: MySourcePos ->
                                T_Statement  ->
                                T_SourcePosStatement 
sem_SourcePosStatement_Tuple x1_ x2_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _x2OsourcePos :: MySourcePos
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: SourcePosStatement
              _x2OinLoop :: Bool
              _x2Oscope :: Scope
              _x2IactualValue :: Statement
              _x2Imessages :: ([Message])
              _x2InodeType :: Type
              _x2OsourcePos =
                  x1_
              _lhsOmessages =
                  _x2Imessages
              _lhsOnodeType =
                  _x2InodeType
              _actualValue =
                  (x1_,_x2IactualValue)
              _lhsOactualValue =
                  _actualValue
              _x2OinLoop =
                  _lhsIinLoop
              _x2Oscope =
                  _lhsIscope
              ( _x2IactualValue,_x2Imessages,_x2InodeType) =
                  (x2_ _x2OinLoop _x2Oscope _x2OsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- Statement ---------------------------------------------------
data Statement  = Assignment (String) (Expression) 
                | CaseStatement (Expression) (ExpressionListStatementListPairList) (StatementList) 
                | ContinueStatement 
                | Copy (String) (StringList) (CopySource) 
                | CopyData (String) 
                | CreateDomain (String) (TypeName) (Maybe Expression) 
                | CreateFunction (Language) (String) (ParamDefList) (TypeName) (String) (FnBody) (Volatility) 
                | CreateTable (String) (AttributeDefList) (ConstraintList) 
                | CreateTableAs (String) (SelectExpression) 
                | CreateType (String) (TypeAttributeDefList) 
                | CreateView (String) (SelectExpression) 
                | Delete (String) (Maybe Expression) (Maybe SelectList) 
                | DropFunction (IfExists) (StringStringListPairList) (Cascade) 
                | DropSomething (DropType) (IfExists) (StringList) (Cascade) 
                | Execute (Expression) 
                | ExecuteInto (Expression) (StringList) 
                | ForIntegerStatement (String) (Expression) (Expression) (StatementList) 
                | ForSelectStatement (String) (SelectExpression) (StatementList) 
                | If (ExpressionStatementListPairList) (StatementList) 
                | Insert (String) (StringList) (SelectExpression) (Maybe SelectList) 
                | NullStatement 
                | Perform (Expression) 
                | Raise (RaiseType) (String) (ExpressionList) 
                | Return (Maybe Expression) 
                | ReturnNext (Expression) 
                | ReturnQuery (SelectExpression) 
                | SelectStatement (SelectExpression) 
                | Truncate (StringList) (RestartIdentity) (Cascade) 
                | Update (String) (SetClauseList) (Maybe Expression) (Maybe SelectList) 
                | WhileStatement (Expression) (StatementList) 
                deriving ( Eq,Show)
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (Assignment _target _value )  =
    (sem_Statement_Assignment _target (sem_Expression _value ) )
sem_Statement (CaseStatement _val _cases _els )  =
    (sem_Statement_CaseStatement (sem_Expression _val ) (sem_ExpressionListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (ContinueStatement )  =
    (sem_Statement_ContinueStatement )
sem_Statement (Copy _table _targetCols _source )  =
    (sem_Statement_Copy _table (sem_StringList _targetCols ) (sem_CopySource _source ) )
sem_Statement (CopyData _insData )  =
    (sem_Statement_CopyData _insData )
sem_Statement (CreateDomain _name _typ _check )  =
    (sem_Statement_CreateDomain _name (sem_TypeName _typ ) _check )
sem_Statement (CreateFunction _lang _name _params _rettype _bodyQuote _body _vol )  =
    (sem_Statement_CreateFunction (sem_Language _lang ) _name (sem_ParamDefList _params ) (sem_TypeName _rettype ) _bodyQuote (sem_FnBody _body ) (sem_Volatility _vol ) )
sem_Statement (CreateTable _name _atts _cons )  =
    (sem_Statement_CreateTable _name (sem_AttributeDefList _atts ) (sem_ConstraintList _cons ) )
sem_Statement (CreateTableAs _name _expr )  =
    (sem_Statement_CreateTableAs _name (sem_SelectExpression _expr ) )
sem_Statement (CreateType _name _atts )  =
    (sem_Statement_CreateType _name (sem_TypeAttributeDefList _atts ) )
sem_Statement (CreateView _name _expr )  =
    (sem_Statement_CreateView _name (sem_SelectExpression _expr ) )
sem_Statement (Delete _table _whr _returning )  =
    (sem_Statement_Delete _table _whr _returning )
sem_Statement (DropFunction _ifE _sigs _cascade )  =
    (sem_Statement_DropFunction (sem_IfExists _ifE ) (sem_StringStringListPairList _sigs ) (sem_Cascade _cascade ) )
sem_Statement (DropSomething _dropType _ifE _names _cascade )  =
    (sem_Statement_DropSomething (sem_DropType _dropType ) (sem_IfExists _ifE ) (sem_StringList _names ) (sem_Cascade _cascade ) )
sem_Statement (Execute _expr )  =
    (sem_Statement_Execute (sem_Expression _expr ) )
sem_Statement (ExecuteInto _expr _targets )  =
    (sem_Statement_ExecuteInto (sem_Expression _expr ) (sem_StringList _targets ) )
sem_Statement (ForIntegerStatement _var _from _to _sts )  =
    (sem_Statement_ForIntegerStatement _var (sem_Expression _from ) (sem_Expression _to ) (sem_StatementList _sts ) )
sem_Statement (ForSelectStatement _var _sel _sts )  =
    (sem_Statement_ForSelectStatement _var (sem_SelectExpression _sel ) (sem_StatementList _sts ) )
sem_Statement (If _cases _els )  =
    (sem_Statement_If (sem_ExpressionStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (Insert _table _targetCols _insData _returning )  =
    (sem_Statement_Insert _table (sem_StringList _targetCols ) (sem_SelectExpression _insData ) _returning )
sem_Statement (NullStatement )  =
    (sem_Statement_NullStatement )
sem_Statement (Perform _expr )  =
    (sem_Statement_Perform (sem_Expression _expr ) )
sem_Statement (Raise _level _message _args )  =
    (sem_Statement_Raise (sem_RaiseType _level ) _message (sem_ExpressionList _args ) )
sem_Statement (Return _value )  =
    (sem_Statement_Return _value )
sem_Statement (ReturnNext _expr )  =
    (sem_Statement_ReturnNext (sem_Expression _expr ) )
sem_Statement (ReturnQuery _sel )  =
    (sem_Statement_ReturnQuery (sem_SelectExpression _sel ) )
sem_Statement (SelectStatement _ex )  =
    (sem_Statement_SelectStatement (sem_SelectExpression _ex ) )
sem_Statement (Truncate _tables _restartIdentity _cascade )  =
    (sem_Statement_Truncate (sem_StringList _tables ) (sem_RestartIdentity _restartIdentity ) (sem_Cascade _cascade ) )
sem_Statement (Update _table _assigns _whr _returning )  =
    (sem_Statement_Update _table (sem_SetClauseList _assigns ) _whr _returning )
sem_Statement (WhileStatement _expr _sts )  =
    (sem_Statement_WhileStatement (sem_Expression _expr ) (sem_StatementList _sts ) )
-- semantic domain
type T_Statement  = Bool ->
                    Scope ->
                    MySourcePos ->
                    ( Statement,([Message]),Type)
data Inh_Statement  = Inh_Statement {inLoop_Inh_Statement :: Bool,scope_Inh_Statement :: Scope,sourcePos_Inh_Statement :: MySourcePos}
data Syn_Statement  = Syn_Statement {actualValue_Syn_Statement :: Statement,messages_Syn_Statement :: [Message],nodeType_Syn_Statement :: Type}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_Statement _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_Statement_Assignment :: String ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_Assignment target_ value_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _valueOinLoop :: Bool
              _valueOscope :: Scope
              _valueOsourcePos :: MySourcePos
              _valueIactualValue :: Expression
              _valueIliftedColumnName :: String
              _valueImessages :: ([Message])
              _valueInodeType :: Type
              _lhsOmessages =
                  _valueImessages
              _lhsOnodeType =
                  _valueInodeType
              _actualValue =
                  Assignment target_ _valueIactualValue
              _lhsOactualValue =
                  _actualValue
              _valueOinLoop =
                  _lhsIinLoop
              _valueOscope =
                  _lhsIscope
              _valueOsourcePos =
                  _lhsIsourcePos
              ( _valueIactualValue,_valueIliftedColumnName,_valueImessages,_valueInodeType) =
                  (value_ _valueOinLoop _valueOscope _valueOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_CaseStatement :: T_Expression  ->
                               T_ExpressionListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement val_ cases_ els_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _valOinLoop :: Bool
              _valOscope :: Scope
              _valOsourcePos :: MySourcePos
              _casesOinLoop :: Bool
              _casesOscope :: Scope
              _casesOsourcePos :: MySourcePos
              _elsOinLoop :: Bool
              _elsOscope :: Scope
              _elsOsourcePos :: MySourcePos
              _valIactualValue :: Expression
              _valIliftedColumnName :: String
              _valImessages :: ([Message])
              _valInodeType :: Type
              _casesIactualValue :: ExpressionListStatementListPairList
              _casesImessages :: ([Message])
              _casesInodeType :: Type
              _elsIactualValue :: StatementList
              _elsImessages :: ([Message])
              _elsInodeType :: Type
              _lhsOmessages =
                  _valImessages ++ _casesImessages ++ _elsImessages
              _lhsOnodeType =
                  _valInodeType `setUnknown` _casesInodeType `setUnknown` _elsInodeType
              _actualValue =
                  CaseStatement _valIactualValue _casesIactualValue _elsIactualValue
              _lhsOactualValue =
                  _actualValue
              _valOinLoop =
                  _lhsIinLoop
              _valOscope =
                  _lhsIscope
              _valOsourcePos =
                  _lhsIsourcePos
              _casesOinLoop =
                  _lhsIinLoop
              _casesOscope =
                  _lhsIscope
              _casesOsourcePos =
                  _lhsIsourcePos
              _elsOinLoop =
                  _lhsIinLoop
              _elsOscope =
                  _lhsIscope
              _elsOsourcePos =
                  _lhsIsourcePos
              ( _valIactualValue,_valIliftedColumnName,_valImessages,_valInodeType) =
                  (val_ _valOinLoop _valOscope _valOsourcePos )
              ( _casesIactualValue,_casesImessages,_casesInodeType) =
                  (cases_ _casesOinLoop _casesOscope _casesOsourcePos )
              ( _elsIactualValue,_elsImessages,_elsInodeType) =
                  (els_ _elsOinLoop _elsOscope _elsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_ContinueStatement :: T_Statement 
sem_Statement_ContinueStatement  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _lhsOmessages =
                  if not _lhsIinLoop
                    then [Error _lhsIsourcePos ContinueNotInLoop]
                    else []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  ContinueStatement
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_Copy :: String ->
                      T_StringList  ->
                      T_CopySource  ->
                      T_Statement 
sem_Statement_Copy table_ targetCols_ source_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _targetColsOinLoop :: Bool
              _targetColsOscope :: Scope
              _targetColsOsourcePos :: MySourcePos
              _sourceOinLoop :: Bool
              _sourceOscope :: Scope
              _sourceOsourcePos :: MySourcePos
              _targetColsIactualValue :: StringList
              _targetColsImessages :: ([Message])
              _targetColsInodeType :: Type
              _sourceIactualValue :: CopySource
              _sourceImessages :: ([Message])
              _sourceInodeType :: Type
              _lhsOmessages =
                  _targetColsImessages ++ _sourceImessages
              _lhsOnodeType =
                  _targetColsInodeType `setUnknown` _sourceInodeType
              _actualValue =
                  Copy table_ _targetColsIactualValue _sourceIactualValue
              _lhsOactualValue =
                  _actualValue
              _targetColsOinLoop =
                  _lhsIinLoop
              _targetColsOscope =
                  _lhsIscope
              _targetColsOsourcePos =
                  _lhsIsourcePos
              _sourceOinLoop =
                  _lhsIinLoop
              _sourceOscope =
                  _lhsIscope
              _sourceOsourcePos =
                  _lhsIsourcePos
              ( _targetColsIactualValue,_targetColsImessages,_targetColsInodeType) =
                  (targetCols_ _targetColsOinLoop _targetColsOscope _targetColsOsourcePos )
              ( _sourceIactualValue,_sourceImessages,_sourceInodeType) =
                  (source_ _sourceOinLoop _sourceOscope _sourceOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_CopyData :: String ->
                          T_Statement 
sem_Statement_CopyData insData_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  CopyData insData_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateDomain :: String ->
                              T_TypeName  ->
                              (Maybe Expression) ->
                              T_Statement 
sem_Statement_CreateDomain name_ typ_ check_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _typOinLoop :: Bool
              _typOscope :: Scope
              _typOsourcePos :: MySourcePos
              _typIactualValue :: TypeName
              _typImessages :: ([Message])
              _typInodeType :: Type
              _lhsOmessages =
                  _typImessages
              _lhsOnodeType =
                  _typInodeType
              _actualValue =
                  CreateDomain name_ _typIactualValue check_
              _lhsOactualValue =
                  _actualValue
              _typOinLoop =
                  _lhsIinLoop
              _typOscope =
                  _lhsIscope
              _typOsourcePos =
                  _lhsIsourcePos
              ( _typIactualValue,_typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOscope _typOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateFunction :: T_Language  ->
                                String ->
                                T_ParamDefList  ->
                                T_TypeName  ->
                                String ->
                                T_FnBody  ->
                                T_Volatility  ->
                                T_Statement 
sem_Statement_CreateFunction lang_ name_ params_ rettype_ bodyQuote_ body_ vol_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _bodyOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _langOinLoop :: Bool
              _langOscope :: Scope
              _langOsourcePos :: MySourcePos
              _paramsOinLoop :: Bool
              _paramsOscope :: Scope
              _paramsOsourcePos :: MySourcePos
              _rettypeOinLoop :: Bool
              _rettypeOscope :: Scope
              _rettypeOsourcePos :: MySourcePos
              _bodyOscope :: Scope
              _bodyOsourcePos :: MySourcePos
              _volOinLoop :: Bool
              _volOscope :: Scope
              _volOsourcePos :: MySourcePos
              _langIactualValue :: Language
              _langImessages :: ([Message])
              _langInodeType :: Type
              _paramsIactualValue :: ParamDefList
              _paramsImessages :: ([Message])
              _paramsInodeType :: Type
              _rettypeIactualValue :: TypeName
              _rettypeImessages :: ([Message])
              _rettypeInodeType :: Type
              _bodyIactualValue :: FnBody
              _bodyImessages :: ([Message])
              _bodyInodeType :: Type
              _volIactualValue :: Volatility
              _volImessages :: ([Message])
              _volInodeType :: Type
              _bodyOinLoop =
                  False
              _lhsOmessages =
                  _langImessages ++ _paramsImessages ++ _rettypeImessages ++ _bodyImessages ++ _volImessages
              _lhsOnodeType =
                  _langInodeType `setUnknown` _paramsInodeType `setUnknown` _rettypeInodeType `setUnknown` _bodyInodeType `setUnknown` _volInodeType
              _actualValue =
                  CreateFunction _langIactualValue name_ _paramsIactualValue _rettypeIactualValue bodyQuote_ _bodyIactualValue _volIactualValue
              _lhsOactualValue =
                  _actualValue
              _langOinLoop =
                  _lhsIinLoop
              _langOscope =
                  _lhsIscope
              _langOsourcePos =
                  _lhsIsourcePos
              _paramsOinLoop =
                  _lhsIinLoop
              _paramsOscope =
                  _lhsIscope
              _paramsOsourcePos =
                  _lhsIsourcePos
              _rettypeOinLoop =
                  _lhsIinLoop
              _rettypeOscope =
                  _lhsIscope
              _rettypeOsourcePos =
                  _lhsIsourcePos
              _bodyOscope =
                  _lhsIscope
              _bodyOsourcePos =
                  _lhsIsourcePos
              _volOinLoop =
                  _lhsIinLoop
              _volOscope =
                  _lhsIscope
              _volOsourcePos =
                  _lhsIsourcePos
              ( _langIactualValue,_langImessages,_langInodeType) =
                  (lang_ _langOinLoop _langOscope _langOsourcePos )
              ( _paramsIactualValue,_paramsImessages,_paramsInodeType) =
                  (params_ _paramsOinLoop _paramsOscope _paramsOsourcePos )
              ( _rettypeIactualValue,_rettypeImessages,_rettypeInodeType) =
                  (rettype_ _rettypeOinLoop _rettypeOscope _rettypeOsourcePos )
              ( _bodyIactualValue,_bodyImessages,_bodyInodeType) =
                  (body_ _bodyOinLoop _bodyOscope _bodyOsourcePos )
              ( _volIactualValue,_volImessages,_volInodeType) =
                  (vol_ _volOinLoop _volOscope _volOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateTable :: String ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable name_ atts_ cons_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _attsOinLoop :: Bool
              _attsOscope :: Scope
              _attsOsourcePos :: MySourcePos
              _consOinLoop :: Bool
              _consOscope :: Scope
              _consOsourcePos :: MySourcePos
              _attsIactualValue :: AttributeDefList
              _attsImessages :: ([Message])
              _attsInodeType :: Type
              _consIactualValue :: ConstraintList
              _consImessages :: ([Message])
              _consInodeType :: Type
              _lhsOmessages =
                  _attsImessages ++ _consImessages
              _lhsOnodeType =
                  _attsInodeType `setUnknown` _consInodeType
              _actualValue =
                  CreateTable name_ _attsIactualValue _consIactualValue
              _lhsOactualValue =
                  _actualValue
              _attsOinLoop =
                  _lhsIinLoop
              _attsOscope =
                  _lhsIscope
              _attsOsourcePos =
                  _lhsIsourcePos
              _consOinLoop =
                  _lhsIinLoop
              _consOscope =
                  _lhsIscope
              _consOsourcePos =
                  _lhsIsourcePos
              ( _attsIactualValue,_attsImessages,_attsInodeType) =
                  (atts_ _attsOinLoop _attsOscope _attsOsourcePos )
              ( _consIactualValue,_consImessages,_consInodeType) =
                  (cons_ _consOinLoop _consOscope _consOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateTableAs :: String ->
                               T_SelectExpression  ->
                               T_Statement 
sem_Statement_CreateTableAs name_ expr_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _exprOinLoop :: Bool
              _exprOscope :: Scope
              _exprOsourcePos :: MySourcePos
              _exprIactualValue :: SelectExpression
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _actualValue =
                  CreateTableAs name_ _exprIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprOinLoop =
                  _lhsIinLoop
              _exprOscope =
                  _lhsIscope
              _exprOsourcePos =
                  _lhsIsourcePos
              ( _exprIactualValue,_exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOscope _exprOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateType :: String ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType name_ atts_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _attsOinLoop :: Bool
              _attsOscope :: Scope
              _attsOsourcePos :: MySourcePos
              _attsIactualValue :: TypeAttributeDefList
              _attsImessages :: ([Message])
              _attsInodeType :: Type
              _lhsOmessages =
                  _attsImessages
              _lhsOnodeType =
                  _attsInodeType
              _actualValue =
                  CreateType name_ _attsIactualValue
              _lhsOactualValue =
                  _actualValue
              _attsOinLoop =
                  _lhsIinLoop
              _attsOscope =
                  _lhsIscope
              _attsOsourcePos =
                  _lhsIsourcePos
              ( _attsIactualValue,_attsImessages,_attsInodeType) =
                  (atts_ _attsOinLoop _attsOscope _attsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateView :: String ->
                            T_SelectExpression  ->
                            T_Statement 
sem_Statement_CreateView name_ expr_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _exprOinLoop :: Bool
              _exprOscope :: Scope
              _exprOsourcePos :: MySourcePos
              _exprIactualValue :: SelectExpression
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _actualValue =
                  CreateView name_ _exprIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprOinLoop =
                  _lhsIinLoop
              _exprOscope =
                  _lhsIscope
              _exprOsourcePos =
                  _lhsIsourcePos
              ( _exprIactualValue,_exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOscope _exprOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_Delete :: String ->
                        (Maybe Expression) ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Delete table_ whr_ returning_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Delete table_ whr_ returning_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_DropFunction :: T_IfExists  ->
                              T_StringStringListPairList  ->
                              T_Cascade  ->
                              T_Statement 
sem_Statement_DropFunction ifE_ sigs_ cascade_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _ifEOinLoop :: Bool
              _ifEOscope :: Scope
              _ifEOsourcePos :: MySourcePos
              _sigsOinLoop :: Bool
              _sigsOscope :: Scope
              _sigsOsourcePos :: MySourcePos
              _cascadeOinLoop :: Bool
              _cascadeOscope :: Scope
              _cascadeOsourcePos :: MySourcePos
              _ifEIactualValue :: IfExists
              _ifEImessages :: ([Message])
              _ifEInodeType :: Type
              _sigsIactualValue :: StringStringListPairList
              _sigsImessages :: ([Message])
              _sigsInodeType :: Type
              _cascadeIactualValue :: Cascade
              _cascadeImessages :: ([Message])
              _cascadeInodeType :: Type
              _lhsOmessages =
                  _ifEImessages ++ _sigsImessages ++ _cascadeImessages
              _lhsOnodeType =
                  _ifEInodeType `setUnknown` _sigsInodeType `setUnknown` _cascadeInodeType
              _actualValue =
                  DropFunction _ifEIactualValue _sigsIactualValue _cascadeIactualValue
              _lhsOactualValue =
                  _actualValue
              _ifEOinLoop =
                  _lhsIinLoop
              _ifEOscope =
                  _lhsIscope
              _ifEOsourcePos =
                  _lhsIsourcePos
              _sigsOinLoop =
                  _lhsIinLoop
              _sigsOscope =
                  _lhsIscope
              _sigsOsourcePos =
                  _lhsIsourcePos
              _cascadeOinLoop =
                  _lhsIinLoop
              _cascadeOscope =
                  _lhsIscope
              _cascadeOsourcePos =
                  _lhsIsourcePos
              ( _ifEIactualValue,_ifEImessages,_ifEInodeType) =
                  (ifE_ _ifEOinLoop _ifEOscope _ifEOsourcePos )
              ( _sigsIactualValue,_sigsImessages,_sigsInodeType) =
                  (sigs_ _sigsOinLoop _sigsOscope _sigsOsourcePos )
              ( _cascadeIactualValue,_cascadeImessages,_cascadeInodeType) =
                  (cascade_ _cascadeOinLoop _cascadeOscope _cascadeOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_DropSomething :: T_DropType  ->
                               T_IfExists  ->
                               T_StringList  ->
                               T_Cascade  ->
                               T_Statement 
sem_Statement_DropSomething dropType_ ifE_ names_ cascade_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _dropTypeOinLoop :: Bool
              _dropTypeOscope :: Scope
              _dropTypeOsourcePos :: MySourcePos
              _ifEOinLoop :: Bool
              _ifEOscope :: Scope
              _ifEOsourcePos :: MySourcePos
              _namesOinLoop :: Bool
              _namesOscope :: Scope
              _namesOsourcePos :: MySourcePos
              _cascadeOinLoop :: Bool
              _cascadeOscope :: Scope
              _cascadeOsourcePos :: MySourcePos
              _dropTypeIactualValue :: DropType
              _dropTypeImessages :: ([Message])
              _dropTypeInodeType :: Type
              _ifEIactualValue :: IfExists
              _ifEImessages :: ([Message])
              _ifEInodeType :: Type
              _namesIactualValue :: StringList
              _namesImessages :: ([Message])
              _namesInodeType :: Type
              _cascadeIactualValue :: Cascade
              _cascadeImessages :: ([Message])
              _cascadeInodeType :: Type
              _lhsOmessages =
                  _dropTypeImessages ++ _ifEImessages ++ _namesImessages ++ _cascadeImessages
              _lhsOnodeType =
                  _dropTypeInodeType `setUnknown` _ifEInodeType `setUnknown` _namesInodeType `setUnknown` _cascadeInodeType
              _actualValue =
                  DropSomething _dropTypeIactualValue _ifEIactualValue _namesIactualValue _cascadeIactualValue
              _lhsOactualValue =
                  _actualValue
              _dropTypeOinLoop =
                  _lhsIinLoop
              _dropTypeOscope =
                  _lhsIscope
              _dropTypeOsourcePos =
                  _lhsIsourcePos
              _ifEOinLoop =
                  _lhsIinLoop
              _ifEOscope =
                  _lhsIscope
              _ifEOsourcePos =
                  _lhsIsourcePos
              _namesOinLoop =
                  _lhsIinLoop
              _namesOscope =
                  _lhsIscope
              _namesOsourcePos =
                  _lhsIsourcePos
              _cascadeOinLoop =
                  _lhsIinLoop
              _cascadeOscope =
                  _lhsIscope
              _cascadeOsourcePos =
                  _lhsIsourcePos
              ( _dropTypeIactualValue,_dropTypeImessages,_dropTypeInodeType) =
                  (dropType_ _dropTypeOinLoop _dropTypeOscope _dropTypeOsourcePos )
              ( _ifEIactualValue,_ifEImessages,_ifEInodeType) =
                  (ifE_ _ifEOinLoop _ifEOscope _ifEOsourcePos )
              ( _namesIactualValue,_namesImessages,_namesInodeType) =
                  (names_ _namesOinLoop _namesOscope _namesOsourcePos )
              ( _cascadeIactualValue,_cascadeImessages,_cascadeInodeType) =
                  (cascade_ _cascadeOinLoop _cascadeOscope _cascadeOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_Execute :: T_Expression  ->
                         T_Statement 
sem_Statement_Execute expr_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _exprOinLoop :: Bool
              _exprOscope :: Scope
              _exprOsourcePos :: MySourcePos
              _exprIactualValue :: Expression
              _exprIliftedColumnName :: String
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _actualValue =
                  Execute _exprIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprOinLoop =
                  _lhsIinLoop
              _exprOscope =
                  _lhsIscope
              _exprOsourcePos =
                  _lhsIsourcePos
              ( _exprIactualValue,_exprIliftedColumnName,_exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOscope _exprOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_ExecuteInto :: T_Expression  ->
                             T_StringList  ->
                             T_Statement 
sem_Statement_ExecuteInto expr_ targets_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _exprOinLoop :: Bool
              _exprOscope :: Scope
              _exprOsourcePos :: MySourcePos
              _targetsOinLoop :: Bool
              _targetsOscope :: Scope
              _targetsOsourcePos :: MySourcePos
              _exprIactualValue :: Expression
              _exprIliftedColumnName :: String
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _targetsIactualValue :: StringList
              _targetsImessages :: ([Message])
              _targetsInodeType :: Type
              _lhsOmessages =
                  _exprImessages ++ _targetsImessages
              _lhsOnodeType =
                  _exprInodeType `setUnknown` _targetsInodeType
              _actualValue =
                  ExecuteInto _exprIactualValue _targetsIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprOinLoop =
                  _lhsIinLoop
              _exprOscope =
                  _lhsIscope
              _exprOsourcePos =
                  _lhsIsourcePos
              _targetsOinLoop =
                  _lhsIinLoop
              _targetsOscope =
                  _lhsIscope
              _targetsOsourcePos =
                  _lhsIsourcePos
              ( _exprIactualValue,_exprIliftedColumnName,_exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOscope _exprOsourcePos )
              ( _targetsIactualValue,_targetsImessages,_targetsInodeType) =
                  (targets_ _targetsOinLoop _targetsOscope _targetsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_ForIntegerStatement :: String ->
                                     T_Expression  ->
                                     T_Expression  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement var_ from_ to_ sts_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _stsOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _fromOinLoop :: Bool
              _fromOscope :: Scope
              _fromOsourcePos :: MySourcePos
              _toOinLoop :: Bool
              _toOscope :: Scope
              _toOsourcePos :: MySourcePos
              _stsOscope :: Scope
              _stsOsourcePos :: MySourcePos
              _fromIactualValue :: Expression
              _fromIliftedColumnName :: String
              _fromImessages :: ([Message])
              _fromInodeType :: Type
              _toIactualValue :: Expression
              _toIliftedColumnName :: String
              _toImessages :: ([Message])
              _toInodeType :: Type
              _stsIactualValue :: StatementList
              _stsImessages :: ([Message])
              _stsInodeType :: Type
              _stsOinLoop =
                  True
              _lhsOmessages =
                  _fromImessages ++ _toImessages ++ _stsImessages
              _lhsOnodeType =
                  _fromInodeType `setUnknown` _toInodeType `setUnknown` _stsInodeType
              _actualValue =
                  ForIntegerStatement var_ _fromIactualValue _toIactualValue _stsIactualValue
              _lhsOactualValue =
                  _actualValue
              _fromOinLoop =
                  _lhsIinLoop
              _fromOscope =
                  _lhsIscope
              _fromOsourcePos =
                  _lhsIsourcePos
              _toOinLoop =
                  _lhsIinLoop
              _toOscope =
                  _lhsIscope
              _toOsourcePos =
                  _lhsIsourcePos
              _stsOscope =
                  _lhsIscope
              _stsOsourcePos =
                  _lhsIsourcePos
              ( _fromIactualValue,_fromIliftedColumnName,_fromImessages,_fromInodeType) =
                  (from_ _fromOinLoop _fromOscope _fromOsourcePos )
              ( _toIactualValue,_toIliftedColumnName,_toImessages,_toInodeType) =
                  (to_ _toOinLoop _toOscope _toOsourcePos )
              ( _stsIactualValue,_stsImessages,_stsInodeType) =
                  (sts_ _stsOinLoop _stsOscope _stsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_ForSelectStatement :: String ->
                                    T_SelectExpression  ->
                                    T_StatementList  ->
                                    T_Statement 
sem_Statement_ForSelectStatement var_ sel_ sts_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _stsOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _selOinLoop :: Bool
              _selOscope :: Scope
              _selOsourcePos :: MySourcePos
              _stsOscope :: Scope
              _stsOsourcePos :: MySourcePos
              _selIactualValue :: SelectExpression
              _selImessages :: ([Message])
              _selInodeType :: Type
              _stsIactualValue :: StatementList
              _stsImessages :: ([Message])
              _stsInodeType :: Type
              _stsOinLoop =
                  True
              _lhsOmessages =
                  _selImessages ++ _stsImessages
              _lhsOnodeType =
                  _selInodeType `setUnknown` _stsInodeType
              _actualValue =
                  ForSelectStatement var_ _selIactualValue _stsIactualValue
              _lhsOactualValue =
                  _actualValue
              _selOinLoop =
                  _lhsIinLoop
              _selOscope =
                  _lhsIscope
              _selOsourcePos =
                  _lhsIsourcePos
              _stsOscope =
                  _lhsIscope
              _stsOsourcePos =
                  _lhsIsourcePos
              ( _selIactualValue,_selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOscope _selOsourcePos )
              ( _stsIactualValue,_stsImessages,_stsInodeType) =
                  (sts_ _stsOinLoop _stsOscope _stsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_If :: T_ExpressionStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If cases_ els_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _casesOinLoop :: Bool
              _casesOscope :: Scope
              _casesOsourcePos :: MySourcePos
              _elsOinLoop :: Bool
              _elsOscope :: Scope
              _elsOsourcePos :: MySourcePos
              _casesIactualValue :: ExpressionStatementListPairList
              _casesImessages :: ([Message])
              _casesInodeType :: Type
              _elsIactualValue :: StatementList
              _elsImessages :: ([Message])
              _elsInodeType :: Type
              _lhsOmessages =
                  _casesImessages ++ _elsImessages
              _lhsOnodeType =
                  _casesInodeType `setUnknown` _elsInodeType
              _actualValue =
                  If _casesIactualValue _elsIactualValue
              _lhsOactualValue =
                  _actualValue
              _casesOinLoop =
                  _lhsIinLoop
              _casesOscope =
                  _lhsIscope
              _casesOsourcePos =
                  _lhsIsourcePos
              _elsOinLoop =
                  _lhsIinLoop
              _elsOscope =
                  _lhsIscope
              _elsOsourcePos =
                  _lhsIsourcePos
              ( _casesIactualValue,_casesImessages,_casesInodeType) =
                  (cases_ _casesOinLoop _casesOscope _casesOsourcePos )
              ( _elsIactualValue,_elsImessages,_elsInodeType) =
                  (els_ _elsOinLoop _elsOscope _elsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_Insert :: String ->
                        T_StringList  ->
                        T_SelectExpression  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Insert table_ targetCols_ insData_ returning_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _targetColsOinLoop :: Bool
              _targetColsOscope :: Scope
              _targetColsOsourcePos :: MySourcePos
              _insDataOinLoop :: Bool
              _insDataOscope :: Scope
              _insDataOsourcePos :: MySourcePos
              _targetColsIactualValue :: StringList
              _targetColsImessages :: ([Message])
              _targetColsInodeType :: Type
              _insDataIactualValue :: SelectExpression
              _insDataImessages :: ([Message])
              _insDataInodeType :: Type
              _lhsOmessages =
                  _targetColsImessages ++ _insDataImessages
              _lhsOnodeType =
                  _targetColsInodeType `setUnknown` _insDataInodeType
              _actualValue =
                  Insert table_ _targetColsIactualValue _insDataIactualValue returning_
              _lhsOactualValue =
                  _actualValue
              _targetColsOinLoop =
                  _lhsIinLoop
              _targetColsOscope =
                  _lhsIscope
              _targetColsOsourcePos =
                  _lhsIsourcePos
              _insDataOinLoop =
                  _lhsIinLoop
              _insDataOscope =
                  _lhsIscope
              _insDataOsourcePos =
                  _lhsIsourcePos
              ( _targetColsIactualValue,_targetColsImessages,_targetColsInodeType) =
                  (targetCols_ _targetColsOinLoop _targetColsOscope _targetColsOsourcePos )
              ( _insDataIactualValue,_insDataImessages,_insDataInodeType) =
                  (insData_ _insDataOinLoop _insDataOscope _insDataOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_NullStatement :: T_Statement 
sem_Statement_NullStatement  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  NullStatement
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_Perform :: T_Expression  ->
                         T_Statement 
sem_Statement_Perform expr_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _exprOinLoop :: Bool
              _exprOscope :: Scope
              _exprOsourcePos :: MySourcePos
              _exprIactualValue :: Expression
              _exprIliftedColumnName :: String
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _actualValue =
                  Perform _exprIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprOinLoop =
                  _lhsIinLoop
              _exprOscope =
                  _lhsIscope
              _exprOsourcePos =
                  _lhsIsourcePos
              ( _exprIactualValue,_exprIliftedColumnName,_exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOscope _exprOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_Raise :: T_RaiseType  ->
                       String ->
                       T_ExpressionList  ->
                       T_Statement 
sem_Statement_Raise level_ message_ args_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _levelOinLoop :: Bool
              _levelOscope :: Scope
              _levelOsourcePos :: MySourcePos
              _argsOinLoop :: Bool
              _argsOscope :: Scope
              _argsOsourcePos :: MySourcePos
              _levelIactualValue :: RaiseType
              _levelImessages :: ([Message])
              _levelInodeType :: Type
              _argsIactualValue :: ExpressionList
              _argsImessages :: ([Message])
              _argsInodeType :: Type
              _lhsOmessages =
                  _levelImessages ++ _argsImessages
              _lhsOnodeType =
                  _levelInodeType `setUnknown` _argsInodeType
              _actualValue =
                  Raise _levelIactualValue message_ _argsIactualValue
              _lhsOactualValue =
                  _actualValue
              _levelOinLoop =
                  _lhsIinLoop
              _levelOscope =
                  _lhsIscope
              _levelOsourcePos =
                  _lhsIsourcePos
              _argsOinLoop =
                  _lhsIinLoop
              _argsOscope =
                  _lhsIscope
              _argsOsourcePos =
                  _lhsIsourcePos
              ( _levelIactualValue,_levelImessages,_levelInodeType) =
                  (level_ _levelOinLoop _levelOscope _levelOsourcePos )
              ( _argsIactualValue,_argsImessages,_argsInodeType) =
                  (args_ _argsOinLoop _argsOscope _argsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_Return :: (Maybe Expression) ->
                        T_Statement 
sem_Statement_Return value_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Return value_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_ReturnNext :: T_Expression  ->
                            T_Statement 
sem_Statement_ReturnNext expr_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _exprOinLoop :: Bool
              _exprOscope :: Scope
              _exprOsourcePos :: MySourcePos
              _exprIactualValue :: Expression
              _exprIliftedColumnName :: String
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _actualValue =
                  ReturnNext _exprIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprOinLoop =
                  _lhsIinLoop
              _exprOscope =
                  _lhsIscope
              _exprOsourcePos =
                  _lhsIsourcePos
              ( _exprIactualValue,_exprIliftedColumnName,_exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOscope _exprOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_ReturnQuery :: T_SelectExpression  ->
                             T_Statement 
sem_Statement_ReturnQuery sel_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _selOinLoop :: Bool
              _selOscope :: Scope
              _selOsourcePos :: MySourcePos
              _selIactualValue :: SelectExpression
              _selImessages :: ([Message])
              _selInodeType :: Type
              _lhsOmessages =
                  _selImessages
              _lhsOnodeType =
                  _selInodeType
              _actualValue =
                  ReturnQuery _selIactualValue
              _lhsOactualValue =
                  _actualValue
              _selOinLoop =
                  _lhsIinLoop
              _selOscope =
                  _lhsIscope
              _selOsourcePos =
                  _lhsIsourcePos
              ( _selIactualValue,_selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOscope _selOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_SelectStatement :: T_SelectExpression  ->
                                 T_Statement 
sem_Statement_SelectStatement ex_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Statement
              _exOinLoop :: Bool
              _exOscope :: Scope
              _exOsourcePos :: MySourcePos
              _exIactualValue :: SelectExpression
              _exImessages :: ([Message])
              _exInodeType :: Type
              _lhsOnodeType =
                  _exInodeType
              _lhsOmessages =
                  _exImessages
              _actualValue =
                  SelectStatement _exIactualValue
              _lhsOactualValue =
                  _actualValue
              _exOinLoop =
                  _lhsIinLoop
              _exOscope =
                  _lhsIscope
              _exOsourcePos =
                  _lhsIsourcePos
              ( _exIactualValue,_exImessages,_exInodeType) =
                  (ex_ _exOinLoop _exOscope _exOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_Truncate :: T_StringList  ->
                          T_RestartIdentity  ->
                          T_Cascade  ->
                          T_Statement 
sem_Statement_Truncate tables_ restartIdentity_ cascade_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _tablesOinLoop :: Bool
              _tablesOscope :: Scope
              _tablesOsourcePos :: MySourcePos
              _restartIdentityOinLoop :: Bool
              _restartIdentityOscope :: Scope
              _restartIdentityOsourcePos :: MySourcePos
              _cascadeOinLoop :: Bool
              _cascadeOscope :: Scope
              _cascadeOsourcePos :: MySourcePos
              _tablesIactualValue :: StringList
              _tablesImessages :: ([Message])
              _tablesInodeType :: Type
              _restartIdentityIactualValue :: RestartIdentity
              _restartIdentityImessages :: ([Message])
              _restartIdentityInodeType :: Type
              _cascadeIactualValue :: Cascade
              _cascadeImessages :: ([Message])
              _cascadeInodeType :: Type
              _lhsOmessages =
                  _tablesImessages ++ _restartIdentityImessages ++ _cascadeImessages
              _lhsOnodeType =
                  _tablesInodeType `setUnknown` _restartIdentityInodeType `setUnknown` _cascadeInodeType
              _actualValue =
                  Truncate _tablesIactualValue _restartIdentityIactualValue _cascadeIactualValue
              _lhsOactualValue =
                  _actualValue
              _tablesOinLoop =
                  _lhsIinLoop
              _tablesOscope =
                  _lhsIscope
              _tablesOsourcePos =
                  _lhsIsourcePos
              _restartIdentityOinLoop =
                  _lhsIinLoop
              _restartIdentityOscope =
                  _lhsIscope
              _restartIdentityOsourcePos =
                  _lhsIsourcePos
              _cascadeOinLoop =
                  _lhsIinLoop
              _cascadeOscope =
                  _lhsIscope
              _cascadeOsourcePos =
                  _lhsIsourcePos
              ( _tablesIactualValue,_tablesImessages,_tablesInodeType) =
                  (tables_ _tablesOinLoop _tablesOscope _tablesOsourcePos )
              ( _restartIdentityIactualValue,_restartIdentityImessages,_restartIdentityInodeType) =
                  (restartIdentity_ _restartIdentityOinLoop _restartIdentityOscope _restartIdentityOsourcePos )
              ( _cascadeIactualValue,_cascadeImessages,_cascadeInodeType) =
                  (cascade_ _cascadeOinLoop _cascadeOscope _cascadeOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_Update :: String ->
                        T_SetClauseList  ->
                        (Maybe Expression) ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Update table_ assigns_ whr_ returning_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _assignsOinLoop :: Bool
              _assignsOscope :: Scope
              _assignsOsourcePos :: MySourcePos
              _assignsIactualValue :: SetClauseList
              _assignsImessages :: ([Message])
              _assignsInodeType :: Type
              _lhsOmessages =
                  _assignsImessages
              _lhsOnodeType =
                  _assignsInodeType
              _actualValue =
                  Update table_ _assignsIactualValue whr_ returning_
              _lhsOactualValue =
                  _actualValue
              _assignsOinLoop =
                  _lhsIinLoop
              _assignsOscope =
                  _lhsIscope
              _assignsOsourcePos =
                  _lhsIsourcePos
              ( _assignsIactualValue,_assignsImessages,_assignsInodeType) =
                  (assigns_ _assignsOinLoop _assignsOscope _assignsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Statement_WhileStatement :: T_Expression  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement expr_ sts_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _stsOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Statement
              _exprOinLoop :: Bool
              _exprOscope :: Scope
              _exprOsourcePos :: MySourcePos
              _stsOscope :: Scope
              _stsOsourcePos :: MySourcePos
              _exprIactualValue :: Expression
              _exprIliftedColumnName :: String
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _stsIactualValue :: StatementList
              _stsImessages :: ([Message])
              _stsInodeType :: Type
              _stsOinLoop =
                  True
              _lhsOmessages =
                  _exprImessages ++ _stsImessages
              _lhsOnodeType =
                  _exprInodeType `setUnknown` _stsInodeType
              _actualValue =
                  WhileStatement _exprIactualValue _stsIactualValue
              _lhsOactualValue =
                  _actualValue
              _exprOinLoop =
                  _lhsIinLoop
              _exprOscope =
                  _lhsIscope
              _exprOsourcePos =
                  _lhsIsourcePos
              _stsOscope =
                  _lhsIscope
              _stsOsourcePos =
                  _lhsIsourcePos
              ( _exprIactualValue,_exprIliftedColumnName,_exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOscope _exprOsourcePos )
              ( _stsIactualValue,_stsImessages,_stsInodeType) =
                  (sts_ _stsOinLoop _stsOscope _stsOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- StatementList -----------------------------------------------
type StatementList  = [(SourcePosStatement)]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_SourcePosStatement list) )
-- semantic domain
type T_StatementList  = Bool ->
                        Scope ->
                        MySourcePos ->
                        ( StatementList,([Message]),Type)
data Inh_StatementList  = Inh_StatementList {inLoop_Inh_StatementList :: Bool,scope_Inh_StatementList :: Scope,sourcePos_Inh_StatementList :: MySourcePos}
data Syn_StatementList  = Syn_StatementList {actualValue_Syn_StatementList :: StatementList,messages_Syn_StatementList :: [Message],nodeType_Syn_StatementList :: Type}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_StatementList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_StatementList_Cons :: T_SourcePosStatement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: StatementList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: SourcePosStatement
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: StatementList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: StatementList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- StringList --------------------------------------------------
type StringList  = [(String)]
-- cata
sem_StringList :: StringList  ->
                  T_StringList 
sem_StringList list  =
    (Prelude.foldr sem_StringList_Cons sem_StringList_Nil list )
-- semantic domain
type T_StringList  = Bool ->
                     Scope ->
                     MySourcePos ->
                     ( StringList,([Message]),Type)
data Inh_StringList  = Inh_StringList {inLoop_Inh_StringList :: Bool,scope_Inh_StringList :: Scope,sourcePos_Inh_StringList :: MySourcePos}
data Syn_StringList  = Syn_StringList {actualValue_Syn_StringList :: StringList,messages_Syn_StringList :: [Message],nodeType_Syn_StringList :: Type}
wrap_StringList :: T_StringList  ->
                   Inh_StringList  ->
                   Syn_StringList 
wrap_StringList sem (Inh_StringList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_StringList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_StringList_Cons :: String ->
                       T_StringList  ->
                       T_StringList 
sem_StringList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: StringList
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _tlIactualValue :: StringList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _tlImessages
              _lhsOnodeType =
                  _tlInodeType
              _actualValue =
                  (:) hd_ _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_StringList_Nil :: T_StringList 
sem_StringList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: StringList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- StringStringListPair ----------------------------------------
type StringStringListPair  = ( (String),(StringList))
-- cata
sem_StringStringListPair :: StringStringListPair  ->
                            T_StringStringListPair 
sem_StringStringListPair ( x1,x2)  =
    (sem_StringStringListPair_Tuple x1 (sem_StringList x2 ) )
-- semantic domain
type T_StringStringListPair  = Bool ->
                               Scope ->
                               MySourcePos ->
                               ( StringStringListPair,([Message]),Type)
data Inh_StringStringListPair  = Inh_StringStringListPair {inLoop_Inh_StringStringListPair :: Bool,scope_Inh_StringStringListPair :: Scope,sourcePos_Inh_StringStringListPair :: MySourcePos}
data Syn_StringStringListPair  = Syn_StringStringListPair {actualValue_Syn_StringStringListPair :: StringStringListPair,messages_Syn_StringStringListPair :: [Message],nodeType_Syn_StringStringListPair :: Type}
wrap_StringStringListPair :: T_StringStringListPair  ->
                             Inh_StringStringListPair  ->
                             Syn_StringStringListPair 
wrap_StringStringListPair sem (Inh_StringStringListPair _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_StringStringListPair _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_StringStringListPair_Tuple :: String ->
                                  T_StringList  ->
                                  T_StringStringListPair 
sem_StringStringListPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: StringStringListPair
              _x2OinLoop :: Bool
              _x2Oscope :: Scope
              _x2OsourcePos :: MySourcePos
              _x2IactualValue :: StringList
              _x2Imessages :: ([Message])
              _x2InodeType :: Type
              _lhsOmessages =
                  _x2Imessages
              _lhsOnodeType =
                  _x2InodeType
              _actualValue =
                  (x1_,_x2IactualValue)
              _lhsOactualValue =
                  _actualValue
              _x2OinLoop =
                  _lhsIinLoop
              _x2Oscope =
                  _lhsIscope
              _x2OsourcePos =
                  _lhsIsourcePos
              ( _x2IactualValue,_x2Imessages,_x2InodeType) =
                  (x2_ _x2OinLoop _x2Oscope _x2OsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- StringStringListPairList ------------------------------------
type StringStringListPairList  = [(StringStringListPair)]
-- cata
sem_StringStringListPairList :: StringStringListPairList  ->
                                T_StringStringListPairList 
sem_StringStringListPairList list  =
    (Prelude.foldr sem_StringStringListPairList_Cons sem_StringStringListPairList_Nil (Prelude.map sem_StringStringListPair list) )
-- semantic domain
type T_StringStringListPairList  = Bool ->
                                   Scope ->
                                   MySourcePos ->
                                   ( StringStringListPairList,([Message]),Type)
data Inh_StringStringListPairList  = Inh_StringStringListPairList {inLoop_Inh_StringStringListPairList :: Bool,scope_Inh_StringStringListPairList :: Scope,sourcePos_Inh_StringStringListPairList :: MySourcePos}
data Syn_StringStringListPairList  = Syn_StringStringListPairList {actualValue_Syn_StringStringListPairList :: StringStringListPairList,messages_Syn_StringStringListPairList :: [Message],nodeType_Syn_StringStringListPairList :: Type}
wrap_StringStringListPairList :: T_StringStringListPairList  ->
                                 Inh_StringStringListPairList  ->
                                 Syn_StringStringListPairList 
wrap_StringStringListPairList sem (Inh_StringStringListPairList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_StringStringListPairList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_StringStringListPairList_Cons :: T_StringStringListPair  ->
                                     T_StringStringListPairList  ->
                                     T_StringStringListPairList 
sem_StringStringListPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: StringStringListPairList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: StringStringListPair
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: StringStringListPairList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_StringStringListPairList_Nil :: T_StringStringListPairList 
sem_StringStringListPairList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: StringStringListPairList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_TableRef  = Bool ->
                   Scope ->
                   MySourcePos ->
                   ( TableRef,([QualifiedScope]),([String]),([Message]),Type)
data Inh_TableRef  = Inh_TableRef {inLoop_Inh_TableRef :: Bool,scope_Inh_TableRef :: Scope,sourcePos_Inh_TableRef :: MySourcePos}
data Syn_TableRef  = Syn_TableRef {actualValue_Syn_TableRef :: TableRef,idens_Syn_TableRef :: [QualifiedScope],joinIdens_Syn_TableRef :: [String],messages_Syn_TableRef :: [Message],nodeType_Syn_TableRef :: Type}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOidens,_lhsOjoinIdens,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_TableRef _lhsOactualValue _lhsOidens _lhsOjoinIdens _lhsOmessages _lhsOnodeType ))
sem_TableRef_JoinedTref :: T_TableRef  ->
                           T_Natural  ->
                           T_JoinType  ->
                           T_TableRef  ->
                           T_OnExpr  ->
                           T_TableRef 
sem_TableRef_JoinedTref tbl_ nat_ joinType_ tbl1_ onExpr_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOidens :: ([QualifiedScope])
              _lhsOjoinIdens :: ([String])
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: TableRef
              _tblOinLoop :: Bool
              _tblOscope :: Scope
              _tblOsourcePos :: MySourcePos
              _natOinLoop :: Bool
              _natOscope :: Scope
              _natOsourcePos :: MySourcePos
              _joinTypeOinLoop :: Bool
              _joinTypeOscope :: Scope
              _joinTypeOsourcePos :: MySourcePos
              _tbl1OinLoop :: Bool
              _tbl1Oscope :: Scope
              _tbl1OsourcePos :: MySourcePos
              _onExprOinLoop :: Bool
              _onExprOscope :: Scope
              _onExprOsourcePos :: MySourcePos
              _tblIactualValue :: TableRef
              _tblIidens :: ([QualifiedScope])
              _tblIjoinIdens :: ([String])
              _tblImessages :: ([Message])
              _tblInodeType :: Type
              _natIactualValue :: Natural
              _natImessages :: ([Message])
              _natInodeType :: Type
              _joinTypeIactualValue :: JoinType
              _joinTypeImessages :: ([Message])
              _joinTypeInodeType :: Type
              _tbl1IactualValue :: TableRef
              _tbl1Iidens :: ([QualifiedScope])
              _tbl1IjoinIdens :: ([String])
              _tbl1Imessages :: ([Message])
              _tbl1InodeType :: Type
              _onExprIactualValue :: OnExpr
              _onExprImessages :: ([Message])
              _onExprInodeType :: Type
              _lhsOnodeType =
                  checkErrors [_tblInodeType
                              ,_tbl1InodeType]
                              ret
                  where
                    ret = case (_natIactualValue, _onExprIactualValue) of
                            (Natural, _) -> unionJoinList $ commonFieldNames
                                                              _tblInodeType
                                                              _tbl1InodeType
                            (_,Just (JoinUsing s)) -> unionJoinList s
                            _ -> unionJoinList []
                    unionJoinList s = combineTableTypesWithUsingList
                                        _lhsIscope
                                        _lhsIsourcePos
                                        s
                                        _tblInodeType
                                        _tbl1InodeType
              _lhsOidens =
                  _tblIidens ++ _tbl1Iidens
              _lhsOjoinIdens =
                  commonFieldNames _tblInodeType _tbl1InodeType
              _lhsOmessages =
                  _tblImessages ++ _natImessages ++ _joinTypeImessages ++ _tbl1Imessages ++ _onExprImessages
              _actualValue =
                  JoinedTref _tblIactualValue _natIactualValue _joinTypeIactualValue _tbl1IactualValue _onExprIactualValue
              _lhsOactualValue =
                  _actualValue
              _tblOinLoop =
                  _lhsIinLoop
              _tblOscope =
                  _lhsIscope
              _tblOsourcePos =
                  _lhsIsourcePos
              _natOinLoop =
                  _lhsIinLoop
              _natOscope =
                  _lhsIscope
              _natOsourcePos =
                  _lhsIsourcePos
              _joinTypeOinLoop =
                  _lhsIinLoop
              _joinTypeOscope =
                  _lhsIscope
              _joinTypeOsourcePos =
                  _lhsIsourcePos
              _tbl1OinLoop =
                  _lhsIinLoop
              _tbl1Oscope =
                  _lhsIscope
              _tbl1OsourcePos =
                  _lhsIsourcePos
              _onExprOinLoop =
                  _lhsIinLoop
              _onExprOscope =
                  _lhsIscope
              _onExprOsourcePos =
                  _lhsIsourcePos
              ( _tblIactualValue,_tblIidens,_tblIjoinIdens,_tblImessages,_tblInodeType) =
                  (tbl_ _tblOinLoop _tblOscope _tblOsourcePos )
              ( _natIactualValue,_natImessages,_natInodeType) =
                  (nat_ _natOinLoop _natOscope _natOsourcePos )
              ( _joinTypeIactualValue,_joinTypeImessages,_joinTypeInodeType) =
                  (joinType_ _joinTypeOinLoop _joinTypeOscope _joinTypeOsourcePos )
              ( _tbl1IactualValue,_tbl1Iidens,_tbl1IjoinIdens,_tbl1Imessages,_tbl1InodeType) =
                  (tbl1_ _tbl1OinLoop _tbl1Oscope _tbl1OsourcePos )
              ( _onExprIactualValue,_onExprImessages,_onExprInodeType) =
                  (onExpr_ _onExprOinLoop _onExprOscope _onExprOsourcePos )
          in  ( _lhsOactualValue,_lhsOidens,_lhsOjoinIdens,_lhsOmessages,_lhsOnodeType)))
sem_TableRef_SubTref :: T_SelectExpression  ->
                        String ->
                        T_TableRef 
sem_TableRef_SubTref sel_ alias_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOidens :: ([QualifiedScope])
              _lhsOjoinIdens :: ([String])
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: TableRef
              _selOinLoop :: Bool
              _selOscope :: Scope
              _selOsourcePos :: MySourcePos
              _selIactualValue :: SelectExpression
              _selImessages :: ([Message])
              _selInodeType :: Type
              _lhsOnodeType =
                  checkErrors [_selInodeType] $ unwrapSetOfComposite _selInodeType
              _lhsOidens =
                  [(alias_, (unwrapComposite $ unwrapSetOf _selInodeType, []))]
              _lhsOjoinIdens =
                  []
              _lhsOmessages =
                  _selImessages
              _actualValue =
                  SubTref _selIactualValue alias_
              _lhsOactualValue =
                  _actualValue
              _selOinLoop =
                  _lhsIinLoop
              _selOscope =
                  _lhsIscope
              _selOsourcePos =
                  _lhsIsourcePos
              ( _selIactualValue,_selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOscope _selOsourcePos )
          in  ( _lhsOactualValue,_lhsOidens,_lhsOjoinIdens,_lhsOmessages,_lhsOnodeType)))
sem_TableRef_Tref :: String ->
                     T_TableRef 
sem_TableRef_Tref tbl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([QualifiedScope])
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: TableRef
              _lhsOnodeType =
                  fst $ getRelationType _lhsIscope _lhsIsourcePos tbl_
              _lhsOjoinIdens =
                  []
              _lhsOidens =
                  [(tbl_, both unwrapComposite $ getRelationType _lhsIscope _lhsIsourcePos tbl_)]
              _lhsOmessages =
                  []
              _actualValue =
                  Tref tbl_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOidens,_lhsOjoinIdens,_lhsOmessages,_lhsOnodeType)))
sem_TableRef_TrefAlias :: String ->
                          String ->
                          T_TableRef 
sem_TableRef_TrefAlias tbl_ alias_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([QualifiedScope])
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: TableRef
              _lhsOnodeType =
                  fst $ getRelationType _lhsIscope _lhsIsourcePos tbl_
              _lhsOjoinIdens =
                  []
              _lhsOidens =
                  [(alias_, both unwrapComposite $ getRelationType _lhsIscope _lhsIsourcePos tbl_)]
              _lhsOmessages =
                  []
              _actualValue =
                  TrefAlias tbl_ alias_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOidens,_lhsOjoinIdens,_lhsOmessages,_lhsOnodeType)))
sem_TableRef_TrefFun :: T_Expression  ->
                        T_TableRef 
sem_TableRef_TrefFun fn_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([QualifiedScope])
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: TableRef
              _fnOinLoop :: Bool
              _fnOscope :: Scope
              _fnOsourcePos :: MySourcePos
              _fnIactualValue :: Expression
              _fnIliftedColumnName :: String
              _fnImessages :: ([Message])
              _fnInodeType :: Type
              _lhsOnodeType =
                  getFnType _lhsIscope _lhsIsourcePos "" _fnIactualValue _fnInodeType
              _lhsOjoinIdens =
                  []
              _lhsOidens =
                  [second (\l -> (unwrapComposite l, [])) $ getFunIdens _lhsIscope _lhsIsourcePos "" _fnIactualValue _fnInodeType]
              _lhsOmessages =
                  _fnImessages
              _actualValue =
                  TrefFun _fnIactualValue
              _lhsOactualValue =
                  _actualValue
              _fnOinLoop =
                  _lhsIinLoop
              _fnOscope =
                  _lhsIscope
              _fnOsourcePos =
                  _lhsIsourcePos
              ( _fnIactualValue,_fnIliftedColumnName,_fnImessages,_fnInodeType) =
                  (fn_ _fnOinLoop _fnOscope _fnOsourcePos )
          in  ( _lhsOactualValue,_lhsOidens,_lhsOjoinIdens,_lhsOmessages,_lhsOnodeType)))
sem_TableRef_TrefFunAlias :: T_Expression  ->
                             String ->
                             T_TableRef 
sem_TableRef_TrefFunAlias fn_ alias_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([QualifiedScope])
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: TableRef
              _fnOinLoop :: Bool
              _fnOscope :: Scope
              _fnOsourcePos :: MySourcePos
              _fnIactualValue :: Expression
              _fnIliftedColumnName :: String
              _fnImessages :: ([Message])
              _fnInodeType :: Type
              _lhsOnodeType =
                  getFnType _lhsIscope _lhsIsourcePos alias_ _fnIactualValue _fnInodeType
              _lhsOjoinIdens =
                  []
              _lhsOidens =
                  [second (\l -> (unwrapComposite l, [])) $ getFunIdens _lhsIscope _lhsIsourcePos alias_ _fnIactualValue _fnInodeType]
              _lhsOmessages =
                  _fnImessages
              _actualValue =
                  TrefFunAlias _fnIactualValue alias_
              _lhsOactualValue =
                  _actualValue
              _fnOinLoop =
                  _lhsIinLoop
              _fnOscope =
                  _lhsIscope
              _fnOsourcePos =
                  _lhsIsourcePos
              ( _fnIactualValue,_fnIliftedColumnName,_fnImessages,_fnInodeType) =
                  (fn_ _fnOinLoop _fnOscope _fnOsourcePos )
          in  ( _lhsOactualValue,_lhsOidens,_lhsOjoinIdens,_lhsOmessages,_lhsOnodeType)))
-- TypeAttributeDef --------------------------------------------
data TypeAttributeDef  = TypeAttDef (String) (TypeName) 
                       deriving ( Eq,Show)
-- cata
sem_TypeAttributeDef :: TypeAttributeDef  ->
                        T_TypeAttributeDef 
sem_TypeAttributeDef (TypeAttDef _name _typ )  =
    (sem_TypeAttributeDef_TypeAttDef _name (sem_TypeName _typ ) )
-- semantic domain
type T_TypeAttributeDef  = Bool ->
                           Scope ->
                           MySourcePos ->
                           ( TypeAttributeDef,([Message]),Type)
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {inLoop_Inh_TypeAttributeDef :: Bool,scope_Inh_TypeAttributeDef :: Scope,sourcePos_Inh_TypeAttributeDef :: MySourcePos}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {actualValue_Syn_TypeAttributeDef :: TypeAttributeDef,messages_Syn_TypeAttributeDef :: [Message],nodeType_Syn_TypeAttributeDef :: Type}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_TypeAttributeDef _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_TypeAttributeDef_TypeAttDef :: String ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef name_ typ_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: TypeAttributeDef
              _typOinLoop :: Bool
              _typOscope :: Scope
              _typOsourcePos :: MySourcePos
              _typIactualValue :: TypeName
              _typImessages :: ([Message])
              _typInodeType :: Type
              _lhsOmessages =
                  _typImessages
              _lhsOnodeType =
                  _typInodeType
              _actualValue =
                  TypeAttDef name_ _typIactualValue
              _lhsOactualValue =
                  _actualValue
              _typOinLoop =
                  _lhsIinLoop
              _typOscope =
                  _lhsIscope
              _typOsourcePos =
                  _lhsIsourcePos
              ( _typIactualValue,_typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOscope _typOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- TypeAttributeDefList ----------------------------------------
type TypeAttributeDefList  = [(TypeAttributeDef)]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = Bool ->
                               Scope ->
                               MySourcePos ->
                               ( TypeAttributeDefList,([Message]),Type)
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {inLoop_Inh_TypeAttributeDefList :: Bool,scope_Inh_TypeAttributeDefList :: Scope,sourcePos_Inh_TypeAttributeDefList :: MySourcePos}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {actualValue_Syn_TypeAttributeDefList :: TypeAttributeDefList,messages_Syn_TypeAttributeDefList :: [Message],nodeType_Syn_TypeAttributeDefList :: Type}
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_TypeAttributeDefList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: TypeAttributeDefList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: TypeAttributeDef
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: TypeAttributeDefList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: TypeAttributeDefList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_TypeName  = Bool ->
                   Scope ->
                   MySourcePos ->
                   ( TypeName,([Message]),Type)
data Inh_TypeName  = Inh_TypeName {inLoop_Inh_TypeName :: Bool,scope_Inh_TypeName :: Scope,sourcePos_Inh_TypeName :: MySourcePos}
data Syn_TypeName  = Syn_TypeName {actualValue_Syn_TypeName :: TypeName,messages_Syn_TypeName :: [Message],nodeType_Syn_TypeName :: Type}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_TypeName _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_TypeName_ArrayTypeName :: T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName typ_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: TypeName
              _typOinLoop :: Bool
              _typOscope :: Scope
              _typOsourcePos :: MySourcePos
              _typIactualValue :: TypeName
              _typImessages :: ([Message])
              _typInodeType :: Type
              _lhsOnodeType =
                  let t = ArrayType _typInodeType
                  in checkErrors
                         [_typInodeType
                         ,checkTypeExists _lhsIscope _lhsIsourcePos t]
                         t
              _lhsOmessages =
                  _typImessages
              _actualValue =
                  ArrayTypeName _typIactualValue
              _lhsOactualValue =
                  _actualValue
              _typOinLoop =
                  _lhsIinLoop
              _typOscope =
                  _lhsIscope
              _typOsourcePos =
                  _lhsIsourcePos
              ( _typIactualValue,_typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOscope _typOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_TypeName_PrecTypeName :: String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName tn_ prec_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: TypeName
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  PrecTypeName tn_ prec_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_TypeName_SetOfTypeName :: T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName typ_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: TypeName
              _typOinLoop :: Bool
              _typOscope :: Scope
              _typOsourcePos :: MySourcePos
              _typIactualValue :: TypeName
              _typImessages :: ([Message])
              _typInodeType :: Type
              _lhsOnodeType =
                  checkErrors [_typInodeType]
                    (SetOfType _typInodeType)
              _lhsOmessages =
                  _typImessages
              _actualValue =
                  SetOfTypeName _typIactualValue
              _lhsOactualValue =
                  _actualValue
              _typOinLoop =
                  _lhsIinLoop
              _typOscope =
                  _lhsIscope
              _typOsourcePos =
                  _lhsIsourcePos
              ( _typIactualValue,_typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOscope _typOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_TypeName_SimpleTypeName :: String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName tn_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: TypeName
              _lhsOnodeType =
                  lookupTypeByName _lhsIscope _lhsIsourcePos $ canonicalizeTypeName tn_
              _lhsOmessages =
                  []
              _actualValue =
                  SimpleTypeName tn_
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- VarDef ------------------------------------------------------
data VarDef  = VarDef (String) (TypeName) (Maybe Expression) 
             deriving ( Eq,Show)
-- cata
sem_VarDef :: VarDef  ->
              T_VarDef 
sem_VarDef (VarDef _name _typ _value )  =
    (sem_VarDef_VarDef _name (sem_TypeName _typ ) _value )
-- semantic domain
type T_VarDef  = Bool ->
                 Scope ->
                 MySourcePos ->
                 ( VarDef,([Message]),Type)
data Inh_VarDef  = Inh_VarDef {inLoop_Inh_VarDef :: Bool,scope_Inh_VarDef :: Scope,sourcePos_Inh_VarDef :: MySourcePos}
data Syn_VarDef  = Syn_VarDef {actualValue_Syn_VarDef :: VarDef,messages_Syn_VarDef :: [Message],nodeType_Syn_VarDef :: Type}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_VarDef _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_VarDef_VarDef :: String ->
                     T_TypeName  ->
                     (Maybe Expression) ->
                     T_VarDef 
sem_VarDef_VarDef name_ typ_ value_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: VarDef
              _typOinLoop :: Bool
              _typOscope :: Scope
              _typOsourcePos :: MySourcePos
              _typIactualValue :: TypeName
              _typImessages :: ([Message])
              _typInodeType :: Type
              _lhsOmessages =
                  _typImessages
              _lhsOnodeType =
                  _typInodeType
              _actualValue =
                  VarDef name_ _typIactualValue value_
              _lhsOactualValue =
                  _actualValue
              _typOinLoop =
                  _lhsIinLoop
              _typOscope =
                  _lhsIscope
              _typOsourcePos =
                  _lhsIsourcePos
              ( _typIactualValue,_typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOscope _typOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
-- VarDefList --------------------------------------------------
type VarDefList  = [(VarDef)]
-- cata
sem_VarDefList :: VarDefList  ->
                  T_VarDefList 
sem_VarDefList list  =
    (Prelude.foldr sem_VarDefList_Cons sem_VarDefList_Nil (Prelude.map sem_VarDef list) )
-- semantic domain
type T_VarDefList  = Bool ->
                     Scope ->
                     MySourcePos ->
                     ( VarDefList,([Message]),Type)
data Inh_VarDefList  = Inh_VarDefList {inLoop_Inh_VarDefList :: Bool,scope_Inh_VarDefList :: Scope,sourcePos_Inh_VarDefList :: MySourcePos}
data Syn_VarDefList  = Syn_VarDefList {actualValue_Syn_VarDefList :: VarDefList,messages_Syn_VarDefList :: [Message],nodeType_Syn_VarDefList :: Type}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_VarDefList _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: VarDefList
              _hdOinLoop :: Bool
              _hdOscope :: Scope
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOscope :: Scope
              _tlOsourcePos :: MySourcePos
              _hdIactualValue :: VarDef
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlIactualValue :: VarDefList
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _actualValue =
                  (:) _hdIactualValue _tlIactualValue
              _lhsOactualValue =
                  _actualValue
              _hdOinLoop =
                  _lhsIinLoop
              _hdOscope =
                  _lhsIscope
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOscope =
                  _lhsIscope
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdIactualValue,_hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOscope _hdOsourcePos )
              ( _tlIactualValue,_tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOscope _tlOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: VarDefList
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
              _actualValue =
                  []
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_Volatility  = Bool ->
                     Scope ->
                     MySourcePos ->
                     ( Volatility,([Message]),Type)
data Inh_Volatility  = Inh_Volatility {inLoop_Inh_Volatility :: Bool,scope_Inh_Volatility :: Scope,sourcePos_Inh_Volatility :: MySourcePos}
data Syn_Volatility  = Syn_Volatility {actualValue_Syn_Volatility :: Volatility,messages_Syn_Volatility :: [Message],nodeType_Syn_Volatility :: Type}
wrap_Volatility :: T_Volatility  ->
                   Inh_Volatility  ->
                   Syn_Volatility 
wrap_Volatility sem (Inh_Volatility _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_Volatility _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_Volatility_Immutable :: T_Volatility 
sem_Volatility_Immutable  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Volatility
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Immutable
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Volatility_Stable :: T_Volatility 
sem_Volatility_Stable  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Volatility
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Stable
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Volatility_Volatile :: T_Volatility 
sem_Volatility_Volatile  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOactualValue :: Volatility
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _actualValue =
                  Volatile
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
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
type T_Where  = Bool ->
                Scope ->
                MySourcePos ->
                ( Where,([Message]),Type)
data Inh_Where  = Inh_Where {inLoop_Inh_Where :: Bool,scope_Inh_Where :: Scope,sourcePos_Inh_Where :: MySourcePos}
data Syn_Where  = Syn_Where {actualValue_Syn_Where :: Where,messages_Syn_Where :: [Message],nodeType_Syn_Where :: Type}
wrap_Where :: T_Where  ->
              Inh_Where  ->
              Syn_Where 
wrap_Where sem (Inh_Where _lhsIinLoop _lhsIscope _lhsIsourcePos )  =
    (let ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIscope _lhsIsourcePos )
     in  (Syn_Where _lhsOactualValue _lhsOmessages _lhsOnodeType ))
sem_Where_Just :: T_Expression  ->
                  T_Where 
sem_Where_Just just_  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Where
              _justOinLoop :: Bool
              _justOscope :: Scope
              _justOsourcePos :: MySourcePos
              _justIactualValue :: Expression
              _justIliftedColumnName :: String
              _justImessages :: ([Message])
              _justInodeType :: Type
              _lhsOnodeType =
                  checkErrors
                    [_justInodeType]
                    (if _justInodeType /= typeBool
                       then TypeError _lhsIsourcePos ExpressionMustBeBool
                       else typeBool)
              _lhsOmessages =
                  _justImessages
              _actualValue =
                  Just _justIactualValue
              _lhsOactualValue =
                  _actualValue
              _justOinLoop =
                  _lhsIinLoop
              _justOscope =
                  _lhsIscope
              _justOsourcePos =
                  _lhsIsourcePos
              ( _justIactualValue,_justIliftedColumnName,_justImessages,_justInodeType) =
                  (just_ _justOinLoop _justOscope _justOsourcePos )
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))
sem_Where_Nothing :: T_Where 
sem_Where_Nothing  =
    (\ _lhsIinLoop
       _lhsIscope
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOactualValue :: Where
              _lhsOnodeType =
                  typeBool
              _lhsOmessages =
                  []
              _actualValue =
                  Nothing
              _lhsOactualValue =
                  _actualValue
          in  ( _lhsOactualValue,_lhsOmessages,_lhsOnodeType)))