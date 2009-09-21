

-- UUAGC 0.9.10 (AstInternal.ag)
module Database.HsSqlPpp.TypeChecking.AstInternal(
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

import Database.HsSqlPpp.TypeChecking.TypeType
import Database.HsSqlPpp.TypeChecking.AstUtils
import Database.HsSqlPpp.TypeChecking.TypeConversion
import Database.HsSqlPpp.TypeChecking.TypeCheckingH
import Database.HsSqlPpp.TypeChecking.AstAnnotation
import Database.HsSqlPpp.TypeChecking.EnvironmentInternal
import Database.HsSqlPpp.TypeChecking.DefaultTemplate1Environment
import Database.HsSqlPpp.Utils


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
              doCases = map (second (cars f)) cases
        If a cases els -> If (f a) doCases $ cars f els
            where
              doCases = map (second (cars f)) cases
    --where
     -- doCases cs = map (\(ex,sts) -> (ex,cars f sts)) cs
  getAnnChildren st =
    case st of
        SelectStatement _ ex -> gacse ex
        Insert _ _ _ ins _ -> gacse ins
        Update _ _ as whr _ -> mp (gacscl as) ++ gacme whr
        Delete _ _ whr _ -> gacme whr
        --Copy _ _ _ _ -> []
        --CopyData _ _ -> []
        --Truncate _ _ _ _ -> []
        --CreateTable _ _ _ _ -> []
        --CreateTableAs _ _ _ -> []
        CreateView _ _ expr -> gacse expr
        --CreateType _ _ _ -> []
        --CreateFunction a lang name params rettype bodyQuote body vol ->
        CreateFunction _ _    _    _      _       _         body _   ->
            case body of
              SqlFnBody sts -> mp sts
              PlpgsqlFnBody _ sts -> mp sts
        --CreateDomain _ _ _ _ -> []
        --DropFunction _ _ _ _ -> []
        --DropSomething _ _ _ _ _ -> []
        --Assignment _ _ _ -> []
        --Return a v -> Return (f a) v
        --ReturnNext a ex -> ReturnNext (f a) ex
        --ReturnQuery a sel -> ReturnQuery (f a) sel
        --Raise a l m args -> Raise (f a) l m args
        --NullStatement a -> NullStatement (f a)
        --Perform a expr -> Perform (f a) expr
        --Execute a expr -> Execute (f a) expr
        --ExecuteInto a expr tgts -> ExecuteInto (f a) expr tgts
        ForSelectStatement _ _ sel sts -> gacse sel ++ mp sts
        ForIntegerStatement _ _ _ _ sts -> mp sts
        WhileStatement _ expr sts -> pack expr : mp sts
        --ContinueStatement a -> ContinueStatement (f a)
        CaseStatement _ val cases els -> pack val : mp (doCases cases) ++ mp els
        If _ cases els -> mp $ doCases cases ++ els
        _ -> []
    where
      doCases = concatMap snd
      --gacse :: Annotated a => SelectExpression -> [a]
      gacse se = [pack se]
      gacscl :: Annotated a => SetClauseList -> [a]
      gacscl _ = []
      --gacme :: Annotated a => Maybe Expression -> [a]
      gacme e = case e of
                  Nothing -> []
                  Just e1 -> [pack e1]
      mp = map pack

cars = map . changeAnnRecurse

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

  changeAnnRecurse f ex =
    case ex of
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

  getAnnChildren ex =
    case ex of
      Cast _ expr _ -> mp [expr]
      Case _ cases els -> gacce cases els
      CaseSimple _ val cases els -> pack val : gacce cases els
      Exists a sel -> [pack sel]
      FunCall _ _ args -> mp args
      --InPredicate a expr i list -> InPredicate (f a) expr i list
      --WindowFn a fn par ord dir -> WindowFn (f a) fn par ord dir
      --ScalarSubQuery a sel -> ScalarSubQuery (f a) sel
      _ -> []
    where
      gacme e = case e of
                  Nothing -> []
                  Just e1 -> [pack e1]
      gacce cs el = mp (concatMap (\(el,e) -> el ++ [e]) cs) ++ gacme el
      mp = map pack



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
  changeAnnRecurse f ex =
    case ex of
      Select a dis sl tref whr grp hav ord dir lim off ->
          Select (f a) dis sl tref whr grp hav ord dir lim off
      CombineSelect a ctype sel1 sel2 -> CombineSelect (f a) ctype
                                             (changeAnnRecurse f sel1)
                                             (changeAnnRecurse f sel2)
      Values a vll -> Values (f a) vll
  getAnnChildren ex =
    case ex of
      Select a dis sl tref whr grp hav ord dir lim off ->
          doSl ++
          map pack (maybeToList tref) ++
          doME whr ++ mp grp ++ doME hav ++ mp ord ++ doME lim ++ doME off
          where
            doSl = let SelectList x _ = sl
                       ses = map (\s -> case s of
                                         SelExp se -> se
                                         SelectItem se _ -> se) x
                   in map pack ses
            doME me = case me of
                        Nothing -> []
                        Just e -> [pack e]
      CombineSelect _ _ sel1 sel2 -> [pack sel1,pack sel2]
      Values _ vll -> mp $ concat vll
    where
      mp = map pack

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
  changeAnnRecurse f ex =
    case ex of
        Tref a tbl -> Tref (f a) tbl
        TrefAlias a tbl alias -> TrefAlias (f a) tbl alias
        JoinedTref a tbl nat joinType tbl1 onExpr ->
          JoinedTref (f a)
                     (changeAnnRecurse f tbl)
                     nat
                     joinType
                     (changeAnnRecurse f tbl1)
                     onExpr
        SubTref a sel alias -> SubTref (f a) (changeAnnRecurse f sel) alias
        TrefFun a fn -> TrefFun (f a) (changeAnnRecurse f fn)
        TrefFunAlias a fn alias -> TrefFunAlias (f a) (changeAnnRecurse f fn) alias
  getAnnChildren ex =
    case ex of
        Tref a tbl -> []
        TrefAlias a tbl alias -> []
        JoinedTref _ tbl _ _ tbl1 onExpr ->
          getAnnChildren tbl ++ getAnnChildren tbl1
        SubTref a sel alias -> getAnnChildren sel
        TrefFun a fn -> getAnnChildren fn
        TrefFunAlias a fn alias -> getAnnChildren fn


annTypesAndErrors :: Annotated a => a -> Type -> [TypeError]
                  -> Maybe AnnotationElement -> a
annTypesAndErrors item nt errs add =
    changeAnn item $
     (([TypeAnnotation nt] ++ maybeToList add ++
       map TypeErrorA errs) ++)


checkExpressionBool :: Maybe Expression -> Either [TypeError] Type
checkExpressionBool whr = do
  let ty = fromMaybe typeBool $ fmap getTypeAnnotation whr
  when (ty `notElem` [typeBool, TypeCheckFailed]) $
       Left [ExpressionMustBeBool]
  return ty


getTbCols = unwrapComposite . unwrapSetOf . getTypeAnnotation



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


fixStar ex =
  changeAnnRecurse fs ex
  where
    fs a = if TypeAnnotation TypeCheckFailed `elem` a
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


fixedValue :: a -> a -> a -> a
fixedValue a _ _ = a





getRowTypes :: [Type] -> [Type]
getRowTypes [RowCtor ts] = ts
getRowTypes ts = ts
-- AttributeDef ------------------------------------------------
data AttributeDef  = AttributeDef (String) (TypeName) (Maybe Expression) (RowConstraintList) 
                   deriving ( Eq,Show)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _name _typ _check _cons )  =
    (sem_AttributeDef_AttributeDef _name (sem_TypeName _typ ) _check (sem_RowConstraintList _cons ) )
-- semantic domain
type T_AttributeDef  = Environment ->
                       ( AttributeDef,String,(Either [TypeError] Type))
data Inh_AttributeDef  = Inh_AttributeDef {env_Inh_AttributeDef :: Environment}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef,attrName_Syn_AttributeDef :: String,namedType_Syn_AttributeDef :: Either [TypeError] Type}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_AttributeDef _lhsOannotatedTree _lhsOattrName _lhsOnamedType ))
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
              _typOenv :: Environment
              _consOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: (Either [TypeError] Type)
              _consIannotatedTree :: RowConstraintList
              _lhsOattrName =
                  name_
              _lhsOnamedType =
                  _typInamedType
              _annotatedTree =
                  AttributeDef name_ _typIannotatedTree check_ _consIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _typOenv =
                  _lhsIenv
              _consOenv =
                  _lhsIenv
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
              ( _consIannotatedTree) =
                  (cons_ _consOenv )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType)))
-- AttributeDefList --------------------------------------------
type AttributeDefList  = [(AttributeDef)]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = Environment ->
                           ( AttributeDefList,([(String, Either [TypeError] Type)]))
data Inh_AttributeDefList  = Inh_AttributeDefList {env_Inh_AttributeDefList :: Environment}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList,attrs_Syn_AttributeDefList :: [(String, Either [TypeError] Type)]}
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
         (let _lhsOattrs :: ([(String, Either [TypeError] Type)])
              _lhsOannotatedTree :: AttributeDefList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: AttributeDef
              _hdIattrName :: String
              _hdInamedType :: (Either [TypeError] Type)
              _tlIannotatedTree :: AttributeDefList
              _tlIattrs :: ([(String, Either [TypeError] Type)])
              _lhsOattrs =
                  (_hdIattrName, _hdInamedType) : _tlIattrs
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree,_hdIattrName,_hdInamedType) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIattrs) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOattrs)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOattrs :: ([(String, Either [TypeError] Type)])
              _lhsOannotatedTree :: AttributeDefList
              _lhsOattrs =
                  []
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree,_lhsOattrs)))
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
              _annotatedTree =
                  Cascade
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Cascade_Restrict :: T_Cascade 
sem_Cascade_Restrict  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Cascade
              _annotatedTree =
                  Restrict
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
-- CaseExpressionList ------------------------------------------
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
         (let _lhsOannotatedTree :: CaseExpressionList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: Expression
              _hdIliftedColumnName :: String
              _tlIannotatedTree :: CaseExpressionList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree,_hdIliftedColumnName) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree)))
sem_CaseExpressionList_Nil :: T_CaseExpressionList 
sem_CaseExpressionList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CaseExpressionList
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
-- CaseExpressionListExpressionPair ----------------------------
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
         (let _lhsOannotatedTree :: CaseExpressionListExpressionPair
              _x1Oenv :: Environment
              _x2Oenv :: Environment
              _x1IannotatedTree :: CaseExpressionList
              _x2IannotatedTree :: Expression
              _x2IliftedColumnName :: String
              _annotatedTree =
                  (_x1IannotatedTree,_x2IannotatedTree)
              _lhsOannotatedTree =
                  _annotatedTree
              _x1Oenv =
                  _lhsIenv
              _x2Oenv =
                  _lhsIenv
              ( _x1IannotatedTree) =
                  (x1_ _x1Oenv )
              ( _x2IannotatedTree,_x2IliftedColumnName) =
                  (x2_ _x2Oenv )
          in  ( _lhsOannotatedTree)))
-- CaseExpressionListExpressionPairList ------------------------
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
         (let _lhsOannotatedTree :: CaseExpressionListExpressionPairList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: CaseExpressionListExpressionPair
              _tlIannotatedTree :: CaseExpressionListExpressionPairList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree)))
sem_CaseExpressionListExpressionPairList_Nil :: T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CaseExpressionListExpressionPairList
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  Except
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_CombineType_Intersect :: T_CombineType 
sem_CombineType_Intersect  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              _annotatedTree =
                  Intersect
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_CombineType_Union :: T_CombineType 
sem_CombineType_Union  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              _annotatedTree =
                  Union
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_CombineType_UnionAll :: T_CombineType 
sem_CombineType_UnionAll  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CombineType
              _annotatedTree =
                  UnionAll
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
sem_Constraint_CheckConstraint :: T_Expression  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint expression_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIliftedColumnName :: String
              _annotatedTree =
                  CheckConstraint _expressionIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _expressionOenv =
                  _lhsIenv
              ( _expressionIannotatedTree,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
          in  ( _lhsOannotatedTree)))
sem_Constraint_PrimaryKeyConstraint :: T_StringList  ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint stringList_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _annotatedTree =
                  PrimaryKeyConstraint _stringListIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _stringListOenv =
                  _lhsIenv
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree)))
sem_Constraint_ReferenceConstraint :: T_StringList  ->
                                      String ->
                                      T_StringList  ->
                                      T_Cascade  ->
                                      T_Cascade  ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _attsOenv :: Environment
              _tableAttsOenv :: Environment
              _onUpdateOenv :: Environment
              _onDeleteOenv :: Environment
              _attsIannotatedTree :: StringList
              _attsIstrings :: ([String])
              _tableAttsIannotatedTree :: StringList
              _tableAttsIstrings :: ([String])
              _onUpdateIannotatedTree :: Cascade
              _onDeleteIannotatedTree :: Cascade
              _annotatedTree =
                  ReferenceConstraint _attsIannotatedTree table_ _tableAttsIannotatedTree _onUpdateIannotatedTree _onDeleteIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _attsOenv =
                  _lhsIenv
              _tableAttsOenv =
                  _lhsIenv
              _onUpdateOenv =
                  _lhsIenv
              _onDeleteOenv =
                  _lhsIenv
              ( _attsIannotatedTree,_attsIstrings) =
                  (atts_ _attsOenv )
              ( _tableAttsIannotatedTree,_tableAttsIstrings) =
                  (tableAtts_ _tableAttsOenv )
              ( _onUpdateIannotatedTree) =
                  (onUpdate_ _onUpdateOenv )
              ( _onDeleteIannotatedTree) =
                  (onDelete_ _onDeleteOenv )
          in  ( _lhsOannotatedTree)))
sem_Constraint_UniqueConstraint :: T_StringList  ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint stringList_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Constraint
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _annotatedTree =
                  UniqueConstraint _stringListIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _stringListOenv =
                  _lhsIenv
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree)))
-- ConstraintList ----------------------------------------------
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
         (let _lhsOannotatedTree :: ConstraintList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: Constraint
              _tlIannotatedTree :: ConstraintList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ConstraintList
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  CopyFilename string_
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_CopySource_Stdin :: T_CopySource 
sem_CopySource_Stdin  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: CopySource
              _annotatedTree =
                  Stdin
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  Asc
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Direction_Desc :: T_Direction 
sem_Direction_Desc  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Direction
              _annotatedTree =
                  Desc
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  Distinct
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Distinct_Dupes :: T_Distinct 
sem_Distinct_Dupes  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Distinct
              _annotatedTree =
                  Dupes
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  Domain
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_DropType_Table :: T_DropType 
sem_DropType_Table  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              _annotatedTree =
                  Table
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_DropType_Type :: T_DropType 
sem_DropType_Type  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              _annotatedTree =
                  Type
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_DropType_View :: T_DropType 
sem_DropType_View  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: DropType
              _annotatedTree =
                  View
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _backTree =
                  BooleanLit ann_ b_
              _tpe =
                  Right typeBool
              _lhsOliftedColumnName =
                  ""
              _annotatedTree =
                  BooleanLit ann_ b_
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Case :: Annotation ->
                       T_CaseExpressionListExpressionPairList  ->
                       T_MaybeExpression  ->
                       T_Expression 
sem_Expression_Case ann_ cases_ els_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _casesOenv :: Environment
              _elsOenv :: Environment
              _casesIannotatedTree :: CaseExpressionListExpressionPairList
              _elsIannotatedTree :: MaybeExpression
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _whenTypes =
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
              _thenTypes =
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
              _tpe =
                  checkTypes _whenTypes     $ do
                     when (any (/= typeBool) _whenTypes    ) $
                       Left [WrongTypes typeBool _whenTypes    ]
                     checkTypes _thenTypes     $
                              resolveResultSetType
                                _lhsIenv
                                _thenTypes
              _backTree =
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
              _lhsOliftedColumnName =
                  ""
              _annotatedTree =
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
              _casesOenv =
                  _lhsIenv
              _elsOenv =
                  _lhsIenv
              ( _casesIannotatedTree) =
                  (cases_ _casesOenv )
              ( _elsIannotatedTree) =
                  (els_ _elsOenv )
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_CaseSimple :: Annotation ->
                             T_Expression  ->
                             T_CaseExpressionListExpressionPairList  ->
                             T_MaybeExpression  ->
                             T_Expression 
sem_Expression_CaseSimple ann_ value_ cases_ els_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _valueOenv :: Environment
              _casesOenv :: Environment
              _elsOenv :: Environment
              _valueIannotatedTree :: Expression
              _valueIliftedColumnName :: String
              _casesIannotatedTree :: CaseExpressionListExpressionPairList
              _elsIannotatedTree :: MaybeExpression
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _whenTypes =
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
              _thenTypes =
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
              _tpe =
                  checkTypes _whenTypes     $ do
                  checkWhenTypes <- resolveResultSetType
                                         _lhsIenv
                                         (getTypeAnnotation _valueIannotatedTree: _whenTypes    )
                  checkTypes _thenTypes     $
                             resolveResultSetType
                                      _lhsIenv
                                      _thenTypes
              _backTree =
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
              _lhsOliftedColumnName =
                  _valueIliftedColumnName
              _annotatedTree =
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
              _valueOenv =
                  _lhsIenv
              _casesOenv =
                  _lhsIenv
              _elsOenv =
                  _lhsIenv
              ( _valueIannotatedTree,_valueIliftedColumnName) =
                  (value_ _valueOenv )
              ( _casesIannotatedTree) =
                  (cases_ _casesOenv )
              ( _elsIannotatedTree) =
                  (els_ _elsOenv )
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Cast :: Annotation ->
                       T_Expression  ->
                       T_TypeName  ->
                       T_Expression 
sem_Expression_Cast ann_ expr_ tn_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _exprOenv :: Environment
              _tnOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _tnIannotatedTree :: TypeName
              _tnInamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  _tnInamedType
              _backTree =
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
              _lhsOliftedColumnName =
                  case _tnIannotatedTree of
                    SimpleTypeName tn -> tn
                    _ -> ""
              _annotatedTree =
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
              _exprOenv =
                  _lhsIenv
              _tnOenv =
                  _lhsIenv
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              ( _tnIannotatedTree,_tnInamedType) =
                  (tn_ _tnOenv )
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Exists :: Annotation ->
                         T_SelectExpression  ->
                         T_Expression 
sem_Expression_Exists ann_ sel_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  Right typeBool
              _backTree =
                  Exists ann_ _selIannotatedTree
              _lhsOliftedColumnName =
                  ""
              _annotatedTree =
                  Exists ann_ _selIannotatedTree
              _selOenv =
                  _lhsIenv
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_FloatLit :: Annotation ->
                           Double ->
                           T_Expression 
sem_Expression_FloatLit ann_ d_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _backTree =
                  FloatLit ann_ d_
              _tpe =
                  Right typeNumeric
              _lhsOliftedColumnName =
                  ""
              _annotatedTree =
                  FloatLit ann_ d_
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_FunCall :: Annotation ->
                          String ->
                          T_ExpressionList  ->
                          T_Expression 
sem_Expression_FunCall ann_ funName_ args_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _argsOenv :: Environment
              _argsIannotatedTree :: ExpressionList
              _argsItypeList :: ([Type])
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  checkTypes _argsItypeList $
                    typeCheckFunCall
                      _lhsIenv
                      funName_
                      _argsItypeList
              _backTree =
                  FunCall ann_ funName_ _argsIannotatedTree
              _lhsOliftedColumnName =
                  if isOperatorName funName_
                     then ""
                     else funName_
              _annotatedTree =
                  FunCall ann_ funName_ _argsIannotatedTree
              _argsOenv =
                  _lhsIenv
              ( _argsIannotatedTree,_argsItypeList) =
                  (args_ _argsOenv )
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Identifier :: Annotation ->
                             String ->
                             T_Expression 
sem_Expression_Identifier ann_ i_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  let (correlationName,iden) = splitIdentifier i_
                  in envLookupID _lhsIenv correlationName iden
              _backTree =
                  Identifier ann_ i_
              _lhsOliftedColumnName =
                  i_
              _annotatedTree =
                  Identifier ann_ i_
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_InPredicate :: Annotation ->
                              T_Expression  ->
                              Bool ->
                              T_InList  ->
                              T_Expression 
sem_Expression_InPredicate ann_ expr_ i_ list_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _exprOenv :: Environment
              _listOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _listIannotatedTree :: InList
              _listIlistType :: (Either [TypeError] Type)
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  do
                    lt <- _listIlistType
                    ty <- resolveResultSetType
                            _lhsIenv
                            [getTypeAnnotation _exprIannotatedTree, lt]
                    return typeBool
              _backTree =
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
              _lhsOliftedColumnName =
                  _exprIliftedColumnName
              _annotatedTree =
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
              _exprOenv =
                  _lhsIenv
              _listOenv =
                  _lhsIenv
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              ( _listIannotatedTree,_listIlistType) =
                  (list_ _listOenv )
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_IntegerLit :: Annotation ->
                             Integer ->
                             T_Expression 
sem_Expression_IntegerLit ann_ i_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _backTree =
                  IntegerLit ann_ i_
              _tpe =
                  Right typeInt
              _lhsOliftedColumnName =
                  ""
              _annotatedTree =
                  IntegerLit ann_ i_
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_NullLit :: Annotation ->
                          T_Expression 
sem_Expression_NullLit ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _backTree =
                  NullLit ann_
              _tpe =
                  Right UnknownStringLit
              _lhsOliftedColumnName =
                  ""
              _annotatedTree =
                  NullLit ann_
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_PositionalArg :: Annotation ->
                                Integer ->
                                T_Expression 
sem_Expression_PositionalArg ann_ p_  =
    (\ _lhsIenv ->
         (let _lhsOliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName =
                  ""
              _annotatedTree =
                  PositionalArg ann_ p_
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_ScalarSubQuery :: Annotation ->
                                 T_SelectExpression  ->
                                 T_Expression 
sem_Expression_ScalarSubQuery ann_ sel_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  let selType = getTypeAnnotation _selIannotatedTree
                  in checkTypes [selType]
                       $ let f = map snd $ unwrapComposite $ unwrapSetOf selType
                         in case length f of
                              0 -> error "internal error: no columns in scalar subquery?"
                              1 -> Right $ head f
                              _ -> Right $ RowCtor f
              _backTree =
                  ScalarSubQuery ann_ _selIannotatedTree
              _lhsOliftedColumnName =
                  ""
              _annotatedTree =
                  ScalarSubQuery ann_ _selIannotatedTree
              _selOenv =
                  _lhsIenv
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_StringLit :: Annotation ->
                            String ->
                            String ->
                            T_Expression 
sem_Expression_StringLit ann_ quote_ value_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _backTree =
                  StringLit ann_ quote_ value_
              _tpe =
                  Right UnknownStringLit
              _lhsOliftedColumnName =
                  ""
              _annotatedTree =
                  StringLit ann_ quote_ value_
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
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
              _fnOenv :: Environment
              _partitionByOenv :: Environment
              _orderByOenv :: Environment
              _dirOenv :: Environment
              _fnIannotatedTree :: Expression
              _fnIliftedColumnName :: String
              _partitionByIannotatedTree :: ExpressionList
              _partitionByItypeList :: ([Type])
              _orderByIannotatedTree :: ExpressionList
              _orderByItypeList :: ([Type])
              _dirIannotatedTree :: Direction
              _lhsOliftedColumnName =
                  _fnIliftedColumnName
              _annotatedTree =
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree _dirIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _fnOenv =
                  _lhsIenv
              _partitionByOenv =
                  _lhsIenv
              _orderByOenv =
                  _lhsIenv
              _dirOenv =
                  _lhsIenv
              ( _fnIannotatedTree,_fnIliftedColumnName) =
                  (fn_ _fnOenv )
              ( _partitionByIannotatedTree,_partitionByItypeList) =
                  (partitionBy_ _partitionByOenv )
              ( _orderByIannotatedTree,_orderByItypeList) =
                  (orderBy_ _orderByOenv )
              ( _dirIannotatedTree) =
                  (dir_ _dirOenv )
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
-- ExpressionList ----------------------------------------------
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
         (let _lhsOtypeList :: ([Type])
              _lhsOannotatedTree :: ExpressionList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: Expression
              _hdIliftedColumnName :: String
              _tlIannotatedTree :: ExpressionList
              _tlItypeList :: ([Type])
              _lhsOtypeList =
                  getTypeAnnotation _hdIannotatedTree : _tlItypeList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree,_hdIliftedColumnName) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlItypeList) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOtypeList)))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOtypeList :: ([Type])
              _lhsOannotatedTree :: ExpressionList
              _lhsOtypeList =
                  []
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree,_lhsOtypeList)))
-- ExpressionListList ------------------------------------------
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
         (let _lhsOtypeListList :: ([[Type]])
              _lhsOannotatedTree :: ExpressionListList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: ExpressionList
              _hdItypeList :: ([Type])
              _tlIannotatedTree :: ExpressionListList
              _tlItypeListList :: ([[Type]])
              _lhsOtypeListList =
                  _hdItypeList : _tlItypeListList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree,_hdItypeList) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlItypeListList) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOtypeListList)))
sem_ExpressionListList_Nil :: T_ExpressionListList 
sem_ExpressionListList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOtypeListList :: ([[Type]])
              _lhsOannotatedTree :: ExpressionListList
              _lhsOtypeListList =
                  []
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree,_lhsOtypeListList)))
-- ExpressionListStatementListPair -----------------------------
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
         (let _lhsOannotatedTree :: ExpressionListStatementListPair
              _x1Oenv :: Environment
              _x2Oenv :: Environment
              _x1IannotatedTree :: ExpressionList
              _x1ItypeList :: ([Type])
              _x2IannotatedTree :: StatementList
              _annotatedTree =
                  (_x1IannotatedTree,_x2IannotatedTree)
              _lhsOannotatedTree =
                  _annotatedTree
              _x1Oenv =
                  _lhsIenv
              _x2Oenv =
                  _lhsIenv
              ( _x1IannotatedTree,_x1ItypeList) =
                  (x1_ _x1Oenv )
              ( _x2IannotatedTree) =
                  (x2_ _x2Oenv )
          in  ( _lhsOannotatedTree)))
-- ExpressionListStatementListPairList -------------------------
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
         (let _lhsOannotatedTree :: ExpressionListStatementListPairList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: ExpressionListStatementListPair
              _tlIannotatedTree :: ExpressionListStatementListPairList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree)))
sem_ExpressionListStatementListPairList_Nil :: T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionListStatementListPairList
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
         (let _lhsOannotatedTree :: ExpressionRoot
              _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _annotatedTree =
                  ExpressionRoot _exprIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _exprOenv =
                  _lhsIenv
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree)))
-- ExpressionStatementListPair ---------------------------------
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
         (let _lhsOannotatedTree :: ExpressionStatementListPair
              _x1Oenv :: Environment
              _x2Oenv :: Environment
              _x1IannotatedTree :: Expression
              _x1IliftedColumnName :: String
              _x2IannotatedTree :: StatementList
              _annotatedTree =
                  (_x1IannotatedTree,_x2IannotatedTree)
              _lhsOannotatedTree =
                  _annotatedTree
              _x1Oenv =
                  _lhsIenv
              _x2Oenv =
                  _lhsIenv
              ( _x1IannotatedTree,_x1IliftedColumnName) =
                  (x1_ _x1Oenv )
              ( _x2IannotatedTree) =
                  (x2_ _x2Oenv )
          in  ( _lhsOannotatedTree)))
-- ExpressionStatementListPairList -----------------------------
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
         (let _lhsOannotatedTree :: ExpressionStatementListPairList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: ExpressionStatementListPair
              _tlIannotatedTree :: ExpressionStatementListPairList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree)))
sem_ExpressionStatementListPairList_Nil :: T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: ExpressionStatementListPairList
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
sem_FnBody_PlpgsqlFnBody :: T_VarDefList  ->
                            T_StatementList  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody varDefList_ sts_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: FnBody
              _varDefListOenv :: Environment
              _stsOenv :: Environment
              _varDefListIannotatedTree :: VarDefList
              _stsIannotatedTree :: StatementList
              _annotatedTree =
                  PlpgsqlFnBody _varDefListIannotatedTree _stsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _varDefListOenv =
                  _lhsIenv
              _stsOenv =
                  _lhsIenv
              ( _varDefListIannotatedTree) =
                  (varDefList_ _varDefListOenv )
              ( _stsIannotatedTree) =
                  (sts_ _stsOenv )
          in  ( _lhsOannotatedTree)))
sem_FnBody_SqlFnBody :: T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody sts_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: FnBody
              _stsOenv :: Environment
              _stsIannotatedTree :: StatementList
              _annotatedTree =
                  SqlFnBody _stsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _stsOenv =
                  _lhsIenv
              ( _stsIannotatedTree) =
                  (sts_ _stsOenv )
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  IfExists
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_IfExists_Require :: T_IfExists 
sem_IfExists_Require  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: IfExists
              _annotatedTree =
                  Require
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
sem_InList_InList :: T_ExpressionList  ->
                     T_InList 
sem_InList_InList exprs_  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: InList
              _exprsOenv :: Environment
              _exprsIannotatedTree :: ExpressionList
              _exprsItypeList :: ([Type])
              _lhsOlistType =
                  resolveResultSetType
                    _lhsIenv
                    _exprsItypeList
              _annotatedTree =
                  InList _exprsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _exprsOenv =
                  _lhsIenv
              ( _exprsIannotatedTree,_exprsItypeList) =
                  (exprs_ _exprsOenv )
          in  ( _lhsOannotatedTree,_lhsOlistType)))
sem_InList_InSelect :: T_SelectExpression  ->
                       T_InList 
sem_InList_InSelect sel_  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: InList
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _lhsOlistType =
                  let attrs = map snd $ unwrapComposite $ unwrapSetOf $ getTypeAnnotation _selIannotatedTree
                      typ =  case length attrs of
                               0 -> error "internal error - got subquery with no columns? in inselect"
                               1 -> head attrs
                               _ -> RowCtor attrs
                  in checkTypes attrs $ Right typ
              _annotatedTree =
                  InSelect _selIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _selOenv =
                  _lhsIenv
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree,_lhsOlistType)))
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
sem_JoinExpression_JoinOn :: T_Expression  ->
                             T_JoinExpression 
sem_JoinExpression_JoinOn expression_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinExpression
              _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIliftedColumnName :: String
              _annotatedTree =
                  JoinOn _expressionIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _expressionOenv =
                  _lhsIenv
              ( _expressionIannotatedTree,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
          in  ( _lhsOannotatedTree)))
sem_JoinExpression_JoinUsing :: T_StringList  ->
                                T_JoinExpression 
sem_JoinExpression_JoinUsing stringList_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinExpression
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _annotatedTree =
                  JoinUsing _stringListIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _stringListOenv =
                  _lhsIenv
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  Cross
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_JoinType_FullOuter :: T_JoinType 
sem_JoinType_FullOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _annotatedTree =
                  FullOuter
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_JoinType_Inner :: T_JoinType 
sem_JoinType_Inner  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _annotatedTree =
                  Inner
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_JoinType_LeftOuter :: T_JoinType 
sem_JoinType_LeftOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _annotatedTree =
                  LeftOuter
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_JoinType_RightOuter :: T_JoinType 
sem_JoinType_RightOuter  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: JoinType
              _annotatedTree =
                  RightOuter
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  Plpgsql
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Language_Sql :: T_Language 
sem_Language_Sql  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Language
              _annotatedTree =
                  Sql
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
                      ( MExpression)
data Inh_MExpression  = Inh_MExpression {env_Inh_MExpression :: Environment}
data Syn_MExpression  = Syn_MExpression {annotatedTree_Syn_MExpression :: MExpression}
wrap_MExpression :: T_MExpression  ->
                    Inh_MExpression  ->
                    Syn_MExpression 
wrap_MExpression sem (Inh_MExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_MExpression _lhsOannotatedTree ))
sem_MExpression_Just :: T_Expression  ->
                        T_MExpression 
sem_MExpression_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MExpression
              _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIliftedColumnName :: String
              _annotatedTree =
                  Just _justIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _justOenv =
                  _lhsIenv
              ( _justIannotatedTree,_justIliftedColumnName) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree)))
sem_MExpression_Nothing :: T_MExpression 
sem_MExpression_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MExpression
              _annotatedTree =
                  Nothing
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
                    ( MTableRef,([QualifiedIDs]),([String]))
data Inh_MTableRef  = Inh_MTableRef {env_Inh_MTableRef :: Environment}
data Syn_MTableRef  = Syn_MTableRef {annotatedTree_Syn_MTableRef :: MTableRef,idens_Syn_MTableRef :: [QualifiedIDs],joinIdens_Syn_MTableRef :: [String]}
wrap_MTableRef :: T_MTableRef  ->
                  Inh_MTableRef  ->
                  Syn_MTableRef 
wrap_MTableRef sem (Inh_MTableRef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens) =
             (sem _lhsIenv )
     in  (Syn_MTableRef _lhsOannotatedTree _lhsOidens _lhsOjoinIdens ))
sem_MTableRef_Just :: T_TableRef  ->
                      T_MTableRef 
sem_MTableRef_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MTableRef
              _lhsOidens :: ([QualifiedIDs])
              _lhsOjoinIdens :: ([String])
              _justOenv :: Environment
              _justIannotatedTree :: TableRef
              _justIidens :: ([QualifiedIDs])
              _justIjoinIdens :: ([String])
              _annotatedTree =
                  Just _justIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _lhsOidens =
                  _justIidens
              _lhsOjoinIdens =
                  _justIjoinIdens
              _justOenv =
                  _lhsIenv
              ( _justIannotatedTree,_justIidens,_justIjoinIdens) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_MTableRef_Nothing :: T_MTableRef 
sem_MTableRef_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOidens :: ([QualifiedIDs])
              _lhsOjoinIdens :: ([String])
              _lhsOannotatedTree :: MTableRef
              _lhsOidens =
                  []
              _lhsOjoinIdens =
                  []
              _annotatedTree =
                  Nothing
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
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
                          ( MaybeExpression)
data Inh_MaybeExpression  = Inh_MaybeExpression {env_Inh_MaybeExpression :: Environment}
data Syn_MaybeExpression  = Syn_MaybeExpression {annotatedTree_Syn_MaybeExpression :: MaybeExpression}
wrap_MaybeExpression :: T_MaybeExpression  ->
                        Inh_MaybeExpression  ->
                        Syn_MaybeExpression 
wrap_MaybeExpression sem (Inh_MaybeExpression _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_MaybeExpression _lhsOannotatedTree ))
sem_MaybeExpression_Just :: T_Expression  ->
                            T_MaybeExpression 
sem_MaybeExpression_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MaybeExpression
              _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIliftedColumnName :: String
              _annotatedTree =
                  Just _justIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _justOenv =
                  _lhsIenv
              ( _justIannotatedTree,_justIliftedColumnName) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree)))
sem_MaybeExpression_Nothing :: T_MaybeExpression 
sem_MaybeExpression_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: MaybeExpression
              _annotatedTree =
                  Nothing
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  Natural
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Natural_Unnatural :: T_Natural 
sem_Natural_Unnatural  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Natural
              _annotatedTree =
                  Unnatural
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
         (let _lhsOannotatedTree :: OnExpr
              _justOenv :: Environment
              _justIannotatedTree :: JoinExpression
              _annotatedTree =
                  Just _justIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _justOenv =
                  _lhsIenv
              ( _justIannotatedTree) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree)))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: OnExpr
              _annotatedTree =
                  Nothing
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
type T_ParamDef  = Environment ->
                   ( ParamDef,(Either [TypeError] Type),String)
data Inh_ParamDef  = Inh_ParamDef {env_Inh_ParamDef :: Environment}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef,namedType_Syn_ParamDef :: Either [TypeError] Type,paramName_Syn_ParamDef :: String}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOnamedType,_lhsOparamName) =
             (sem _lhsIenv )
     in  (Syn_ParamDef _lhsOannotatedTree _lhsOnamedType _lhsOparamName ))
sem_ParamDef_ParamDef :: String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef name_ typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOparamName :: String
              _lhsOannotatedTree :: ParamDef
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: (Either [TypeError] Type)
              _lhsOnamedType =
                  _typInamedType
              _lhsOparamName =
                  name_
              _annotatedTree =
                  ParamDef name_ _typIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _typOenv =
                  _lhsIenv
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOparamName)))
sem_ParamDef_ParamDefTp :: T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOparamName :: String
              _lhsOannotatedTree :: ParamDef
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: (Either [TypeError] Type)
              _lhsOnamedType =
                  _typInamedType
              _lhsOparamName =
                  ""
              _annotatedTree =
                  ParamDefTp _typIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _typOenv =
                  _lhsIenv
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOparamName)))
-- ParamDefList ------------------------------------------------
type ParamDefList  = [(ParamDef)]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = Environment ->
                       ( ParamDefList,([(String,Either [TypeError] Type)]))
data Inh_ParamDefList  = Inh_ParamDefList {env_Inh_ParamDefList :: Environment}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList,params_Syn_ParamDefList :: [(String,Either [TypeError] Type)]}
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
         (let _lhsOparams :: ([(String,Either [TypeError] Type)])
              _lhsOannotatedTree :: ParamDefList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: ParamDef
              _hdInamedType :: (Either [TypeError] Type)
              _hdIparamName :: String
              _tlIannotatedTree :: ParamDefList
              _tlIparams :: ([(String,Either [TypeError] Type)])
              _lhsOparams =
                  ((_hdIparamName, _hdInamedType) : _tlIparams)
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree,_hdInamedType,_hdIparamName) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIparams) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOparams)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOparams :: ([(String,Either [TypeError] Type)])
              _lhsOannotatedTree :: ParamDefList
              _lhsOparams =
                  []
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree,_lhsOparams)))
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
              _annotatedTree =
                  RError
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_RaiseType_RException :: T_RaiseType 
sem_RaiseType_RException  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RaiseType
              _annotatedTree =
                  RException
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_RaiseType_RNotice :: T_RaiseType 
sem_RaiseType_RNotice  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RaiseType
              _annotatedTree =
                  RNotice
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  ContinueIdentity
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_RestartIdentity_RestartIdentity :: T_RestartIdentity 
sem_RestartIdentity_RestartIdentity  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RestartIdentity
              _annotatedTree =
                  RestartIdentity
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
               ( Root)
data Inh_Root  = Inh_Root {env_Inh_Root :: Environment}
data Syn_Root  = Syn_Root {annotatedTree_Syn_Root :: Root}
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_Root _lhsOannotatedTree ))
sem_Root_Root :: T_StatementList  ->
                 T_Root 
sem_Root_Root statements_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Root
              _statementsOenv :: Environment
              _statementsIannotatedTree :: StatementList
              _annotatedTree =
                  Root _statementsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _statementsOenv =
                  _lhsIenv
              ( _statementsIannotatedTree) =
                  (statements_ _statementsOenv )
          in  ( _lhsOannotatedTree)))
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
sem_RowConstraint_NotNullConstraint :: T_RowConstraint 
sem_RowConstraint_NotNullConstraint  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _annotatedTree =
                  NotNullConstraint
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_RowConstraint_NullConstraint :: T_RowConstraint 
sem_RowConstraint_NullConstraint  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _annotatedTree =
                  NullConstraint
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_RowConstraint_RowCheckConstraint :: T_Expression  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint expression_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIliftedColumnName :: String
              _annotatedTree =
                  RowCheckConstraint _expressionIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _expressionOenv =
                  _lhsIenv
              ( _expressionIannotatedTree,_expressionIliftedColumnName) =
                  (expression_ _expressionOenv )
          in  ( _lhsOannotatedTree)))
sem_RowConstraint_RowPrimaryKeyConstraint :: T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _annotatedTree =
                  RowPrimaryKeyConstraint
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_RowConstraint_RowReferenceConstraint :: String ->
                                            (Maybe String) ->
                                            T_Cascade  ->
                                            T_Cascade  ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _onUpdateOenv :: Environment
              _onDeleteOenv :: Environment
              _onUpdateIannotatedTree :: Cascade
              _onDeleteIannotatedTree :: Cascade
              _annotatedTree =
                  RowReferenceConstraint table_ att_ _onUpdateIannotatedTree _onDeleteIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _onUpdateOenv =
                  _lhsIenv
              _onDeleteOenv =
                  _lhsIenv
              ( _onUpdateIannotatedTree) =
                  (onUpdate_ _onUpdateOenv )
              ( _onDeleteIannotatedTree) =
                  (onDelete_ _onDeleteOenv )
          in  ( _lhsOannotatedTree)))
sem_RowConstraint_RowUniqueConstraint :: T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraint
              _annotatedTree =
                  RowUniqueConstraint
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
-- RowConstraintList -------------------------------------------
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
         (let _lhsOannotatedTree :: RowConstraintList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: RowConstraint
              _tlIannotatedTree :: RowConstraintList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: RowConstraintList
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
-- SelectExpression --------------------------------------------
data SelectExpression  = CombineSelect (Annotation) (CombineType) (SelectExpression) (SelectExpression) 
                       | Select (Annotation) (Distinct) (SelectList) (MTableRef) (Where) (ExpressionList) (MExpression) (ExpressionList) (Direction) (MExpression) (MExpression) 
                       | Values (Annotation) (ExpressionListList) 
                       deriving ( Eq,Show)
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
         (let _lhsOannotatedTree :: SelectExpression
              _ctypeOenv :: Environment
              _sel1Oenv :: Environment
              _sel2Oenv :: Environment
              _ctypeIannotatedTree :: CombineType
              _sel1IannotatedTree :: SelectExpression
              _sel2IannotatedTree :: SelectExpression
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  let sel1t = getTypeAnnotation _sel1IannotatedTree
                      sel2t = getTypeAnnotation _sel2IannotatedTree
                  in checkTypes [sel1t, sel2t] $
                        typeCheckCombineSelect _lhsIenv sel1t sel2t
              _backTree =
                  CombineSelect ann_ _ctypeIannotatedTree
                                _sel1IannotatedTree
                                _sel2IannotatedTree
              _annotatedTree =
                  CombineSelect ann_ _ctypeIannotatedTree _sel1IannotatedTree _sel2IannotatedTree
              _ctypeOenv =
                  _lhsIenv
              _sel1Oenv =
                  _lhsIenv
              _sel2Oenv =
                  _lhsIenv
              ( _ctypeIannotatedTree) =
                  (ctype_ _ctypeOenv )
              ( _sel1IannotatedTree) =
                  (sel1_ _sel1Oenv )
              ( _sel2IannotatedTree) =
                  (sel2_ _sel2Oenv )
          in  ( _lhsOannotatedTree)))
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
              _selDistinctOenv :: Environment
              _selTrefOenv :: Environment
              _selGroupByOenv :: Environment
              _selHavingOenv :: Environment
              _selOrderByOenv :: Environment
              _selDirOenv :: Environment
              _selLimitOenv :: Environment
              _selOffsetOenv :: Environment
              _selDistinctIannotatedTree :: Distinct
              _selSelectListIannotatedTree :: SelectList
              _selSelectListIlistType :: Type
              _selTrefIannotatedTree :: MTableRef
              _selTrefIidens :: ([QualifiedIDs])
              _selTrefIjoinIdens :: ([String])
              _selWhereIannotatedTree :: Where
              _selGroupByIannotatedTree :: ExpressionList
              _selGroupByItypeList :: ([Type])
              _selHavingIannotatedTree :: MExpression
              _selOrderByIannotatedTree :: ExpressionList
              _selOrderByItypeList :: ([Type])
              _selDirIannotatedTree :: Direction
              _selLimitIannotatedTree :: MExpression
              _selOffsetIannotatedTree :: MExpression
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  do
                  whereType <- checkExpressionBool _selWhereIannotatedTree
                  let trefType = fromMaybe typeBool $ fmap getTypeAnnotation
                                                           _selTrefIannotatedTree
                      slType = _selSelectListIlistType
                  chainTypeCheckFailed [trefType, whereType, slType] $
                    Right $ case slType of
                              UnnamedCompositeType [(_,Pseudo Void)] -> Pseudo Void
                              _ -> SetOfType slType
              _backTree =
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
              _newEnv =
                  case updateEnvironment _lhsIenv [EnvUpdateIDs _selTrefIidens _selTrefIjoinIdens] of
                    Left _ -> _lhsIenv
                    Right e -> e
              _selSelectListOenv =
                  _newEnv
              _selWhereOenv =
                  _newEnv
              _annotatedTree =
                  Select ann_ _selDistinctIannotatedTree _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selDirIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
              _selDistinctOenv =
                  _lhsIenv
              _selTrefOenv =
                  _lhsIenv
              _selGroupByOenv =
                  _lhsIenv
              _selHavingOenv =
                  _lhsIenv
              _selOrderByOenv =
                  _lhsIenv
              _selDirOenv =
                  _lhsIenv
              _selLimitOenv =
                  _lhsIenv
              _selOffsetOenv =
                  _lhsIenv
              ( _selDistinctIannotatedTree) =
                  (selDistinct_ _selDistinctOenv )
              ( _selSelectListIannotatedTree,_selSelectListIlistType) =
                  (selSelectList_ _selSelectListOenv )
              ( _selTrefIannotatedTree,_selTrefIidens,_selTrefIjoinIdens) =
                  (selTref_ _selTrefOenv )
              ( _selWhereIannotatedTree) =
                  (selWhere_ _selWhereOenv )
              ( _selGroupByIannotatedTree,_selGroupByItypeList) =
                  (selGroupBy_ _selGroupByOenv )
              ( _selHavingIannotatedTree) =
                  (selHaving_ _selHavingOenv )
              ( _selOrderByIannotatedTree,_selOrderByItypeList) =
                  (selOrderBy_ _selOrderByOenv )
              ( _selDirIannotatedTree) =
                  (selDir_ _selDirOenv )
              ( _selLimitIannotatedTree) =
                  (selLimit_ _selLimitOenv )
              ( _selOffsetIannotatedTree) =
                  (selOffset_ _selOffsetOenv )
          in  ( _lhsOannotatedTree)))
sem_SelectExpression_Values :: Annotation ->
                               T_ExpressionListList  ->
                               T_SelectExpression 
sem_SelectExpression_Values ann_ vll_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: SelectExpression
              _vllOenv :: Environment
              _vllIannotatedTree :: ExpressionListList
              _vllItypeListList :: ([[Type]])
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  typeCheckValuesExpr
                              _lhsIenv
                              _vllItypeListList
              _backTree =
                  Values ann_ _vllIannotatedTree
              _annotatedTree =
                  Values ann_ _vllIannotatedTree
              _vllOenv =
                  _lhsIenv
              ( _vllIannotatedTree,_vllItypeListList) =
                  (vll_ _vllOenv )
          in  ( _lhsOannotatedTree)))
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
sem_SelectItem_SelExp :: T_Expression  ->
                         T_SelectItem 
sem_SelectItem_SelExp ex_  =
    (\ _lhsIenv ->
         (let _lhsOitemType :: Type
              _lhsOcolumnName :: String
              _lhsOannotatedTree :: SelectItem
              _exOenv :: Environment
              _exIannotatedTree :: Expression
              _exIliftedColumnName :: String
              _lhsOitemType =
                  getTypeAnnotation _exIannotatedTree
              _annotatedTree =
                  SelExp $ fixStar _exIannotatedTree
              _lhsOcolumnName =
                  case _exIliftedColumnName of
                    "" -> "?column?"
                    s -> s
              _lhsOannotatedTree =
                  _annotatedTree
              _exOenv =
                  _lhsIenv
              ( _exIannotatedTree,_exIliftedColumnName) =
                  (ex_ _exOenv )
          in  ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOitemType)))
sem_SelectItem_SelectItem :: T_Expression  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem ex_ name_  =
    (\ _lhsIenv ->
         (let _lhsOitemType :: Type
              _lhsOcolumnName :: String
              _lhsOannotatedTree :: SelectItem
              _exOenv :: Environment
              _exIannotatedTree :: Expression
              _exIliftedColumnName :: String
              _lhsOitemType =
                  getTypeAnnotation _exIannotatedTree
              _backTree =
                  SelectItem (fixStar _exIannotatedTree) name_
              _lhsOcolumnName =
                  name_
              _annotatedTree =
                  SelectItem _exIannotatedTree name_
              _lhsOannotatedTree =
                  _annotatedTree
              _exOenv =
                  _lhsIenv
              ( _exIannotatedTree,_exIliftedColumnName) =
                  (ex_ _exOenv )
          in  ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOitemType)))
-- SelectItemList ----------------------------------------------
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
         (let _lhsOlistType :: Type
              _lhsOannotatedTree :: SelectItemList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: SelectItem
              _hdIcolumnName :: String
              _hdIitemType :: Type
              _tlIannotatedTree :: SelectItemList
              _tlIlistType :: Type
              _lhsOlistType =
                  doSelectItemListTpe _lhsIenv _hdIcolumnName _hdIitemType _tlIlistType
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree,_hdIcolumnName,_hdIitemType) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIlistType) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOlistType)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: Type
              _lhsOannotatedTree :: SelectItemList
              _lhsOlistType =
                  UnnamedCompositeType []
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree,_lhsOlistType)))
-- SelectList --------------------------------------------------
data SelectList  = SelectList (SelectItemList) (StringList) 
                 deriving ( Eq,Show)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _items _stringList )  =
    (sem_SelectList_SelectList (sem_SelectItemList _items ) (sem_StringList _stringList ) )
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
sem_SelectList_SelectList :: T_SelectItemList  ->
                             T_StringList  ->
                             T_SelectList 
sem_SelectList_SelectList items_ stringList_  =
    (\ _lhsIenv ->
         (let _lhsOlistType :: Type
              _lhsOannotatedTree :: SelectList
              _itemsOenv :: Environment
              _stringListOenv :: Environment
              _itemsIannotatedTree :: SelectItemList
              _itemsIlistType :: Type
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _lhsOlistType =
                  _itemsIlistType
              _annotatedTree =
                  SelectList _itemsIannotatedTree _stringListIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _itemsOenv =
                  _lhsIenv
              _stringListOenv =
                  _lhsIenv
              ( _itemsIannotatedTree,_itemsIlistType) =
                  (items_ _itemsOenv )
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_ _stringListOenv )
          in  ( _lhsOannotatedTree,_lhsOlistType)))
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
sem_SetClause_RowSetClause :: T_StringList  ->
                              T_ExpressionList  ->
                              T_SetClause 
sem_SetClause_RowSetClause atts_ vals_  =
    (\ _lhsIenv ->
         (let _lhsOpairs :: ([(String,Type)])
              _lhsOannotatedTree :: SetClause
              _lhsOrowSetError :: (Maybe TypeError)
              _attsOenv :: Environment
              _valsOenv :: Environment
              _attsIannotatedTree :: StringList
              _attsIstrings :: ([String])
              _valsIannotatedTree :: ExpressionList
              _valsItypeList :: ([Type])
              _rowSetError =
                  let atts = _attsIstrings
                      types = getRowTypes _valsItypeList
                  in if length atts /= length types
                       then Just WrongNumberOfColumns
                       else Nothing
              _lhsOpairs =
                  zip _attsIstrings $ getRowTypes _valsItypeList
              _annotatedTree =
                  RowSetClause _attsIannotatedTree _valsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _lhsOrowSetError =
                  _rowSetError
              _attsOenv =
                  _lhsIenv
              _valsOenv =
                  _lhsIenv
              ( _attsIannotatedTree,_attsIstrings) =
                  (atts_ _attsOenv )
              ( _valsIannotatedTree,_valsItypeList) =
                  (vals_ _valsOenv )
          in  ( _lhsOannotatedTree,_lhsOpairs,_lhsOrowSetError)))
sem_SetClause_SetClause :: String ->
                           T_Expression  ->
                           T_SetClause 
sem_SetClause_SetClause att_ val_  =
    (\ _lhsIenv ->
         (let _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetError :: (Maybe TypeError)
              _lhsOannotatedTree :: SetClause
              _valOenv :: Environment
              _valIannotatedTree :: Expression
              _valIliftedColumnName :: String
              _lhsOpairs =
                  [(att_, getTypeAnnotation _valIannotatedTree)]
              _lhsOrowSetError =
                  Nothing
              _annotatedTree =
                  SetClause att_ _valIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _valOenv =
                  _lhsIenv
              ( _valIannotatedTree,_valIliftedColumnName) =
                  (val_ _valOenv )
          in  ( _lhsOannotatedTree,_lhsOpairs,_lhsOrowSetError)))
-- SetClauseList -----------------------------------------------
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
         (let _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetErrors :: ([TypeError])
              _lhsOannotatedTree :: SetClauseList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: SetClause
              _hdIpairs :: ([(String,Type)])
              _hdIrowSetError :: (Maybe TypeError)
              _tlIannotatedTree :: SetClauseList
              _tlIpairs :: ([(String,Type)])
              _tlIrowSetErrors :: ([TypeError])
              _lhsOpairs =
                  _hdIpairs ++ _tlIpairs
              _lhsOrowSetErrors =
                  maybeToList _hdIrowSetError ++ _tlIrowSetErrors
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree,_hdIpairs,_hdIrowSetError) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIpairs,_tlIrowSetErrors) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOpairs,_lhsOrowSetErrors)))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetErrors :: ([TypeError])
              _lhsOannotatedTree :: SetClauseList
              _lhsOpairs =
                  []
              _lhsOrowSetErrors =
                  []
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree,_lhsOpairs,_lhsOrowSetErrors)))
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
type T_Statement  = Environment ->
                    ( Statement)
data Inh_Statement  = Inh_Statement {env_Inh_Statement :: Environment}
data Syn_Statement  = Syn_Statement {annotatedTree_Syn_Statement :: Statement}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_Statement _lhsOannotatedTree ))
sem_Statement_Assignment :: Annotation ->
                            String ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _valueOenv :: Environment
              _valueIannotatedTree :: Expression
              _valueIliftedColumnName :: String
              _annotatedTree =
                  Assignment ann_ target_ _valueIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _valueOenv =
                  _lhsIenv
              ( _valueIannotatedTree,_valueIliftedColumnName) =
                  (value_ _valueOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_CaseStatement :: Annotation ->
                               T_Expression  ->
                               T_ExpressionListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement ann_ val_ cases_ els_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _valOenv :: Environment
              _casesOenv :: Environment
              _elsOenv :: Environment
              _valIannotatedTree :: Expression
              _valIliftedColumnName :: String
              _casesIannotatedTree :: ExpressionListStatementListPairList
              _elsIannotatedTree :: StatementList
              _annotatedTree =
                  CaseStatement ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _valOenv =
                  _lhsIenv
              _casesOenv =
                  _lhsIenv
              _elsOenv =
                  _lhsIenv
              ( _valIannotatedTree,_valIliftedColumnName) =
                  (val_ _valOenv )
              ( _casesIannotatedTree) =
                  (cases_ _casesOenv )
              ( _elsIannotatedTree) =
                  (els_ _elsOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_ContinueStatement :: Annotation ->
                                   T_Statement 
sem_Statement_ContinueStatement ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _annotatedTree =
                  ContinueStatement ann_
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Statement_Copy :: Annotation ->
                      String ->
                      T_StringList  ->
                      T_CopySource  ->
                      T_Statement 
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _targetColsOenv :: Environment
              _sourceOenv :: Environment
              _targetColsIannotatedTree :: StringList
              _targetColsIstrings :: ([String])
              _sourceIannotatedTree :: CopySource
              _annotatedTree =
                  Copy ann_ table_ _targetColsIannotatedTree _sourceIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _targetColsOenv =
                  _lhsIenv
              _sourceOenv =
                  _lhsIenv
              ( _targetColsIannotatedTree,_targetColsIstrings) =
                  (targetCols_ _targetColsOenv )
              ( _sourceIannotatedTree) =
                  (source_ _sourceOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_CopyData :: Annotation ->
                          String ->
                          T_Statement 
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _annotatedTree =
                  CopyData ann_ insData_
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Statement_CreateDomain :: Annotation ->
                              String ->
                              T_TypeName  ->
                              (Maybe Expression) ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ check_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just $ StatementInfoA _statementInfo
              _namedTypeType =
                  case _typInamedType of
                    Left _ -> TypeCheckFailed
                    Right x -> x
              _tpe =
                  checkTypes [_namedTypeType    ] $ Right $ Pseudo Void
              _backTree =
                  CreateDomain ann_ name_ _typIannotatedTree check_
              _statementInfo =
                  CreateDomainInfo name_ _namedTypeType
              _annotatedTree =
                  CreateDomain ann_ name_ _typIannotatedTree check_
              _typOenv =
                  _lhsIenv
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree)))
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
              _langOenv :: Environment
              _paramsOenv :: Environment
              _rettypeOenv :: Environment
              _bodyOenv :: Environment
              _volOenv :: Environment
              _langIannotatedTree :: Language
              _paramsIannotatedTree :: ParamDefList
              _paramsIparams :: ([(String,Either [TypeError] Type)])
              _rettypeIannotatedTree :: TypeName
              _rettypeInamedType :: (Either [TypeError] Type)
              _bodyIannotatedTree :: FnBody
              _volIannotatedTree :: Volatility
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just $ StatementInfoA _statementInfo
              _retTypeType =
                  errorToTypeFail _rettypeInamedType
              _paramTypes =
                  let tpes = map snd _paramsIparams
                  in if null $ concat $ lefts tpes
                     then rights tpes
                     else [TypeCheckFailed]
              _tpe =
                  do
                    _rettypeInamedType
                    let tpes = map snd _paramsIparams
                    checkErrorList (concat $ lefts tpes) $ Pseudo Void
              _backTree =
                  CreateFunction ann_
                                 _langIannotatedTree
                                 name_
                                 _paramsIannotatedTree
                                 _rettypeIannotatedTree
                                 bodyQuote_
                                 _bodyIannotatedTree
                                 _volIannotatedTree
              _statementInfo =
                  CreateFunctionInfo (name_,_paramTypes    ,_retTypeType    )
              _annotatedTree =
                  CreateFunction ann_ _langIannotatedTree name_ _paramsIannotatedTree _rettypeIannotatedTree bodyQuote_ _bodyIannotatedTree _volIannotatedTree
              _langOenv =
                  _lhsIenv
              _paramsOenv =
                  _lhsIenv
              _rettypeOenv =
                  _lhsIenv
              _bodyOenv =
                  _lhsIenv
              _volOenv =
                  _lhsIenv
              ( _langIannotatedTree) =
                  (lang_ _langOenv )
              ( _paramsIannotatedTree,_paramsIparams) =
                  (params_ _paramsOenv )
              ( _rettypeIannotatedTree,_rettypeInamedType) =
                  (rettype_ _rettypeOenv )
              ( _bodyIannotatedTree) =
                  (body_ _bodyOenv )
              ( _volIannotatedTree) =
                  (vol_ _volOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_CreateTable :: Annotation ->
                             String ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _attsOenv :: Environment
              _consOenv :: Environment
              _attsIannotatedTree :: AttributeDefList
              _attsIattrs :: ([(String, Either [TypeError] Type)])
              _consIannotatedTree :: ConstraintList
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just $ StatementInfoA _statementInfo
              _attrTypes =
                  map snd _attsIattrs
              _tpe =
                  checkErrorList (concat $ lefts _attrTypes    ) $ Pseudo Void
              _compositeType =
                  errorToTypeFailF (const $ UnnamedCompositeType doneAtts) _tpe
                  where
                    doneAtts = map (second errorToTypeFail) _attsIattrs
              _backTree =
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
              _statementInfo =
                  RelvarInfo (name_, TableComposite, _compositeType    , UnnamedCompositeType [])
              _annotatedTree =
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
              _attsOenv =
                  _lhsIenv
              _consOenv =
                  _lhsIenv
              ( _attsIannotatedTree,_attsIattrs) =
                  (atts_ _attsOenv )
              ( _consIannotatedTree) =
                  (cons_ _consOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_CreateTableAs :: Annotation ->
                               String ->
                               T_SelectExpression  ->
                               T_Statement 
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _exprOenv :: Environment
              _exprIannotatedTree :: SelectExpression
              _selType =
                  getTypeAnnotation _exprIannotatedTree
              _tpe =
                  Right _selType
              _backTree =
                  CreateTableAs ann_ name_ _exprIannotatedTree
              _statementInfo =
                  RelvarInfo (name_, TableComposite, _selType    , UnnamedCompositeType [])
              _annotatedTree =
                  CreateTableAs ann_ name_ _exprIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _exprOenv =
                  _lhsIenv
              ( _exprIannotatedTree) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_CreateType :: Annotation ->
                            String ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _attsOenv :: Environment
              _attsIannotatedTree :: TypeAttributeDefList
              _attsIattrs :: ([(String, Either [TypeError] Type)])
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just $ StatementInfoA _statementInfo
              _attrTypes =
                  map snd _attsIattrs
              _tpe =
                  checkErrorList (concat $ lefts _attrTypes    ) $ Pseudo Void
              _compositeType =
                  errorToTypeFailF (const $ UnnamedCompositeType doneAtts) _tpe
                  where
                    doneAtts = map (second errorToTypeFail) _attsIattrs
              _backTree =
                  CreateType ann_ name_ _attsIannotatedTree
              _statementInfo =
                  RelvarInfo (name_, Composite, _compositeType    , UnnamedCompositeType [])
              _annotatedTree =
                  CreateType ann_ name_ _attsIannotatedTree
              _attsOenv =
                  _lhsIenv
              ( _attsIannotatedTree,_attsIattrs) =
                  (atts_ _attsOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_CreateView :: Annotation ->
                            String ->
                            T_SelectExpression  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ expr_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _exprOenv :: Environment
              _exprIannotatedTree :: SelectExpression
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just $ StatementInfoA _statementInfo
              _tpe =
                  checkTypes [getTypeAnnotation _exprIannotatedTree] $ Right $ Pseudo Void
              _backTree =
                  CreateView ann_ name_ _exprIannotatedTree
              _statementInfo =
                  RelvarInfo (name_, ViewComposite, getTypeAnnotation _exprIannotatedTree, UnnamedCompositeType [])
              _annotatedTree =
                  CreateView ann_ name_ _exprIannotatedTree
              _exprOenv =
                  _lhsIenv
              ( _exprIannotatedTree) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_Delete :: Annotation ->
                        String ->
                        T_Where  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Delete ann_ table_ whr_ returning_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _whrOenv :: Environment
              _whrIannotatedTree :: Where
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just $ StatementInfoA _statementInfo
              _tpe =
                  case checkRelationExists _lhsIenv table_ of
                    Just e -> Left [e]
                    Nothing -> do
                      whereType <- checkExpressionBool _whrIannotatedTree
                      return $ Pseudo Void
              _statementInfo =
                  DeleteInfo table_
              _backTree =
                  Delete ann_ table_ _whrIannotatedTree returning_
              _annotatedTree =
                  Delete ann_ table_ _whrIannotatedTree returning_
              _whrOenv =
                  _lhsIenv
              ( _whrIannotatedTree) =
                  (whr_ _whrOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_DropFunction :: Annotation ->
                              T_IfExists  ->
                              T_StringStringListPairList  ->
                              T_Cascade  ->
                              T_Statement 
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _ifEOenv :: Environment
              _sigsOenv :: Environment
              _cascadeOenv :: Environment
              _ifEIannotatedTree :: IfExists
              _sigsIannotatedTree :: StringStringListPairList
              _cascadeIannotatedTree :: Cascade
              _annotatedTree =
                  DropFunction ann_ _ifEIannotatedTree _sigsIannotatedTree _cascadeIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _ifEOenv =
                  _lhsIenv
              _sigsOenv =
                  _lhsIenv
              _cascadeOenv =
                  _lhsIenv
              ( _ifEIannotatedTree) =
                  (ifE_ _ifEOenv )
              ( _sigsIannotatedTree) =
                  (sigs_ _sigsOenv )
              ( _cascadeIannotatedTree) =
                  (cascade_ _cascadeOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_DropSomething :: Annotation ->
                               T_DropType  ->
                               T_IfExists  ->
                               T_StringList  ->
                               T_Cascade  ->
                               T_Statement 
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _dropTypeOenv :: Environment
              _ifEOenv :: Environment
              _namesOenv :: Environment
              _cascadeOenv :: Environment
              _dropTypeIannotatedTree :: DropType
              _ifEIannotatedTree :: IfExists
              _namesIannotatedTree :: StringList
              _namesIstrings :: ([String])
              _cascadeIannotatedTree :: Cascade
              _annotatedTree =
                  DropSomething ann_ _dropTypeIannotatedTree _ifEIannotatedTree _namesIannotatedTree _cascadeIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _dropTypeOenv =
                  _lhsIenv
              _ifEOenv =
                  _lhsIenv
              _namesOenv =
                  _lhsIenv
              _cascadeOenv =
                  _lhsIenv
              ( _dropTypeIannotatedTree) =
                  (dropType_ _dropTypeOenv )
              ( _ifEIannotatedTree) =
                  (ifE_ _ifEOenv )
              ( _namesIannotatedTree,_namesIstrings) =
                  (names_ _namesOenv )
              ( _cascadeIannotatedTree) =
                  (cascade_ _cascadeOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_Execute :: Annotation ->
                         T_Expression  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _annotatedTree =
                  Execute ann_ _exprIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _exprOenv =
                  _lhsIenv
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_ExecuteInto :: Annotation ->
                             T_Expression  ->
                             T_StringList  ->
                             T_Statement 
sem_Statement_ExecuteInto ann_ expr_ targets_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _exprOenv :: Environment
              _targetsOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _targetsIannotatedTree :: StringList
              _targetsIstrings :: ([String])
              _annotatedTree =
                  ExecuteInto ann_ _exprIannotatedTree _targetsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _exprOenv =
                  _lhsIenv
              _targetsOenv =
                  _lhsIenv
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              ( _targetsIannotatedTree,_targetsIstrings) =
                  (targets_ _targetsOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_ForIntegerStatement :: Annotation ->
                                     String ->
                                     T_Expression  ->
                                     T_Expression  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement ann_ var_ from_ to_ sts_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _fromOenv :: Environment
              _toOenv :: Environment
              _stsOenv :: Environment
              _fromIannotatedTree :: Expression
              _fromIliftedColumnName :: String
              _toIannotatedTree :: Expression
              _toIliftedColumnName :: String
              _stsIannotatedTree :: StatementList
              _annotatedTree =
                  ForIntegerStatement ann_ var_ _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _fromOenv =
                  _lhsIenv
              _toOenv =
                  _lhsIenv
              _stsOenv =
                  _lhsIenv
              ( _fromIannotatedTree,_fromIliftedColumnName) =
                  (from_ _fromOenv )
              ( _toIannotatedTree,_toIliftedColumnName) =
                  (to_ _toOenv )
              ( _stsIannotatedTree) =
                  (sts_ _stsOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_ForSelectStatement :: Annotation ->
                                    String ->
                                    T_SelectExpression  ->
                                    T_StatementList  ->
                                    T_Statement 
sem_Statement_ForSelectStatement ann_ var_ sel_ sts_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _selOenv :: Environment
              _stsOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _stsIannotatedTree :: StatementList
              _annotatedTree =
                  ForSelectStatement ann_ var_ _selIannotatedTree _stsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _selOenv =
                  _lhsIenv
              _stsOenv =
                  _lhsIenv
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
              ( _stsIannotatedTree) =
                  (sts_ _stsOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_If :: Annotation ->
                    T_ExpressionStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _casesOenv :: Environment
              _elsOenv :: Environment
              _casesIannotatedTree :: ExpressionStatementListPairList
              _elsIannotatedTree :: StatementList
              _annotatedTree =
                  If ann_ _casesIannotatedTree _elsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _casesOenv =
                  _lhsIenv
              _elsOenv =
                  _lhsIenv
              ( _casesIannotatedTree) =
                  (cases_ _casesOenv )
              ( _elsIannotatedTree) =
                  (els_ _elsOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_Insert :: Annotation ->
                        String ->
                        T_StringList  ->
                        T_SelectExpression  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _targetColsOenv :: Environment
              _insDataOenv :: Environment
              _targetColsIannotatedTree :: StringList
              _targetColsIstrings :: ([String])
              _insDataIannotatedTree :: SelectExpression
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just $ StatementInfoA _statementInfo
              _columnStuff =
                  checkColumnConsistency _lhsIenv
                                         table_
                                         _targetColsIstrings
                                         (unwrapComposite $ unwrapSetOf $
                                                          getTypeAnnotation _insDataIannotatedTree)
              _tpe =
                  checkTypes [getTypeAnnotation _insDataIannotatedTree] $ do
                    _columnStuff
                    Right $ Pseudo Void
              _statementInfo =
                  InsertInfo table_ $ errorToTypeFailF UnnamedCompositeType _columnStuff
              _backTree =
                  Insert ann_ table_ _targetColsIannotatedTree
                         _insDataIannotatedTree returning_
              _annotatedTree =
                  Insert ann_ table_ _targetColsIannotatedTree _insDataIannotatedTree returning_
              _targetColsOenv =
                  _lhsIenv
              _insDataOenv =
                  _lhsIenv
              ( _targetColsIannotatedTree,_targetColsIstrings) =
                  (targetCols_ _targetColsOenv )
              ( _insDataIannotatedTree) =
                  (insData_ _insDataOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_NullStatement :: Annotation ->
                               T_Statement 
sem_Statement_NullStatement ann_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _annotatedTree =
                  NullStatement ann_
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Statement_Perform :: Annotation ->
                         T_Expression  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _annotatedTree =
                  Perform ann_ _exprIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _exprOenv =
                  _lhsIenv
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_Raise :: Annotation ->
                       T_RaiseType  ->
                       String ->
                       T_ExpressionList  ->
                       T_Statement 
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _levelOenv :: Environment
              _argsOenv :: Environment
              _levelIannotatedTree :: RaiseType
              _argsIannotatedTree :: ExpressionList
              _argsItypeList :: ([Type])
              _annotatedTree =
                  Raise ann_ _levelIannotatedTree message_ _argsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _levelOenv =
                  _lhsIenv
              _argsOenv =
                  _lhsIenv
              ( _levelIannotatedTree) =
                  (level_ _levelOenv )
              ( _argsIannotatedTree,_argsItypeList) =
                  (args_ _argsOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_Return :: Annotation ->
                        (Maybe Expression) ->
                        T_Statement 
sem_Statement_Return ann_ value_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _annotatedTree =
                  Return ann_ value_
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Statement_ReturnNext :: Annotation ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _exprOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _annotatedTree =
                  ReturnNext ann_ _exprIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _exprOenv =
                  _lhsIenv
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_ReturnQuery :: Annotation ->
                             T_SelectExpression  ->
                             T_Statement 
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _annotatedTree =
                  ReturnQuery ann_ _selIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _selOenv =
                  _lhsIenv
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_SelectStatement :: Annotation ->
                                 T_SelectExpression  ->
                                 T_Statement 
sem_Statement_SelectStatement ann_ ex_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _exOenv :: Environment
              _exIannotatedTree :: SelectExpression
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just $ StatementInfoA _statementInfo
              _tpe =
                  checkTypes [getTypeAnnotation _exIannotatedTree] $ Right $ Pseudo Void
              _statementInfo =
                  SelectInfo $ getTypeAnnotation _exIannotatedTree
              _backTree =
                  SelectStatement ann_ _exIannotatedTree
              _annotatedTree =
                  SelectStatement ann_ _exIannotatedTree
              _exOenv =
                  _lhsIenv
              ( _exIannotatedTree) =
                  (ex_ _exOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_Truncate :: Annotation ->
                          T_StringList  ->
                          T_RestartIdentity  ->
                          T_Cascade  ->
                          T_Statement 
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _tablesOenv :: Environment
              _restartIdentityOenv :: Environment
              _cascadeOenv :: Environment
              _tablesIannotatedTree :: StringList
              _tablesIstrings :: ([String])
              _restartIdentityIannotatedTree :: RestartIdentity
              _cascadeIannotatedTree :: Cascade
              _annotatedTree =
                  Truncate ann_ _tablesIannotatedTree _restartIdentityIannotatedTree _cascadeIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _tablesOenv =
                  _lhsIenv
              _restartIdentityOenv =
                  _lhsIenv
              _cascadeOenv =
                  _lhsIenv
              ( _tablesIannotatedTree,_tablesIstrings) =
                  (tables_ _tablesOenv )
              ( _restartIdentityIannotatedTree) =
                  (restartIdentity_ _restartIdentityOenv )
              ( _cascadeIannotatedTree) =
                  (cascade_ _cascadeOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_Update :: Annotation ->
                        String ->
                        T_SetClauseList  ->
                        T_Where  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Update ann_ table_ assigns_ whr_ returning_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _assignsOenv :: Environment
              _whrOenv :: Environment
              _assignsIannotatedTree :: SetClauseList
              _assignsIpairs :: ([(String,Type)])
              _assignsIrowSetErrors :: ([TypeError])
              _whrIannotatedTree :: Where
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    $ Just $ StatementInfoA _statementInfo
              _tpe =
                  do
                  let re = checkRelationExists _lhsIenv table_
                  when (isJust re) $
                       Left [fromJust $ re]
                  whereType <- checkExpressionBool _whrIannotatedTree
                  chainTypeCheckFailed (whereType:map snd _assignsIpairs) $ do
                    _columnsConsistent
                    checkErrorList _assignsIrowSetErrors $ Pseudo Void
              _columnsConsistent =
                  checkColumnConsistency _lhsIenv table_ (map fst _assignsIpairs) _assignsIpairs
              _statementInfo =
                  UpdateInfo table_ $ flip errorToTypeFailF _columnsConsistent     $
                                           \c -> let colNames = map fst _assignsIpairs
                                                 in UnnamedCompositeType $ map (\t -> (t,getType c t)) colNames
                  where
                    getType cols t = fromJust $ lookup t cols
              _backTree =
                  Update ann_ table_ _assignsIannotatedTree _whrIannotatedTree returning_
              _annotatedTree =
                  Update ann_ table_ _assignsIannotatedTree _whrIannotatedTree returning_
              _assignsOenv =
                  _lhsIenv
              _whrOenv =
                  _lhsIenv
              ( _assignsIannotatedTree,_assignsIpairs,_assignsIrowSetErrors) =
                  (assigns_ _assignsOenv )
              ( _whrIannotatedTree) =
                  (whr_ _whrOenv )
          in  ( _lhsOannotatedTree)))
sem_Statement_WhileStatement :: Annotation ->
                                T_Expression  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement ann_ expr_ sts_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Statement
              _exprOenv :: Environment
              _stsOenv :: Environment
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _stsIannotatedTree :: StatementList
              _annotatedTree =
                  WhileStatement ann_ _exprIannotatedTree _stsIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _exprOenv =
                  _lhsIenv
              _stsOenv =
                  _lhsIenv
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_ _exprOenv )
              ( _stsIannotatedTree) =
                  (sts_ _stsOenv )
          in  ( _lhsOannotatedTree)))
-- StatementList -----------------------------------------------
type StatementList  = [(Statement)]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_Statement list) )
-- semantic domain
type T_StatementList  = Environment ->
                        ( StatementList)
data Inh_StatementList  = Inh_StatementList {env_Inh_StatementList :: Environment}
data Syn_StatementList  = Syn_StatementList {annotatedTree_Syn_StatementList :: StatementList}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_StatementList _lhsOannotatedTree ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: StatementList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: Statement
              _tlIannotatedTree :: StatementList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: StatementList
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
-- StringList --------------------------------------------------
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
         (let _lhsOstrings :: ([String])
              _lhsOannotatedTree :: StringList
              _tlOenv :: Environment
              _tlIannotatedTree :: StringList
              _tlIstrings :: ([String])
              _lhsOstrings =
                  hd_ : _tlIstrings
              _annotatedTree =
                  (:) hd_ _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _tlOenv =
                  _lhsIenv
              ( _tlIannotatedTree,_tlIstrings) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOstrings)))
sem_StringList_Nil :: T_StringList 
sem_StringList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOstrings :: ([String])
              _lhsOannotatedTree :: StringList
              _lhsOstrings =
                  []
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree,_lhsOstrings)))
-- StringStringListPair ----------------------------------------
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
         (let _lhsOannotatedTree :: StringStringListPair
              _x2Oenv :: Environment
              _x2IannotatedTree :: StringList
              _x2Istrings :: ([String])
              _annotatedTree =
                  (x1_,_x2IannotatedTree)
              _lhsOannotatedTree =
                  _annotatedTree
              _x2Oenv =
                  _lhsIenv
              ( _x2IannotatedTree,_x2Istrings) =
                  (x2_ _x2Oenv )
          in  ( _lhsOannotatedTree)))
-- StringStringListPairList ------------------------------------
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
         (let _lhsOannotatedTree :: StringStringListPairList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: StringStringListPair
              _tlIannotatedTree :: StringStringListPairList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree)))
sem_StringStringListPairList_Nil :: T_StringStringListPairList 
sem_StringStringListPairList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: StringStringListPairList
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
-- TableRef ----------------------------------------------------
data TableRef  = JoinedTref (Annotation) (TableRef) (Natural) (JoinType) (TableRef) (OnExpr) 
               | SubTref (Annotation) (SelectExpression) (String) 
               | Tref (Annotation) (String) 
               | TrefAlias (Annotation) (String) (String) 
               | TrefFun (Annotation) (Expression) 
               | TrefFunAlias (Annotation) (Expression) (String) 
               deriving ( Eq,Show)
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
                   ( TableRef,([QualifiedIDs]),([String]))
data Inh_TableRef  = Inh_TableRef {env_Inh_TableRef :: Environment}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef,idens_Syn_TableRef :: [QualifiedIDs],joinIdens_Syn_TableRef :: [String]}
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
         (let _lhsOannotatedTree :: TableRef
              _lhsOidens :: ([QualifiedIDs])
              _lhsOjoinIdens :: ([String])
              _tblOenv :: Environment
              _natOenv :: Environment
              _joinTypeOenv :: Environment
              _tbl1Oenv :: Environment
              _onExprOenv :: Environment
              _tblIannotatedTree :: TableRef
              _tblIidens :: ([QualifiedIDs])
              _tblIjoinIdens :: ([String])
              _natIannotatedTree :: Natural
              _joinTypeIannotatedTree :: JoinType
              _tbl1IannotatedTree :: TableRef
              _tbl1Iidens :: ([QualifiedIDs])
              _tbl1IjoinIdens :: ([String])
              _onExprIannotatedTree :: OnExpr
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
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
              _lhsOidens =
                  _tblIidens ++ _tbl1Iidens
              _lhsOjoinIdens =
                  commonFieldNames (getTypeAnnotation _tblIannotatedTree)
                                   (getTypeAnnotation _tbl1IannotatedTree)
              _backTree =
                  JoinedTref ann_
                             _tblIannotatedTree
                             _natIannotatedTree
                             _joinTypeIannotatedTree
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
              _annotatedTree =
                  JoinedTref ann_ _tblIannotatedTree _natIannotatedTree _joinTypeIannotatedTree _tbl1IannotatedTree _onExprIannotatedTree
              _tblOenv =
                  _lhsIenv
              _natOenv =
                  _lhsIenv
              _joinTypeOenv =
                  _lhsIenv
              _tbl1Oenv =
                  _lhsIenv
              _onExprOenv =
                  _lhsIenv
              ( _tblIannotatedTree,_tblIidens,_tblIjoinIdens) =
                  (tbl_ _tblOenv )
              ( _natIannotatedTree) =
                  (nat_ _natOenv )
              ( _joinTypeIannotatedTree) =
                  (joinType_ _joinTypeOenv )
              ( _tbl1IannotatedTree,_tbl1Iidens,_tbl1IjoinIdens) =
                  (tbl1_ _tbl1Oenv )
              ( _onExprIannotatedTree) =
                  (onExpr_ _onExprOenv )
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_SubTref :: Annotation ->
                        T_SelectExpression  ->
                        String ->
                        T_TableRef 
sem_TableRef_SubTref ann_ sel_ alias_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOidens :: ([QualifiedIDs])
              _lhsOjoinIdens :: ([String])
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  checkTypes [getTypeAnnotation _selIannotatedTree] $
                  Right $ unwrapSetOfComposite $ getTypeAnnotation _selIannotatedTree
              _backTree =
                  SubTref ann_ _selIannotatedTree alias_
              _lhsOidens =
                  [(alias_, (getTbCols _selIannotatedTree, []))]
              _lhsOjoinIdens =
                  []
              _annotatedTree =
                  SubTref ann_ _selIannotatedTree alias_
              _selOenv =
                  _lhsIenv
              ( _selIannotatedTree) =
                  (sel_ _selOenv )
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_Tref :: Annotation ->
                     String ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([QualifiedIDs])
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  either Left (Right . fst) _relType
              _lhsOjoinIdens =
                  []
              _relType =
                  getRelationType _lhsIenv tbl_
              _unwrappedRelType =
                  either (const ([], [])) (both unwrapComposite) _relType
              _lhsOidens =
                  [(tbl_, _unwrappedRelType    )]
              _backTree =
                  Tref ann_ tbl_
              _annotatedTree =
                  Tref ann_ tbl_
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_TrefAlias :: Annotation ->
                          String ->
                          String ->
                          T_TableRef 
sem_TableRef_TrefAlias ann_ tbl_ alias_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([QualifiedIDs])
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  either Left (Right . fst) _relType
              _lhsOjoinIdens =
                  []
              _relType =
                  getRelationType _lhsIenv tbl_
              _unwrappedRelType =
                  either (const ([], [])) (both unwrapComposite) _relType
              _lhsOidens =
                  [(alias_, _unwrappedRelType    )]
              _backTree =
                  TrefAlias ann_ tbl_ alias_
              _annotatedTree =
                  TrefAlias ann_ tbl_ alias_
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_TrefFun :: Annotation ->
                        T_Expression  ->
                        T_TableRef 
sem_TableRef_TrefFun ann_ fn_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([QualifiedIDs])
              _fnOenv :: Environment
              _fnIannotatedTree :: Expression
              _fnIliftedColumnName :: String
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  getFnType _lhsIenv _alias _fnIannotatedTree
              _lhsOjoinIdens =
                  []
              _lhsOidens =
                  case getFunIdens
                            _lhsIenv _alias
                            _fnIannotatedTree of
                    Left e -> []
                    Right x -> [second (\l -> (unwrapComposite l, [])) x]
              _alias =
                  ""
              _backTree =
                  TrefFun ann_ _fnIannotatedTree
              _annotatedTree =
                  TrefFun ann_ _fnIannotatedTree
              _fnOenv =
                  _lhsIenv
              ( _fnIannotatedTree,_fnIliftedColumnName) =
                  (fn_ _fnOenv )
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
sem_TableRef_TrefFunAlias :: Annotation ->
                             T_Expression  ->
                             String ->
                             T_TableRef 
sem_TableRef_TrefFunAlias ann_ fn_ alias_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOjoinIdens :: ([String])
              _lhsOidens :: ([QualifiedIDs])
              _fnOenv :: Environment
              _fnIannotatedTree :: Expression
              _fnIliftedColumnName :: String
              _lhsOannotatedTree =
                  annTypesAndErrors _backTree
                    (errorToTypeFail _tpe    )
                    (getErrors _tpe    )
                    Nothing
              _tpe =
                  getFnType _lhsIenv alias_ _fnIannotatedTree
              _lhsOjoinIdens =
                  []
              _lhsOidens =
                  case getFunIdens
                            _lhsIenv _alias
                            _fnIannotatedTree of
                    Left e -> []
                    Right x -> [second (\l -> (unwrapComposite l, [])) x]
              _alias =
                  alias_
              _backTree =
                  TrefFunAlias ann_ _fnIannotatedTree alias_
              _annotatedTree =
                  TrefFunAlias ann_ _fnIannotatedTree _alias
              _fnOenv =
                  _lhsIenv
              ( _fnIannotatedTree,_fnIliftedColumnName) =
                  (fn_ _fnOenv )
          in  ( _lhsOannotatedTree,_lhsOidens,_lhsOjoinIdens)))
-- TypeAttributeDef --------------------------------------------
data TypeAttributeDef  = TypeAttDef (String) (TypeName) 
                       deriving ( Eq,Show)
-- cata
sem_TypeAttributeDef :: TypeAttributeDef  ->
                        T_TypeAttributeDef 
sem_TypeAttributeDef (TypeAttDef _name _typ )  =
    (sem_TypeAttributeDef_TypeAttDef _name (sem_TypeName _typ ) )
-- semantic domain
type T_TypeAttributeDef  = Environment ->
                           ( TypeAttributeDef,String,(Either [TypeError] Type))
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {env_Inh_TypeAttributeDef :: Environment}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef,attrName_Syn_TypeAttributeDef :: String,namedType_Syn_TypeAttributeDef :: Either [TypeError] Type}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_TypeAttributeDef _lhsOannotatedTree _lhsOattrName _lhsOnamedType ))
sem_TypeAttributeDef_TypeAttDef :: String ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef name_ typ_  =
    (\ _lhsIenv ->
         (let _lhsOattrName :: String
              _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeAttributeDef
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: (Either [TypeError] Type)
              _lhsOattrName =
                  name_
              _lhsOnamedType =
                  _typInamedType
              _annotatedTree =
                  TypeAttDef name_ _typIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _typOenv =
                  _lhsIenv
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType)))
-- TypeAttributeDefList ----------------------------------------
type TypeAttributeDefList  = [(TypeAttributeDef)]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = Environment ->
                               ( TypeAttributeDefList,([(String, Either [TypeError] Type)]))
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {env_Inh_TypeAttributeDefList :: Environment}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,attrs_Syn_TypeAttributeDefList :: [(String, Either [TypeError] Type)]}
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
         (let _lhsOattrs :: ([(String, Either [TypeError] Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: TypeAttributeDef
              _hdIattrName :: String
              _hdInamedType :: (Either [TypeError] Type)
              _tlIannotatedTree :: TypeAttributeDefList
              _tlIattrs :: ([(String, Either [TypeError] Type)])
              _lhsOattrs =
                  (_hdIattrName, _hdInamedType) : _tlIattrs
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree,_hdIattrName,_hdInamedType) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree,_tlIattrs) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree,_lhsOattrs)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOattrs :: ([(String, Either [TypeError] Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOattrs =
                  []
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree,_lhsOattrs)))
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
type T_TypeName  = Environment ->
                   ( TypeName,(Either [TypeError] Type))
data Inh_TypeName  = Inh_TypeName {env_Inh_TypeName :: Environment}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName,namedType_Syn_TypeName :: Either [TypeError] Type}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIenv )  =
    (let ( _lhsOannotatedTree,_lhsOnamedType) =
             (sem _lhsIenv )
     in  (Syn_TypeName _lhsOannotatedTree _lhsOnamedType ))
sem_TypeName_ArrayTypeName :: T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: (Either [TypeError] Type)
              _lhsOnamedType =
                  ArrayType <$> _typInamedType
              _lhsOannotatedTree =
                  ArrayTypeName _typIannotatedTree
              _annotatedTree =
                  ArrayTypeName _typIannotatedTree
              _typOenv =
                  _lhsIenv
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
sem_TypeName_PrecTypeName :: String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName tn_ prec_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName
              _lhsOnamedType =
                  Right TypeCheckFailed
              _lhsOannotatedTree =
                  PrecTypeName tn_ prec_
              _annotatedTree =
                  PrecTypeName tn_ prec_
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
sem_TypeName_SetOfTypeName :: T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName typ_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: (Either [TypeError] Type)
              _lhsOnamedType =
                  SetOfType <$> _typInamedType
              _lhsOannotatedTree =
                  SetOfTypeName _typIannotatedTree
              _annotatedTree =
                  SetOfTypeName _typIannotatedTree
              _typOenv =
                  _lhsIenv
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
sem_TypeName_SimpleTypeName :: String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName tn_  =
    (\ _lhsIenv ->
         (let _lhsOnamedType :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName
              _lhsOnamedType =
                  envLookupType _lhsIenv $ canonicalizeTypeName tn_
              _lhsOannotatedTree =
                  SimpleTypeName tn_
              _annotatedTree =
                  SimpleTypeName tn_
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
-- VarDef ------------------------------------------------------
data VarDef  = VarDef (String) (TypeName) (Maybe Expression) 
             deriving ( Eq,Show)
-- cata
sem_VarDef :: VarDef  ->
              T_VarDef 
sem_VarDef (VarDef _name _typ _value )  =
    (sem_VarDef_VarDef _name (sem_TypeName _typ ) _value )
-- semantic domain
type T_VarDef  = Environment ->
                 ( VarDef)
data Inh_VarDef  = Inh_VarDef {env_Inh_VarDef :: Environment}
data Syn_VarDef  = Syn_VarDef {annotatedTree_Syn_VarDef :: VarDef}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_VarDef _lhsOannotatedTree ))
sem_VarDef_VarDef :: String ->
                     T_TypeName  ->
                     (Maybe Expression) ->
                     T_VarDef 
sem_VarDef_VarDef name_ typ_ value_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: VarDef
              _typOenv :: Environment
              _typIannotatedTree :: TypeName
              _typInamedType :: (Either [TypeError] Type)
              _annotatedTree =
                  VarDef name_ _typIannotatedTree value_
              _lhsOannotatedTree =
                  _annotatedTree
              _typOenv =
                  _lhsIenv
              ( _typIannotatedTree,_typInamedType) =
                  (typ_ _typOenv )
          in  ( _lhsOannotatedTree)))
-- VarDefList --------------------------------------------------
type VarDefList  = [(VarDef)]
-- cata
sem_VarDefList :: VarDefList  ->
                  T_VarDefList 
sem_VarDefList list  =
    (Prelude.foldr sem_VarDefList_Cons sem_VarDefList_Nil (Prelude.map sem_VarDef list) )
-- semantic domain
type T_VarDefList  = Environment ->
                     ( VarDefList)
data Inh_VarDefList  = Inh_VarDefList {env_Inh_VarDefList :: Environment}
data Syn_VarDefList  = Syn_VarDefList {annotatedTree_Syn_VarDefList :: VarDefList}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_VarDefList _lhsOannotatedTree ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: VarDefList
              _hdOenv :: Environment
              _tlOenv :: Environment
              _hdIannotatedTree :: VarDef
              _tlIannotatedTree :: VarDefList
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _hdOenv =
                  _lhsIenv
              _tlOenv =
                  _lhsIenv
              ( _hdIannotatedTree) =
                  (hd_ _hdOenv )
              ( _tlIannotatedTree) =
                  (tl_ _tlOenv )
          in  ( _lhsOannotatedTree)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: VarDefList
              _annotatedTree =
                  []
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
              _annotatedTree =
                  Immutable
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Volatility_Stable :: T_Volatility 
sem_Volatility_Stable  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Volatility
              _annotatedTree =
                  Stable
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
sem_Volatility_Volatile :: T_Volatility 
sem_Volatility_Volatile  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Volatility
              _annotatedTree =
                  Volatile
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))
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
                ( Where)
data Inh_Where  = Inh_Where {env_Inh_Where :: Environment}
data Syn_Where  = Syn_Where {annotatedTree_Syn_Where :: Where}
wrap_Where :: T_Where  ->
              Inh_Where  ->
              Syn_Where 
wrap_Where sem (Inh_Where _lhsIenv )  =
    (let ( _lhsOannotatedTree) =
             (sem _lhsIenv )
     in  (Syn_Where _lhsOannotatedTree ))
sem_Where_Just :: T_Expression  ->
                  T_Where 
sem_Where_Just just_  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Where
              _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIliftedColumnName :: String
              _annotatedTree =
                  Just _justIannotatedTree
              _lhsOannotatedTree =
                  _annotatedTree
              _justOenv =
                  _lhsIenv
              ( _justIannotatedTree,_justIliftedColumnName) =
                  (just_ _justOenv )
          in  ( _lhsOannotatedTree)))
sem_Where_Nothing :: T_Where 
sem_Where_Nothing  =
    (\ _lhsIenv ->
         (let _lhsOannotatedTree :: Where
              _annotatedTree =
                  Nothing
              _lhsOannotatedTree =
                  _annotatedTree
          in  ( _lhsOannotatedTree)))