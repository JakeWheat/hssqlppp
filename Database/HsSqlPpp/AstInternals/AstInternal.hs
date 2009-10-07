

-- UUAGC 0.9.11 (AstInternal.ag)
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
   ,LiftFlavour(..)
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
import Database.HsSqlPpp.AstInternals.TypeChecking.TypeConversion
import Database.HsSqlPpp.AstInternals.TypeChecking.ErrorUtils
import Database.HsSqlPpp.AstInternals.AstAnnotation
import Database.HsSqlPpp.AstInternals.Environment.EnvironmentInternal
import Database.HsSqlPpp.AstInternals.Environment.LocalIdentifierBindings
import Database.HsSqlPpp.AstInternals.Environment.DefaultTemplate1Environment
import Database.HsSqlPpp.Utils
import Data.Generics.PlateData


{-# LINE 617 "AstInternal.ag" #-}

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
        ta = wrap_Root t Inh_Root {env_Inh_Root = env
                                  ,lib_Inh_Root = emptyBindings}
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
        ta = wrap_Root t Inh_Root {env_Inh_Root = env
                                  ,lib_Inh_Root = emptyBindings}
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
              (wrap_ExpressionRoot t Inh_ExpressionRoot {env_Inh_ExpressionRoot = env
                                                        ,lib_Inh_ExpressionRoot = emptyBindings}))
    in case rt of
         ExpressionRoot e -> e

{-# LINE 148 "AstInternal.hs" #-}

{-# LINE 81 "./TypeChecking/Misc.ag" #-}

{-
================================================================================

= couple of small utils

I think this should be alright, an identifier referenced in an
expression can only have zero or one dot in it.
-}

splitIdentifier :: String -> (String,String)
splitIdentifier s = let (a,b) = span (/= '.') s
                    in if b == ""
                         then ("", a)
                         else (a,tail b)

{-
helper to make adding annotations a bit easier
-}

annTypesAndErrors :: Data a => a -> Type -> [TypeError]
                  -> Maybe [AnnotationElement] -> a
annTypesAndErrors item nt errs add =
    updateAnnotation modifier item
    where
      modifier = (([TypeAnnotation nt] ++ fromMaybe [] add ++
       map TypeErrorA errs) ++)

{-# LINE 179 "AstInternal.hs" #-}

{-# LINE 73 "./TypeChecking/Expressions.ag" #-}

{-

small shim in front of findCallMatch in the type conversion code, to
handle some special cases.

Some of the special cases will no longer be needed when variadic
support is added.

between, greatest and least are treated as syntactic sugar so we
delegate the function lookups to the <=/>= operators.

the row comparison should be more general than this, since it supports
any operator satifying some properties


TODO: move all of this into find call match. Don't know why it's separate
-}
typeCheckFunCall :: Environment -> String -> [Type] -> Either [TypeError] Type
typeCheckFunCall env fnName argsType =
    dependsOnRTpe argsType $
      case fnName of
              --"!arrayCtor" ->
              --      ArrayType <$> resolveResultSetType env argsType
              "!between" -> do
                    f1 <- lookupFn ">=" [argsType !! 0, argsType !! 1]
                    f2 <- lookupFn "<=" [argsType !! 0, argsType !! 2]
                    lookupFn "!and" [f1,f2]
              --"coalesce" -> resolveResultSetType env argsType
              "greatest" -> do
                    t <- lookupFn fnName argsType -- t <- resolveResultSetType env argsType
                    lookupFn ">=" [t,t]
                    return t
              "least" -> do
                    t <- lookupFn fnName argsType -- resolveResultSetType env argsType
                    lookupFn "<=" [t,t]
                    return t
              "!rowCtor" -> return $ AnonymousRecordType argsType
                    -- special case the row comparison ops
                    -- this needs to be fixed: we want to match
                    -- any implicit casts to functions on composite types
                    -- first, then we can use the anonymous record type on
                    -- any composite
              _ | let isRowCtor t = case t of
                                      AnonymousRecordType _ -> True
                                      _ -> False
                  in fnName `elem` ["=", "<>", "<=", ">=", "<", ">"]
                         && length argsType == 2
                         && all isRowCtor argsType ->
                    checkRowTypesMatch (head argsType) (head $ tail argsType)
              --checked for all special cases, so run general case now
              s -> lookupFn s argsType
    where
      lookupFn :: String -> [Type] -> Either [TypeError] Type
      lookupFn s1 args = do
        (_,_,r,_) <- findCallMatch env
                             (if s1 == "u-" then "-" else s1) args
        return r
      checkRowTypesMatch (AnonymousRecordType t1s) (AnonymousRecordType t2s) = do
        when (length t1s /= length t2s) $ Left [ValuesListsMustBeSameLength]
        let errs = map (resolveResultSetType env . (\(a,b) -> [a,b])) $
                     zip t1s t2s
        liftErrors $ concat $ lefts errs
        return typeBool
      checkRowTypesMatch x y  =
        error $ "internal error: checkRowTypesMatch called with " ++
                show x ++ "," ++ show y
{-# LINE 249 "AstInternal.hs" #-}

{-# LINE 137 "./TypeChecking/SelectStatement.ag" #-}


typeCheckValuesExpr :: Environment -> [[Type]] -> Either [TypeError] Type
typeCheckValuesExpr env rowsTs =
        let colNames = zipWith (++)
                           (repeat "column")
                           (map show [1..length $ head rowsTs])
        in unionRelTypes env rowsTs colNames


typeCheckCombineSelect :: Environment -> Type -> Type -> Either [TypeError] Type
typeCheckCombineSelect env v1 v2 = do
    u1 <- unwrapSetOfComposite v1
    let colNames = map fst u1
    u2 <- unwrapSetOfComposite v2
    let colTypes1 = map snd u1
    let colTypes2 = map snd u2
    unionRelTypes env [colTypes1,colTypes2] colNames

unionRelTypes :: Environment -> [[Type]] -> [String] -> Either [TypeError] Type
unionRelTypes env rowsTs colNames =
  let lengths = map length rowsTs
  in case () of
             _ | null rowsTs ->
                   Left [NoRowsGivenForValues]
               | not (all (==head lengths) lengths) ->
                   Left [ValuesListsMustBeSameLength]
               | otherwise ->
                   --i don't think this propagates all the errors, just the first set
                   mapM (resolveResultSetType env) (transpose rowsTs) >>=
                     (return . SetOfType . CompositeType . zip colNames)

{-# LINE 284 "AstInternal.hs" #-}

{-# LINE 194 "./TypeChecking/TableRefs.ag" #-}

{-
convert a function call into a [String,[(string,type)]] list for use
in a tableref context
first consideration is the alias: if there is an alias in the select,
e.g. select * from generate_series(1,2) x;  (alias is x)
we use that, otherwise we use the name of the function
second consideration is the attributes coming out, roughly speaking
we have to convert an arbitrary type to a relation type
if we have a relation valued function, we don't need to do anything
if we have a setof non composite, we lift the single type to an
attribute, using the function name for the attribute name
if we have a non setof, we lift the single type to an attribute and
then relation, using the function name for the attribute name
need to check to see what should happen with arrayof

-}
funIdens :: Environment -> String -> Expression -> Either [TypeError] (String,[(String,Type)])
funIdens env alias fnVal = do
   errorWhen (case fnVal of
                FunCall _ _ _ -> False
                _ -> True)
             [ContextError "FunCall"]
   let (FunCall _ fnName _) = fnVal
       correlationName = if alias /= ""
                           then alias
                           else fnName
   attrs <- do
     case getTypeAnnotation fnVal of
       SetOfType (NamedCompositeType t) -> envCompositePublicAttrs env [] t
       SetOfType x -> return [(correlationName,x)]
       y -> return [(correlationName,y)]
   return (correlationName, attrs)
{-# LINE 320 "AstInternal.hs" #-}

{-# LINE 92 "./TypeChecking/SelectLists.ag" #-}

expandStar :: LocalIdentifierBindings
           -> String
           -> Type
           -> [(String,Type)]
           -> [(String,Type)]
expandStar env colName colType types =
    fromRight types $ do
    let (correlationName,iden) = splitIdentifier colName
    newCols <- if iden == "*"
                 then libExpandStar env correlationName
                 else return [(iden, colType)]
    return $ newCols ++ types

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
{-# LINE 355 "AstInternal.hs" #-}

{-# LINE 136 "./TypeChecking/Dml.ag" #-}

getRowTypes :: [Type] -> [Type]
getRowTypes [AnonymousRecordType ts] = ts
getRowTypes ts = ts
{-# LINE 362 "AstInternal.hs" #-}

{-# LINE 174 "./TypeChecking/Dml.ag" #-}


--small shortcut to help produce better errors?
checkRelationExists :: Environment -> String -> Either [TypeError] ()
checkRelationExists env tbl =
    envCompositeDef env relationComposites tbl >>
    return ()

--used by both insert and update
checkColumnConsistency :: Environment ->  String -> [String] -> [(String,Type)]
                       -> Either [TypeError] [(String,Type)]
checkColumnConsistency env tbl cols' insNameTypePairs = do
  ttcols <- envCompositePublicAttrs env [] tbl
  let cols = if null cols'
               then map fst ttcols
               else cols'
  errorWhen (length insNameTypePairs /= length cols) [WrongNumberOfColumns]
  let nonMatchingColumns = cols \\ map fst ttcols
  errorWhen (not $ null nonMatchingColumns) $
       map UnrecognisedIdentifier nonMatchingColumns
  let targetNameTypePairs = map (\l -> (l,fromJust $ lookup l ttcols)) cols
        --check the types of the insdata match the column targets
        --name datatype columntype
      typeTriples = map (\((a,b),c) -> (a,b,c)) $
                    zip targetNameTypePairs $
                    map snd insNameTypePairs
      errs :: [TypeError]
      errs = concat $ lefts $
             map (\(_,b,c) -> checkAssignmentValid env c b) typeTriples
  liftErrors errs
  return targetNameTypePairs

{-# LINE 397 "AstInternal.hs" #-}
-- AttributeDef ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         attrName             : String
         namedType            : Type
         originalTree         : SELF 
   alternatives:
      alternative AttributeDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         child def            : MaybeExpression 
         child cons           : RowConstraintList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                       LocalIdentifierBindings ->
                       ( AttributeDef,String,Type,AttributeDef)
data Inh_AttributeDef  = Inh_AttributeDef {env_Inh_AttributeDef :: Environment,lib_Inh_AttributeDef :: LocalIdentifierBindings}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef,attrName_Syn_AttributeDef :: String,namedType_Syn_AttributeDef :: Type,originalTree_Syn_AttributeDef :: AttributeDef}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_AttributeDef _lhsOannotatedTree _lhsOattrName _lhsOnamedType _lhsOoriginalTree ))
sem_AttributeDef_AttributeDef ann_ name_ typ_ def_ cons_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _consOenv :: Environment
              _defOlib :: LocalIdentifierBindings
              _defOenv :: Environment
              _typOenv :: Environment
              typ_1 :: T_TypeName_1 
              _typIoriginalTree :: TypeName
              _typOlib :: LocalIdentifierBindings
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _consOlib :: LocalIdentifierBindings
              _consIannotatedTree :: RowConstraintList
              _consIoriginalTree :: RowConstraintList
              def_1 :: T_MaybeExpression_1 
              _defIoriginalTree :: MaybeExpression
              _defIannotatedTree :: MaybeExpression
              _lhsOannotatedTree :: AttributeDef
              _lhsOattrName :: String
              _lhsOnamedType :: Type
              _lhsOoriginalTree :: AttributeDef
              -- copy rule (down)
              _consOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 466 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 471 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 476 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 481 "AstInternal.hs" #-}
              ( _typIoriginalTree,typ_1) =
                  (typ_ )
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 488 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_1 _typOenv _typOlib )
              -- "./TypeChecking/CreateTable.ag"(line 80, column 9)
              _consOlib =
                  {-# LINE 80 "./TypeChecking/CreateTable.ag" #-}
                  case updateBindings _lhsIlib _lhsIenv
                           [LibStackIDs [("", [(name_, _typInamedType)])]] of
                    Left x -> error $ show x
                    Right e -> e
                  {-# LINE 498 "AstInternal.hs" #-}
              ( _consIannotatedTree,_consIoriginalTree) =
                  (cons_ _consOenv _consOlib )
              ( _defIoriginalTree,def_1) =
                  (def_ )
              ( _defIannotatedTree) =
                  (def_1 _defOenv _defOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
                  {-# LINE 509 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 514 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 64, column 9)
              _lhsOattrName =
                  {-# LINE 64 "./TypeChecking/CreateTable.ag" #-}
                  name_
                  {-# LINE 519 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 65, column 9)
              _lhsOnamedType =
                  {-# LINE 65 "./TypeChecking/CreateTable.ag" #-}
                  _typInamedType
                  {-# LINE 524 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIoriginalTree _defIoriginalTree _consIoriginalTree
                  {-# LINE 529 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 534 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType,_lhsOoriginalTree)))
-- AttributeDefList --------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         attrs                : [(String, Type)]
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : AttributeDef 
         child tl             : AttributeDefList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type AttributeDefList  = [(AttributeDef)]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = Environment ->
                           LocalIdentifierBindings ->
                           ( AttributeDefList,([(String, Type)]),AttributeDefList)
data Inh_AttributeDefList  = Inh_AttributeDefList {env_Inh_AttributeDefList :: Environment,lib_Inh_AttributeDefList :: LocalIdentifierBindings}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList,attrs_Syn_AttributeDefList :: [(String, Type)],originalTree_Syn_AttributeDefList :: AttributeDefList}
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_AttributeDefList _lhsOannotatedTree _lhsOattrs _lhsOoriginalTree ))
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: AttributeDefList
              _tlIattrs :: ([(String, Type)])
              _tlIoriginalTree :: AttributeDefList
              _hdIannotatedTree :: AttributeDef
              _hdIattrName :: String
              _hdInamedType :: Type
              _hdIoriginalTree :: AttributeDef
              _lhsOannotatedTree :: AttributeDefList
              _lhsOattrs :: ([(String, Type)])
              _lhsOoriginalTree :: AttributeDefList
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 598 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 603 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 608 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 613 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIattrs,_tlIoriginalTree) =
                  (tl_ _tlOenv _tlOlib )
              ( _hdIannotatedTree,_hdIattrName,_hdInamedType,_hdIoriginalTree) =
                  (hd_ _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 622 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 627 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 70, column 12)
              _lhsOattrs =
                  {-# LINE 70 "./TypeChecking/CreateTable.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 632 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 637 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 642 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree)))
sem_AttributeDefList_Nil  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: AttributeDefList
              _lhsOattrs :: ([(String, Type)])
              _lhsOoriginalTree :: AttributeDefList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 654 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 659 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 71, column 11)
              _lhsOattrs =
                  {-# LINE 71 "./TypeChecking/CreateTable.ag" #-}
                  []
                  {-# LINE 664 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 669 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 674 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree)))
-- Cascade -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cascade:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Restrict:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                  LocalIdentifierBindings ->
                  ( Cascade,Cascade)
data Inh_Cascade  = Inh_Cascade {env_Inh_Cascade :: Environment,lib_Inh_Cascade :: LocalIdentifierBindings}
data Syn_Cascade  = Syn_Cascade {annotatedTree_Syn_Cascade :: Cascade,originalTree_Syn_Cascade :: Cascade}
wrap_Cascade :: T_Cascade  ->
                Inh_Cascade  ->
                Syn_Cascade 
wrap_Cascade sem (Inh_Cascade _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_Cascade _lhsOannotatedTree _lhsOoriginalTree ))
sem_Cascade_Cascade  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Cascade
              _lhsOoriginalTree :: Cascade
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Cascade
                  {-# LINE 727 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 732 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Cascade
                  {-# LINE 737 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 742 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Cascade_Restrict  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Cascade
              _lhsOoriginalTree :: Cascade
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Restrict
                  {-# LINE 753 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 758 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Restrict
                  {-# LINE 763 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 768 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- CaseExpressionList ------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cons:
         child hd             : Expression 
         child tl             : CaseExpressionList 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
-}
type CaseExpressionList  = [(Expression)]
-- cata
sem_CaseExpressionList :: CaseExpressionList  ->
                          T_CaseExpressionList 
sem_CaseExpressionList list  =
    (Prelude.foldr sem_CaseExpressionList_Cons sem_CaseExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_CaseExpressionList  = ( CaseExpressionList,T_CaseExpressionList_1 )
type T_CaseExpressionList_1  = Environment ->
                               LocalIdentifierBindings ->
                               ( CaseExpressionList)
data Inh_CaseExpressionList  = Inh_CaseExpressionList {env_Inh_CaseExpressionList :: Environment,lib_Inh_CaseExpressionList :: LocalIdentifierBindings}
data Syn_CaseExpressionList  = Syn_CaseExpressionList {annotatedTree_Syn_CaseExpressionList :: CaseExpressionList,originalTree_Syn_CaseExpressionList :: CaseExpressionList}
wrap_CaseExpressionList :: T_CaseExpressionList  ->
                           Inh_CaseExpressionList  ->
                           Syn_CaseExpressionList 
wrap_CaseExpressionList sem (Inh_CaseExpressionList _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_CaseExpressionList _lhsOannotatedTree _lhsOoriginalTree ))
sem_CaseExpressionList_Cons hd_ tl_  =
    (let tl_1 :: T_CaseExpressionList_1 
         _tlIoriginalTree :: CaseExpressionList
         hd_1 :: T_Expression_1 
         _hdIoriginalTree :: Expression
         _lhsOoriginalTree :: CaseExpressionList
         ( _tlIoriginalTree,tl_1) =
             (tl_ )
         ( _hdIoriginalTree,hd_1) =
             (hd_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             (:) _hdIoriginalTree _tlIoriginalTree
             {-# LINE 831 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 836 "AstInternal.hs" #-}
         ( sem_CaseExpressionList_1) =
             (sem_CaseExpressionList_Cons_1 tl_1 hd_1 )
     in  ( _lhsOoriginalTree,sem_CaseExpressionList_1))
sem_CaseExpressionList_Cons_1 tl_1 hd_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: CaseExpressionList
              _hdIannotatedTree :: Expression
              _hdIliftedColumnName :: String
              _lhsOannotatedTree :: CaseExpressionList
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 855 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 860 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 865 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 870 "AstInternal.hs" #-}
              ( _tlIannotatedTree) =
                  (tl_1 _tlOenv _tlOlib )
              ( _hdIannotatedTree,_hdIliftedColumnName) =
                  (hd_1 _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 879 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 884 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CaseExpressionList_Nil  =
    (let _lhsOoriginalTree :: CaseExpressionList
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             []
             {-# LINE 892 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 897 "AstInternal.hs" #-}
         ( sem_CaseExpressionList_1) =
             (sem_CaseExpressionList_Nil_1 )
     in  ( _lhsOoriginalTree,sem_CaseExpressionList_1))
sem_CaseExpressionList_Nil_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: CaseExpressionList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 909 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 914 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- CaseExpressionListExpressionPair ----------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Tuple:
         child x1             : CaseExpressionList 
         child x2             : Expression 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
-}
type CaseExpressionListExpressionPair  = ( (CaseExpressionList),(Expression))
-- cata
sem_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair  ->
                                        T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair ( x1,x2)  =
    (sem_CaseExpressionListExpressionPair_Tuple (sem_CaseExpressionList x1 ) (sem_Expression x2 ) )
-- semantic domain
type T_CaseExpressionListExpressionPair  = ( CaseExpressionListExpressionPair,T_CaseExpressionListExpressionPair_1 )
type T_CaseExpressionListExpressionPair_1  = Environment ->
                                             LocalIdentifierBindings ->
                                             ( CaseExpressionListExpressionPair)
data Inh_CaseExpressionListExpressionPair  = Inh_CaseExpressionListExpressionPair {env_Inh_CaseExpressionListExpressionPair :: Environment,lib_Inh_CaseExpressionListExpressionPair :: LocalIdentifierBindings}
data Syn_CaseExpressionListExpressionPair  = Syn_CaseExpressionListExpressionPair {annotatedTree_Syn_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair,originalTree_Syn_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair}
wrap_CaseExpressionListExpressionPair :: T_CaseExpressionListExpressionPair  ->
                                         Inh_CaseExpressionListExpressionPair  ->
                                         Syn_CaseExpressionListExpressionPair 
wrap_CaseExpressionListExpressionPair sem (Inh_CaseExpressionListExpressionPair _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_CaseExpressionListExpressionPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_CaseExpressionListExpressionPair_Tuple x1_ x2_  =
    (let x2_1 :: T_Expression_1 
         _x2IoriginalTree :: Expression
         x1_1 :: T_CaseExpressionList_1 
         _x1IoriginalTree :: CaseExpressionList
         _lhsOoriginalTree :: CaseExpressionListExpressionPair
         ( _x2IoriginalTree,x2_1) =
             (x2_ )
         ( _x1IoriginalTree,x1_1) =
             (x1_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             (_x1IoriginalTree,_x2IoriginalTree)
             {-# LINE 972 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 977 "AstInternal.hs" #-}
         ( sem_CaseExpressionListExpressionPair_1) =
             (sem_CaseExpressionListExpressionPair_Tuple_1 x2_1 x1_1 )
     in  ( _lhsOoriginalTree,sem_CaseExpressionListExpressionPair_1))
sem_CaseExpressionListExpressionPair_Tuple_1 x2_1 x1_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _x2Olib :: LocalIdentifierBindings
              _x2Oenv :: Environment
              _x1Olib :: LocalIdentifierBindings
              _x1Oenv :: Environment
              _x2IannotatedTree :: Expression
              _x2IliftedColumnName :: String
              _x1IannotatedTree :: CaseExpressionList
              _lhsOannotatedTree :: CaseExpressionListExpressionPair
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 996 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1001 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1006 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1011 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2IliftedColumnName) =
                  (x2_1 _x2Oenv _x2Olib )
              ( _x1IannotatedTree) =
                  (x1_1 _x1Oenv _x1Olib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 1020 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1025 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- CaseExpressionListExpressionPairList ------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cons:
         child hd             : CaseExpressionListExpressionPair 
         child tl             : CaseExpressionListExpressionPairList 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
-}
type CaseExpressionListExpressionPairList  = [(CaseExpressionListExpressionPair)]
-- cata
sem_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList  ->
                                            T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList list  =
    (Prelude.foldr sem_CaseExpressionListExpressionPairList_Cons sem_CaseExpressionListExpressionPairList_Nil (Prelude.map sem_CaseExpressionListExpressionPair list) )
-- semantic domain
type T_CaseExpressionListExpressionPairList  = ( CaseExpressionListExpressionPairList,T_CaseExpressionListExpressionPairList_1 )
type T_CaseExpressionListExpressionPairList_1  = Environment ->
                                                 LocalIdentifierBindings ->
                                                 ( CaseExpressionListExpressionPairList)
data Inh_CaseExpressionListExpressionPairList  = Inh_CaseExpressionListExpressionPairList {env_Inh_CaseExpressionListExpressionPairList :: Environment,lib_Inh_CaseExpressionListExpressionPairList :: LocalIdentifierBindings}
data Syn_CaseExpressionListExpressionPairList  = Syn_CaseExpressionListExpressionPairList {annotatedTree_Syn_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList,originalTree_Syn_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList}
wrap_CaseExpressionListExpressionPairList :: T_CaseExpressionListExpressionPairList  ->
                                             Inh_CaseExpressionListExpressionPairList  ->
                                             Syn_CaseExpressionListExpressionPairList 
wrap_CaseExpressionListExpressionPairList sem (Inh_CaseExpressionListExpressionPairList _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_CaseExpressionListExpressionPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_CaseExpressionListExpressionPairList_Cons hd_ tl_  =
    (let tl_1 :: T_CaseExpressionListExpressionPairList_1 
         _tlIoriginalTree :: CaseExpressionListExpressionPairList
         hd_1 :: T_CaseExpressionListExpressionPair_1 
         _hdIoriginalTree :: CaseExpressionListExpressionPair
         _lhsOoriginalTree :: CaseExpressionListExpressionPairList
         ( _tlIoriginalTree,tl_1) =
             (tl_ )
         ( _hdIoriginalTree,hd_1) =
             (hd_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             (:) _hdIoriginalTree _tlIoriginalTree
             {-# LINE 1088 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 1093 "AstInternal.hs" #-}
         ( sem_CaseExpressionListExpressionPairList_1) =
             (sem_CaseExpressionListExpressionPairList_Cons_1 tl_1 hd_1 )
     in  ( _lhsOoriginalTree,sem_CaseExpressionListExpressionPairList_1))
sem_CaseExpressionListExpressionPairList_Cons_1 tl_1 hd_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: CaseExpressionListExpressionPairList
              _hdIannotatedTree :: CaseExpressionListExpressionPair
              _lhsOannotatedTree :: CaseExpressionListExpressionPairList
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1111 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1116 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1121 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1126 "AstInternal.hs" #-}
              ( _tlIannotatedTree) =
                  (tl_1 _tlOenv _tlOlib )
              ( _hdIannotatedTree) =
                  (hd_1 _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1135 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1140 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CaseExpressionListExpressionPairList_Nil  =
    (let _lhsOoriginalTree :: CaseExpressionListExpressionPairList
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             []
             {-# LINE 1148 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 1153 "AstInternal.hs" #-}
         ( sem_CaseExpressionListExpressionPairList_1) =
             (sem_CaseExpressionListExpressionPairList_Nil_1 )
     in  ( _lhsOoriginalTree,sem_CaseExpressionListExpressionPairList_1))
sem_CaseExpressionListExpressionPairList_Nil_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: CaseExpressionListExpressionPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1165 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1170 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- CombineType -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Except:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Intersect:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Union:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative UnionAll:
         visit 0:
            local originalTree : _
         visit 1:
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
type T_CombineType  = ( CombineType,T_CombineType_1 )
type T_CombineType_1  = Environment ->
                        LocalIdentifierBindings ->
                        ( CombineType)
data Inh_CombineType  = Inh_CombineType {env_Inh_CombineType :: Environment,lib_Inh_CombineType :: LocalIdentifierBindings}
data Syn_CombineType  = Syn_CombineType {annotatedTree_Syn_CombineType :: CombineType,originalTree_Syn_CombineType :: CombineType}
wrap_CombineType :: T_CombineType  ->
                    Inh_CombineType  ->
                    Syn_CombineType 
wrap_CombineType sem (Inh_CombineType _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_CombineType _lhsOannotatedTree _lhsOoriginalTree ))
sem_CombineType_Except  =
    (let _lhsOoriginalTree :: CombineType
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Except
             {-# LINE 1243 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 1248 "AstInternal.hs" #-}
         ( sem_CombineType_1) =
             (sem_CombineType_Except_1 )
     in  ( _lhsOoriginalTree,sem_CombineType_1))
sem_CombineType_Except_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: CombineType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Except
                  {-# LINE 1260 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1265 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CombineType_Intersect  =
    (let _lhsOoriginalTree :: CombineType
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Intersect
             {-# LINE 1273 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 1278 "AstInternal.hs" #-}
         ( sem_CombineType_1) =
             (sem_CombineType_Intersect_1 )
     in  ( _lhsOoriginalTree,sem_CombineType_1))
sem_CombineType_Intersect_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: CombineType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Intersect
                  {-# LINE 1290 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1295 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CombineType_Union  =
    (let _lhsOoriginalTree :: CombineType
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Union
             {-# LINE 1303 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 1308 "AstInternal.hs" #-}
         ( sem_CombineType_1) =
             (sem_CombineType_Union_1 )
     in  ( _lhsOoriginalTree,sem_CombineType_1))
sem_CombineType_Union_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: CombineType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Union
                  {-# LINE 1320 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1325 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_CombineType_UnionAll  =
    (let _lhsOoriginalTree :: CombineType
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             UnionAll
             {-# LINE 1333 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 1338 "AstInternal.hs" #-}
         ( sem_CombineType_1) =
             (sem_CombineType_UnionAll_1 )
     in  ( _lhsOoriginalTree,sem_CombineType_1))
sem_CombineType_UnionAll_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: CombineType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  UnionAll
                  {-# LINE 1350 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1355 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- Constraint --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative CheckConstraint:
         child ann            : {Annotation}
         child expression     : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative PrimaryKeyConstraint:
         child ann            : {Annotation}
         child stringList     : StringList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ReferenceConstraint:
         child ann            : {Annotation}
         child atts           : StringList 
         child table          : {String}
         child tableAtts      : StringList 
         child onUpdate       : Cascade 
         child onDelete       : Cascade 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative UniqueConstraint:
         child ann            : {Annotation}
         child stringList     : StringList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                     LocalIdentifierBindings ->
                     ( Constraint,Constraint)
data Inh_Constraint  = Inh_Constraint {env_Inh_Constraint :: Environment,lib_Inh_Constraint :: LocalIdentifierBindings}
data Syn_Constraint  = Syn_Constraint {annotatedTree_Syn_Constraint :: Constraint,originalTree_Syn_Constraint :: Constraint}
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_Constraint _lhsOannotatedTree _lhsOoriginalTree ))
sem_Constraint_CheckConstraint ann_ expression_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _expressionOlib :: LocalIdentifierBindings
              _expressionOenv :: Environment
              expression_1 :: T_Expression_1 
              _expressionIoriginalTree :: Expression
              _expressionIannotatedTree :: Expression
              _expressionIliftedColumnName :: String
              _lhsOannotatedTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- copy rule (down)
              _expressionOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1440 "AstInternal.hs" #-}
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1445 "AstInternal.hs" #-}
              ( _expressionIoriginalTree,expression_1) =
                  (expression_ )
              ( _expressionIannotatedTree,_expressionIliftedColumnName) =
                  (expression_1 _expressionOenv _expressionOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CheckConstraint ann_ _expressionIannotatedTree
                  {-# LINE 1454 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1459 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CheckConstraint ann_ _expressionIoriginalTree
                  {-# LINE 1464 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1469 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Constraint_PrimaryKeyConstraint ann_ stringList_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let stringList_1 :: T_StringList_1 
              _stringListIoriginalTree :: StringList
              _stringListOlib :: LocalIdentifierBindings
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _lhsOannotatedTree :: Constraint
              _lhsOoriginalTree :: Constraint
              ( _stringListIoriginalTree,stringList_1) =
                  (stringList_ )
              -- copy rule (down)
              _stringListOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1488 "AstInternal.hs" #-}
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1493 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_1 _stringListOenv _stringListOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  PrimaryKeyConstraint ann_ _stringListIannotatedTree
                  {-# LINE 1500 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1505 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  PrimaryKeyConstraint ann_ _stringListIoriginalTree
                  {-# LINE 1510 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1515 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Constraint_ReferenceConstraint ann_ atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _onDeleteOlib :: LocalIdentifierBindings
              _onDeleteOenv :: Environment
              _onDeleteIannotatedTree :: Cascade
              _onDeleteIoriginalTree :: Cascade
              _onUpdateOlib :: LocalIdentifierBindings
              _onUpdateOenv :: Environment
              _onUpdateIannotatedTree :: Cascade
              _onUpdateIoriginalTree :: Cascade
              tableAtts_1 :: T_StringList_1 
              _tableAttsIoriginalTree :: StringList
              _tableAttsOlib :: LocalIdentifierBindings
              _tableAttsOenv :: Environment
              _tableAttsIannotatedTree :: StringList
              _tableAttsIstrings :: ([String])
              atts_1 :: T_StringList_1 
              _attsIoriginalTree :: StringList
              _attsOlib :: LocalIdentifierBindings
              _attsOenv :: Environment
              _attsIannotatedTree :: StringList
              _attsIstrings :: ([String])
              _lhsOannotatedTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- copy rule (down)
              _onDeleteOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1546 "AstInternal.hs" #-}
              -- copy rule (down)
              _onDeleteOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1551 "AstInternal.hs" #-}
              ( _onDeleteIannotatedTree,_onDeleteIoriginalTree) =
                  (onDelete_ _onDeleteOenv _onDeleteOlib )
              -- copy rule (down)
              _onUpdateOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1558 "AstInternal.hs" #-}
              -- copy rule (down)
              _onUpdateOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1563 "AstInternal.hs" #-}
              ( _onUpdateIannotatedTree,_onUpdateIoriginalTree) =
                  (onUpdate_ _onUpdateOenv _onUpdateOlib )
              ( _tableAttsIoriginalTree,tableAtts_1) =
                  (tableAtts_ )
              -- copy rule (down)
              _tableAttsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1572 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableAttsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1577 "AstInternal.hs" #-}
              ( _tableAttsIannotatedTree,_tableAttsIstrings) =
                  (tableAtts_1 _tableAttsOenv _tableAttsOlib )
              ( _attsIoriginalTree,atts_1) =
                  (atts_ )
              -- copy rule (down)
              _attsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1586 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1591 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIstrings) =
                  (atts_1 _attsOenv _attsOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ReferenceConstraint ann_ _attsIannotatedTree table_ _tableAttsIannotatedTree _onUpdateIannotatedTree _onDeleteIannotatedTree
                  {-# LINE 1598 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1603 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ReferenceConstraint ann_ _attsIoriginalTree table_ _tableAttsIoriginalTree _onUpdateIoriginalTree _onDeleteIoriginalTree
                  {-# LINE 1608 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1613 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Constraint_UniqueConstraint ann_ stringList_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let stringList_1 :: T_StringList_1 
              _stringListIoriginalTree :: StringList
              _stringListOlib :: LocalIdentifierBindings
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _lhsOannotatedTree :: Constraint
              _lhsOoriginalTree :: Constraint
              ( _stringListIoriginalTree,stringList_1) =
                  (stringList_ )
              -- copy rule (down)
              _stringListOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1632 "AstInternal.hs" #-}
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1637 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_1 _stringListOenv _stringListOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  UniqueConstraint ann_ _stringListIannotatedTree
                  {-# LINE 1644 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1649 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  UniqueConstraint ann_ _stringListIoriginalTree
                  {-# LINE 1654 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1659 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ConstraintList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : Constraint 
         child tl             : ConstraintList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ConstraintList  = [(Constraint)]
-- cata
sem_ConstraintList :: ConstraintList  ->
                      T_ConstraintList 
sem_ConstraintList list  =
    (Prelude.foldr sem_ConstraintList_Cons sem_ConstraintList_Nil (Prelude.map sem_Constraint list) )
-- semantic domain
type T_ConstraintList  = Environment ->
                         LocalIdentifierBindings ->
                         ( ConstraintList,ConstraintList)
data Inh_ConstraintList  = Inh_ConstraintList {env_Inh_ConstraintList :: Environment,lib_Inh_ConstraintList :: LocalIdentifierBindings}
data Syn_ConstraintList  = Syn_ConstraintList {annotatedTree_Syn_ConstraintList :: ConstraintList,originalTree_Syn_ConstraintList :: ConstraintList}
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_ConstraintList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: ConstraintList
              _tlIoriginalTree :: ConstraintList
              _hdIannotatedTree :: Constraint
              _hdIoriginalTree :: Constraint
              _lhsOannotatedTree :: ConstraintList
              _lhsOoriginalTree :: ConstraintList
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1718 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1723 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1728 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 1733 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOenv _tlOlib )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1742 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1747 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 1752 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1757 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ConstraintList_Nil  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ConstraintList
              _lhsOoriginalTree :: ConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1768 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1773 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1778 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1783 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- CopySource --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative CopyFilename:
         child string         : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Stdin:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                     LocalIdentifierBindings ->
                     ( CopySource,CopySource)
data Inh_CopySource  = Inh_CopySource {env_Inh_CopySource :: Environment,lib_Inh_CopySource :: LocalIdentifierBindings}
data Syn_CopySource  = Syn_CopySource {annotatedTree_Syn_CopySource :: CopySource,originalTree_Syn_CopySource :: CopySource}
wrap_CopySource :: T_CopySource  ->
                   Inh_CopySource  ->
                   Syn_CopySource 
wrap_CopySource sem (Inh_CopySource _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_CopySource _lhsOannotatedTree _lhsOoriginalTree ))
sem_CopySource_CopyFilename string_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: CopySource
              _lhsOoriginalTree :: CopySource
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CopyFilename string_
                  {-# LINE 1837 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1842 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CopyFilename string_
                  {-# LINE 1847 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1852 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_CopySource_Stdin  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: CopySource
              _lhsOoriginalTree :: CopySource
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Stdin
                  {-# LINE 1863 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1868 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Stdin
                  {-# LINE 1873 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1878 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- Direction ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Asc:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Desc:
         visit 0:
            local originalTree : _
         visit 1:
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
type T_Direction  = ( Direction,T_Direction_1 )
type T_Direction_1  = Environment ->
                      LocalIdentifierBindings ->
                      ( Direction)
data Inh_Direction  = Inh_Direction {env_Inh_Direction :: Environment,lib_Inh_Direction :: LocalIdentifierBindings}
data Syn_Direction  = Syn_Direction {annotatedTree_Syn_Direction :: Direction,originalTree_Syn_Direction :: Direction}
wrap_Direction :: T_Direction  ->
                  Inh_Direction  ->
                  Syn_Direction 
wrap_Direction sem (Inh_Direction _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_Direction _lhsOannotatedTree _lhsOoriginalTree ))
sem_Direction_Asc  =
    (let _lhsOoriginalTree :: Direction
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Asc
             {-# LINE 1935 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 1940 "AstInternal.hs" #-}
         ( sem_Direction_1) =
             (sem_Direction_Asc_1 )
     in  ( _lhsOoriginalTree,sem_Direction_1))
sem_Direction_Asc_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Direction
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Asc
                  {-# LINE 1952 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1957 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Direction_Desc  =
    (let _lhsOoriginalTree :: Direction
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Desc
             {-# LINE 1965 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 1970 "AstInternal.hs" #-}
         ( sem_Direction_1) =
             (sem_Direction_Desc_1 )
     in  ( _lhsOoriginalTree,sem_Direction_1))
sem_Direction_Desc_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Direction
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Desc
                  {-# LINE 1982 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1987 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- Distinct ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Distinct:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Dupes:
         visit 0:
            local originalTree : _
         visit 1:
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
type T_Distinct  = ( Distinct,T_Distinct_1 )
type T_Distinct_1  = Environment ->
                     LocalIdentifierBindings ->
                     ( Distinct)
data Inh_Distinct  = Inh_Distinct {env_Inh_Distinct :: Environment,lib_Inh_Distinct :: LocalIdentifierBindings}
data Syn_Distinct  = Syn_Distinct {annotatedTree_Syn_Distinct :: Distinct,originalTree_Syn_Distinct :: Distinct}
wrap_Distinct :: T_Distinct  ->
                 Inh_Distinct  ->
                 Syn_Distinct 
wrap_Distinct sem (Inh_Distinct _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_Distinct _lhsOannotatedTree _lhsOoriginalTree ))
sem_Distinct_Distinct  =
    (let _lhsOoriginalTree :: Distinct
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Distinct
             {-# LINE 2044 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 2049 "AstInternal.hs" #-}
         ( sem_Distinct_1) =
             (sem_Distinct_Distinct_1 )
     in  ( _lhsOoriginalTree,sem_Distinct_1))
sem_Distinct_Distinct_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Distinct
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Distinct
                  {-# LINE 2061 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2066 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Distinct_Dupes  =
    (let _lhsOoriginalTree :: Distinct
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Dupes
             {-# LINE 2074 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 2079 "AstInternal.hs" #-}
         ( sem_Distinct_1) =
             (sem_Distinct_Dupes_1 )
     in  ( _lhsOoriginalTree,sem_Distinct_1))
sem_Distinct_Dupes_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Distinct
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Dupes
                  {-# LINE 2091 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2096 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- DropType ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Domain:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Table:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Type:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative View:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                   LocalIdentifierBindings ->
                   ( DropType,DropType)
data Inh_DropType  = Inh_DropType {env_Inh_DropType :: Environment,lib_Inh_DropType :: LocalIdentifierBindings}
data Syn_DropType  = Syn_DropType {annotatedTree_Syn_DropType :: DropType,originalTree_Syn_DropType :: DropType}
wrap_DropType :: T_DropType  ->
                 Inh_DropType  ->
                 Syn_DropType 
wrap_DropType sem (Inh_DropType _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_DropType _lhsOannotatedTree _lhsOoriginalTree ))
sem_DropType_Domain  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: DropType
              _lhsOoriginalTree :: DropType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Domain
                  {-# LINE 2163 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2168 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Domain
                  {-# LINE 2173 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2178 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_DropType_Table  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: DropType
              _lhsOoriginalTree :: DropType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Table
                  {-# LINE 2189 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2194 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Table
                  {-# LINE 2199 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2204 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_DropType_Type  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: DropType
              _lhsOoriginalTree :: DropType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Type
                  {-# LINE 2215 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2220 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Type
                  {-# LINE 2225 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2230 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_DropType_View  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: DropType
              _lhsOoriginalTree :: DropType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  View
                  {-# LINE 2241 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2246 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  View
                  {-# LINE 2251 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2256 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- Expression --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         liftedColumnName     : String
   alternatives:
      alternative BooleanLit:
         child ann            : {Annotation}
         child b              : {Bool}
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative Case:
         child ann            : {Annotation}
         child cases          : CaseExpressionListExpressionPairList 
         child els            : MaybeExpression 
         visit 0:
            local originalTree : _
         visit 1:
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
            local originalTree : _
         visit 1:
            local backTree    : _
            local thenTypes   : _
            local whenTypes   : _
            local tpe         : _
      alternative Cast:
         child ann            : {Annotation}
         child expr           : Expression 
         child tn             : TypeName 
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative Exists:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative FloatLit:
         child ann            : {Annotation}
         child d              : {Double}
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative FunCall:
         child ann            : {Annotation}
         child funName        : {String}
         child args           : ExpressionList 
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative Identifier:
         child ann            : {Annotation}
         child i              : {String}
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative InPredicate:
         child ann            : {Annotation}
         child expr           : Expression 
         child i              : {Bool}
         child list           : InList 
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative IntegerLit:
         child ann            : {Annotation}
         child i              : {Integer}
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative LiftOperator:
         child ann            : {Annotation}
         child oper           : {String}
         child flav           : LiftFlavour 
         child args           : ExpressionList 
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative NullLit:
         child ann            : {Annotation}
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative PositionalArg:
         child ann            : {Annotation}
         child p              : {Integer}
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative ScalarSubQuery:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative StringLit:
         child ann            : {Annotation}
         child quote          : {String}
         child value          : {String}
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative WindowFn:
         child ann            : {Annotation}
         child fn             : Expression 
         child partitionBy    : ExpressionList 
         child orderBy        : ExpressionList 
         child dir            : Direction 
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
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
                 | LiftOperator (Annotation) (String) (LiftFlavour) (ExpressionList) 
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
sem_Expression (LiftOperator _ann _oper _flav _args )  =
    (sem_Expression_LiftOperator _ann _oper (sem_LiftFlavour _flav ) (sem_ExpressionList _args ) )
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
type T_Expression  = ( Expression,T_Expression_1 )
type T_Expression_1  = Environment ->
                       LocalIdentifierBindings ->
                       ( Expression,String)
data Inh_Expression  = Inh_Expression {env_Inh_Expression :: Environment,lib_Inh_Expression :: LocalIdentifierBindings}
data Syn_Expression  = Syn_Expression {annotatedTree_Syn_Expression :: Expression,liftedColumnName_Syn_Expression :: String,originalTree_Syn_Expression :: Expression}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression sem (Inh_Expression _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOliftedColumnName) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_Expression _lhsOannotatedTree _lhsOliftedColumnName _lhsOoriginalTree ))
sem_Expression_BooleanLit ann_ b_  =
    (let _lhsOoriginalTree :: Expression
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             BooleanLit ann_ b_
             {-# LINE 2490 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 2495 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_BooleanLit_1 b_ ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_BooleanLit_1 b_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking/Expressions.ag"(line 41, column 9)
              _backTree =
                  {-# LINE 41 "./TypeChecking/Expressions.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 2508 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 29, column 19)
              _tpe =
                  {-# LINE 29 "./TypeChecking/Expressions.ag" #-}
                  Right typeBool
                  {-# LINE 2513 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2521 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 177, column 7)
              _lhsOliftedColumnName =
                  {-# LINE 177 "./TypeChecking/SelectLists.ag" #-}
                  ""
                  {-# LINE 2526 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Case ann_ cases_ els_  =
    (let els_1 :: T_MaybeExpression_1 
         _elsIoriginalTree :: MaybeExpression
         cases_1 :: T_CaseExpressionListExpressionPairList_1 
         _casesIoriginalTree :: CaseExpressionListExpressionPairList
         _lhsOoriginalTree :: Expression
         ( _elsIoriginalTree,els_1) =
             (els_ )
         ( _casesIoriginalTree,cases_1) =
             (cases_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Case ann_ _casesIoriginalTree _elsIoriginalTree
             {-# LINE 2542 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 2547 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_Case_1 els_1 cases_1 ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_Case_1 els_1 cases_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _elsOlib :: LocalIdentifierBindings
              _elsOenv :: Environment
              _casesOlib :: LocalIdentifierBindings
              _casesOenv :: Environment
              _elsIannotatedTree :: MaybeExpression
              _casesIannotatedTree :: CaseExpressionListExpressionPairList
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2566 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2571 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2576 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2581 "AstInternal.hs" #-}
              ( _elsIannotatedTree) =
                  (els_1 _elsOenv _elsOlib )
              ( _casesIannotatedTree) =
                  (cases_1 _casesOenv _casesOlib )
              -- "./TypeChecking/Expressions.ag"(line 203, column 9)
              _backTree =
                  {-# LINE 203 "./TypeChecking/Expressions.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 2590 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 191, column 9)
              _thenTypes =
                  {-# LINE 191 "./TypeChecking/Expressions.ag" #-}
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
                  {-# LINE 2597 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 189, column 9)
              _whenTypes =
                  {-# LINE 189 "./TypeChecking/Expressions.ag" #-}
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
                  {-# LINE 2603 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 197, column 9)
              _tpe =
                  {-# LINE 197 "./TypeChecking/Expressions.ag" #-}
                  dependsOnRTpe _whenTypes     $ do
                  errorWhen (any (/= typeBool) _whenTypes    ) $
                            [WrongTypes typeBool _whenTypes    ]
                  dependsOnRTpe _thenTypes     $
                    resolveResultSetType _lhsIenv _thenTypes
                  {-# LINE 2612 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2620 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 177, column 7)
              _lhsOliftedColumnName =
                  {-# LINE 177 "./TypeChecking/SelectLists.ag" #-}
                  ""
                  {-# LINE 2625 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_CaseSimple ann_ value_ cases_ els_  =
    (let els_1 :: T_MaybeExpression_1 
         _elsIoriginalTree :: MaybeExpression
         cases_1 :: T_CaseExpressionListExpressionPairList_1 
         _casesIoriginalTree :: CaseExpressionListExpressionPairList
         value_1 :: T_Expression_1 
         _valueIoriginalTree :: Expression
         _lhsOoriginalTree :: Expression
         ( _elsIoriginalTree,els_1) =
             (els_ )
         ( _casesIoriginalTree,cases_1) =
             (cases_ )
         ( _valueIoriginalTree,value_1) =
             (value_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             CaseSimple ann_ _valueIoriginalTree _casesIoriginalTree _elsIoriginalTree
             {-# LINE 2645 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 2650 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_CaseSimple_1 els_1 cases_1 value_1 ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_CaseSimple_1 els_1 cases_1 value_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _elsOlib :: LocalIdentifierBindings
              _elsOenv :: Environment
              _casesOlib :: LocalIdentifierBindings
              _casesOenv :: Environment
              _valueOlib :: LocalIdentifierBindings
              _valueOenv :: Environment
              _elsIannotatedTree :: MaybeExpression
              _casesIannotatedTree :: CaseExpressionListExpressionPairList
              _valueIannotatedTree :: Expression
              _valueIliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2673 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2678 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2683 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2688 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2693 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2698 "AstInternal.hs" #-}
              ( _elsIannotatedTree) =
                  (els_1 _elsOenv _elsOlib )
              ( _casesIannotatedTree) =
                  (cases_1 _casesOenv _casesOlib )
              ( _valueIannotatedTree,_valueIliftedColumnName) =
                  (value_1 _valueOenv _valueOlib )
              -- "./TypeChecking/Expressions.ag"(line 215, column 9)
              _backTree =
                  {-# LINE 215 "./TypeChecking/Expressions.ag" #-}
                  CaseSimple ann_
                             _valueIannotatedTree
                             _casesIannotatedTree
                             _elsIannotatedTree
                  {-# LINE 2712 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 191, column 9)
              _thenTypes =
                  {-# LINE 191 "./TypeChecking/Expressions.ag" #-}
                  map getTypeAnnotation $
                      (map snd $ _casesIannotatedTree) ++
                        maybeToList _elsIannotatedTree
                  {-# LINE 2719 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 189, column 9)
              _whenTypes =
                  {-# LINE 189 "./TypeChecking/Expressions.ag" #-}
                  map getTypeAnnotation $ concatMap fst $
                  _casesIannotatedTree
                  {-# LINE 2725 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 208, column 9)
              _tpe =
                  {-# LINE 208 "./TypeChecking/Expressions.ag" #-}
                  dependsOnRTpe _whenTypes     $ do
                  let valueType = getTypeAnnotation _valueIannotatedTree
                  checkWhenTypes <-
                      resolveResultSetType _lhsIenv (valueType : _whenTypes    )
                  dependsOnRTpe _thenTypes     $
                    resolveResultSetType _lhsIenv _thenTypes
                  {-# LINE 2735 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2743 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOliftedColumnName =
                  {-# LINE 161 "./TypeChecking/SelectLists.ag" #-}
                  _valueIliftedColumnName
                  {-# LINE 2748 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Cast ann_ expr_ tn_  =
    (let tn_1 :: T_TypeName_1 
         _tnIoriginalTree :: TypeName
         expr_1 :: T_Expression_1 
         _exprIoriginalTree :: Expression
         _lhsOoriginalTree :: Expression
         ( _tnIoriginalTree,tn_1) =
             (tn_ )
         ( _exprIoriginalTree,expr_1) =
             (expr_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Cast ann_ _exprIoriginalTree _tnIoriginalTree
             {-# LINE 2764 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 2769 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_Cast_1 tn_1 expr_1 ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_Cast_1 tn_1 expr_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tnOenv :: Environment
              _exprOlib :: LocalIdentifierBindings
              _exprOenv :: Environment
              _tnOlib :: LocalIdentifierBindings
              _tnIannotatedTree :: TypeName
              _tnInamedType :: Type
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _tnOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2790 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2795 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2800 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2805 "AstInternal.hs" #-}
              ( _tnIannotatedTree,_tnInamedType) =
                  (tn_1 _tnOenv _tnOlib )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_1 _exprOenv _exprOlib )
              -- "./TypeChecking/Expressions.ag"(line 55, column 12)
              _backTree =
                  {-# LINE 55 "./TypeChecking/Expressions.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 2814 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 54, column 12)
              _tpe =
                  {-# LINE 54 "./TypeChecking/Expressions.ag" #-}
                  Right $ _tnInamedType
                  {-# LINE 2819 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2827 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 169, column 10)
              _lhsOliftedColumnName =
                  {-# LINE 169 "./TypeChecking/SelectLists.ag" #-}
                  case _tnIannotatedTree of
                    SimpleTypeName _ tn -> tn
                    _ -> ""
                  {-# LINE 2834 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Exists ann_ sel_  =
    (let sel_1 :: T_SelectExpression_1 
         _selIoriginalTree :: SelectExpression
         _lhsOoriginalTree :: Expression
         ( _selIoriginalTree,sel_1) =
             (sel_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Exists ann_ _selIoriginalTree
             {-# LINE 2846 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 2851 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_Exists_1 sel_1 ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_Exists_1 sel_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _selOlib :: LocalIdentifierBindings
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2868 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2873 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIlibUpdates) =
                  (sel_1 _selOenv _selOlib )
              -- "./TypeChecking/Expressions.ag"(line 242, column 9)
              _backTree =
                  {-# LINE 242 "./TypeChecking/Expressions.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 2880 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 241, column 9)
              _tpe =
                  {-# LINE 241 "./TypeChecking/Expressions.ag" #-}
                  Right typeBool
                  {-# LINE 2885 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2893 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 177, column 7)
              _lhsOliftedColumnName =
                  {-# LINE 177 "./TypeChecking/SelectLists.ag" #-}
                  ""
                  {-# LINE 2898 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_FloatLit ann_ d_  =
    (let _lhsOoriginalTree :: Expression
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             FloatLit ann_ d_
             {-# LINE 2906 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 2911 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_FloatLit_1 d_ ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_FloatLit_1 d_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking/Expressions.ag"(line 39, column 9)
              _backTree =
                  {-# LINE 39 "./TypeChecking/Expressions.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 2924 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 28, column 17)
              _tpe =
                  {-# LINE 28 "./TypeChecking/Expressions.ag" #-}
                  Right typeNumeric
                  {-# LINE 2929 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 2937 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 177, column 7)
              _lhsOliftedColumnName =
                  {-# LINE 177 "./TypeChecking/SelectLists.ag" #-}
                  ""
                  {-# LINE 2942 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_FunCall ann_ funName_ args_  =
    (let args_1 :: T_ExpressionList_1 
         _argsIoriginalTree :: ExpressionList
         _lhsOoriginalTree :: Expression
         ( _argsIoriginalTree,args_1) =
             (args_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             FunCall ann_ funName_ _argsIoriginalTree
             {-# LINE 2954 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 2959 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_FunCall_1 args_1 funName_ ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_FunCall_1 args_1 funName_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _argsOlib :: LocalIdentifierBindings
              _argsOenv :: Environment
              _argsIannotatedTree :: ExpressionList
              _argsItypeList :: ([Type])
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2976 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 2981 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsItypeList) =
                  (args_1 _argsOenv _argsOlib )
              -- "./TypeChecking/Expressions.ag"(line 67, column 9)
              _backTree =
                  {-# LINE 67 "./TypeChecking/Expressions.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 2988 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tpe =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  dependsOnRTpe _argsItypeList $
                    typeCheckFunCall
                      _lhsIenv
                      funName_
                      _argsItypeList
                  {-# LINE 2997 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 3005 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 165, column 13)
              _lhsOliftedColumnName =
                  {-# LINE 165 "./TypeChecking/SelectLists.ag" #-}
                  if isOperatorName funName_
                     then ""
                     else funName_
                  {-# LINE 3012 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_Identifier ann_ i_  =
    (let _lhsOoriginalTree :: Expression
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Identifier ann_ i_
             {-# LINE 3020 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3025 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_Identifier_1 i_ ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_Identifier_1 i_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking/Expressions.ag"(line 229, column 9)
              _backTree =
                  {-# LINE 229 "./TypeChecking/Expressions.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 3038 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 228, column 9)
              _tpe =
                  {-# LINE 228 "./TypeChecking/Expressions.ag" #-}
                  libLookupID _lhsIlib i_
                  {-# LINE 3043 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 3051 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 164, column 16)
              _lhsOliftedColumnName =
                  {-# LINE 164 "./TypeChecking/SelectLists.ag" #-}
                  i_
                  {-# LINE 3056 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_InPredicate ann_ expr_ i_ list_  =
    (let list_1 :: T_InList_1 
         _listIoriginalTree :: InList
         expr_1 :: T_Expression_1 
         _exprIoriginalTree :: Expression
         _lhsOoriginalTree :: Expression
         ( _listIoriginalTree,list_1) =
             (list_ )
         ( _exprIoriginalTree,expr_1) =
             (expr_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             InPredicate ann_ _exprIoriginalTree i_ _listIoriginalTree
             {-# LINE 3072 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3077 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_InPredicate_1 list_1 expr_1 i_ ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_InPredicate_1 list_1 expr_1 i_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _listOlib :: LocalIdentifierBindings
              _listOenv :: Environment
              _exprOlib :: LocalIdentifierBindings
              _exprOenv :: Environment
              _listIannotatedTree :: InList
              _listIlistType :: (Either [TypeError] Type)
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _listOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3098 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3103 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3108 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3113 "AstInternal.hs" #-}
              ( _listIannotatedTree,_listIlistType) =
                  (list_1 _listOenv _listOlib )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_1 _exprOenv _exprOlib )
              -- "./TypeChecking/Expressions.ag"(line 277, column 9)
              _backTree =
                  {-# LINE 277 "./TypeChecking/Expressions.ag" #-}
                  InPredicate ann_
                              _exprIannotatedTree
                              i_
                              _listIannotatedTree
                  {-# LINE 3125 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 271, column 9)
              _tpe =
                  {-# LINE 271 "./TypeChecking/Expressions.ag" #-}
                  do
                  lt <- _listIlistType
                  ty <- resolveResultSetType
                            _lhsIenv
                            [getTypeAnnotation _exprIannotatedTree, lt]
                  return typeBool
                  {-# LINE 3135 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 3143 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOliftedColumnName =
                  {-# LINE 161 "./TypeChecking/SelectLists.ag" #-}
                  _exprIliftedColumnName
                  {-# LINE 3148 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_IntegerLit ann_ i_  =
    (let _lhsOoriginalTree :: Expression
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             IntegerLit ann_ i_
             {-# LINE 3156 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3161 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_IntegerLit_1 i_ ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_IntegerLit_1 i_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking/Expressions.ag"(line 35, column 9)
              _backTree =
                  {-# LINE 35 "./TypeChecking/Expressions.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 3174 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 26, column 19)
              _tpe =
                  {-# LINE 26 "./TypeChecking/Expressions.ag" #-}
                  Right typeInt
                  {-# LINE 3179 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 3187 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 177, column 7)
              _lhsOliftedColumnName =
                  {-# LINE 177 "./TypeChecking/SelectLists.ag" #-}
                  ""
                  {-# LINE 3192 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_LiftOperator ann_ oper_ flav_ args_  =
    (let args_1 :: T_ExpressionList_1 
         _argsIoriginalTree :: ExpressionList
         flav_1 :: T_LiftFlavour_1 
         _flavIoriginalTree :: LiftFlavour
         _lhsOoriginalTree :: Expression
         ( _argsIoriginalTree,args_1) =
             (args_ )
         ( _flavIoriginalTree,flav_1) =
             (flav_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             LiftOperator ann_ oper_ _flavIoriginalTree _argsIoriginalTree
             {-# LINE 3208 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3213 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_LiftOperator_1 args_1 flav_1 oper_ ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_LiftOperator_1 args_1 flav_1 oper_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _argsOlib :: LocalIdentifierBindings
              _argsOenv :: Environment
              _argsIannotatedTree :: ExpressionList
              _argsItypeList :: ([Type])
              _flavOlib :: LocalIdentifierBindings
              _flavOenv :: Environment
              _flavIannotatedTree :: LiftFlavour
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3233 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3238 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsItypeList) =
                  (args_1 _argsOenv _argsOlib )
              -- copy rule (down)
              _flavOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3245 "AstInternal.hs" #-}
              -- copy rule (down)
              _flavOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3250 "AstInternal.hs" #-}
              ( _flavIannotatedTree) =
                  (flav_1 _flavOenv _flavOlib )
              -- "./TypeChecking/Expressions.ag"(line 172, column 9)
              _backTree =
                  {-# LINE 172 "./TypeChecking/Expressions.ag" #-}
                  LiftOperator ann_ oper_ _flavIannotatedTree _argsIannotatedTree
                  {-# LINE 3257 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 154, column 9)
              _tpe =
                  {-# LINE 154 "./TypeChecking/Expressions.ag" #-}
                  dependsOnRTpe _argsItypeList $ do
                  let args = _argsIannotatedTree
                  errorWhen (length args /= 2)
                            [AnyAllError $ "must have two args, got " ++ show args]
                  let [a,b] = args
                      aType = getTypeAnnotation a
                      bType = getTypeAnnotation b
                  dependsOnRTpe [aType,bType] $ do
                  errorWhen (not $ isArrayType bType)
                            [AnyAllError $ "second arg must be array, got " ++ show args]
                  elemType <- unwrapArray $ bType
                  resType <- typeCheckFunCall
                                     _lhsIenv
                                     oper_
                                     [aType,elemType]
                  errorWhen (resType /= typeBool)
                            [AnyAllError $ "operator must have bool return, got " ++ show resType]
                  return resType
                  {-# LINE 3279 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 3287 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 177, column 7)
              _lhsOliftedColumnName =
                  {-# LINE 177 "./TypeChecking/SelectLists.ag" #-}
                  ""
                  {-# LINE 3292 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_NullLit ann_  =
    (let _lhsOoriginalTree :: Expression
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             NullLit ann_
             {-# LINE 3300 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3305 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_NullLit_1 ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_NullLit_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking/Expressions.ag"(line 43, column 9)
              _backTree =
                  {-# LINE 43 "./TypeChecking/Expressions.ag" #-}
                  NullLit ann_
                  {-# LINE 3318 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 31, column 16)
              _tpe =
                  {-# LINE 31 "./TypeChecking/Expressions.ag" #-}
                  Right UnknownType
                  {-# LINE 3323 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 3331 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 177, column 7)
              _lhsOliftedColumnName =
                  {-# LINE 177 "./TypeChecking/SelectLists.ag" #-}
                  ""
                  {-# LINE 3336 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_PositionalArg ann_ p_  =
    (let _lhsOoriginalTree :: Expression
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             PositionalArg ann_ p_
             {-# LINE 3344 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3349 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_PositionalArg_1 p_ ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_PositionalArg_1 p_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking/Expressions.ag"(line 235, column 9)
              _backTree =
                  {-# LINE 235 "./TypeChecking/Expressions.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 3362 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 234, column 9)
              _tpe =
                  {-# LINE 234 "./TypeChecking/Expressions.ag" #-}
                  libLookupID _lhsIlib ('$':show p_)
                  {-# LINE 3367 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 3375 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 177, column 7)
              _lhsOliftedColumnName =
                  {-# LINE 177 "./TypeChecking/SelectLists.ag" #-}
                  ""
                  {-# LINE 3380 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_ScalarSubQuery ann_ sel_  =
    (let sel_1 :: T_SelectExpression_1 
         _selIoriginalTree :: SelectExpression
         _lhsOoriginalTree :: Expression
         ( _selIoriginalTree,sel_1) =
             (sel_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             ScalarSubQuery ann_ _selIoriginalTree
             {-# LINE 3392 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3397 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_ScalarSubQuery_1 sel_1 ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_ScalarSubQuery_1 sel_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _selOlib :: LocalIdentifierBindings
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3414 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3419 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIlibUpdates) =
                  (sel_1 _selOenv _selOlib )
              -- "./TypeChecking/Expressions.ag"(line 263, column 9)
              _backTree =
                  {-# LINE 263 "./TypeChecking/Expressions.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 3426 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 253, column 9)
              _tpe =
                  {-# LINE 253 "./TypeChecking/Expressions.ag" #-}
                  do
                  let selType = getTypeAnnotation _selIannotatedTree
                  dependsOnRTpe [selType] $ do
                  f <- map snd <$> unwrapSetOfComposite selType
                  case length f of
                    0 -> Left [InternalError "no columns in scalar subquery?"]
                    1 -> Right $ head f
                    _ -> Right $ AnonymousRecordType f
                  {-# LINE 3438 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 3446 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 177, column 7)
              _lhsOliftedColumnName =
                  {-# LINE 177 "./TypeChecking/SelectLists.ag" #-}
                  ""
                  {-# LINE 3451 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_StringLit ann_ quote_ value_  =
    (let _lhsOoriginalTree :: Expression
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             StringLit ann_ quote_ value_
             {-# LINE 3459 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3464 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_StringLit_1 value_ quote_ ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_StringLit_1 value_ quote_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Expression
              _lhsOliftedColumnName :: String
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _backTree =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  StringLit ann_ quote_ value_
                  {-# LINE 3477 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 18)
              _tpe =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Right UnknownType
                  {-# LINE 3482 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 3490 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 177, column 7)
              _lhsOliftedColumnName =
                  {-# LINE 177 "./TypeChecking/SelectLists.ag" #-}
                  ""
                  {-# LINE 3495 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
sem_Expression_WindowFn ann_ fn_ partitionBy_ orderBy_ dir_  =
    (let dir_1 :: T_Direction_1 
         _dirIoriginalTree :: Direction
         orderBy_1 :: T_ExpressionList_1 
         _orderByIoriginalTree :: ExpressionList
         partitionBy_1 :: T_ExpressionList_1 
         _partitionByIoriginalTree :: ExpressionList
         fn_1 :: T_Expression_1 
         _fnIoriginalTree :: Expression
         _lhsOoriginalTree :: Expression
         ( _dirIoriginalTree,dir_1) =
             (dir_ )
         ( _orderByIoriginalTree,orderBy_1) =
             (orderBy_ )
         ( _partitionByIoriginalTree,partitionBy_1) =
             (partitionBy_ )
         ( _fnIoriginalTree,fn_1) =
             (fn_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             WindowFn ann_ _fnIoriginalTree _partitionByIoriginalTree _orderByIoriginalTree _dirIoriginalTree
             {-# LINE 3519 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3524 "AstInternal.hs" #-}
         ( sem_Expression_1) =
             (sem_Expression_WindowFn_1 dir_1 orderBy_1 partitionBy_1 fn_1 ann_ )
     in  ( _lhsOoriginalTree,sem_Expression_1))
sem_Expression_WindowFn_1 dir_1 orderBy_1 partitionBy_1 fn_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _orderByOlib :: LocalIdentifierBindings
              _orderByOenv :: Environment
              _partitionByOlib :: LocalIdentifierBindings
              _partitionByOenv :: Environment
              _fnOlib :: LocalIdentifierBindings
              _fnOenv :: Environment
              _dirOlib :: LocalIdentifierBindings
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
              _orderByOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3552 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3557 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3562 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3567 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3572 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3577 "AstInternal.hs" #-}
              -- copy rule (down)
              _dirOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3582 "AstInternal.hs" #-}
              -- copy rule (down)
              _dirOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3587 "AstInternal.hs" #-}
              ( _dirIannotatedTree) =
                  (dir_1 _dirOenv _dirOlib )
              ( _orderByIannotatedTree,_orderByItypeList) =
                  (orderBy_1 _orderByOenv _orderByOlib )
              ( _partitionByIannotatedTree,_partitionByItypeList) =
                  (partitionBy_1 _partitionByOenv _partitionByOlib )
              ( _fnIannotatedTree,_fnIliftedColumnName) =
                  (fn_1 _fnOenv _fnOlib )
              -- "./TypeChecking/Expressions.ag"(line 72, column 9)
              _backTree =
                  {-# LINE 72 "./TypeChecking/Expressions.ag" #-}
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree _dirIannotatedTree
                  {-# LINE 3600 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 71, column 9)
              _tpe =
                  {-# LINE 71 "./TypeChecking/Expressions.ag" #-}
                  Right (getTypeAnnotation _fnIannotatedTree)
                  {-# LINE 3605 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 3613 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOliftedColumnName =
                  {-# LINE 161 "./TypeChecking/SelectLists.ag" #-}
                  _fnIliftedColumnName
                  {-# LINE 3618 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOliftedColumnName)))
-- ExpressionList ----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         typeList             : [Type]
   alternatives:
      alternative Cons:
         child hd             : Expression 
         child tl             : ExpressionList 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
-}
type ExpressionList  = [(Expression)]
-- cata
sem_ExpressionList :: ExpressionList  ->
                      T_ExpressionList 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_ExpressionList  = ( ExpressionList,T_ExpressionList_1 )
type T_ExpressionList_1  = Environment ->
                           LocalIdentifierBindings ->
                           ( ExpressionList,([Type]))
data Inh_ExpressionList  = Inh_ExpressionList {env_Inh_ExpressionList :: Environment,lib_Inh_ExpressionList :: LocalIdentifierBindings}
data Syn_ExpressionList  = Syn_ExpressionList {annotatedTree_Syn_ExpressionList :: ExpressionList,originalTree_Syn_ExpressionList :: ExpressionList,typeList_Syn_ExpressionList :: [Type]}
wrap_ExpressionList :: T_ExpressionList  ->
                       Inh_ExpressionList  ->
                       Syn_ExpressionList 
wrap_ExpressionList sem (Inh_ExpressionList _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOtypeList) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_ExpressionList _lhsOannotatedTree _lhsOoriginalTree _lhsOtypeList ))
sem_ExpressionList_Cons hd_ tl_  =
    (let tl_1 :: T_ExpressionList_1 
         _tlIoriginalTree :: ExpressionList
         hd_1 :: T_Expression_1 
         _hdIoriginalTree :: Expression
         _lhsOoriginalTree :: ExpressionList
         ( _tlIoriginalTree,tl_1) =
             (tl_ )
         ( _hdIoriginalTree,hd_1) =
             (hd_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             (:) _hdIoriginalTree _tlIoriginalTree
             {-# LINE 3682 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3687 "AstInternal.hs" #-}
         ( sem_ExpressionList_1) =
             (sem_ExpressionList_Cons_1 tl_1 hd_1 )
     in  ( _lhsOoriginalTree,sem_ExpressionList_1))
sem_ExpressionList_Cons_1 tl_1 hd_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: ExpressionList
              _tlItypeList :: ([Type])
              _hdIannotatedTree :: Expression
              _hdIliftedColumnName :: String
              _lhsOannotatedTree :: ExpressionList
              _lhsOtypeList :: ([Type])
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3708 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3713 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3718 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3723 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlItypeList) =
                  (tl_1 _tlOenv _tlOlib )
              ( _hdIannotatedTree,_hdIliftedColumnName) =
                  (hd_1 _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 3732 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3737 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 52, column 12)
              _lhsOtypeList =
                  {-# LINE 52 "./TypeChecking/Misc.ag" #-}
                  getTypeAnnotation _hdIannotatedTree : _tlItypeList
                  {-# LINE 3742 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOtypeList)))
sem_ExpressionList_Nil  =
    (let _lhsOoriginalTree :: ExpressionList
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             []
             {-# LINE 3750 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3755 "AstInternal.hs" #-}
         ( sem_ExpressionList_1) =
             (sem_ExpressionList_Nil_1 )
     in  ( _lhsOoriginalTree,sem_ExpressionList_1))
sem_ExpressionList_Nil_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ExpressionList
              _lhsOtypeList :: ([Type])
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 3768 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3773 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 53, column 11)
              _lhsOtypeList =
                  {-# LINE 53 "./TypeChecking/Misc.ag" #-}
                  []
                  {-# LINE 3778 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOtypeList)))
-- ExpressionListList ------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         typeListList         : [[Type]]
   alternatives:
      alternative Cons:
         child hd             : ExpressionList 
         child tl             : ExpressionListList 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
-}
type ExpressionListList  = [(ExpressionList)]
-- cata
sem_ExpressionListList :: ExpressionListList  ->
                          T_ExpressionListList 
sem_ExpressionListList list  =
    (Prelude.foldr sem_ExpressionListList_Cons sem_ExpressionListList_Nil (Prelude.map sem_ExpressionList list) )
-- semantic domain
type T_ExpressionListList  = ( ExpressionListList,T_ExpressionListList_1 )
type T_ExpressionListList_1  = Environment ->
                               LocalIdentifierBindings ->
                               ( ExpressionListList,([[Type]]))
data Inh_ExpressionListList  = Inh_ExpressionListList {env_Inh_ExpressionListList :: Environment,lib_Inh_ExpressionListList :: LocalIdentifierBindings}
data Syn_ExpressionListList  = Syn_ExpressionListList {annotatedTree_Syn_ExpressionListList :: ExpressionListList,originalTree_Syn_ExpressionListList :: ExpressionListList,typeListList_Syn_ExpressionListList :: [[Type]]}
wrap_ExpressionListList :: T_ExpressionListList  ->
                           Inh_ExpressionListList  ->
                           Syn_ExpressionListList 
wrap_ExpressionListList sem (Inh_ExpressionListList _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOtypeListList) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_ExpressionListList _lhsOannotatedTree _lhsOoriginalTree _lhsOtypeListList ))
sem_ExpressionListList_Cons hd_ tl_  =
    (let tl_1 :: T_ExpressionListList_1 
         _tlIoriginalTree :: ExpressionListList
         hd_1 :: T_ExpressionList_1 
         _hdIoriginalTree :: ExpressionList
         _lhsOoriginalTree :: ExpressionListList
         ( _tlIoriginalTree,tl_1) =
             (tl_ )
         ( _hdIoriginalTree,hd_1) =
             (hd_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             (:) _hdIoriginalTree _tlIoriginalTree
             {-# LINE 3842 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3847 "AstInternal.hs" #-}
         ( sem_ExpressionListList_1) =
             (sem_ExpressionListList_Cons_1 tl_1 hd_1 )
     in  ( _lhsOoriginalTree,sem_ExpressionListList_1))
sem_ExpressionListList_Cons_1 tl_1 hd_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: ExpressionListList
              _tlItypeListList :: ([[Type]])
              _hdIannotatedTree :: ExpressionList
              _hdItypeList :: ([Type])
              _lhsOannotatedTree :: ExpressionListList
              _lhsOtypeListList :: ([[Type]])
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3868 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3873 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3878 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 3883 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlItypeListList) =
                  (tl_1 _tlOenv _tlOlib )
              ( _hdIannotatedTree,_hdItypeList) =
                  (hd_1 _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 3892 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3897 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 59, column 12)
              _lhsOtypeListList =
                  {-# LINE 59 "./TypeChecking/Misc.ag" #-}
                  _hdItypeList : _tlItypeListList
                  {-# LINE 3902 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOtypeListList)))
sem_ExpressionListList_Nil  =
    (let _lhsOoriginalTree :: ExpressionListList
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             []
             {-# LINE 3910 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 3915 "AstInternal.hs" #-}
         ( sem_ExpressionListList_1) =
             (sem_ExpressionListList_Nil_1 )
     in  ( _lhsOoriginalTree,sem_ExpressionListList_1))
sem_ExpressionListList_Nil_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ExpressionListList
              _lhsOtypeListList :: ([[Type]])
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 3928 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3933 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 60, column 11)
              _lhsOtypeListList =
                  {-# LINE 60 "./TypeChecking/Misc.ag" #-}
                  []
                  {-# LINE 3938 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOtypeListList)))
-- ExpressionListStatementListPair -----------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ExpressionList 
         child x2             : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ExpressionListStatementListPair  = ( (ExpressionList),(StatementList))
-- cata
sem_ExpressionListStatementListPair :: ExpressionListStatementListPair  ->
                                       T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair ( x1,x2)  =
    (sem_ExpressionListStatementListPair_Tuple (sem_ExpressionList x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionListStatementListPair  = Environment ->
                                          LocalIdentifierBindings ->
                                          ( ExpressionListStatementListPair,ExpressionListStatementListPair)
data Inh_ExpressionListStatementListPair  = Inh_ExpressionListStatementListPair {env_Inh_ExpressionListStatementListPair :: Environment,lib_Inh_ExpressionListStatementListPair :: LocalIdentifierBindings}
data Syn_ExpressionListStatementListPair  = Syn_ExpressionListStatementListPair {annotatedTree_Syn_ExpressionListStatementListPair :: ExpressionListStatementListPair,originalTree_Syn_ExpressionListStatementListPair :: ExpressionListStatementListPair}
wrap_ExpressionListStatementListPair :: T_ExpressionListStatementListPair  ->
                                        Inh_ExpressionListStatementListPair  ->
                                        Syn_ExpressionListStatementListPair 
wrap_ExpressionListStatementListPair sem (Inh_ExpressionListStatementListPair _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_ExpressionListStatementListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _x2Olib :: LocalIdentifierBindings
              _x2Oenv :: Environment
              _x1Olib :: LocalIdentifierBindings
              _x1Oenv :: Environment
              _x2OlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _x2OenvUpdates :: ([EnvironmentUpdate])
              _x2IannotatedTree :: StatementList
              _x2IoriginalTree :: StatementList
              _x2IproducedEnv :: Environment
              _x2IproducedLib :: LocalIdentifierBindings
              x1_1 :: T_ExpressionList_1 
              _x1IoriginalTree :: ExpressionList
              _x1IannotatedTree :: ExpressionList
              _x1ItypeList :: ([Type])
              _lhsOannotatedTree :: ExpressionListStatementListPair
              _lhsOoriginalTree :: ExpressionListStatementListPair
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3999 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4004 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4009 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4014 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 91, column 9)
              _x2OlibUpdates =
                  {-# LINE 91 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4019 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
              _x2OenvUpdates =
                  {-# LINE 90 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4024 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2IoriginalTree,_x2IproducedEnv,_x2IproducedLib) =
                  (x2_ _x2Oenv _x2OenvUpdates _x2Olib _x2OlibUpdates )
              ( _x1IoriginalTree,x1_1) =
                  (x1_ )
              ( _x1IannotatedTree,_x1ItypeList) =
                  (x1_1 _x1Oenv _x1Olib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 4035 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4040 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 4045 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4050 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ExpressionListStatementListPairList -------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ExpressionListStatementListPair 
         child tl             : ExpressionListStatementListPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ExpressionListStatementListPairList  = [(ExpressionListStatementListPair)]
-- cata
sem_ExpressionListStatementListPairList :: ExpressionListStatementListPairList  ->
                                           T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList list  =
    (Prelude.foldr sem_ExpressionListStatementListPairList_Cons sem_ExpressionListStatementListPairList_Nil (Prelude.map sem_ExpressionListStatementListPair list) )
-- semantic domain
type T_ExpressionListStatementListPairList  = Environment ->
                                              LocalIdentifierBindings ->
                                              ( ExpressionListStatementListPairList,ExpressionListStatementListPairList)
data Inh_ExpressionListStatementListPairList  = Inh_ExpressionListStatementListPairList {env_Inh_ExpressionListStatementListPairList :: Environment,lib_Inh_ExpressionListStatementListPairList :: LocalIdentifierBindings}
data Syn_ExpressionListStatementListPairList  = Syn_ExpressionListStatementListPairList {annotatedTree_Syn_ExpressionListStatementListPairList :: ExpressionListStatementListPairList,originalTree_Syn_ExpressionListStatementListPairList :: ExpressionListStatementListPairList}
wrap_ExpressionListStatementListPairList :: T_ExpressionListStatementListPairList  ->
                                            Inh_ExpressionListStatementListPairList  ->
                                            Syn_ExpressionListStatementListPairList 
wrap_ExpressionListStatementListPairList sem (Inh_ExpressionListStatementListPairList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_ExpressionListStatementListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: ExpressionListStatementListPairList
              _tlIoriginalTree :: ExpressionListStatementListPairList
              _hdIannotatedTree :: ExpressionListStatementListPair
              _hdIoriginalTree :: ExpressionListStatementListPair
              _lhsOannotatedTree :: ExpressionListStatementListPairList
              _lhsOoriginalTree :: ExpressionListStatementListPairList
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4109 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4114 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4119 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4124 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOenv _tlOlib )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 4133 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4138 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 4143 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4148 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ExpressionListStatementListPairList_Nil  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ExpressionListStatementListPairList
              _lhsOoriginalTree :: ExpressionListStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 4159 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4164 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 4169 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4174 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ExpressionRoot ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative ExpressionRoot:
         child expr           : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                         LocalIdentifierBindings ->
                         ( ExpressionRoot,ExpressionRoot)
data Inh_ExpressionRoot  = Inh_ExpressionRoot {env_Inh_ExpressionRoot :: Environment,lib_Inh_ExpressionRoot :: LocalIdentifierBindings}
data Syn_ExpressionRoot  = Syn_ExpressionRoot {annotatedTree_Syn_ExpressionRoot :: ExpressionRoot,originalTree_Syn_ExpressionRoot :: ExpressionRoot}
wrap_ExpressionRoot :: T_ExpressionRoot  ->
                       Inh_ExpressionRoot  ->
                       Syn_ExpressionRoot 
wrap_ExpressionRoot sem (Inh_ExpressionRoot _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_ExpressionRoot _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionRoot_ExpressionRoot expr_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _exprOlib :: LocalIdentifierBindings
              _exprOenv :: Environment
              expr_1 :: T_Expression_1 
              _exprIoriginalTree :: Expression
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: ExpressionRoot
              _lhsOoriginalTree :: ExpressionRoot
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4227 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4232 "AstInternal.hs" #-}
              ( _exprIoriginalTree,expr_1) =
                  (expr_ )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_1 _exprOenv _exprOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ExpressionRoot _exprIannotatedTree
                  {-# LINE 4241 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4246 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ExpressionRoot _exprIoriginalTree
                  {-# LINE 4251 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4256 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ExpressionStatementListPair ---------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : Expression 
         child x2             : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ExpressionStatementListPair  = ( (Expression),(StatementList))
-- cata
sem_ExpressionStatementListPair :: ExpressionStatementListPair  ->
                                   T_ExpressionStatementListPair 
sem_ExpressionStatementListPair ( x1,x2)  =
    (sem_ExpressionStatementListPair_Tuple (sem_Expression x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionStatementListPair  = Environment ->
                                      LocalIdentifierBindings ->
                                      ( ExpressionStatementListPair,ExpressionStatementListPair)
data Inh_ExpressionStatementListPair  = Inh_ExpressionStatementListPair {env_Inh_ExpressionStatementListPair :: Environment,lib_Inh_ExpressionStatementListPair :: LocalIdentifierBindings}
data Syn_ExpressionStatementListPair  = Syn_ExpressionStatementListPair {annotatedTree_Syn_ExpressionStatementListPair :: ExpressionStatementListPair,originalTree_Syn_ExpressionStatementListPair :: ExpressionStatementListPair}
wrap_ExpressionStatementListPair :: T_ExpressionStatementListPair  ->
                                    Inh_ExpressionStatementListPair  ->
                                    Syn_ExpressionStatementListPair 
wrap_ExpressionStatementListPair sem (Inh_ExpressionStatementListPair _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_ExpressionStatementListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _x2Olib :: LocalIdentifierBindings
              _x2Oenv :: Environment
              _x1Olib :: LocalIdentifierBindings
              _x1Oenv :: Environment
              _x2OlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _x2OenvUpdates :: ([EnvironmentUpdate])
              _x2IannotatedTree :: StatementList
              _x2IoriginalTree :: StatementList
              _x2IproducedEnv :: Environment
              _x2IproducedLib :: LocalIdentifierBindings
              x1_1 :: T_Expression_1 
              _x1IoriginalTree :: Expression
              _x1IannotatedTree :: Expression
              _x1IliftedColumnName :: String
              _lhsOannotatedTree :: ExpressionStatementListPair
              _lhsOoriginalTree :: ExpressionStatementListPair
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4317 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4322 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4327 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4332 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 95, column 9)
              _x2OlibUpdates =
                  {-# LINE 95 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4337 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _x2OenvUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4342 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2IoriginalTree,_x2IproducedEnv,_x2IproducedLib) =
                  (x2_ _x2Oenv _x2OenvUpdates _x2Olib _x2OlibUpdates )
              ( _x1IoriginalTree,x1_1) =
                  (x1_ )
              ( _x1IannotatedTree,_x1IliftedColumnName) =
                  (x1_1 _x1Oenv _x1Olib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 4353 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4358 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 4363 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4368 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ExpressionStatementListPairList -----------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ExpressionStatementListPair 
         child tl             : ExpressionStatementListPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ExpressionStatementListPairList  = [(ExpressionStatementListPair)]
-- cata
sem_ExpressionStatementListPairList :: ExpressionStatementListPairList  ->
                                       T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList list  =
    (Prelude.foldr sem_ExpressionStatementListPairList_Cons sem_ExpressionStatementListPairList_Nil (Prelude.map sem_ExpressionStatementListPair list) )
-- semantic domain
type T_ExpressionStatementListPairList  = Environment ->
                                          LocalIdentifierBindings ->
                                          ( ExpressionStatementListPairList,ExpressionStatementListPairList)
data Inh_ExpressionStatementListPairList  = Inh_ExpressionStatementListPairList {env_Inh_ExpressionStatementListPairList :: Environment,lib_Inh_ExpressionStatementListPairList :: LocalIdentifierBindings}
data Syn_ExpressionStatementListPairList  = Syn_ExpressionStatementListPairList {annotatedTree_Syn_ExpressionStatementListPairList :: ExpressionStatementListPairList,originalTree_Syn_ExpressionStatementListPairList :: ExpressionStatementListPairList}
wrap_ExpressionStatementListPairList :: T_ExpressionStatementListPairList  ->
                                        Inh_ExpressionStatementListPairList  ->
                                        Syn_ExpressionStatementListPairList 
wrap_ExpressionStatementListPairList sem (Inh_ExpressionStatementListPairList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_ExpressionStatementListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: ExpressionStatementListPairList
              _tlIoriginalTree :: ExpressionStatementListPairList
              _hdIannotatedTree :: ExpressionStatementListPair
              _hdIoriginalTree :: ExpressionStatementListPair
              _lhsOannotatedTree :: ExpressionStatementListPairList
              _lhsOoriginalTree :: ExpressionStatementListPairList
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4427 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4432 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4437 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4442 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOenv _tlOlib )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 4451 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4456 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 4461 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4466 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ExpressionStatementListPairList_Nil  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ExpressionStatementListPairList
              _lhsOoriginalTree :: ExpressionStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 4477 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4482 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 4487 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4492 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- FnBody ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative PlpgsqlFnBody:
         child ann            : {Annotation}
         child vars           : VarDefList 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SqlFnBody:
         child ann            : {Annotation}
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                 LocalIdentifierBindings ->
                 ( FnBody,FnBody)
data Inh_FnBody  = Inh_FnBody {env_Inh_FnBody :: Environment,lib_Inh_FnBody :: LocalIdentifierBindings}
data Syn_FnBody  = Syn_FnBody {annotatedTree_Syn_FnBody :: FnBody,originalTree_Syn_FnBody :: FnBody}
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_FnBody _lhsOannotatedTree _lhsOoriginalTree ))
sem_FnBody_PlpgsqlFnBody ann_ vars_ sts_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _stsOenv :: Environment
              _varsOenv :: Environment
              _varsOlib :: LocalIdentifierBindings
              _varsIannotatedTree :: VarDefList
              _varsIdefs :: ([(String,Type)])
              _varsIoriginalTree :: VarDefList
              _stsOlib :: LocalIdentifierBindings
              _stsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedEnv :: Environment
              _stsIproducedLib :: LocalIdentifierBindings
              _lhsOannotatedTree :: FnBody
              _lhsOoriginalTree :: FnBody
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4563 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4568 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4573 "AstInternal.hs" #-}
              ( _varsIannotatedTree,_varsIdefs,_varsIoriginalTree) =
                  (vars_ _varsOenv _varsOlib )
              -- "./TypeChecking/CreateFunction.ag"(line 121, column 9)
              _stsOlib =
                  {-# LINE 121 "./TypeChecking/CreateFunction.ag" #-}
                  fromRight _lhsIlib $
                  updateBindings _lhsIlib _lhsIenv
                                 [LibStackIDs [("", _varsIdefs)]]
                  {-# LINE 4582 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 99, column 9)
              _stsOlibUpdates =
                  {-# LINE 99 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4587 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 98, column 9)
              _stsOenvUpdates =
                  {-# LINE 98 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4592 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedEnv,_stsIproducedLib) =
                  (sts_ _stsOenv _stsOenvUpdates _stsOlib _stsOlibUpdates )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _varsIannotatedTree _stsIannotatedTree
                  {-# LINE 4599 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4604 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _varsIoriginalTree _stsIoriginalTree
                  {-# LINE 4609 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4614 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_FnBody_SqlFnBody ann_ sts_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _stsOlib :: LocalIdentifierBindings
              _stsOenv :: Environment
              _stsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedEnv :: Environment
              _stsIproducedLib :: LocalIdentifierBindings
              _lhsOannotatedTree :: FnBody
              _lhsOoriginalTree :: FnBody
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4633 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4638 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 99, column 9)
              _stsOlibUpdates =
                  {-# LINE 99 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4643 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 98, column 9)
              _stsOenvUpdates =
                  {-# LINE 98 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4648 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedEnv,_stsIproducedLib) =
                  (sts_ _stsOenv _stsOenvUpdates _stsOlib _stsOlibUpdates )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIannotatedTree
                  {-# LINE 4655 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4660 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIoriginalTree
                  {-# LINE 4665 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4670 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- IfExists ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative IfExists:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Require:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                   LocalIdentifierBindings ->
                   ( IfExists,IfExists)
data Inh_IfExists  = Inh_IfExists {env_Inh_IfExists :: Environment,lib_Inh_IfExists :: LocalIdentifierBindings}
data Syn_IfExists  = Syn_IfExists {annotatedTree_Syn_IfExists :: IfExists,originalTree_Syn_IfExists :: IfExists}
wrap_IfExists :: T_IfExists  ->
                 Inh_IfExists  ->
                 Syn_IfExists 
wrap_IfExists sem (Inh_IfExists _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_IfExists _lhsOannotatedTree _lhsOoriginalTree ))
sem_IfExists_IfExists  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: IfExists
              _lhsOoriginalTree :: IfExists
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  IfExists
                  {-# LINE 4723 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4728 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  IfExists
                  {-# LINE 4733 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4738 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_IfExists_Require  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: IfExists
              _lhsOoriginalTree :: IfExists
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Require
                  {-# LINE 4749 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4754 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Require
                  {-# LINE 4759 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4764 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- InList ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         listType             : Either [TypeError] Type
   alternatives:
      alternative InList:
         child ann            : {Annotation}
         child exprs          : ExpressionList 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative InSelect:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local originalTree : _
         visit 1:
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
type T_InList  = ( InList,T_InList_1 )
type T_InList_1  = Environment ->
                   LocalIdentifierBindings ->
                   ( InList,(Either [TypeError] Type))
data Inh_InList  = Inh_InList {env_Inh_InList :: Environment,lib_Inh_InList :: LocalIdentifierBindings}
data Syn_InList  = Syn_InList {annotatedTree_Syn_InList :: InList,listType_Syn_InList :: Either [TypeError] Type,originalTree_Syn_InList :: InList}
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOlistType) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_InList _lhsOannotatedTree _lhsOlistType _lhsOoriginalTree ))
sem_InList_InList ann_ exprs_  =
    (let exprs_1 :: T_ExpressionList_1 
         _exprsIoriginalTree :: ExpressionList
         _lhsOoriginalTree :: InList
         ( _exprsIoriginalTree,exprs_1) =
             (exprs_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             InList ann_ _exprsIoriginalTree
             {-# LINE 4830 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 4835 "AstInternal.hs" #-}
         ( sem_InList_1) =
             (sem_InList_InList_1 exprs_1 ann_ )
     in  ( _lhsOoriginalTree,sem_InList_1))
sem_InList_InList_1 exprs_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _exprsOlib :: LocalIdentifierBindings
              _exprsOenv :: Environment
              _exprsIannotatedTree :: ExpressionList
              _exprsItypeList :: ([Type])
              _lhsOannotatedTree :: InList
              _lhsOlistType :: (Either [TypeError] Type)
              -- copy rule (down)
              _exprsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4852 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4857 "AstInternal.hs" #-}
              ( _exprsIannotatedTree,_exprsItypeList) =
                  (exprs_1 _exprsOenv _exprsOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  InList ann_ _exprsIannotatedTree
                  {-# LINE 4864 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4869 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 287, column 9)
              _lhsOlistType =
                  {-# LINE 287 "./TypeChecking/Expressions.ag" #-}
                  resolveResultSetType _lhsIenv _exprsItypeList
                  {-# LINE 4874 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType)))
sem_InList_InSelect ann_ sel_  =
    (let sel_1 :: T_SelectExpression_1 
         _selIoriginalTree :: SelectExpression
         _lhsOoriginalTree :: InList
         ( _selIoriginalTree,sel_1) =
             (sel_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             InSelect ann_ _selIoriginalTree
             {-# LINE 4886 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 4891 "AstInternal.hs" #-}
         ( sem_InList_1) =
             (sem_InList_InSelect_1 sel_1 ann_ )
     in  ( _lhsOoriginalTree,sem_InList_1))
sem_InList_InSelect_1 sel_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _selOlib :: LocalIdentifierBindings
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOannotatedTree :: InList
              _lhsOlistType :: (Either [TypeError] Type)
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4908 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 4913 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIlibUpdates) =
                  (sel_1 _selOenv _selOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  InSelect ann_ _selIannotatedTree
                  {-# LINE 4920 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4925 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 289, column 9)
              _lhsOlistType =
                  {-# LINE 289 "./TypeChecking/Expressions.ag" #-}
                  do
                  attrs <- map snd <$> (unwrapSetOfComposite $
                                        getTypeAnnotation _selIannotatedTree)
                  typ <- case length attrs of
                            0 -> Left [InternalError
                                       "got subquery with no columns? in inselect"]
                            1 -> Right $ head attrs
                            _ -> Right $ AnonymousRecordType attrs
                  dependsOnRTpe attrs $ Right typ
                  {-# LINE 4938 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType)))
-- JoinExpression ----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative JoinOn:
         child ann            : {Annotation}
         child expression     : Expression 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative JoinUsing:
         child ann            : {Annotation}
         child stringList     : StringList 
         visit 0:
            local originalTree : _
         visit 1:
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
type T_JoinExpression  = ( JoinExpression,T_JoinExpression_1 )
type T_JoinExpression_1  = Environment ->
                           LocalIdentifierBindings ->
                           ( JoinExpression)
data Inh_JoinExpression  = Inh_JoinExpression {env_Inh_JoinExpression :: Environment,lib_Inh_JoinExpression :: LocalIdentifierBindings}
data Syn_JoinExpression  = Syn_JoinExpression {annotatedTree_Syn_JoinExpression :: JoinExpression,originalTree_Syn_JoinExpression :: JoinExpression}
wrap_JoinExpression :: T_JoinExpression  ->
                       Inh_JoinExpression  ->
                       Syn_JoinExpression 
wrap_JoinExpression sem (Inh_JoinExpression _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_JoinExpression _lhsOannotatedTree _lhsOoriginalTree ))
sem_JoinExpression_JoinOn ann_ expression_  =
    (let expression_1 :: T_Expression_1 
         _expressionIoriginalTree :: Expression
         _lhsOoriginalTree :: JoinExpression
         ( _expressionIoriginalTree,expression_1) =
             (expression_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             JoinOn ann_ _expressionIoriginalTree
             {-# LINE 5003 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5008 "AstInternal.hs" #-}
         ( sem_JoinExpression_1) =
             (sem_JoinExpression_JoinOn_1 expression_1 ann_ )
     in  ( _lhsOoriginalTree,sem_JoinExpression_1))
sem_JoinExpression_JoinOn_1 expression_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _expressionOlib :: LocalIdentifierBindings
              _expressionOenv :: Environment
              _expressionIannotatedTree :: Expression
              _expressionIliftedColumnName :: String
              _lhsOannotatedTree :: JoinExpression
              -- copy rule (down)
              _expressionOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5024 "AstInternal.hs" #-}
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5029 "AstInternal.hs" #-}
              ( _expressionIannotatedTree,_expressionIliftedColumnName) =
                  (expression_1 _expressionOenv _expressionOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  JoinOn ann_ _expressionIannotatedTree
                  {-# LINE 5036 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5041 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_JoinExpression_JoinUsing ann_ stringList_  =
    (let stringList_1 :: T_StringList_1 
         _stringListIoriginalTree :: StringList
         _lhsOoriginalTree :: JoinExpression
         ( _stringListIoriginalTree,stringList_1) =
             (stringList_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             JoinUsing ann_ _stringListIoriginalTree
             {-# LINE 5053 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5058 "AstInternal.hs" #-}
         ( sem_JoinExpression_1) =
             (sem_JoinExpression_JoinUsing_1 stringList_1 ann_ )
     in  ( _lhsOoriginalTree,sem_JoinExpression_1))
sem_JoinExpression_JoinUsing_1 stringList_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _stringListOlib :: LocalIdentifierBindings
              _stringListOenv :: Environment
              _stringListIannotatedTree :: StringList
              _stringListIstrings :: ([String])
              _lhsOannotatedTree :: JoinExpression
              -- copy rule (down)
              _stringListOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5074 "AstInternal.hs" #-}
              -- copy rule (down)
              _stringListOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5079 "AstInternal.hs" #-}
              ( _stringListIannotatedTree,_stringListIstrings) =
                  (stringList_1 _stringListOenv _stringListOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  JoinUsing ann_ _stringListIannotatedTree
                  {-# LINE 5086 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5091 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- JoinType ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Cross:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative FullOuter:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Inner:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative LeftOuter:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative RightOuter:
         visit 0:
            local originalTree : _
         visit 1:
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
type T_JoinType  = ( JoinType,T_JoinType_1 )
type T_JoinType_1  = Environment ->
                     LocalIdentifierBindings ->
                     ( JoinType)
data Inh_JoinType  = Inh_JoinType {env_Inh_JoinType :: Environment,lib_Inh_JoinType :: LocalIdentifierBindings}
data Syn_JoinType  = Syn_JoinType {annotatedTree_Syn_JoinType :: JoinType,originalTree_Syn_JoinType :: JoinType}
wrap_JoinType :: T_JoinType  ->
                 Inh_JoinType  ->
                 Syn_JoinType 
wrap_JoinType sem (Inh_JoinType _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_JoinType _lhsOannotatedTree _lhsOoriginalTree ))
sem_JoinType_Cross  =
    (let _lhsOoriginalTree :: JoinType
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Cross
             {-# LINE 5172 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5177 "AstInternal.hs" #-}
         ( sem_JoinType_1) =
             (sem_JoinType_Cross_1 )
     in  ( _lhsOoriginalTree,sem_JoinType_1))
sem_JoinType_Cross_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: JoinType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Cross
                  {-# LINE 5189 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5194 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_JoinType_FullOuter  =
    (let _lhsOoriginalTree :: JoinType
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             FullOuter
             {-# LINE 5202 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5207 "AstInternal.hs" #-}
         ( sem_JoinType_1) =
             (sem_JoinType_FullOuter_1 )
     in  ( _lhsOoriginalTree,sem_JoinType_1))
sem_JoinType_FullOuter_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: JoinType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  FullOuter
                  {-# LINE 5219 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5224 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_JoinType_Inner  =
    (let _lhsOoriginalTree :: JoinType
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Inner
             {-# LINE 5232 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5237 "AstInternal.hs" #-}
         ( sem_JoinType_1) =
             (sem_JoinType_Inner_1 )
     in  ( _lhsOoriginalTree,sem_JoinType_1))
sem_JoinType_Inner_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: JoinType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Inner
                  {-# LINE 5249 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5254 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_JoinType_LeftOuter  =
    (let _lhsOoriginalTree :: JoinType
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             LeftOuter
             {-# LINE 5262 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5267 "AstInternal.hs" #-}
         ( sem_JoinType_1) =
             (sem_JoinType_LeftOuter_1 )
     in  ( _lhsOoriginalTree,sem_JoinType_1))
sem_JoinType_LeftOuter_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: JoinType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  LeftOuter
                  {-# LINE 5279 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5284 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_JoinType_RightOuter  =
    (let _lhsOoriginalTree :: JoinType
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             RightOuter
             {-# LINE 5292 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5297 "AstInternal.hs" #-}
         ( sem_JoinType_1) =
             (sem_JoinType_RightOuter_1 )
     in  ( _lhsOoriginalTree,sem_JoinType_1))
sem_JoinType_RightOuter_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: JoinType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RightOuter
                  {-# LINE 5309 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5314 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- Language ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Plpgsql:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Sql:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                   LocalIdentifierBindings ->
                   ( Language,Language)
data Inh_Language  = Inh_Language {env_Inh_Language :: Environment,lib_Inh_Language :: LocalIdentifierBindings}
data Syn_Language  = Syn_Language {annotatedTree_Syn_Language :: Language,originalTree_Syn_Language :: Language}
wrap_Language :: T_Language  ->
                 Inh_Language  ->
                 Syn_Language 
wrap_Language sem (Inh_Language _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_Language _lhsOannotatedTree _lhsOoriginalTree ))
sem_Language_Plpgsql  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Language
              _lhsOoriginalTree :: Language
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Plpgsql
                  {-# LINE 5367 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5372 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Plpgsql
                  {-# LINE 5377 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5382 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Language_Sql  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Language
              _lhsOoriginalTree :: Language
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Sql
                  {-# LINE 5393 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5398 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Sql
                  {-# LINE 5403 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5408 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- LiftFlavour -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative LiftAll:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative LiftAny:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
-}
data LiftFlavour  = LiftAll 
                  | LiftAny 
                  deriving ( Data,Eq,Show,Typeable)
-- cata
sem_LiftFlavour :: LiftFlavour  ->
                   T_LiftFlavour 
sem_LiftFlavour (LiftAll )  =
    (sem_LiftFlavour_LiftAll )
sem_LiftFlavour (LiftAny )  =
    (sem_LiftFlavour_LiftAny )
-- semantic domain
type T_LiftFlavour  = ( LiftFlavour,T_LiftFlavour_1 )
type T_LiftFlavour_1  = Environment ->
                        LocalIdentifierBindings ->
                        ( LiftFlavour)
data Inh_LiftFlavour  = Inh_LiftFlavour {env_Inh_LiftFlavour :: Environment,lib_Inh_LiftFlavour :: LocalIdentifierBindings}
data Syn_LiftFlavour  = Syn_LiftFlavour {annotatedTree_Syn_LiftFlavour :: LiftFlavour,originalTree_Syn_LiftFlavour :: LiftFlavour}
wrap_LiftFlavour :: T_LiftFlavour  ->
                    Inh_LiftFlavour  ->
                    Syn_LiftFlavour 
wrap_LiftFlavour sem (Inh_LiftFlavour _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_LiftFlavour _lhsOannotatedTree _lhsOoriginalTree ))
sem_LiftFlavour_LiftAll  =
    (let _lhsOoriginalTree :: LiftFlavour
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             LiftAll
             {-# LINE 5465 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5470 "AstInternal.hs" #-}
         ( sem_LiftFlavour_1) =
             (sem_LiftFlavour_LiftAll_1 )
     in  ( _lhsOoriginalTree,sem_LiftFlavour_1))
sem_LiftFlavour_LiftAll_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: LiftFlavour
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  LiftAll
                  {-# LINE 5482 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5487 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_LiftFlavour_LiftAny  =
    (let _lhsOoriginalTree :: LiftFlavour
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             LiftAny
             {-# LINE 5495 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5500 "AstInternal.hs" #-}
         ( sem_LiftFlavour_1) =
             (sem_LiftFlavour_LiftAny_1 )
     in  ( _lhsOoriginalTree,sem_LiftFlavour_1))
sem_LiftFlavour_LiftAny_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: LiftFlavour
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  LiftAny
                  {-# LINE 5512 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5517 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- MaybeBoolExpression -----------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Just:
         child just           : Expression 
         visit 0:
            local originalTree : _
      alternative Nothing:
         visit 0:
            local originalTree : _
         visit 1:
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
type T_MaybeBoolExpression  = ( MaybeBoolExpression,T_MaybeBoolExpression_1 )
type T_MaybeBoolExpression_1  = Environment ->
                                LocalIdentifierBindings ->
                                ( MaybeBoolExpression)
data Inh_MaybeBoolExpression  = Inh_MaybeBoolExpression {env_Inh_MaybeBoolExpression :: Environment,lib_Inh_MaybeBoolExpression :: LocalIdentifierBindings}
data Syn_MaybeBoolExpression  = Syn_MaybeBoolExpression {annotatedTree_Syn_MaybeBoolExpression :: MaybeBoolExpression,originalTree_Syn_MaybeBoolExpression :: MaybeBoolExpression}
wrap_MaybeBoolExpression :: T_MaybeBoolExpression  ->
                            Inh_MaybeBoolExpression  ->
                            Syn_MaybeBoolExpression 
wrap_MaybeBoolExpression sem (Inh_MaybeBoolExpression _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_MaybeBoolExpression _lhsOannotatedTree _lhsOoriginalTree ))
sem_MaybeBoolExpression_Just just_  =
    (let just_1 :: T_Expression_1 
         _justIoriginalTree :: Expression
         _lhsOoriginalTree :: MaybeBoolExpression
         ( _justIoriginalTree,just_1) =
             (just_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Just _justIoriginalTree
             {-# LINE 5575 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5580 "AstInternal.hs" #-}
         ( sem_MaybeBoolExpression_1) =
             (sem_MaybeBoolExpression_Just_1 just_1 )
     in  ( _lhsOoriginalTree,sem_MaybeBoolExpression_1))
sem_MaybeBoolExpression_Just_1 just_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _justOlib :: LocalIdentifierBindings
              _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIliftedColumnName :: String
              _lhsOannotatedTree :: MaybeBoolExpression
              -- copy rule (down)
              _justOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5596 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5601 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIliftedColumnName) =
                  (just_1 _justOenv _justOlib )
              -- "./TypeChecking/Misc.ag"(line 74, column 9)
              _lhsOannotatedTree =
                  {-# LINE 74 "./TypeChecking/Misc.ag" #-}
                  if getTypeAnnotation _justIannotatedTree `notElem` [typeBool, TypeCheckFailed]
                    then Just $ updateAnnotation ((TypeErrorA ExpressionMustBeBool) :)
                                  _justIannotatedTree
                    else Just $ _justIannotatedTree
                  {-# LINE 5611 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_MaybeBoolExpression_Nothing  =
    (let _lhsOoriginalTree :: MaybeBoolExpression
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Nothing
             {-# LINE 5619 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5624 "AstInternal.hs" #-}
         ( sem_MaybeBoolExpression_1) =
             (sem_MaybeBoolExpression_Nothing_1 )
     in  ( _lhsOoriginalTree,sem_MaybeBoolExpression_1))
sem_MaybeBoolExpression_Nothing_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: MaybeBoolExpression
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 5636 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5641 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- MaybeExpression ---------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Just:
         child just           : Expression 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Nothing:
         visit 0:
            local originalTree : _
         visit 1:
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
type T_MaybeExpression  = ( MaybeExpression,T_MaybeExpression_1 )
type T_MaybeExpression_1  = Environment ->
                            LocalIdentifierBindings ->
                            ( MaybeExpression)
data Inh_MaybeExpression  = Inh_MaybeExpression {env_Inh_MaybeExpression :: Environment,lib_Inh_MaybeExpression :: LocalIdentifierBindings}
data Syn_MaybeExpression  = Syn_MaybeExpression {annotatedTree_Syn_MaybeExpression :: MaybeExpression,originalTree_Syn_MaybeExpression :: MaybeExpression}
wrap_MaybeExpression :: T_MaybeExpression  ->
                        Inh_MaybeExpression  ->
                        Syn_MaybeExpression 
wrap_MaybeExpression sem (Inh_MaybeExpression _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_MaybeExpression _lhsOannotatedTree _lhsOoriginalTree ))
sem_MaybeExpression_Just just_  =
    (let just_1 :: T_Expression_1 
         _justIoriginalTree :: Expression
         _lhsOoriginalTree :: MaybeExpression
         ( _justIoriginalTree,just_1) =
             (just_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Just _justIoriginalTree
             {-# LINE 5701 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5706 "AstInternal.hs" #-}
         ( sem_MaybeExpression_1) =
             (sem_MaybeExpression_Just_1 just_1 )
     in  ( _lhsOoriginalTree,sem_MaybeExpression_1))
sem_MaybeExpression_Just_1 just_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _justOlib :: LocalIdentifierBindings
              _justOenv :: Environment
              _justIannotatedTree :: Expression
              _justIliftedColumnName :: String
              _lhsOannotatedTree :: MaybeExpression
              -- copy rule (down)
              _justOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5722 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5727 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIliftedColumnName) =
                  (just_1 _justOenv _justOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 5734 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5739 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_MaybeExpression_Nothing  =
    (let _lhsOoriginalTree :: MaybeExpression
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Nothing
             {-# LINE 5747 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5752 "AstInternal.hs" #-}
         ( sem_MaybeExpression_1) =
             (sem_MaybeExpression_Nothing_1 )
     in  ( _lhsOoriginalTree,sem_MaybeExpression_1))
sem_MaybeExpression_Nothing_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: MaybeExpression
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 5764 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5769 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- MaybeSelectList ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         listType             : Maybe [(String,Type)]
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : SelectList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type MaybeSelectList  = (Maybe (SelectList))
-- cata
sem_MaybeSelectList :: MaybeSelectList  ->
                       T_MaybeSelectList 
sem_MaybeSelectList (Prelude.Just x )  =
    (sem_MaybeSelectList_Just (sem_SelectList x ) )
sem_MaybeSelectList Prelude.Nothing  =
    sem_MaybeSelectList_Nothing
-- semantic domain
type T_MaybeSelectList  = Environment ->
                          LocalIdentifierBindings ->
                          ( MaybeSelectList,(Maybe [(String,Type)]),MaybeSelectList)
data Inh_MaybeSelectList  = Inh_MaybeSelectList {env_Inh_MaybeSelectList :: Environment,lib_Inh_MaybeSelectList :: LocalIdentifierBindings}
data Syn_MaybeSelectList  = Syn_MaybeSelectList {annotatedTree_Syn_MaybeSelectList :: MaybeSelectList,listType_Syn_MaybeSelectList :: Maybe [(String,Type)],originalTree_Syn_MaybeSelectList :: MaybeSelectList}
wrap_MaybeSelectList :: T_MaybeSelectList  ->
                        Inh_MaybeSelectList  ->
                        Syn_MaybeSelectList 
wrap_MaybeSelectList sem (Inh_MaybeSelectList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_MaybeSelectList _lhsOannotatedTree _lhsOlistType _lhsOoriginalTree ))
sem_MaybeSelectList_Just just_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _justOlib :: LocalIdentifierBindings
              _justOenv :: Environment
              just_1 :: T_SelectList_1 
              _justIoriginalTree :: SelectList
              _justIannotatedTree :: SelectList
              _justIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _justIlistType :: ([(String,Type)])
              _lhsOannotatedTree :: MaybeSelectList
              _lhsOlistType :: (Maybe [(String,Type)])
              _lhsOoriginalTree :: MaybeSelectList
              -- copy rule (down)
              _justOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5830 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5835 "AstInternal.hs" #-}
              ( _justIoriginalTree,just_1) =
                  (just_ )
              ( _justIannotatedTree,_justIlibUpdates,_justIlistType) =
                  (just_1 _justOenv _justOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 5844 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5849 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 25, column 12)
              _lhsOlistType =
                  {-# LINE 25 "./TypeChecking/SelectLists.ag" #-}
                  Just _justIlistType
                  {-# LINE 5854 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 5859 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5864 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree)))
sem_MaybeSelectList_Nothing  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: MaybeSelectList
              _lhsOlistType :: (Maybe [(String,Type)])
              _lhsOoriginalTree :: MaybeSelectList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 5876 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5881 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 26, column 15)
              _lhsOlistType =
                  {-# LINE 26 "./TypeChecking/SelectLists.ag" #-}
                  Nothing
                  {-# LINE 5886 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 5891 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5896 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree)))
-- MaybeTableRef -----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         libUpdates           : [LocalIdentifierBindingsUpdate]
   alternatives:
      alternative Just:
         child just           : TableRef 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Nothing:
         visit 0:
            local originalTree : _
         visit 1:
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
type T_MaybeTableRef  = ( MaybeTableRef,T_MaybeTableRef_1 )
type T_MaybeTableRef_1  = Environment ->
                          LocalIdentifierBindings ->
                          ( MaybeTableRef,([LocalIdentifierBindingsUpdate]))
data Inh_MaybeTableRef  = Inh_MaybeTableRef {env_Inh_MaybeTableRef :: Environment,lib_Inh_MaybeTableRef :: LocalIdentifierBindings}
data Syn_MaybeTableRef  = Syn_MaybeTableRef {annotatedTree_Syn_MaybeTableRef :: MaybeTableRef,libUpdates_Syn_MaybeTableRef :: [LocalIdentifierBindingsUpdate],originalTree_Syn_MaybeTableRef :: MaybeTableRef}
wrap_MaybeTableRef :: T_MaybeTableRef  ->
                      Inh_MaybeTableRef  ->
                      Syn_MaybeTableRef 
wrap_MaybeTableRef sem (Inh_MaybeTableRef _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOlibUpdates) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_MaybeTableRef _lhsOannotatedTree _lhsOlibUpdates _lhsOoriginalTree ))
sem_MaybeTableRef_Just just_  =
    (let just_1 :: T_TableRef_1 
         _justIoriginalTree :: TableRef
         _lhsOoriginalTree :: MaybeTableRef
         ( _justIoriginalTree,just_1) =
             (just_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Just _justIoriginalTree
             {-# LINE 5957 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 5962 "AstInternal.hs" #-}
         ( sem_MaybeTableRef_1) =
             (sem_MaybeTableRef_Just_1 just_1 )
     in  ( _lhsOoriginalTree,sem_MaybeTableRef_1))
sem_MaybeTableRef_Just_1 just_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _justOlib :: LocalIdentifierBindings
              _justOenv :: Environment
              _justOjlibUpdates :: ([LocalIdentifierBindingsUpdate])
              just_2 :: T_TableRef_2 
              _justIidLookups :: ([(String,Type)])
              _justIqidLookups :: ([(String,[(String,Type)])])
              _justIqstarExpansion :: ([(String,[(String,Type)])])
              _justIstarExpansion :: ([(String,Type)])
              _justIannotatedTree :: TableRef
              _justIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOannotatedTree :: MaybeTableRef
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _justOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5985 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 5990 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 192, column 12)
              _justOjlibUpdates =
                  {-# LINE 192 "./TypeChecking/TableRefs.ag" #-}
                  []
                  {-# LINE 5995 "AstInternal.hs" #-}
              ( _justIidLookups,_justIqidLookups,_justIqstarExpansion,_justIstarExpansion,just_2) =
                  (just_1 _justOenv _justOlib )
              ( _justIannotatedTree,_justIlibUpdates) =
                  (just_2 _justOjlibUpdates )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 6004 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6009 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOlibUpdates =
                  {-# LINE 14 "./TypeChecking/TableRefs.ag" #-}
                  _justIlibUpdates
                  {-# LINE 6014 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
sem_MaybeTableRef_Nothing  =
    (let _lhsOoriginalTree :: MaybeTableRef
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Nothing
             {-# LINE 6022 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 6027 "AstInternal.hs" #-}
         ( sem_MaybeTableRef_1) =
             (sem_MaybeTableRef_Nothing_1 )
     in  ( _lhsOoriginalTree,sem_MaybeTableRef_1))
sem_MaybeTableRef_Nothing_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: MaybeTableRef
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 6040 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6045 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 38, column 9)
              _lhsOlibUpdates =
                  {-# LINE 38 "./TypeChecking/TableRefs.ag" #-}
                  []
                  {-# LINE 6050 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
-- Natural -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Natural:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Unnatural:
         visit 0:
            local originalTree : _
         visit 1:
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
type T_Natural  = ( Natural,T_Natural_1 )
type T_Natural_1  = Environment ->
                    LocalIdentifierBindings ->
                    ( Natural)
data Inh_Natural  = Inh_Natural {env_Inh_Natural :: Environment,lib_Inh_Natural :: LocalIdentifierBindings}
data Syn_Natural  = Syn_Natural {annotatedTree_Syn_Natural :: Natural,originalTree_Syn_Natural :: Natural}
wrap_Natural :: T_Natural  ->
                Inh_Natural  ->
                Syn_Natural 
wrap_Natural sem (Inh_Natural _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_Natural _lhsOannotatedTree _lhsOoriginalTree ))
sem_Natural_Natural  =
    (let _lhsOoriginalTree :: Natural
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Natural
             {-# LINE 6107 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 6112 "AstInternal.hs" #-}
         ( sem_Natural_1) =
             (sem_Natural_Natural_1 )
     in  ( _lhsOoriginalTree,sem_Natural_1))
sem_Natural_Natural_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Natural
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Natural
                  {-# LINE 6124 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6129 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_Natural_Unnatural  =
    (let _lhsOoriginalTree :: Natural
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Unnatural
             {-# LINE 6137 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 6142 "AstInternal.hs" #-}
         ( sem_Natural_1) =
             (sem_Natural_Unnatural_1 )
     in  ( _lhsOoriginalTree,sem_Natural_1))
sem_Natural_Unnatural_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Natural
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Unnatural
                  {-# LINE 6154 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6159 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- OnExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Just:
         child just           : JoinExpression 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Nothing:
         visit 0:
            local originalTree : _
         visit 1:
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
type T_OnExpr  = ( OnExpr,T_OnExpr_1 )
type T_OnExpr_1  = Environment ->
                   LocalIdentifierBindings ->
                   ( OnExpr)
data Inh_OnExpr  = Inh_OnExpr {env_Inh_OnExpr :: Environment,lib_Inh_OnExpr :: LocalIdentifierBindings}
data Syn_OnExpr  = Syn_OnExpr {annotatedTree_Syn_OnExpr :: OnExpr,originalTree_Syn_OnExpr :: OnExpr}
wrap_OnExpr :: T_OnExpr  ->
               Inh_OnExpr  ->
               Syn_OnExpr 
wrap_OnExpr sem (Inh_OnExpr _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_OnExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_OnExpr_Just just_  =
    (let just_1 :: T_JoinExpression_1 
         _justIoriginalTree :: JoinExpression
         _lhsOoriginalTree :: OnExpr
         ( _justIoriginalTree,just_1) =
             (just_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Just _justIoriginalTree
             {-# LINE 6219 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 6224 "AstInternal.hs" #-}
         ( sem_OnExpr_1) =
             (sem_OnExpr_Just_1 just_1 )
     in  ( _lhsOoriginalTree,sem_OnExpr_1))
sem_OnExpr_Just_1 just_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _justOlib :: LocalIdentifierBindings
              _justOenv :: Environment
              _justIannotatedTree :: JoinExpression
              _lhsOannotatedTree :: OnExpr
              -- copy rule (down)
              _justOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6239 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6244 "AstInternal.hs" #-}
              ( _justIannotatedTree) =
                  (just_1 _justOenv _justOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 6251 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6256 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
sem_OnExpr_Nothing  =
    (let _lhsOoriginalTree :: OnExpr
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Nothing
             {-# LINE 6264 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 6269 "AstInternal.hs" #-}
         ( sem_OnExpr_1) =
             (sem_OnExpr_Nothing_1 )
     in  ( _lhsOoriginalTree,sem_OnExpr_1))
sem_OnExpr_Nothing_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: OnExpr
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 6281 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6286 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree)))
-- ParamDef ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         namedType            : Type
         originalTree         : SELF 
         paramName            : String
   alternatives:
      alternative ParamDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ParamDefTp:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                   LocalIdentifierBindings ->
                   ( ParamDef,Type,ParamDef,String)
data Inh_ParamDef  = Inh_ParamDef {env_Inh_ParamDef :: Environment,lib_Inh_ParamDef :: LocalIdentifierBindings}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef,namedType_Syn_ParamDef :: Type,originalTree_Syn_ParamDef :: ParamDef,paramName_Syn_ParamDef :: String}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree,_lhsOparamName) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_ParamDef _lhsOannotatedTree _lhsOnamedType _lhsOoriginalTree _lhsOparamName ))
sem_ParamDef_ParamDef ann_ name_ typ_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _typOenv :: Environment
              typ_1 :: T_TypeName_1 
              _typIoriginalTree :: TypeName
              _typOlib :: LocalIdentifierBindings
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: ParamDef
              _lhsOnamedType :: Type
              _lhsOoriginalTree :: ParamDef
              _lhsOparamName :: String
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6354 "AstInternal.hs" #-}
              ( _typIoriginalTree,typ_1) =
                  (typ_ )
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6361 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_1 _typOenv _typOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIannotatedTree
                  {-# LINE 6368 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6373 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 56, column 9)
              _lhsOnamedType =
                  {-# LINE 56 "./TypeChecking/CreateFunction.ag" #-}
                  _typInamedType
                  {-# LINE 6378 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIoriginalTree
                  {-# LINE 6383 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6388 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 58, column 9)
              _lhsOparamName =
                  {-# LINE 58 "./TypeChecking/CreateFunction.ag" #-}
                  name_
                  {-# LINE 6393 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree,_lhsOparamName)))
sem_ParamDef_ParamDefTp ann_ typ_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _typOenv :: Environment
              typ_1 :: T_TypeName_1 
              _typIoriginalTree :: TypeName
              _typOlib :: LocalIdentifierBindings
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: ParamDef
              _lhsOnamedType :: Type
              _lhsOoriginalTree :: ParamDef
              _lhsOparamName :: String
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6412 "AstInternal.hs" #-}
              ( _typIoriginalTree,typ_1) =
                  (typ_ )
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6419 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_1 _typOenv _typOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIannotatedTree
                  {-# LINE 6426 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6431 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 56, column 9)
              _lhsOnamedType =
                  {-# LINE 56 "./TypeChecking/CreateFunction.ag" #-}
                  _typInamedType
                  {-# LINE 6436 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIoriginalTree
                  {-# LINE 6441 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6446 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 60, column 9)
              _lhsOparamName =
                  {-# LINE 60 "./TypeChecking/CreateFunction.ag" #-}
                  ""
                  {-# LINE 6451 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree,_lhsOparamName)))
-- ParamDefList ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         params               : [(String, Type)]
   alternatives:
      alternative Cons:
         child hd             : ParamDef 
         child tl             : ParamDefList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ParamDefList  = [(ParamDef)]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = Environment ->
                       LocalIdentifierBindings ->
                       ( ParamDefList,ParamDefList,([(String, Type)]))
data Inh_ParamDefList  = Inh_ParamDefList {env_Inh_ParamDefList :: Environment,lib_Inh_ParamDefList :: LocalIdentifierBindings}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList,originalTree_Syn_ParamDefList :: ParamDefList,params_Syn_ParamDefList :: [(String, Type)]}
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOparams) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_ParamDefList _lhsOannotatedTree _lhsOoriginalTree _lhsOparams ))
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlOlib :: LocalIdentifierBindings
              _tlIannotatedTree :: ParamDefList
              _tlIoriginalTree :: ParamDefList
              _tlIparams :: ([(String, Type)])
              _hdOlib :: LocalIdentifierBindings
              _hdIannotatedTree :: ParamDef
              _hdInamedType :: Type
              _hdIoriginalTree :: ParamDef
              _hdIparamName :: String
              _lhsOannotatedTree :: ParamDefList
              _lhsOoriginalTree :: ParamDefList
              _lhsOparams :: ([(String, Type)])
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6515 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6520 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6525 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIparams) =
                  (tl_ _tlOenv _tlOlib )
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6532 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdInamedType,_hdIoriginalTree,_hdIparamName) =
                  (hd_ _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 6539 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6544 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 6549 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6554 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 64, column 13)
              _lhsOparams =
                  {-# LINE 64 "./TypeChecking/CreateFunction.ag" #-}
                  ((_hdIparamName, _hdInamedType) : _tlIparams)
                  {-# LINE 6559 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOparams)))
sem_ParamDefList_Nil  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ParamDefList
              _lhsOoriginalTree :: ParamDefList
              _lhsOparams :: ([(String, Type)])
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 6571 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6576 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 6581 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6586 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 63, column 12)
              _lhsOparams =
                  {-# LINE 63 "./TypeChecking/CreateFunction.ag" #-}
                  []
                  {-# LINE 6591 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOparams)))
-- RaiseType ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative RError:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RException:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RNotice:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                    LocalIdentifierBindings ->
                    ( RaiseType,RaiseType)
data Inh_RaiseType  = Inh_RaiseType {env_Inh_RaiseType :: Environment,lib_Inh_RaiseType :: LocalIdentifierBindings}
data Syn_RaiseType  = Syn_RaiseType {annotatedTree_Syn_RaiseType :: RaiseType,originalTree_Syn_RaiseType :: RaiseType}
wrap_RaiseType :: T_RaiseType  ->
                  Inh_RaiseType  ->
                  Syn_RaiseType 
wrap_RaiseType sem (Inh_RaiseType _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_RaiseType _lhsOannotatedTree _lhsOoriginalTree ))
sem_RaiseType_RError  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RaiseType
              _lhsOoriginalTree :: RaiseType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RError
                  {-# LINE 6651 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6656 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RError
                  {-# LINE 6661 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6666 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RaiseType_RException  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RaiseType
              _lhsOoriginalTree :: RaiseType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RException
                  {-# LINE 6677 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6682 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RException
                  {-# LINE 6687 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6692 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RaiseType_RNotice  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RaiseType
              _lhsOoriginalTree :: RaiseType
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RNotice
                  {-# LINE 6703 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6708 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RNotice
                  {-# LINE 6713 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6718 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- RestartIdentity ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative ContinueIdentity:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RestartIdentity:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                          LocalIdentifierBindings ->
                          ( RestartIdentity,RestartIdentity)
data Inh_RestartIdentity  = Inh_RestartIdentity {env_Inh_RestartIdentity :: Environment,lib_Inh_RestartIdentity :: LocalIdentifierBindings}
data Syn_RestartIdentity  = Syn_RestartIdentity {annotatedTree_Syn_RestartIdentity :: RestartIdentity,originalTree_Syn_RestartIdentity :: RestartIdentity}
wrap_RestartIdentity :: T_RestartIdentity  ->
                        Inh_RestartIdentity  ->
                        Syn_RestartIdentity 
wrap_RestartIdentity sem (Inh_RestartIdentity _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_RestartIdentity _lhsOannotatedTree _lhsOoriginalTree ))
sem_RestartIdentity_ContinueIdentity  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RestartIdentity
              _lhsOoriginalTree :: RestartIdentity
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ContinueIdentity
                  {-# LINE 6771 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6776 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ContinueIdentity
                  {-# LINE 6781 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6786 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RestartIdentity_RestartIdentity  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RestartIdentity
              _lhsOoriginalTree :: RestartIdentity
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RestartIdentity
                  {-# LINE 6797 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6802 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RestartIdentity
                  {-# LINE 6807 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6812 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- Root --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         producedEnv          : Environment
         producedLib          : LocalIdentifierBindings
   alternatives:
      alternative Root:
         child statements     : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
               LocalIdentifierBindings ->
               ( Root,Root,Environment,LocalIdentifierBindings)
data Inh_Root  = Inh_Root {env_Inh_Root :: Environment,lib_Inh_Root :: LocalIdentifierBindings}
data Syn_Root  = Syn_Root {annotatedTree_Syn_Root :: Root,originalTree_Syn_Root :: Root,producedEnv_Syn_Root :: Environment,producedLib_Syn_Root :: LocalIdentifierBindings}
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedEnv,_lhsOproducedLib) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_Root _lhsOannotatedTree _lhsOoriginalTree _lhsOproducedEnv _lhsOproducedLib ))
sem_Root_Root statements_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _statementsOlib :: LocalIdentifierBindings
              _statementsOenv :: Environment
              _statementsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _statementsOenvUpdates :: ([EnvironmentUpdate])
              _statementsIannotatedTree :: StatementList
              _statementsIoriginalTree :: StatementList
              _statementsIproducedEnv :: Environment
              _statementsIproducedLib :: LocalIdentifierBindings
              _lhsOannotatedTree :: Root
              _lhsOoriginalTree :: Root
              _lhsOproducedEnv :: Environment
              _lhsOproducedLib :: LocalIdentifierBindings
              -- copy rule (down)
              _statementsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6871 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 6876 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 78, column 12)
              _statementsOlibUpdates =
                  {-# LINE 78 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 6881 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 77, column 12)
              _statementsOenvUpdates =
                  {-# LINE 77 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 6886 "AstInternal.hs" #-}
              ( _statementsIannotatedTree,_statementsIoriginalTree,_statementsIproducedEnv,_statementsIproducedLib) =
                  (statements_ _statementsOenv _statementsOenvUpdates _statementsOlib _statementsOlibUpdates )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Root _statementsIannotatedTree
                  {-# LINE 6893 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6898 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Root _statementsIoriginalTree
                  {-# LINE 6903 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6908 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOproducedEnv =
                  {-# LINE 27 "./TypeChecking/Statements.ag" #-}
                  _statementsIproducedEnv
                  {-# LINE 6913 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOproducedLib =
                  {-# LINE 28 "./TypeChecking/Statements.ag" #-}
                  _statementsIproducedLib
                  {-# LINE 6918 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedEnv,_lhsOproducedLib)))
-- RowConstraint -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative NotNullConstraint:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative NullConstraint:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowCheckConstraint:
         child ann            : {Annotation}
         child expression     : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowPrimaryKeyConstraint:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowReferenceConstraint:
         child ann            : {Annotation}
         child table          : {String}
         child att            : {Maybe String}
         child onUpdate       : Cascade 
         child onDelete       : Cascade 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowUniqueConstraint:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                        LocalIdentifierBindings ->
                        ( RowConstraint,RowConstraint)
data Inh_RowConstraint  = Inh_RowConstraint {env_Inh_RowConstraint :: Environment,lib_Inh_RowConstraint :: LocalIdentifierBindings}
data Syn_RowConstraint  = Syn_RowConstraint {annotatedTree_Syn_RowConstraint :: RowConstraint,originalTree_Syn_RowConstraint :: RowConstraint}
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_RowConstraint _lhsOannotatedTree _lhsOoriginalTree ))
sem_RowConstraint_NotNullConstraint ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  NotNullConstraint ann_
                  {-# LINE 7010 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7015 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  NotNullConstraint ann_
                  {-# LINE 7020 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7025 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_NullConstraint ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  NullConstraint ann_
                  {-# LINE 7036 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7041 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  NullConstraint ann_
                  {-# LINE 7046 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7051 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowCheckConstraint ann_ expression_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _expressionOlib :: LocalIdentifierBindings
              _expressionOenv :: Environment
              expression_1 :: T_Expression_1 
              _expressionIoriginalTree :: Expression
              _expressionIannotatedTree :: Expression
              _expressionIliftedColumnName :: String
              _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- copy rule (down)
              _expressionOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7068 "AstInternal.hs" #-}
              -- copy rule (down)
              _expressionOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7073 "AstInternal.hs" #-}
              ( _expressionIoriginalTree,expression_1) =
                  (expression_ )
              ( _expressionIannotatedTree,_expressionIliftedColumnName) =
                  (expression_1 _expressionOenv _expressionOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RowCheckConstraint ann_ _expressionIannotatedTree
                  {-# LINE 7082 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7087 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RowCheckConstraint ann_ _expressionIoriginalTree
                  {-# LINE 7092 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7097 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowPrimaryKeyConstraint ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint ann_
                  {-# LINE 7108 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7113 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint ann_
                  {-# LINE 7118 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7123 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowReferenceConstraint ann_ table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _onDeleteOlib :: LocalIdentifierBindings
              _onDeleteOenv :: Environment
              _onDeleteIannotatedTree :: Cascade
              _onDeleteIoriginalTree :: Cascade
              _onUpdateOlib :: LocalIdentifierBindings
              _onUpdateOenv :: Environment
              _onUpdateIannotatedTree :: Cascade
              _onUpdateIoriginalTree :: Cascade
              _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- copy rule (down)
              _onDeleteOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7142 "AstInternal.hs" #-}
              -- copy rule (down)
              _onDeleteOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7147 "AstInternal.hs" #-}
              ( _onDeleteIannotatedTree,_onDeleteIoriginalTree) =
                  (onDelete_ _onDeleteOenv _onDeleteOlib )
              -- copy rule (down)
              _onUpdateOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7154 "AstInternal.hs" #-}
              -- copy rule (down)
              _onUpdateOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7159 "AstInternal.hs" #-}
              ( _onUpdateIannotatedTree,_onUpdateIoriginalTree) =
                  (onUpdate_ _onUpdateOenv _onUpdateOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RowReferenceConstraint ann_ table_ att_ _onUpdateIannotatedTree _onDeleteIannotatedTree
                  {-# LINE 7166 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7171 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RowReferenceConstraint ann_ table_ att_ _onUpdateIoriginalTree _onDeleteIoriginalTree
                  {-# LINE 7176 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7181 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowUniqueConstraint ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RowUniqueConstraint ann_
                  {-# LINE 7192 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7197 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RowUniqueConstraint ann_
                  {-# LINE 7202 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7207 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- RowConstraintList -------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : RowConstraint 
         child tl             : RowConstraintList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type RowConstraintList  = [(RowConstraint)]
-- cata
sem_RowConstraintList :: RowConstraintList  ->
                         T_RowConstraintList 
sem_RowConstraintList list  =
    (Prelude.foldr sem_RowConstraintList_Cons sem_RowConstraintList_Nil (Prelude.map sem_RowConstraint list) )
-- semantic domain
type T_RowConstraintList  = Environment ->
                            LocalIdentifierBindings ->
                            ( RowConstraintList,RowConstraintList)
data Inh_RowConstraintList  = Inh_RowConstraintList {env_Inh_RowConstraintList :: Environment,lib_Inh_RowConstraintList :: LocalIdentifierBindings}
data Syn_RowConstraintList  = Syn_RowConstraintList {annotatedTree_Syn_RowConstraintList :: RowConstraintList,originalTree_Syn_RowConstraintList :: RowConstraintList}
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_RowConstraintList _lhsOannotatedTree _lhsOoriginalTree ))
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: RowConstraintList
              _tlIoriginalTree :: RowConstraintList
              _hdIannotatedTree :: RowConstraint
              _hdIoriginalTree :: RowConstraint
              _lhsOannotatedTree :: RowConstraintList
              _lhsOoriginalTree :: RowConstraintList
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7266 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7271 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7276 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7281 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOenv _tlOlib )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 7290 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7295 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 7300 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7305 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraintList_Nil  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraintList
              _lhsOoriginalTree :: RowConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 7316 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7321 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 7326 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7331 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- SelectExpression --------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         libUpdates           : [LocalIdentifierBindingsUpdate]
   alternatives:
      alternative CombineSelect:
         child ann            : {Annotation}
         child ctype          : CombineType 
         child sel1           : SelectExpression 
         child sel2           : SelectExpression 
         visit 0:
            local originalTree : _
         visit 1:
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
            local originalTree : _
         visit 1:
            local newLib      : _
            local backTree    : _
            local tpe         : _
      alternative Values:
         child ann            : {Annotation}
         child vll            : ExpressionListList 
         visit 0:
            local originalTree : _
         visit 1:
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
type T_SelectExpression  = ( SelectExpression,T_SelectExpression_1 )
type T_SelectExpression_1  = Environment ->
                             LocalIdentifierBindings ->
                             ( SelectExpression,([LocalIdentifierBindingsUpdate]))
data Inh_SelectExpression  = Inh_SelectExpression {env_Inh_SelectExpression :: Environment,lib_Inh_SelectExpression :: LocalIdentifierBindings}
data Syn_SelectExpression  = Syn_SelectExpression {annotatedTree_Syn_SelectExpression :: SelectExpression,libUpdates_Syn_SelectExpression :: [LocalIdentifierBindingsUpdate],originalTree_Syn_SelectExpression :: SelectExpression}
wrap_SelectExpression :: T_SelectExpression  ->
                         Inh_SelectExpression  ->
                         Syn_SelectExpression 
wrap_SelectExpression sem (Inh_SelectExpression _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOlibUpdates) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_SelectExpression _lhsOannotatedTree _lhsOlibUpdates _lhsOoriginalTree ))
sem_SelectExpression_CombineSelect ann_ ctype_ sel1_ sel2_  =
    (let sel2_1 :: T_SelectExpression_1 
         _sel2IoriginalTree :: SelectExpression
         sel1_1 :: T_SelectExpression_1 
         _sel1IoriginalTree :: SelectExpression
         ctype_1 :: T_CombineType_1 
         _ctypeIoriginalTree :: CombineType
         _lhsOoriginalTree :: SelectExpression
         ( _sel2IoriginalTree,sel2_1) =
             (sel2_ )
         ( _sel1IoriginalTree,sel1_1) =
             (sel1_ )
         ( _ctypeIoriginalTree,ctype_1) =
             (ctype_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             CombineSelect ann_ _ctypeIoriginalTree _sel1IoriginalTree _sel2IoriginalTree
             {-# LINE 7430 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 7435 "AstInternal.hs" #-}
         ( sem_SelectExpression_1) =
             (sem_SelectExpression_CombineSelect_1 sel2_1 sel1_1 ctype_1 ann_ )
     in  ( _lhsOoriginalTree,sem_SelectExpression_1))
sem_SelectExpression_CombineSelect_1 sel2_1 sel1_1 ctype_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _sel2Olib :: LocalIdentifierBindings
              _sel2Oenv :: Environment
              _sel1Olib :: LocalIdentifierBindings
              _sel1Oenv :: Environment
              _sel2IannotatedTree :: SelectExpression
              _sel2IlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _sel1IannotatedTree :: SelectExpression
              _sel1IlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _ctypeOlib :: LocalIdentifierBindings
              _ctypeOenv :: Environment
              _ctypeIannotatedTree :: CombineType
              _lhsOannotatedTree :: SelectExpression
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _sel2Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7459 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2Oenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7464 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7469 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Oenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7474 "AstInternal.hs" #-}
              ( _sel2IannotatedTree,_sel2IlibUpdates) =
                  (sel2_1 _sel2Oenv _sel2Olib )
              ( _sel1IannotatedTree,_sel1IlibUpdates) =
                  (sel1_1 _sel1Oenv _sel1Olib )
              -- copy rule (down)
              _ctypeOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7483 "AstInternal.hs" #-}
              -- copy rule (down)
              _ctypeOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7488 "AstInternal.hs" #-}
              ( _ctypeIannotatedTree) =
                  (ctype_1 _ctypeOenv _ctypeOlib )
              -- "./TypeChecking/SelectStatement.ag"(line 133, column 9)
              _backTree =
                  {-# LINE 133 "./TypeChecking/SelectStatement.ag" #-}
                  CombineSelect ann_ _ctypeIannotatedTree
                                _sel1IannotatedTree
                                _sel2IannotatedTree
                  {-# LINE 7497 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 128, column 9)
              _tpe =
                  {-# LINE 128 "./TypeChecking/SelectStatement.ag" #-}
                  let sel1t = getTypeAnnotation _sel1IannotatedTree
                      sel2t = getTypeAnnotation _sel2IannotatedTree
                  in dependsOnRTpe [sel1t, sel2t] $
                        typeCheckCombineSelect _lhsIenv sel1t sel2t
                  {-# LINE 7505 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 21, column 9)
              _lhsOannotatedTree =
                  {-# LINE 21 "./TypeChecking/SelectStatement.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7513 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 87, column 11)
              _lhsOlibUpdates =
                  {-# LINE 87 "./TypeChecking/SelectLists.ag" #-}
                  []
                  {-# LINE 7518 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
sem_SelectExpression_Select ann_ selDistinct_ selSelectList_ selTref_ selWhere_ selGroupBy_ selHaving_ selOrderBy_ selDir_ selLimit_ selOffset_  =
    (let selOffset_1 :: T_MaybeExpression_1 
         _selOffsetIoriginalTree :: MaybeExpression
         selLimit_1 :: T_MaybeExpression_1 
         _selLimitIoriginalTree :: MaybeExpression
         selDir_1 :: T_Direction_1 
         _selDirIoriginalTree :: Direction
         selOrderBy_1 :: T_ExpressionList_1 
         _selOrderByIoriginalTree :: ExpressionList
         selHaving_1 :: T_MaybeBoolExpression_1 
         _selHavingIoriginalTree :: MaybeBoolExpression
         selGroupBy_1 :: T_ExpressionList_1 
         _selGroupByIoriginalTree :: ExpressionList
         selWhere_1 :: T_MaybeBoolExpression_1 
         _selWhereIoriginalTree :: MaybeBoolExpression
         selTref_1 :: T_MaybeTableRef_1 
         _selTrefIoriginalTree :: MaybeTableRef
         selSelectList_1 :: T_SelectList_1 
         _selSelectListIoriginalTree :: SelectList
         selDistinct_1 :: T_Distinct_1 
         _selDistinctIoriginalTree :: Distinct
         _lhsOoriginalTree :: SelectExpression
         ( _selOffsetIoriginalTree,selOffset_1) =
             (selOffset_ )
         ( _selLimitIoriginalTree,selLimit_1) =
             (selLimit_ )
         ( _selDirIoriginalTree,selDir_1) =
             (selDir_ )
         ( _selOrderByIoriginalTree,selOrderBy_1) =
             (selOrderBy_ )
         ( _selHavingIoriginalTree,selHaving_1) =
             (selHaving_ )
         ( _selGroupByIoriginalTree,selGroupBy_1) =
             (selGroupBy_ )
         ( _selWhereIoriginalTree,selWhere_1) =
             (selWhere_ )
         ( _selTrefIoriginalTree,selTref_1) =
             (selTref_ )
         ( _selSelectListIoriginalTree,selSelectList_1) =
             (selSelectList_ )
         ( _selDistinctIoriginalTree,selDistinct_1) =
             (selDistinct_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Select ann_ _selDistinctIoriginalTree _selSelectListIoriginalTree _selTrefIoriginalTree _selWhereIoriginalTree _selGroupByIoriginalTree _selHavingIoriginalTree _selOrderByIoriginalTree _selDirIoriginalTree _selLimitIoriginalTree _selOffsetIoriginalTree
             {-# LINE 7566 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 7571 "AstInternal.hs" #-}
         ( sem_SelectExpression_1) =
             (sem_SelectExpression_Select_1 selTref_1 selOffset_1 selLimit_1 selDir_1 selOrderBy_1 selHaving_1 selGroupBy_1 selWhere_1 selSelectList_1 selDistinct_1 ann_ )
     in  ( _lhsOoriginalTree,sem_SelectExpression_1))
sem_SelectExpression_Select_1 selTref_1 selOffset_1 selLimit_1 selDir_1 selOrderBy_1 selHaving_1 selGroupBy_1 selWhere_1 selSelectList_1 selDistinct_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _selOffsetOlib :: LocalIdentifierBindings
              _selOffsetOenv :: Environment
              _selLimitOlib :: LocalIdentifierBindings
              _selLimitOenv :: Environment
              _selOrderByOenv :: Environment
              _selHavingOlib :: LocalIdentifierBindings
              _selHavingOenv :: Environment
              _selGroupByOenv :: Environment
              _selWhereOenv :: Environment
              _selTrefOlib :: LocalIdentifierBindings
              _selTrefOenv :: Environment
              _selSelectListOenv :: Environment
              _selTrefIannotatedTree :: MaybeTableRef
              _selTrefIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _selOrderByOlib :: LocalIdentifierBindings
              _selGroupByOlib :: LocalIdentifierBindings
              _selWhereOlib :: LocalIdentifierBindings
              _selSelectListOlib :: LocalIdentifierBindings
              _selOffsetIannotatedTree :: MaybeExpression
              _selLimitIannotatedTree :: MaybeExpression
              _selDirOlib :: LocalIdentifierBindings
              _selDirOenv :: Environment
              _selDirIannotatedTree :: Direction
              _selOrderByIannotatedTree :: ExpressionList
              _selOrderByItypeList :: ([Type])
              _selHavingIannotatedTree :: MaybeBoolExpression
              _selGroupByIannotatedTree :: ExpressionList
              _selGroupByItypeList :: ([Type])
              _selWhereIannotatedTree :: MaybeBoolExpression
              _selSelectListIannotatedTree :: SelectList
              _selSelectListIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _selSelectListIlistType :: ([(String,Type)])
              _selDistinctOlib :: LocalIdentifierBindings
              _selDistinctOenv :: Environment
              _selDistinctIannotatedTree :: Distinct
              _lhsOannotatedTree :: SelectExpression
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _selOffsetOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7619 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7624 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7629 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7634 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOrderByOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7639 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7644 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7649 "AstInternal.hs" #-}
              -- copy rule (down)
              _selGroupByOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7654 "AstInternal.hs" #-}
              -- copy rule (down)
              _selWhereOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7659 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7664 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7669 "AstInternal.hs" #-}
              -- copy rule (down)
              _selSelectListOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7674 "AstInternal.hs" #-}
              ( _selTrefIannotatedTree,_selTrefIlibUpdates) =
                  (selTref_1 _selTrefOenv _selTrefOlib )
              -- "./TypeChecking/SelectStatement.ag"(line 92, column 10)
              _newLib =
                  {-# LINE 92 "./TypeChecking/SelectStatement.ag" #-}
                  case updateBindings _lhsIlib _lhsIenv _selTrefIlibUpdates of
                    Left x -> error $ show x
                    Right e -> e
                  {-# LINE 7683 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 98, column 10)
              _selOrderByOlib =
                  {-# LINE 98 "./TypeChecking/SelectStatement.ag" #-}
                  _newLib
                  {-# LINE 7688 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 97, column 10)
              _selGroupByOlib =
                  {-# LINE 97 "./TypeChecking/SelectStatement.ag" #-}
                  _newLib
                  {-# LINE 7693 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 96, column 10)
              _selWhereOlib =
                  {-# LINE 96 "./TypeChecking/SelectStatement.ag" #-}
                  _newLib
                  {-# LINE 7698 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 95, column 10)
              _selSelectListOlib =
                  {-# LINE 95 "./TypeChecking/SelectStatement.ag" #-}
                  _newLib
                  {-# LINE 7703 "AstInternal.hs" #-}
              ( _selOffsetIannotatedTree) =
                  (selOffset_1 _selOffsetOenv _selOffsetOlib )
              ( _selLimitIannotatedTree) =
                  (selLimit_1 _selLimitOenv _selLimitOlib )
              -- copy rule (down)
              _selDirOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7712 "AstInternal.hs" #-}
              -- copy rule (down)
              _selDirOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7717 "AstInternal.hs" #-}
              ( _selDirIannotatedTree) =
                  (selDir_1 _selDirOenv _selDirOlib )
              ( _selOrderByIannotatedTree,_selOrderByItypeList) =
                  (selOrderBy_1 _selOrderByOenv _selOrderByOlib )
              ( _selHavingIannotatedTree) =
                  (selHaving_1 _selHavingOenv _selHavingOlib )
              ( _selGroupByIannotatedTree,_selGroupByItypeList) =
                  (selGroupBy_1 _selGroupByOenv _selGroupByOlib )
              ( _selWhereIannotatedTree) =
                  (selWhere_1 _selWhereOenv _selWhereOlib )
              ( _selSelectListIannotatedTree,_selSelectListIlibUpdates,_selSelectListIlistType) =
                  (selSelectList_1 _selSelectListOenv _selSelectListOlib )
              -- copy rule (down)
              _selDistinctOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7734 "AstInternal.hs" #-}
              -- copy rule (down)
              _selDistinctOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7739 "AstInternal.hs" #-}
              ( _selDistinctIannotatedTree) =
                  (selDistinct_1 _selDistinctOenv _selDistinctOlib )
              -- "./TypeChecking/SelectStatement.ag"(line 116, column 9)
              _backTree =
                  {-# LINE 116 "./TypeChecking/SelectStatement.ag" #-}
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
                  {-# LINE 7756 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 109, column 9)
              _tpe =
                  {-# LINE 109 "./TypeChecking/SelectStatement.ag" #-}
                  do
                  Right $ case _selSelectListIlistType of
                            [(_,Pseudo Void)] -> Pseudo Void
                            _ -> SetOfType $ CompositeType _selSelectListIlistType
                  {-# LINE 7764 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 21, column 9)
              _lhsOannotatedTree =
                  {-# LINE 21 "./TypeChecking/SelectStatement.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7772 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  {-# LINE 85 "./TypeChecking/SelectLists.ag" #-}
                  _selSelectListIlibUpdates
                  {-# LINE 7777 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
sem_SelectExpression_Values ann_ vll_  =
    (let vll_1 :: T_ExpressionListList_1 
         _vllIoriginalTree :: ExpressionListList
         _lhsOoriginalTree :: SelectExpression
         ( _vllIoriginalTree,vll_1) =
             (vll_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Values ann_ _vllIoriginalTree
             {-# LINE 7789 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 7794 "AstInternal.hs" #-}
         ( sem_SelectExpression_1) =
             (sem_SelectExpression_Values_1 vll_1 ann_ )
     in  ( _lhsOoriginalTree,sem_SelectExpression_1))
sem_SelectExpression_Values_1 vll_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _vllOlib :: LocalIdentifierBindings
              _vllOenv :: Environment
              _vllIannotatedTree :: ExpressionListList
              _vllItypeListList :: ([[Type]])
              _lhsOannotatedTree :: SelectExpression
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _vllOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7811 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7816 "AstInternal.hs" #-}
              ( _vllIannotatedTree,_vllItypeListList) =
                  (vll_1 _vllOenv _vllOlib )
              -- "./TypeChecking/SelectStatement.ag"(line 107, column 9)
              _backTree =
                  {-# LINE 107 "./TypeChecking/SelectStatement.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 7823 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 104, column 9)
              _tpe =
                  {-# LINE 104 "./TypeChecking/SelectStatement.ag" #-}
                  typeCheckValuesExpr
                              _lhsIenv
                              _vllItypeListList
                  {-# LINE 7830 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 21, column 9)
              _lhsOannotatedTree =
                  {-# LINE 21 "./TypeChecking/SelectStatement.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    Nothing
                  {-# LINE 7838 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 87, column 11)
              _lhsOlibUpdates =
                  {-# LINE 87 "./TypeChecking/SelectLists.ag" #-}
                  []
                  {-# LINE 7843 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
-- SelectItem --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         columnName           : String
         itemType             : Type
   alternatives:
      alternative SelExp:
         child ann            : {Annotation}
         child ex             : Expression 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative SelectItem:
         child ann            : {Annotation}
         child ex             : Expression 
         child name           : {String}
         visit 0:
            local originalTree : _
         visit 1:
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
type T_SelectItem  = ( SelectItem,T_SelectItem_1 )
type T_SelectItem_1  = Environment ->
                       LocalIdentifierBindings ->
                       ( SelectItem,String,Type)
data Inh_SelectItem  = Inh_SelectItem {env_Inh_SelectItem :: Environment,lib_Inh_SelectItem :: LocalIdentifierBindings}
data Syn_SelectItem  = Syn_SelectItem {annotatedTree_Syn_SelectItem :: SelectItem,columnName_Syn_SelectItem :: String,itemType_Syn_SelectItem :: Type,originalTree_Syn_SelectItem :: SelectItem}
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOitemType) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_SelectItem _lhsOannotatedTree _lhsOcolumnName _lhsOitemType _lhsOoriginalTree ))
sem_SelectItem_SelExp ann_ ex_  =
    (let ex_1 :: T_Expression_1 
         _exIoriginalTree :: Expression
         _lhsOoriginalTree :: SelectItem
         ( _exIoriginalTree,ex_1) =
             (ex_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             SelExp ann_ _exIoriginalTree
             {-# LINE 7911 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 7916 "AstInternal.hs" #-}
         ( sem_SelectItem_1) =
             (sem_SelectItem_SelExp_1 ex_1 ann_ )
     in  ( _lhsOoriginalTree,sem_SelectItem_1))
sem_SelectItem_SelExp_1 ex_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _exOlib :: LocalIdentifierBindings
              _exOenv :: Environment
              _exIannotatedTree :: Expression
              _exIliftedColumnName :: String
              _lhsOannotatedTree :: SelectItem
              _lhsOcolumnName :: String
              _lhsOitemType :: Type
              -- copy rule (down)
              _exOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7934 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 7939 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIliftedColumnName) =
                  (ex_1 _exOenv _exOlib )
              -- "./TypeChecking/SelectLists.ag"(line 13, column 9)
              _annotatedTree =
                  {-# LINE 13 "./TypeChecking/SelectLists.ag" #-}
                  SelExp ann_ $ fixStar _exIannotatedTree
                  {-# LINE 7946 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7951 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 181, column 14)
              _lhsOcolumnName =
                  {-# LINE 181 "./TypeChecking/SelectLists.ag" #-}
                  case _exIliftedColumnName of
                    "" -> "?column?"
                    s -> s
                  {-# LINE 7958 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 35, column 9)
              _lhsOitemType =
                  {-# LINE 35 "./TypeChecking/SelectLists.ag" #-}
                  getTypeAnnotation _exIannotatedTree
                  {-# LINE 7963 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOitemType)))
sem_SelectItem_SelectItem ann_ ex_ name_  =
    (let ex_1 :: T_Expression_1 
         _exIoriginalTree :: Expression
         _lhsOoriginalTree :: SelectItem
         ( _exIoriginalTree,ex_1) =
             (ex_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             SelectItem ann_ _exIoriginalTree name_
             {-# LINE 7975 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 7980 "AstInternal.hs" #-}
         ( sem_SelectItem_1) =
             (sem_SelectItem_SelectItem_1 ex_1 name_ ann_ )
     in  ( _lhsOoriginalTree,sem_SelectItem_1))
sem_SelectItem_SelectItem_1 ex_1 name_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _exOlib :: LocalIdentifierBindings
              _exOenv :: Environment
              _exIannotatedTree :: Expression
              _exIliftedColumnName :: String
              _lhsOannotatedTree :: SelectItem
              _lhsOcolumnName :: String
              _lhsOitemType :: Type
              -- copy rule (down)
              _exOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7998 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8003 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIliftedColumnName) =
                  (ex_1 _exOenv _exOlib )
              -- "./TypeChecking/SelectLists.ag"(line 15, column 9)
              _annotatedTree =
                  {-# LINE 15 "./TypeChecking/SelectLists.ag" #-}
                  SelectItem ann_ (fixStar _exIannotatedTree) name_
                  {-# LINE 8010 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8015 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 184, column 18)
              _lhsOcolumnName =
                  {-# LINE 184 "./TypeChecking/SelectLists.ag" #-}
                  name_
                  {-# LINE 8020 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 35, column 9)
              _lhsOitemType =
                  {-# LINE 35 "./TypeChecking/SelectLists.ag" #-}
                  getTypeAnnotation _exIannotatedTree
                  {-# LINE 8025 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcolumnName,_lhsOitemType)))
-- SelectItemList ----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         listType             : [(String,Type)]
   alternatives:
      alternative Cons:
         child hd             : SelectItem 
         child tl             : SelectItemList 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
-}
type SelectItemList  = [(SelectItem)]
-- cata
sem_SelectItemList :: SelectItemList  ->
                      T_SelectItemList 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList  = ( SelectItemList,T_SelectItemList_1 )
type T_SelectItemList_1  = Environment ->
                           LocalIdentifierBindings ->
                           ( SelectItemList,([(String,Type)]))
data Inh_SelectItemList  = Inh_SelectItemList {env_Inh_SelectItemList :: Environment,lib_Inh_SelectItemList :: LocalIdentifierBindings}
data Syn_SelectItemList  = Syn_SelectItemList {annotatedTree_Syn_SelectItemList :: SelectItemList,listType_Syn_SelectItemList :: [(String,Type)],originalTree_Syn_SelectItemList :: SelectItemList}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOlistType) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_SelectItemList _lhsOannotatedTree _lhsOlistType _lhsOoriginalTree ))
sem_SelectItemList_Cons hd_ tl_  =
    (let tl_1 :: T_SelectItemList_1 
         _tlIoriginalTree :: SelectItemList
         hd_1 :: T_SelectItem_1 
         _hdIoriginalTree :: SelectItem
         _lhsOoriginalTree :: SelectItemList
         ( _tlIoriginalTree,tl_1) =
             (tl_ )
         ( _hdIoriginalTree,hd_1) =
             (hd_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             (:) _hdIoriginalTree _tlIoriginalTree
             {-# LINE 8089 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 8094 "AstInternal.hs" #-}
         ( sem_SelectItemList_1) =
             (sem_SelectItemList_Cons_1 tl_1 hd_1 )
     in  ( _lhsOoriginalTree,sem_SelectItemList_1))
sem_SelectItemList_Cons_1 tl_1 hd_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: SelectItemList
              _tlIlistType :: ([(String,Type)])
              _hdIannotatedTree :: SelectItem
              _hdIcolumnName :: String
              _hdIitemType :: Type
              _lhsOannotatedTree :: SelectItemList
              _lhsOlistType :: ([(String,Type)])
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8116 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8121 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8126 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8131 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIlistType) =
                  (tl_1 _tlOenv _tlOlib )
              ( _hdIannotatedTree,_hdIcolumnName,_hdIitemType) =
                  (hd_1 _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8140 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8145 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 29, column 12)
              _lhsOlistType =
                  {-# LINE 29 "./TypeChecking/SelectLists.ag" #-}
                  expandStar _lhsIlib _hdIcolumnName _hdIitemType _tlIlistType
                  {-# LINE 8150 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType)))
sem_SelectItemList_Nil  =
    (let _lhsOoriginalTree :: SelectItemList
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             []
             {-# LINE 8158 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 8163 "AstInternal.hs" #-}
         ( sem_SelectItemList_1) =
             (sem_SelectItemList_Nil_1 )
     in  ( _lhsOoriginalTree,sem_SelectItemList_1))
sem_SelectItemList_Nil_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: SelectItemList
              _lhsOlistType :: ([(String,Type)])
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8176 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8181 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 30, column 11)
              _lhsOlistType =
                  {-# LINE 30 "./TypeChecking/SelectLists.ag" #-}
                  []
                  {-# LINE 8186 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType)))
-- SelectList --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         libUpdates           : [LocalIdentifierBindingsUpdate]
         listType             : [(String,Type)]
   alternatives:
      alternative SelectList:
         child ann            : {Annotation}
         child items          : SelectItemList 
         child into           : StringList 
         visit 0:
            local originalTree : _
         visit 1:
            local stuff       : _
            local errs        : _
-}
data SelectList  = SelectList (Annotation) (SelectItemList) (StringList) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _ann _items _into )  =
    (sem_SelectList_SelectList _ann (sem_SelectItemList _items ) (sem_StringList _into ) )
-- semantic domain
type T_SelectList  = ( SelectList,T_SelectList_1 )
type T_SelectList_1  = Environment ->
                       LocalIdentifierBindings ->
                       ( SelectList,([LocalIdentifierBindingsUpdate]),([(String,Type)]))
data Inh_SelectList  = Inh_SelectList {env_Inh_SelectList :: Environment,lib_Inh_SelectList :: LocalIdentifierBindings}
data Syn_SelectList  = Syn_SelectList {annotatedTree_Syn_SelectList :: SelectList,libUpdates_Syn_SelectList :: [LocalIdentifierBindingsUpdate],listType_Syn_SelectList :: [(String,Type)],originalTree_Syn_SelectList :: SelectList}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOlistType) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_SelectList _lhsOannotatedTree _lhsOlibUpdates _lhsOlistType _lhsOoriginalTree ))
sem_SelectList_SelectList ann_ items_ into_  =
    (let into_1 :: T_StringList_1 
         _intoIoriginalTree :: StringList
         items_1 :: T_SelectItemList_1 
         _itemsIoriginalTree :: SelectItemList
         _lhsOoriginalTree :: SelectList
         ( _intoIoriginalTree,into_1) =
             (into_ )
         ( _itemsIoriginalTree,items_1) =
             (items_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             SelectList ann_ _itemsIoriginalTree _intoIoriginalTree
             {-# LINE 8249 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 8254 "AstInternal.hs" #-}
         ( sem_SelectList_1) =
             (sem_SelectList_SelectList_1 into_1 items_1 ann_ )
     in  ( _lhsOoriginalTree,sem_SelectList_1))
sem_SelectList_SelectList_1 into_1 items_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _itemsOlib :: LocalIdentifierBindings
              _itemsOenv :: Environment
              _intoOlib :: LocalIdentifierBindings
              _intoOenv :: Environment
              _intoIannotatedTree :: StringList
              _intoIstrings :: ([String])
              _itemsIannotatedTree :: SelectItemList
              _itemsIlistType :: ([(String,Type)])
              _lhsOannotatedTree :: SelectList
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOlistType :: ([(String,Type)])
              -- copy rule (down)
              _itemsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8276 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8281 "AstInternal.hs" #-}
              -- copy rule (down)
              _intoOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8286 "AstInternal.hs" #-}
              -- copy rule (down)
              _intoOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8291 "AstInternal.hs" #-}
              ( _intoIannotatedTree,_intoIstrings) =
                  (into_1 _intoOenv _intoOlib )
              ( _itemsIannotatedTree,_itemsIlistType) =
                  (items_1 _itemsOenv _itemsOlib )
              -- "./TypeChecking/SelectLists.ag"(line 45, column 9)
              _stuff =
                  {-# LINE 45 "./TypeChecking/SelectLists.ag" #-}
                  case () of
                    _ | null sl -> ([],Nothing)
                      | not (null targetTypeErrs) -> (targetTypeErrs,Nothing)
                      | (case targetTypes of
                           [PgRecord _] -> True
                           _ -> False) -> ([],Just (head sl, CompositeType _itemsIlistType))
                      | matchingComposite /= Left [] -> (fromLeft [] matchingComposite,Nothing)
                      | length sl /= length _itemsIlistType -> ([WrongNumberOfColumns],Nothing)
                      | not (null assignErrs) -> (assignErrs,Nothing)
                      | otherwise -> ([],Nothing)
                  where
                    targetTypeEithers = map (libLookupID _lhsIlib) sl
                    targetTypeErrs = concat $ lefts $ targetTypeEithers
                    targetTypes = rights $ targetTypeEithers
                    typePairs = zip (map snd _itemsIlistType) targetTypes
                    assignErrs = concat $ lefts $ map (uncurry $ checkAssignmentValid _lhsIenv) typePairs
                    sl = _intoIstrings
                    matchingComposite =
                        case targetTypes of
                          [t] | isCompositeType t -> checkAssignmentValid _lhsIenv (AnonymousRecordType (map snd _itemsIlistType)) t
                          _ -> Left []
                  {-# LINE 8320 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 43, column 9)
              _errs =
                  {-# LINE 43 "./TypeChecking/SelectLists.ag" #-}
                  case _stuff     of
                    (er,_) -> er
                  {-# LINE 8326 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 68, column 9)
              _lhsOannotatedTree =
                  {-# LINE 68 "./TypeChecking/SelectLists.ag" #-}
                  SelectList (ann_ ++ map TypeErrorA _errs    )
                             _itemsIannotatedTree
                             _intoIannotatedTree
                  {-# LINE 8333 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 71, column 9)
              _lhsOlibUpdates =
                  {-# LINE 71 "./TypeChecking/SelectLists.ag" #-}
                  case _stuff     of
                    (_,Just r) -> [LibStackIDs [("", [r])]]
                    _ -> []
                  {-# LINE 8340 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 41, column 9)
              _lhsOlistType =
                  {-# LINE 41 "./TypeChecking/SelectLists.ag" #-}
                  _itemsIlistType
                  {-# LINE 8345 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOlistType)))
-- SetClause ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         pairs                : [(String,Type)]
         rowSetError          : Maybe TypeError
   alternatives:
      alternative RowSetClause:
         child ann            : {Annotation}
         child atts           : StringList 
         child vals           : ExpressionList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
            local rowSetError : _
      alternative SetClause:
         child ann            : {Annotation}
         child att            : {String}
         child val            : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                    LocalIdentifierBindings ->
                    ( SetClause,SetClause,([(String,Type)]),(Maybe TypeError))
data Inh_SetClause  = Inh_SetClause {env_Inh_SetClause :: Environment,lib_Inh_SetClause :: LocalIdentifierBindings}
data Syn_SetClause  = Syn_SetClause {annotatedTree_Syn_SetClause :: SetClause,originalTree_Syn_SetClause :: SetClause,pairs_Syn_SetClause :: [(String,Type)],rowSetError_Syn_SetClause :: Maybe TypeError}
wrap_SetClause :: T_SetClause  ->
                  Inh_SetClause  ->
                  Syn_SetClause 
wrap_SetClause sem (Inh_SetClause _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOpairs,_lhsOrowSetError) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_SetClause _lhsOannotatedTree _lhsOoriginalTree _lhsOpairs _lhsOrowSetError ))
sem_SetClause_RowSetClause ann_ atts_ vals_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _valsOlib :: LocalIdentifierBindings
              _valsOenv :: Environment
              vals_1 :: T_ExpressionList_1 
              _valsIoriginalTree :: ExpressionList
              _valsIannotatedTree :: ExpressionList
              _valsItypeList :: ([Type])
              atts_1 :: T_StringList_1 
              _attsIoriginalTree :: StringList
              _attsOlib :: LocalIdentifierBindings
              _attsOenv :: Environment
              _attsIannotatedTree :: StringList
              _attsIstrings :: ([String])
              _lhsOannotatedTree :: SetClause
              _lhsOoriginalTree :: SetClause
              _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetError :: (Maybe TypeError)
              -- copy rule (down)
              _valsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8421 "AstInternal.hs" #-}
              -- copy rule (down)
              _valsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8426 "AstInternal.hs" #-}
              ( _valsIoriginalTree,vals_1) =
                  (vals_ )
              ( _valsIannotatedTree,_valsItypeList) =
                  (vals_1 _valsOenv _valsOlib )
              ( _attsIoriginalTree,atts_1) =
                  (atts_ )
              -- copy rule (down)
              _attsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8437 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8442 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIstrings) =
                  (atts_1 _attsOenv _attsOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RowSetClause ann_ _attsIannotatedTree _valsIannotatedTree
                  {-# LINE 8449 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8454 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RowSetClause ann_ _attsIoriginalTree _valsIoriginalTree
                  {-# LINE 8459 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8464 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 134, column 9)
              _lhsOpairs =
                  {-# LINE 134 "./TypeChecking/Dml.ag" #-}
                  zip _attsIstrings $ getRowTypes _valsItypeList
                  {-# LINE 8469 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 128, column 9)
              _rowSetError =
                  {-# LINE 128 "./TypeChecking/Dml.ag" #-}
                  let atts = _attsIstrings
                      types = getRowTypes _valsItypeList
                  in if length atts /= length types
                       then Just WrongNumberOfColumns
                       else Nothing
                  {-# LINE 8478 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOrowSetError =
                  {-# LINE 121 "./TypeChecking/Dml.ag" #-}
                  _rowSetError
                  {-# LINE 8483 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOpairs,_lhsOrowSetError)))
sem_SetClause_SetClause ann_ att_ val_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _valOlib :: LocalIdentifierBindings
              _valOenv :: Environment
              val_1 :: T_Expression_1 
              _valIoriginalTree :: Expression
              _valIannotatedTree :: Expression
              _valIliftedColumnName :: String
              _lhsOannotatedTree :: SetClause
              _lhsOoriginalTree :: SetClause
              _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetError :: (Maybe TypeError)
              -- copy rule (down)
              _valOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8502 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8507 "AstInternal.hs" #-}
              ( _valIoriginalTree,val_1) =
                  (val_ )
              ( _valIannotatedTree,_valIliftedColumnName) =
                  (val_1 _valOenv _valOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  SetClause ann_ att_ _valIannotatedTree
                  {-# LINE 8516 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8521 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SetClause ann_ att_ _valIoriginalTree
                  {-# LINE 8526 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8531 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 125, column 9)
              _lhsOpairs =
                  {-# LINE 125 "./TypeChecking/Dml.ag" #-}
                  [(att_, getTypeAnnotation _valIannotatedTree)]
                  {-# LINE 8536 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 126, column 9)
              _lhsOrowSetError =
                  {-# LINE 126 "./TypeChecking/Dml.ag" #-}
                  Nothing
                  {-# LINE 8541 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOpairs,_lhsOrowSetError)))
-- SetClauseList -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         pairs                : [(String,Type)]
         rowSetErrors         : [TypeError]
   alternatives:
      alternative Cons:
         child hd             : SetClause 
         child tl             : SetClauseList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type SetClauseList  = [(SetClause)]
-- cata
sem_SetClauseList :: SetClauseList  ->
                     T_SetClauseList 
sem_SetClauseList list  =
    (Prelude.foldr sem_SetClauseList_Cons sem_SetClauseList_Nil (Prelude.map sem_SetClause list) )
-- semantic domain
type T_SetClauseList  = Environment ->
                        LocalIdentifierBindings ->
                        ( SetClauseList,SetClauseList,([(String,Type)]),([TypeError]))
data Inh_SetClauseList  = Inh_SetClauseList {env_Inh_SetClauseList :: Environment,lib_Inh_SetClauseList :: LocalIdentifierBindings}
data Syn_SetClauseList  = Syn_SetClauseList {annotatedTree_Syn_SetClauseList :: SetClauseList,originalTree_Syn_SetClauseList :: SetClauseList,pairs_Syn_SetClauseList :: [(String,Type)],rowSetErrors_Syn_SetClauseList :: [TypeError]}
wrap_SetClauseList :: T_SetClauseList  ->
                      Inh_SetClauseList  ->
                      Syn_SetClauseList 
wrap_SetClauseList sem (Inh_SetClauseList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOpairs,_lhsOrowSetErrors) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_SetClauseList _lhsOannotatedTree _lhsOoriginalTree _lhsOpairs _lhsOrowSetErrors ))
sem_SetClauseList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _tlIannotatedTree :: SetClauseList
              _tlIoriginalTree :: SetClauseList
              _tlIpairs :: ([(String,Type)])
              _tlIrowSetErrors :: ([TypeError])
              _hdIannotatedTree :: SetClause
              _hdIoriginalTree :: SetClause
              _hdIpairs :: ([(String,Type)])
              _hdIrowSetError :: (Maybe TypeError)
              _lhsOannotatedTree :: SetClauseList
              _lhsOoriginalTree :: SetClauseList
              _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetErrors :: ([TypeError])
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8608 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8613 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8618 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 8623 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIpairs,_tlIrowSetErrors) =
                  (tl_ _tlOenv _tlOlib )
              ( _hdIannotatedTree,_hdIoriginalTree,_hdIpairs,_hdIrowSetError) =
                  (hd_ _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8632 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8637 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 8642 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8647 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 115, column 10)
              _lhsOpairs =
                  {-# LINE 115 "./TypeChecking/Dml.ag" #-}
                  _hdIpairs ++ _tlIpairs
                  {-# LINE 8652 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 116, column 10)
              _lhsOrowSetErrors =
                  {-# LINE 116 "./TypeChecking/Dml.ag" #-}
                  maybeToList _hdIrowSetError ++ _tlIrowSetErrors
                  {-# LINE 8657 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOpairs,_lhsOrowSetErrors)))
sem_SetClauseList_Nil  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: SetClauseList
              _lhsOoriginalTree :: SetClauseList
              _lhsOpairs :: ([(String,Type)])
              _lhsOrowSetErrors :: ([TypeError])
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8670 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8675 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8680 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8685 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 117, column 9)
              _lhsOpairs =
                  {-# LINE 117 "./TypeChecking/Dml.ag" #-}
                  []
                  {-# LINE 8690 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 118, column 9)
              _lhsOrowSetErrors =
                  {-# LINE 118 "./TypeChecking/Dml.ag" #-}
                  []
                  {-# LINE 8695 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOpairs,_lhsOrowSetErrors)))
-- Statement ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         envUpdates           : [EnvironmentUpdate]
         libUpdates           : [LocalIdentifierBindingsUpdate]
   visit 1:
      inherited attribute:
         inProducedEnv        : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Assignment:
         child ann            : {Annotation}
         child target         : {String}
         child value          : Expression 
         visit 0:
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local statementInfo : _
            local backTree    : _
            local tpe         : _
            local originalTree : _
            intra envUpdates  : _
      alternative CaseStatement:
         child ann            : {Annotation}
         child val            : Expression 
         child cases          : ExpressionListStatementListPairList 
         child els            : StatementList 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative ContinueStatement:
         child ann            : {Annotation}
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative Copy:
         child ann            : {Annotation}
         child table          : {String}
         child targetCols     : StringList 
         child source         : CopySource 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative CopyData:
         child ann            : {Annotation}
         child insData        : {String}
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative CreateDomain:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         child check          : MaybeBoolExpression 
         visit 0:
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local statementInfo : _
            local backTree    : _
            local tpe         : _
            local originalTree : _
            intra envUpdates  : _
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
            local parameterTypes : _
            local tpe         : _
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local statementInfo : _
            local backTree    : _
            local originalTree : _
            intra envUpdates  : _
            intra tpe         : _
      alternative CreateTable:
         child ann            : {Annotation}
         child name           : {String}
         child atts           : AttributeDefList 
         child cons           : ConstraintList 
         visit 0:
            local attrTypes   : _
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local backTree    : _
            local statementInfo : _
            local tpe         : _
            local originalTree : _
            intra attrTypes   : _
            intra envUpdates  : _
      alternative CreateTableAs:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : SelectExpression 
         visit 0:
            local selType     : _
            local attrs       : _
            local tpe         : _
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local statementInfo : _
            local backTree    : _
            local originalTree : _
            intra envUpdates  : _
            intra tpe         : _
      alternative CreateType:
         child ann            : {Annotation}
         child name           : {String}
         child atts           : TypeAttributeDefList 
         visit 0:
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local statementInfo : _
            local backTree    : _
            local tpe         : _
            local originalTree : _
            intra envUpdates  : _
      alternative CreateView:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : SelectExpression 
         visit 0:
            local attrs       : _
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local statementInfo : _
            local backTree    : _
            local tpe         : _
            local originalTree : _
            intra envUpdates  : _
      alternative Delete:
         child ann            : {Annotation}
         child table          : {String}
         child whr            : MaybeBoolExpression 
         child returning      : MaybeSelectList 
         visit 0:
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local lib         : _
            local backTree    : _
            local statementInfo : _
            local tpe         : _
            local originalTree : _
            intra envUpdates  : _
      alternative DropFunction:
         child ann            : {Annotation}
         child ifE            : IfExists 
         child sigs           : StringStringListPairList 
         child cascade        : Cascade 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative DropSomething:
         child ann            : {Annotation}
         child dropType       : DropType 
         child ifE            : IfExists 
         child names          : StringList 
         child cascade        : Cascade 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative Execute:
         child ann            : {Annotation}
         child expr           : Expression 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative ExecuteInto:
         child ann            : {Annotation}
         child expr           : Expression 
         child targets        : StringList 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative ForIntegerStatement:
         child ann            : {Annotation}
         child var            : {String}
         child from           : Expression 
         child to             : Expression 
         child sts            : StatementList 
         visit 0:
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local statementInfo : _
            local varTypeE    : _
            local backTree    : _
            local tpe         : _
            local originalTree : _
            intra envUpdates  : _
      alternative ForSelectStatement:
         child ann            : {Annotation}
         child var            : {String}
         child sel            : SelectExpression 
         child sts            : StatementList 
         visit 0:
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local statementInfo : _
            local selType     : _
            local tpe         : _
            local backTree    : _
            local originalTree : _
            intra envUpdates  : _
      alternative If:
         child ann            : {Annotation}
         child cases          : ExpressionStatementListPairList 
         child els            : StatementList 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative Insert:
         child ann            : {Annotation}
         child table          : {String}
         child targetCols     : StringList 
         child insData        : SelectExpression 
         child returning      : MaybeSelectList 
         visit 0:
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local backTree    : _
            local columnTypes : _
            local statementInfo : _
            local tpe         : _
            local originalTree : _
            intra envUpdates  : _
      alternative NullStatement:
         child ann            : {Annotation}
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative Perform:
         child ann            : {Annotation}
         child expr           : Expression 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative Raise:
         child ann            : {Annotation}
         child level          : RaiseType 
         child message        : {String}
         child args           : ExpressionList 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative Return:
         child ann            : {Annotation}
         child value          : MaybeExpression 
         visit 0:
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local statementInfo : _
            local backTree    : _
            local tpe         : _
            local originalTree : _
            intra envUpdates  : _
      alternative ReturnNext:
         child ann            : {Annotation}
         child expr           : Expression 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative ReturnQuery:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative SelectStatement:
         child ann            : {Annotation}
         child ex             : SelectExpression 
         visit 0:
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local backTree    : _
            local statementInfo : _
            local tpe         : _
            local originalTree : _
            intra envUpdates  : _
      alternative Truncate:
         child ann            : {Annotation}
         child tables         : StringList 
         child restartIdentity : RestartIdentity 
         child cascade        : Cascade 
         visit 1:
            local annotatedTree : _
            local originalTree : _
      alternative Update:
         child ann            : {Annotation}
         child table          : {String}
         child assigns        : SetClauseList 
         child whr            : MaybeBoolExpression 
         child returning      : MaybeSelectList 
         visit 0:
            local envUpdates  : _
            local libUpdates  : _
         visit 1:
            local lib         : _
            local backTree    : _
            local columnTypes : _
            local statementInfo : _
            local tpe         : _
            local originalTree : _
            intra envUpdates  : _
      alternative WhileStatement:
         child ann            : {Annotation}
         child expr           : Expression 
         child sts            : StatementList 
         visit 1:
            local annotatedTree : _
            local originalTree : _
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
                | Delete (Annotation) (String) (MaybeBoolExpression) (MaybeSelectList) 
                | DropFunction (Annotation) (IfExists) (StringStringListPairList) (Cascade) 
                | DropSomething (Annotation) (DropType) (IfExists) (StringList) (Cascade) 
                | Execute (Annotation) (Expression) 
                | ExecuteInto (Annotation) (Expression) (StringList) 
                | ForIntegerStatement (Annotation) (String) (Expression) (Expression) (StatementList) 
                | ForSelectStatement (Annotation) (String) (SelectExpression) (StatementList) 
                | If (Annotation) (ExpressionStatementListPairList) (StatementList) 
                | Insert (Annotation) (String) (StringList) (SelectExpression) (MaybeSelectList) 
                | NullStatement (Annotation) 
                | Perform (Annotation) (Expression) 
                | Raise (Annotation) (RaiseType) (String) (ExpressionList) 
                | Return (Annotation) (MaybeExpression) 
                | ReturnNext (Annotation) (Expression) 
                | ReturnQuery (Annotation) (SelectExpression) 
                | SelectStatement (Annotation) (SelectExpression) 
                | Truncate (Annotation) (StringList) (RestartIdentity) (Cascade) 
                | Update (Annotation) (String) (SetClauseList) (MaybeBoolExpression) (MaybeSelectList) 
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
    (sem_Statement_Delete _ann _table (sem_MaybeBoolExpression _whr ) (sem_MaybeSelectList _returning ) )
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
    (sem_Statement_Insert _ann _table (sem_StringList _targetCols ) (sem_SelectExpression _insData ) (sem_MaybeSelectList _returning ) )
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
    (sem_Statement_Update _ann _table (sem_SetClauseList _assigns ) (sem_MaybeBoolExpression _whr ) (sem_MaybeSelectList _returning ) )
sem_Statement (WhileStatement _ann _expr _sts )  =
    (sem_Statement_WhileStatement _ann (sem_Expression _expr ) (sem_StatementList _sts ) )
-- semantic domain
type T_Statement  = Environment ->
                    LocalIdentifierBindings ->
                    ( ([EnvironmentUpdate]),([LocalIdentifierBindingsUpdate]),T_Statement_1 )
type T_Statement_1  = Environment ->
                      ( Statement,Statement)
data Inh_Statement  = Inh_Statement {env_Inh_Statement :: Environment,inProducedEnv_Inh_Statement :: Environment,lib_Inh_Statement :: LocalIdentifierBindings}
data Syn_Statement  = Syn_Statement {annotatedTree_Syn_Statement :: Statement,envUpdates_Syn_Statement :: [EnvironmentUpdate],libUpdates_Syn_Statement :: [LocalIdentifierBindingsUpdate],originalTree_Syn_Statement :: Statement}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIenv _lhsIinProducedEnv _lhsIlib )  =
    (let ( _lhsOenvUpdates,_lhsOlibUpdates,sem_1) =
             (sem _lhsIenv _lhsIlib )
         ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem_1 _lhsIinProducedEnv )
     in  (Syn_Statement _lhsOannotatedTree _lhsOenvUpdates _lhsOlibUpdates _lhsOoriginalTree ))
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Plpgsql.ag"(line 31, column 9)
              _envUpdates =
                  {-# LINE 31 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 9153 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 9158 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9163 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 9168 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_Assignment_1 _lhsIlib _lhsIenv value_ target_ ann_ _envUpdates )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_Assignment_1 _lhsIlib _lhsIenv value_ target_ ann_ _envUpdates  =
    (\ _lhsIinProducedEnv ->
         (let _valueOlib :: LocalIdentifierBindings
              _valueOenv :: Environment
              value_1 :: T_Expression_1 
              _valueIoriginalTree :: Expression
              _valueIannotatedTree :: Expression
              _valueIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9186 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9191 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 32, column 9)
              _statementInfo =
                  {-# LINE 32 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 9196 "AstInternal.hs" #-}
              ( _valueIoriginalTree,value_1) =
                  (value_ )
              ( _valueIannotatedTree,_valueIliftedColumnName) =
                  (value_1 _valueOenv _valueOlib )
              -- "./TypeChecking/Plpgsql.ag"(line 30, column 9)
              _backTree =
                  {-# LINE 30 "./TypeChecking/Plpgsql.ag" #-}
                  Assignment ann_ target_ _valueIannotatedTree
                  {-# LINE 9205 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 23, column 9)
              _tpe =
                  {-# LINE 23 "./TypeChecking/Plpgsql.ag" #-}
                  do
                  let fromType = getTypeAnnotation _valueIannotatedTree
                  toType <- libLookupID _lhsIlib target_
                  dependsOnRTpe [getTypeAnnotation _valueIannotatedTree, toType] $ do
                  checkAssignmentValid _lhsIenv fromType toType
                  return $ Pseudo Void
                  {-# LINE 9215 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 9224 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Assignment ann_ target_ _valueIoriginalTree
                  {-# LINE 9229 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9234 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CaseStatement ann_ val_ cases_ els_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9245 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9250 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_CaseStatement_1 _lhsIlib _lhsIenv els_ cases_ val_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_CaseStatement_1 _lhsIlib _lhsIenv els_ cases_ val_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _elsOlib :: LocalIdentifierBindings
              _elsOenv :: Environment
              _casesOlib :: LocalIdentifierBindings
              _casesOenv :: Environment
              _valOlib :: LocalIdentifierBindings
              _valOenv :: Environment
              _elsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _elsOenvUpdates :: ([EnvironmentUpdate])
              _elsIannotatedTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedEnv :: Environment
              _elsIproducedLib :: LocalIdentifierBindings
              _casesIannotatedTree :: ExpressionListStatementListPairList
              _casesIoriginalTree :: ExpressionListStatementListPairList
              val_1 :: T_Expression_1 
              _valIoriginalTree :: Expression
              _valIannotatedTree :: Expression
              _valIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9280 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9285 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9290 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9295 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9300 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9305 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 103, column 9)
              _elsOlibUpdates =
                  {-# LINE 103 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9310 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 102, column 9)
              _elsOenvUpdates =
                  {-# LINE 102 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9315 "AstInternal.hs" #-}
              ( _elsIannotatedTree,_elsIoriginalTree,_elsIproducedEnv,_elsIproducedLib) =
                  (els_ _elsOenv _elsOenvUpdates _elsOlib _elsOlibUpdates )
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  (cases_ _casesOenv _casesOlib )
              ( _valIoriginalTree,val_1) =
                  (val_ )
              ( _valIannotatedTree,_valIliftedColumnName) =
                  (val_1 _valOenv _valOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatement ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 9328 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9333 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatement ann_ _valIoriginalTree _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 9338 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9343 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ContinueStatement ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9354 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9359 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_ContinueStatement_1 ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_ContinueStatement_1 ann_  =
    (\ _lhsIinProducedEnv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ContinueStatement ann_
                  {-# LINE 9371 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9376 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ContinueStatement ann_
                  {-# LINE 9381 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9386 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9397 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9402 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_Copy_1 _lhsIlib _lhsIenv source_ targetCols_ table_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_Copy_1 _lhsIlib _lhsIenv source_ targetCols_ table_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _sourceOlib :: LocalIdentifierBindings
              _sourceOenv :: Environment
              _sourceIannotatedTree :: CopySource
              _sourceIoriginalTree :: CopySource
              targetCols_1 :: T_StringList_1 
              _targetColsIoriginalTree :: StringList
              _targetColsOlib :: LocalIdentifierBindings
              _targetColsOenv :: Environment
              _targetColsIannotatedTree :: StringList
              _targetColsIstrings :: ([String])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _sourceOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9424 "AstInternal.hs" #-}
              -- copy rule (down)
              _sourceOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9429 "AstInternal.hs" #-}
              ( _sourceIannotatedTree,_sourceIoriginalTree) =
                  (source_ _sourceOenv _sourceOlib )
              ( _targetColsIoriginalTree,targetCols_1) =
                  (targetCols_ )
              -- copy rule (down)
              _targetColsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9438 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetColsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9443 "AstInternal.hs" #-}
              ( _targetColsIannotatedTree,_targetColsIstrings) =
                  (targetCols_1 _targetColsOenv _targetColsOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Copy ann_ table_ _targetColsIannotatedTree _sourceIannotatedTree
                  {-# LINE 9450 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9455 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Copy ann_ table_ _targetColsIoriginalTree _sourceIoriginalTree
                  {-# LINE 9460 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9465 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9476 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9481 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_CopyData_1 insData_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_CopyData_1 insData_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 9493 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9498 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 9503 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9508 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateDomain ann_ name_ typ_ check_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _typOenv :: Environment
              typ_1 :: T_TypeName_1 
              _typIoriginalTree :: TypeName
              _typOlib :: LocalIdentifierBindings
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9525 "AstInternal.hs" #-}
              ( _typIoriginalTree,typ_1) =
                  (typ_ )
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9532 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_1 _typOenv _typOlib )
              -- "./TypeChecking/MiscCreates.ag"(line 67, column 9)
              _envUpdates =
                  {-# LINE 67 "./TypeChecking/MiscCreates.ag" #-}
                  [EnvCreateDomain (DomainType name_) _typInamedType]
                  {-# LINE 9539 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 9544 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9549 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 9554 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_CreateDomain_1 _lhsIenv _typInamedType _lhsIlib check_ name_ ann_ _typIannotatedTree _envUpdates _typIoriginalTree )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_CreateDomain_1 _lhsIenv _typInamedType _lhsIlib check_ name_ ann_ _typIannotatedTree _envUpdates _typIoriginalTree  =
    (\ _lhsIinProducedEnv ->
         (let _checkOenv :: Environment
              _checkOlib :: LocalIdentifierBindings
              check_1 :: T_MaybeBoolExpression_1 
              _checkIoriginalTree :: MaybeBoolExpression
              _checkIannotatedTree :: MaybeBoolExpression
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _checkOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9571 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 69, column 9)
              _checkOlib =
                  {-# LINE 69 "./TypeChecking/MiscCreates.ag" #-}
                  fromRight _lhsIlib $
                  updateBindings _lhsIlib _lhsIenv
                    [LibStackIDs [("", [("value", _typInamedType)])]]
                  {-# LINE 9578 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 66, column 9)
              _statementInfo =
                  {-# LINE 66 "./TypeChecking/MiscCreates.ag" #-}
                  []
                  {-# LINE 9583 "AstInternal.hs" #-}
              ( _checkIoriginalTree,check_1) =
                  (check_ )
              ( _checkIannotatedTree) =
                  (check_1 _checkOenv _checkOlib )
              -- "./TypeChecking/MiscCreates.ag"(line 65, column 9)
              _backTree =
                  {-# LINE 65 "./TypeChecking/MiscCreates.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree _checkIannotatedTree
                  {-# LINE 9592 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 64, column 9)
              _tpe =
                  {-# LINE 64 "./TypeChecking/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 9597 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 9606 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIoriginalTree _checkIoriginalTree
                  {-# LINE 9611 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9616 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateFunction ann_ lang_ name_ params_ rettype_ bodyQuote_ body_ vol_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _rettypeOenv :: Environment
              _paramsOenv :: Environment
              _paramsOlib :: LocalIdentifierBindings
              _paramsIannotatedTree :: ParamDefList
              _paramsIoriginalTree :: ParamDefList
              _paramsIparams :: ([(String, Type)])
              rettype_1 :: T_TypeName_1 
              _rettypeIoriginalTree :: TypeName
              _rettypeOlib :: LocalIdentifierBindings
              _rettypeIannotatedTree :: TypeName
              _rettypeInamedType :: Type
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _rettypeOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9638 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9643 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9648 "AstInternal.hs" #-}
              ( _paramsIannotatedTree,_paramsIoriginalTree,_paramsIparams) =
                  (params_ _paramsOenv _paramsOlib )
              -- "./TypeChecking/CreateFunction.ag"(line 32, column 9)
              _parameterTypes =
                  {-# LINE 32 "./TypeChecking/CreateFunction.ag" #-}
                  (map snd _paramsIparams)
                  {-# LINE 9655 "AstInternal.hs" #-}
              ( _rettypeIoriginalTree,rettype_1) =
                  (rettype_ )
              -- copy rule (down)
              _rettypeOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9662 "AstInternal.hs" #-}
              ( _rettypeIannotatedTree,_rettypeInamedType) =
                  (rettype_1 _rettypeOenv _rettypeOlib )
              -- "./TypeChecking/CreateFunction.ag"(line 23, column 9)
              _tpe =
                  {-# LINE 23 "./TypeChecking/CreateFunction.ag" #-}
                  dependsOnRTpe
                    (_rettypeInamedType : _parameterTypes    ) $
                    Right $ Pseudo Void
                  {-# LINE 9671 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 26, column 9)
              _envUpdates =
                  {-# LINE 26 "./TypeChecking/CreateFunction.ag" #-}
                  dependsOn [tpeToT _tpe    ] []
                            [EnvCreateFunction FunName
                                               name_
                                               _parameterTypes
                                               _rettypeInamedType
                                               False]
                  {-# LINE 9681 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 9686 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9691 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 9696 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_CreateFunction_1 name_ _paramsIparams _lhsIenv _lhsIlib vol_ body_ lang_ bodyQuote_ ann_ _rettypeIannotatedTree _paramsIannotatedTree _envUpdates _tpe _rettypeIoriginalTree _paramsIoriginalTree )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_CreateFunction_1 name_ _paramsIparams _lhsIenv _lhsIlib vol_ body_ lang_ bodyQuote_ ann_ _rettypeIannotatedTree _paramsIannotatedTree _envUpdates _tpe _rettypeIoriginalTree _paramsIoriginalTree  =
    (\ _lhsIinProducedEnv ->
         (let _bodyOlib :: LocalIdentifierBindings
              _bodyOenv :: Environment
              _volOlib :: LocalIdentifierBindings
              _volOenv :: Environment
              _volIannotatedTree :: Volatility
              _volIoriginalTree :: Volatility
              _bodyIannotatedTree :: FnBody
              _bodyIoriginalTree :: FnBody
              _langOlib :: LocalIdentifierBindings
              _langOenv :: Environment
              _langIannotatedTree :: Language
              _langIoriginalTree :: Language
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/CreateFunction.ag"(line 99, column 9)
              _bodyOlib =
                  {-# LINE 99 "./TypeChecking/CreateFunction.ag" #-}
                  let p = _paramsIparams
                          ++ (zip posNames $ map snd _paramsIparams)
                  in fromRight _lhsIlib $
                     updateBindings _lhsIlib _lhsIenv
                                    [LibStackIDs [("", p)
                                                 ,(name_, _paramsIparams)]]
                  where
                    posNames :: [String]
                    posNames = map (\l -> '$':show l) [1..]
                  {-# LINE 9728 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 43, column 9)
              _bodyOenv =
                  {-# LINE 43 "./TypeChecking/CreateFunction.ag" #-}
                  _lhsIinProducedEnv
                  {-# LINE 9733 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 42, column 9)
              _statementInfo =
                  {-# LINE 42 "./TypeChecking/CreateFunction.ag" #-}
                  []
                  {-# LINE 9738 "AstInternal.hs" #-}
              -- copy rule (down)
              _volOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9743 "AstInternal.hs" #-}
              -- copy rule (down)
              _volOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9748 "AstInternal.hs" #-}
              ( _volIannotatedTree,_volIoriginalTree) =
                  (vol_ _volOenv _volOlib )
              ( _bodyIannotatedTree,_bodyIoriginalTree) =
                  (body_ _bodyOenv _bodyOlib )
              -- copy rule (down)
              _langOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9757 "AstInternal.hs" #-}
              -- copy rule (down)
              _langOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9762 "AstInternal.hs" #-}
              ( _langIannotatedTree,_langIoriginalTree) =
                  (lang_ _langOenv _langOlib )
              -- "./TypeChecking/CreateFunction.ag"(line 34, column 9)
              _backTree =
                  {-# LINE 34 "./TypeChecking/CreateFunction.ag" #-}
                  CreateFunction ann_
                                 _langIannotatedTree
                                 name_
                                 _paramsIannotatedTree
                                 _rettypeIannotatedTree
                                 bodyQuote_
                                 _bodyIannotatedTree
                                 _volIannotatedTree
                  {-# LINE 9776 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 9785 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateFunction ann_ _langIoriginalTree name_ _paramsIoriginalTree _rettypeIoriginalTree bodyQuote_ _bodyIoriginalTree _volIoriginalTree
                  {-# LINE 9790 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9795 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _attsOenv :: Environment
              _attsOlib :: LocalIdentifierBindings
              _attsIannotatedTree :: AttributeDefList
              _attsIattrs :: ([(String, Type)])
              _attsIoriginalTree :: AttributeDefList
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9811 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9816 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIattrs,_attsIoriginalTree) =
                  (atts_ _attsOenv _attsOlib )
              -- "./TypeChecking/CreateTable.ag"(line 28, column 9)
              _attrTypes =
                  {-# LINE 28 "./TypeChecking/CreateTable.ag" #-}
                  map snd _attsIattrs
                  {-# LINE 9823 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 25, column 9)
              _envUpdates =
                  {-# LINE 25 "./TypeChecking/CreateTable.ag" #-}
                  dependsOn _attrTypes     []
                    [EnvCreateTable name_ _attsIattrs []]
                  {-# LINE 9829 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 9834 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9839 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 9844 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_CreateTable_1 _lhsIlib _lhsIenv cons_ name_ ann_ _attsIannotatedTree _attrTypes _envUpdates _attsIoriginalTree )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_CreateTable_1 _lhsIlib _lhsIenv cons_ name_ ann_ _attsIannotatedTree _attrTypes _envUpdates _attsIoriginalTree  =
    (\ _lhsIinProducedEnv ->
         (let _consOlib :: LocalIdentifierBindings
              _consOenv :: Environment
              _consIannotatedTree :: ConstraintList
              _consIoriginalTree :: ConstraintList
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _consOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9860 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9865 "AstInternal.hs" #-}
              ( _consIannotatedTree,_consIoriginalTree) =
                  (cons_ _consOenv _consOlib )
              -- "./TypeChecking/CreateTable.ag"(line 31, column 9)
              _backTree =
                  {-# LINE 31 "./TypeChecking/CreateTable.ag" #-}
                  CreateTable ann_
                              name_
                              _attsIannotatedTree
                              _consIannotatedTree
                  {-# LINE 9875 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 30, column 9)
              _statementInfo =
                  {-# LINE 30 "./TypeChecking/CreateTable.ag" #-}
                  []
                  {-# LINE 9880 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 24, column 9)
              _tpe =
                  {-# LINE 24 "./TypeChecking/CreateTable.ag" #-}
                  dependsOnRTpe _attrTypes     $ Right $ Pseudo Void
                  {-# LINE 9885 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 9894 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIoriginalTree _consIoriginalTree
                  {-# LINE 9899 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9904 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _exprOlib :: LocalIdentifierBindings
              _exprOenv :: Environment
              expr_1 :: T_SelectExpression_1 
              _exprIoriginalTree :: SelectExpression
              _exprIannotatedTree :: SelectExpression
              _exprIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9921 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 9926 "AstInternal.hs" #-}
              ( _exprIoriginalTree,expr_1) =
                  (expr_ )
              ( _exprIannotatedTree,_exprIlibUpdates) =
                  (expr_1 _exprOenv _exprOlib )
              -- "./TypeChecking/CreateTable.ag"(line 48, column 9)
              _selType =
                  {-# LINE 48 "./TypeChecking/CreateTable.ag" #-}
                  getTypeAnnotation _exprIannotatedTree
                  {-# LINE 9935 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 50, column 9)
              _attrs =
                  {-# LINE 50 "./TypeChecking/CreateTable.ag" #-}
                  unwrapSetOfComposite _selType
                  {-# LINE 9940 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 38, column 9)
              _tpe =
                  {-# LINE 38 "./TypeChecking/CreateTable.ag" #-}
                  dependsOnRTpe [_selType    ] $ do
                    _attrs
                    Right _selType
                  {-# LINE 9947 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 42, column 9)
              _envUpdates =
                  {-# LINE 42 "./TypeChecking/CreateTable.ag" #-}
                  leftToEmpty (\as -> [EnvCreateTable name_ as []]) $ do
                     ats <- _attrs
                     return $ dependsOn (tpeToT _tpe     :
                                         (map snd ats)) [] ats
                  {-# LINE 9955 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 9960 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9965 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 9970 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_CreateTableAs_1 name_ ann_ _exprIannotatedTree _envUpdates _tpe _exprIoriginalTree )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_CreateTableAs_1 name_ ann_ _exprIannotatedTree _envUpdates _tpe _exprIoriginalTree  =
    (\ _lhsIinProducedEnv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/CreateTable.ag"(line 53, column 9)
              _statementInfo =
                  {-# LINE 53 "./TypeChecking/CreateTable.ag" #-}
                  []
                  {-# LINE 9982 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 52, column 9)
              _backTree =
                  {-# LINE 52 "./TypeChecking/CreateTable.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 9987 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 9996 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIoriginalTree
                  {-# LINE 10001 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10006 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _attsOenv :: Environment
              _attsOlib :: LocalIdentifierBindings
              _attsIannotatedTree :: TypeAttributeDefList
              _attsIattrs :: ([(String, Type)])
              _attsIoriginalTree :: TypeAttributeDefList
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _attsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10022 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10027 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIattrs,_attsIoriginalTree) =
                  (atts_ _attsOenv _attsOlib )
              -- "./TypeChecking/MiscCreates.ag"(line 54, column 9)
              _envUpdates =
                  {-# LINE 54 "./TypeChecking/MiscCreates.ag" #-}
                  [EnvCreateComposite name_ _attsIattrs]
                  {-# LINE 10034 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 10039 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10044 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10049 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_CreateType_1 name_ ann_ _attsIannotatedTree _envUpdates _attsIoriginalTree )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_CreateType_1 name_ ann_ _attsIannotatedTree _envUpdates _attsIoriginalTree  =
    (\ _lhsIinProducedEnv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/MiscCreates.ag"(line 53, column 9)
              _statementInfo =
                  {-# LINE 53 "./TypeChecking/MiscCreates.ag" #-}
                  []
                  {-# LINE 10061 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 52, column 9)
              _backTree =
                  {-# LINE 52 "./TypeChecking/MiscCreates.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 10066 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 51, column 9)
              _tpe =
                  {-# LINE 51 "./TypeChecking/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 10071 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 10080 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIoriginalTree
                  {-# LINE 10085 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10090 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateView ann_ name_ expr_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _exprOlib :: LocalIdentifierBindings
              _exprOenv :: Environment
              expr_1 :: T_SelectExpression_1 
              _exprIoriginalTree :: SelectExpression
              _exprIannotatedTree :: SelectExpression
              _exprIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10107 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10112 "AstInternal.hs" #-}
              ( _exprIoriginalTree,expr_1) =
                  (expr_ )
              ( _exprIannotatedTree,_exprIlibUpdates) =
                  (expr_1 _exprOenv _exprOlib )
              -- "./TypeChecking/MiscCreates.ag"(line 18, column 9)
              _attrs =
                  {-# LINE 18 "./TypeChecking/MiscCreates.ag" #-}
                  case getTypeAnnotation _exprIannotatedTree of
                    SetOfType (CompositeType c) -> c
                    _ -> []
                  {-# LINE 10123 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 21, column 9)
              _envUpdates =
                  {-# LINE 21 "./TypeChecking/MiscCreates.ag" #-}
                  [EnvCreateView name_ _attrs    ]
                  {-# LINE 10128 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 10133 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10138 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10143 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_CreateView_1 name_ ann_ _exprIannotatedTree _envUpdates _exprIoriginalTree )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_CreateView_1 name_ ann_ _exprIannotatedTree _envUpdates _exprIoriginalTree  =
    (\ _lhsIinProducedEnv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/MiscCreates.ag"(line 22, column 9)
              _statementInfo =
                  {-# LINE 22 "./TypeChecking/MiscCreates.ag" #-}
                  []
                  {-# LINE 10155 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 17, column 9)
              _backTree =
                  {-# LINE 17 "./TypeChecking/MiscCreates.ag" #-}
                  CreateView ann_ name_ _exprIannotatedTree
                  {-# LINE 10160 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 15, column 9)
              _tpe =
                  {-# LINE 15 "./TypeChecking/MiscCreates.ag" #-}
                  dependsOnRTpe [getTypeAnnotation _exprIannotatedTree] $
                    Right $ Pseudo Void
                  {-# LINE 10166 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 10175 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateView ann_ name_ _exprIoriginalTree
                  {-# LINE 10180 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10185 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Delete ann_ table_ whr_ returning_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Dml.ag"(line 159, column 9)
              _envUpdates =
                  {-# LINE 159 "./TypeChecking/Dml.ag" #-}
                  []
                  {-# LINE 10196 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 10201 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10206 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10211 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_Delete_1 _lhsIenv table_ _lhsIlib returning_ whr_ ann_ _envUpdates )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_Delete_1 _lhsIenv table_ _lhsIlib returning_ whr_ ann_ _envUpdates  =
    (\ _lhsIinProducedEnv ->
         (let _returningOenv :: Environment
              _whrOenv :: Environment
              _returningOlib :: LocalIdentifierBindings
              _whrOlib :: LocalIdentifierBindings
              _returningIannotatedTree :: MaybeSelectList
              _returningIlistType :: (Maybe [(String,Type)])
              _returningIoriginalTree :: MaybeSelectList
              whr_1 :: T_MaybeBoolExpression_1 
              _whrIoriginalTree :: MaybeBoolExpression
              _whrIannotatedTree :: MaybeBoolExpression
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _returningOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10233 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10238 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 164, column 9)
              _lib =
                  {-# LINE 164 "./TypeChecking/Dml.ag" #-}
                  fromRight _lhsIlib $ do
                  columnTypes <- envCompositeAttrs _lhsIenv relationComposites table_
                  updateBindings _lhsIlib _lhsIenv [LibStackIDs [("", columnTypes)]]
                  {-# LINE 10245 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 169, column 9)
              _returningOlib =
                  {-# LINE 169 "./TypeChecking/Dml.ag" #-}
                  _lib
                  {-# LINE 10250 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 168, column 9)
              _whrOlib =
                  {-# LINE 168 "./TypeChecking/Dml.ag" #-}
                  _lib
                  {-# LINE 10255 "AstInternal.hs" #-}
              ( _returningIannotatedTree,_returningIlistType,_returningIoriginalTree) =
                  (returning_ _returningOenv _returningOlib )
              ( _whrIoriginalTree,whr_1) =
                  (whr_ )
              ( _whrIannotatedTree) =
                  (whr_1 _whrOenv _whrOlib )
              -- "./TypeChecking/Dml.ag"(line 158, column 9)
              _backTree =
                  {-# LINE 158 "./TypeChecking/Dml.ag" #-}
                  Delete ann_ table_ _whrIannotatedTree _returningIannotatedTree
                  {-# LINE 10266 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 156, column 9)
              _statementInfo =
                  {-# LINE 156 "./TypeChecking/Dml.ag" #-}
                  [DeleteInfo table_ _returningIlistType]
                  {-# LINE 10271 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 153, column 9)
              _tpe =
                  {-# LINE 153 "./TypeChecking/Dml.ag" #-}
                  checkRelationExists _lhsIenv table_ >>
                  Right (Pseudo Void)
                  {-# LINE 10277 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 10286 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Delete ann_ table_ _whrIoriginalTree _returningIoriginalTree
                  {-# LINE 10291 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10296 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10307 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10312 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_DropFunction_1 _lhsIlib _lhsIenv cascade_ sigs_ ifE_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_DropFunction_1 _lhsIlib _lhsIenv cascade_ sigs_ ifE_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _cascadeOlib :: LocalIdentifierBindings
              _cascadeOenv :: Environment
              _cascadeIannotatedTree :: Cascade
              _cascadeIoriginalTree :: Cascade
              _sigsOlib :: LocalIdentifierBindings
              _sigsOenv :: Environment
              _sigsIannotatedTree :: StringStringListPairList
              _sigsIoriginalTree :: StringStringListPairList
              _ifEOlib :: LocalIdentifierBindings
              _ifEOenv :: Environment
              _ifEIannotatedTree :: IfExists
              _ifEIoriginalTree :: IfExists
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _cascadeOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10336 "AstInternal.hs" #-}
              -- copy rule (down)
              _cascadeOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10341 "AstInternal.hs" #-}
              ( _cascadeIannotatedTree,_cascadeIoriginalTree) =
                  (cascade_ _cascadeOenv _cascadeOlib )
              -- copy rule (down)
              _sigsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10348 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10353 "AstInternal.hs" #-}
              ( _sigsIannotatedTree,_sigsIoriginalTree) =
                  (sigs_ _sigsOenv _sigsOlib )
              -- copy rule (down)
              _ifEOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10360 "AstInternal.hs" #-}
              -- copy rule (down)
              _ifEOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10365 "AstInternal.hs" #-}
              ( _ifEIannotatedTree,_ifEIoriginalTree) =
                  (ifE_ _ifEOenv _ifEOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  DropFunction ann_ _ifEIannotatedTree _sigsIannotatedTree _cascadeIannotatedTree
                  {-# LINE 10372 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10377 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  DropFunction ann_ _ifEIoriginalTree _sigsIoriginalTree _cascadeIoriginalTree
                  {-# LINE 10382 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10387 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10398 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10403 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_DropSomething_1 _lhsIlib _lhsIenv cascade_ names_ ifE_ dropType_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_DropSomething_1 _lhsIlib _lhsIenv cascade_ names_ ifE_ dropType_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _cascadeOlib :: LocalIdentifierBindings
              _cascadeOenv :: Environment
              _cascadeIannotatedTree :: Cascade
              _cascadeIoriginalTree :: Cascade
              names_1 :: T_StringList_1 
              _namesIoriginalTree :: StringList
              _namesOlib :: LocalIdentifierBindings
              _namesOenv :: Environment
              _namesIannotatedTree :: StringList
              _namesIstrings :: ([String])
              _ifEOlib :: LocalIdentifierBindings
              _ifEOenv :: Environment
              _ifEIannotatedTree :: IfExists
              _ifEIoriginalTree :: IfExists
              _dropTypeOlib :: LocalIdentifierBindings
              _dropTypeOenv :: Environment
              _dropTypeIannotatedTree :: DropType
              _dropTypeIoriginalTree :: DropType
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _cascadeOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10433 "AstInternal.hs" #-}
              -- copy rule (down)
              _cascadeOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10438 "AstInternal.hs" #-}
              ( _cascadeIannotatedTree,_cascadeIoriginalTree) =
                  (cascade_ _cascadeOenv _cascadeOlib )
              ( _namesIoriginalTree,names_1) =
                  (names_ )
              -- copy rule (down)
              _namesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10447 "AstInternal.hs" #-}
              -- copy rule (down)
              _namesOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10452 "AstInternal.hs" #-}
              ( _namesIannotatedTree,_namesIstrings) =
                  (names_1 _namesOenv _namesOlib )
              -- copy rule (down)
              _ifEOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10459 "AstInternal.hs" #-}
              -- copy rule (down)
              _ifEOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10464 "AstInternal.hs" #-}
              ( _ifEIannotatedTree,_ifEIoriginalTree) =
                  (ifE_ _ifEOenv _ifEOlib )
              -- copy rule (down)
              _dropTypeOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10471 "AstInternal.hs" #-}
              -- copy rule (down)
              _dropTypeOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10476 "AstInternal.hs" #-}
              ( _dropTypeIannotatedTree,_dropTypeIoriginalTree) =
                  (dropType_ _dropTypeOenv _dropTypeOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  DropSomething ann_ _dropTypeIannotatedTree _ifEIannotatedTree _namesIannotatedTree _cascadeIannotatedTree
                  {-# LINE 10483 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10488 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  DropSomething ann_ _dropTypeIoriginalTree _ifEIoriginalTree _namesIoriginalTree _cascadeIoriginalTree
                  {-# LINE 10493 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10498 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10509 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10514 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_Execute_1 _lhsIlib _lhsIenv expr_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_Execute_1 _lhsIlib _lhsIenv expr_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _exprOlib :: LocalIdentifierBindings
              _exprOenv :: Environment
              expr_1 :: T_Expression_1 
              _exprIoriginalTree :: Expression
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10532 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10537 "AstInternal.hs" #-}
              ( _exprIoriginalTree,expr_1) =
                  (expr_ )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_1 _exprOenv _exprOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Execute ann_ _exprIannotatedTree
                  {-# LINE 10546 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10551 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Execute ann_ _exprIoriginalTree
                  {-# LINE 10556 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10561 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ExecuteInto ann_ expr_ targets_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10572 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10577 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_ExecuteInto_1 _lhsIlib _lhsIenv targets_ expr_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_ExecuteInto_1 _lhsIlib _lhsIenv targets_ expr_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _exprOlib :: LocalIdentifierBindings
              _exprOenv :: Environment
              targets_1 :: T_StringList_1 
              _targetsIoriginalTree :: StringList
              _targetsOlib :: LocalIdentifierBindings
              _targetsOenv :: Environment
              _targetsIannotatedTree :: StringList
              _targetsIstrings :: ([String])
              expr_1 :: T_Expression_1 
              _exprIoriginalTree :: Expression
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10601 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10606 "AstInternal.hs" #-}
              ( _targetsIoriginalTree,targets_1) =
                  (targets_ )
              -- copy rule (down)
              _targetsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10613 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10618 "AstInternal.hs" #-}
              ( _targetsIannotatedTree,_targetsIstrings) =
                  (targets_1 _targetsOenv _targetsOlib )
              ( _exprIoriginalTree,expr_1) =
                  (expr_ )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_1 _exprOenv _exprOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIannotatedTree _targetsIannotatedTree
                  {-# LINE 10629 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10634 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIoriginalTree _targetsIoriginalTree
                  {-# LINE 10639 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10644 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ForIntegerStatement ann_ var_ from_ to_ sts_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Plpgsql.ag"(line 57, column 9)
              _envUpdates =
                  {-# LINE 57 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 10655 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 10660 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10665 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10670 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_ForIntegerStatement_1 _lhsIenv _lhsIlib var_ from_ sts_ to_ ann_ _envUpdates )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_ForIntegerStatement_1 _lhsIenv _lhsIlib var_ from_ sts_ to_ ann_ _envUpdates  =
    (\ _lhsIinProducedEnv ->
         (let _stsOenv :: Environment
              _toOlib :: LocalIdentifierBindings
              _toOenv :: Environment
              _fromOlib :: LocalIdentifierBindings
              _fromOenv :: Environment
              from_1 :: T_Expression_1 
              _fromIoriginalTree :: Expression
              _fromIannotatedTree :: Expression
              _fromIliftedColumnName :: String
              _stsOlib :: LocalIdentifierBindings
              _stsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedEnv :: Environment
              _stsIproducedLib :: LocalIdentifierBindings
              to_1 :: T_Expression_1 
              _toIoriginalTree :: Expression
              _toIannotatedTree :: Expression
              _toIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10702 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10707 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10712 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10717 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10722 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 58, column 9)
              _statementInfo =
                  {-# LINE 58 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 10727 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 36, column 9)
              _varTypeE =
                  {-# LINE 36 "./TypeChecking/Plpgsql.ag" #-}
                  libLookupID _lhsIlib var_
                  {-# LINE 10732 "AstInternal.hs" #-}
              ( _fromIoriginalTree,from_1) =
                  (from_ )
              ( _fromIannotatedTree,_fromIliftedColumnName) =
                  (from_1 _fromOenv _fromOlib )
              -- "./TypeChecking/Plpgsql.ag"(line 48, column 9)
              _stsOlib =
                  {-# LINE 48 "./TypeChecking/Plpgsql.ag" #-}
                  case _varTypeE     of
                    Left [UnrecognisedIdentifier var_] ->
                        fromRight _lhsIlib $
                        updateBindings _lhsIlib _lhsIenv
                                       [LibStackIDs [("", [(var_,getTypeAnnotation _fromIannotatedTree)])]]
                    _ -> _lhsIlib
                  {-# LINE 10746 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 107, column 9)
              _stsOlibUpdates =
                  {-# LINE 107 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10751 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 106, column 9)
              _stsOenvUpdates =
                  {-# LINE 106 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10756 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedEnv,_stsIproducedLib) =
                  (sts_ _stsOenv _stsOenvUpdates _stsOlib _stsOlibUpdates )
              ( _toIoriginalTree,to_1) =
                  (to_ )
              ( _toIannotatedTree,_toIliftedColumnName) =
                  (to_1 _toOenv _toOlib )
              -- "./TypeChecking/Plpgsql.ag"(line 56, column 9)
              _backTree =
                  {-# LINE 56 "./TypeChecking/Plpgsql.ag" #-}
                  ForIntegerStatement ann_ var_ _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 10767 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 37, column 9)
              _tpe =
                  {-# LINE 37 "./TypeChecking/Plpgsql.ag" #-}
                  do
                  let fromType = getTypeAnnotation _fromIannotatedTree
                      toType = getTypeAnnotation _toIannotatedTree
                  dependsOnRTpe [fromType,toType] $ do
                  errorWhen (fromType /= toType) [FromToTypesNotSame fromType toType]
                  case _varTypeE     of
                    Right t -> checkAssignmentValid _lhsIenv fromType t
                    Left _ -> return ()
                  return $ Pseudo Void
                  {-# LINE 10780 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 10789 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ForIntegerStatement ann_ var_ _fromIoriginalTree _toIoriginalTree _stsIoriginalTree
                  {-# LINE 10794 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10799 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ForSelectStatement ann_ var_ sel_ sts_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Plpgsql.ag"(line 85, column 9)
              _envUpdates =
                  {-# LINE 85 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 10810 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 10815 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10820 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10825 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_ForSelectStatement_1 _lhsIenv _lhsIlib sel_ var_ sts_ ann_ _envUpdates )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_ForSelectStatement_1 _lhsIenv _lhsIlib sel_ var_ sts_ ann_ _envUpdates  =
    (\ _lhsIinProducedEnv ->
         (let _stsOenv :: Environment
              _selOlib :: LocalIdentifierBindings
              _selOenv :: Environment
              sel_1 :: T_SelectExpression_1 
              _selIoriginalTree :: SelectExpression
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _stsOlib :: LocalIdentifierBindings
              _stsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedEnv :: Environment
              _stsIproducedLib :: LocalIdentifierBindings
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10851 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10856 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10861 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 86, column 9)
              _statementInfo =
                  {-# LINE 86 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 10866 "AstInternal.hs" #-}
              ( _selIoriginalTree,sel_1) =
                  (sel_ )
              ( _selIannotatedTree,_selIlibUpdates) =
                  (sel_1 _selOenv _selOlib )
              -- "./TypeChecking/Plpgsql.ag"(line 63, column 9)
              _selType =
                  {-# LINE 63 "./TypeChecking/Plpgsql.ag" #-}
                  getTypeAnnotation _selIannotatedTree
                  {-# LINE 10875 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 64, column 9)
              _tpe =
                  {-# LINE 64 "./TypeChecking/Plpgsql.ag" #-}
                  do
                  dependsOnRTpe [_selType    ] $ do
                  toType <- libLookupID _lhsIlib var_
                  dependsOnRTpe [toType] $ do
                  checkAssignmentValid _lhsIenv _selType     toType
                  return $ Pseudo Void
                  {-# LINE 10885 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 75, column 9)
              _stsOlib =
                  {-# LINE 75 "./TypeChecking/Plpgsql.ag" #-}
                  if okToUpdate
                    then fromRight _lhsIlib $
                         updateBindings _lhsIlib _lhsIenv [LibStackIDs [("", [(var_,_selType    )])]]
                    else _lhsIlib
                  where
                    okToUpdate = isRight _tpe     && _selType     /= TypeCheckFailed
                  {-# LINE 10895 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 107, column 9)
              _stsOlibUpdates =
                  {-# LINE 107 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10900 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 106, column 9)
              _stsOenvUpdates =
                  {-# LINE 106 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10905 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedEnv,_stsIproducedLib) =
                  (sts_ _stsOenv _stsOenvUpdates _stsOlib _stsOlibUpdates )
              -- "./TypeChecking/Plpgsql.ag"(line 84, column 9)
              _backTree =
                  {-# LINE 84 "./TypeChecking/Plpgsql.ag" #-}
                  ForSelectStatement ann_ var_ _selIannotatedTree _stsIannotatedTree
                  {-# LINE 10912 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 10921 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ForSelectStatement ann_ var_ _selIoriginalTree _stsIoriginalTree
                  {-# LINE 10926 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10931 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10942 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10947 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_If_1 _lhsIlib _lhsIenv els_ cases_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_If_1 _lhsIlib _lhsIenv els_ cases_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _elsOlib :: LocalIdentifierBindings
              _elsOenv :: Environment
              _casesOlib :: LocalIdentifierBindings
              _casesOenv :: Environment
              _elsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _elsOenvUpdates :: ([EnvironmentUpdate])
              _elsIannotatedTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedEnv :: Environment
              _elsIproducedLib :: LocalIdentifierBindings
              _casesIannotatedTree :: ExpressionStatementListPairList
              _casesIoriginalTree :: ExpressionStatementListPairList
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10971 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10976 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10981 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 10986 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 103, column 9)
              _elsOlibUpdates =
                  {-# LINE 103 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10991 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 102, column 9)
              _elsOenvUpdates =
                  {-# LINE 102 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10996 "AstInternal.hs" #-}
              ( _elsIannotatedTree,_elsIoriginalTree,_elsIproducedEnv,_elsIproducedLib) =
                  (els_ _elsOenv _elsOenvUpdates _elsOlib _elsOlibUpdates )
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  (cases_ _casesOenv _casesOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  If ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 11005 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11010 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  If ann_ _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 11015 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11020 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Dml.ag"(line 36, column 9)
              _envUpdates =
                  {-# LINE 36 "./TypeChecking/Dml.ag" #-}
                  []
                  {-# LINE 11031 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 11036 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11041 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11046 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_Insert_1 _lhsIenv _lhsIlib table_ returning_ insData_ targetCols_ ann_ _envUpdates )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_Insert_1 _lhsIenv _lhsIlib table_ returning_ insData_ targetCols_ ann_ _envUpdates  =
    (\ _lhsIinProducedEnv ->
         (let _returningOenv :: Environment
              _insDataOlib :: LocalIdentifierBindings
              _insDataOenv :: Environment
              _returningOlib :: LocalIdentifierBindings
              _returningIannotatedTree :: MaybeSelectList
              _returningIlistType :: (Maybe [(String,Type)])
              _returningIoriginalTree :: MaybeSelectList
              insData_1 :: T_SelectExpression_1 
              _insDataIoriginalTree :: SelectExpression
              _insDataIannotatedTree :: SelectExpression
              _insDataIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              targetCols_1 :: T_StringList_1 
              _targetColsIoriginalTree :: StringList
              _targetColsOlib :: LocalIdentifierBindings
              _targetColsOenv :: Environment
              _targetColsIannotatedTree :: StringList
              _targetColsIstrings :: ([String])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _returningOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11075 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11080 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11085 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 42, column 9)
              _returningOlib =
                  {-# LINE 42 "./TypeChecking/Dml.ag" #-}
                  fromRight _lhsIlib $ do
                    atts <- envCompositeAttrs _lhsIenv relationComposites table_
                    updateBindings _lhsIlib _lhsIenv [LibStackIDs [("", atts)]]
                  {-# LINE 11092 "AstInternal.hs" #-}
              ( _returningIannotatedTree,_returningIlistType,_returningIoriginalTree) =
                  (returning_ _returningOenv _returningOlib )
              ( _insDataIoriginalTree,insData_1) =
                  (insData_ )
              ( _insDataIannotatedTree,_insDataIlibUpdates) =
                  (insData_1 _insDataOenv _insDataOlib )
              ( _targetColsIoriginalTree,targetCols_1) =
                  (targetCols_ )
              -- copy rule (down)
              _targetColsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11105 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetColsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11110 "AstInternal.hs" #-}
              ( _targetColsIannotatedTree,_targetColsIstrings) =
                  (targetCols_1 _targetColsOenv _targetColsOlib )
              -- "./TypeChecking/Dml.ag"(line 34, column 9)
              _backTree =
                  {-# LINE 34 "./TypeChecking/Dml.ag" #-}
                  Insert ann_ table_ _targetColsIannotatedTree
                         _insDataIannotatedTree _returningIannotatedTree
                  {-# LINE 11118 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 25, column 9)
              _columnTypes =
                  {-# LINE 25 "./TypeChecking/Dml.ag" #-}
                  do
                  tys <- unwrapSetOfComposite $
                         getTypeAnnotation _insDataIannotatedTree
                  checkColumnConsistency _lhsIenv
                                         table_
                                         _targetColsIstrings
                                         tys
                  {-# LINE 11129 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 22, column 9)
              _statementInfo =
                  {-# LINE 22 "./TypeChecking/Dml.ag" #-}
                  leftToEmpty (\ct -> [InsertInfo table_ ct _returningIlistType]) _columnTypes
                  {-# LINE 11134 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 18, column 9)
              _tpe =
                  {-# LINE 18 "./TypeChecking/Dml.ag" #-}
                  dependsOnRTpe [getTypeAnnotation _insDataIannotatedTree] $ do
                    _columnTypes
                    Right $ Pseudo Void
                  {-# LINE 11141 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 11150 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Insert ann_ table_ _targetColsIoriginalTree _insDataIoriginalTree _returningIoriginalTree
                  {-# LINE 11155 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11160 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_NullStatement ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11171 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11176 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_NullStatement_1 ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_NullStatement_1 ann_  =
    (\ _lhsIinProducedEnv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 11188 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11193 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 11198 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11203 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11214 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11219 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_Perform_1 _lhsIlib _lhsIenv expr_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_Perform_1 _lhsIlib _lhsIenv expr_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _exprOlib :: LocalIdentifierBindings
              _exprOenv :: Environment
              expr_1 :: T_Expression_1 
              _exprIoriginalTree :: Expression
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11237 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11242 "AstInternal.hs" #-}
              ( _exprIoriginalTree,expr_1) =
                  (expr_ )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_1 _exprOenv _exprOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Perform ann_ _exprIannotatedTree
                  {-# LINE 11251 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11256 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Perform ann_ _exprIoriginalTree
                  {-# LINE 11261 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11266 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11277 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11282 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_Raise_1 _lhsIlib _lhsIenv args_ level_ message_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_Raise_1 _lhsIlib _lhsIenv args_ level_ message_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _argsOlib :: LocalIdentifierBindings
              _argsOenv :: Environment
              args_1 :: T_ExpressionList_1 
              _argsIoriginalTree :: ExpressionList
              _argsIannotatedTree :: ExpressionList
              _argsItypeList :: ([Type])
              _levelOlib :: LocalIdentifierBindings
              _levelOenv :: Environment
              _levelIannotatedTree :: RaiseType
              _levelIoriginalTree :: RaiseType
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11304 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11309 "AstInternal.hs" #-}
              ( _argsIoriginalTree,args_1) =
                  (args_ )
              ( _argsIannotatedTree,_argsItypeList) =
                  (args_1 _argsOenv _argsOlib )
              -- copy rule (down)
              _levelOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11318 "AstInternal.hs" #-}
              -- copy rule (down)
              _levelOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11323 "AstInternal.hs" #-}
              ( _levelIannotatedTree,_levelIoriginalTree) =
                  (level_ _levelOenv _levelOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Raise ann_ _levelIannotatedTree message_ _argsIannotatedTree
                  {-# LINE 11330 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11335 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Raise ann_ _levelIoriginalTree message_ _argsIoriginalTree
                  {-# LINE 11340 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11345 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Return ann_ value_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Plpgsql.ag"(line 17, column 9)
              _envUpdates =
                  {-# LINE 17 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 11356 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 11361 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11366 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11371 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_Return_1 _lhsIlib _lhsIenv value_ ann_ _envUpdates )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_Return_1 _lhsIlib _lhsIenv value_ ann_ _envUpdates  =
    (\ _lhsIinProducedEnv ->
         (let _valueOlib :: LocalIdentifierBindings
              _valueOenv :: Environment
              value_1 :: T_MaybeExpression_1 
              _valueIoriginalTree :: MaybeExpression
              _valueIannotatedTree :: MaybeExpression
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11388 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11393 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 18, column 9)
              _statementInfo =
                  {-# LINE 18 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 11398 "AstInternal.hs" #-}
              ( _valueIoriginalTree,value_1) =
                  (value_ )
              ( _valueIannotatedTree) =
                  (value_1 _valueOenv _valueOlib )
              -- "./TypeChecking/Plpgsql.ag"(line 16, column 9)
              _backTree =
                  {-# LINE 16 "./TypeChecking/Plpgsql.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 11407 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 12, column 9)
              _tpe =
                  {-# LINE 12 "./TypeChecking/Plpgsql.ag" #-}
                  dependsOnRTpe [maybe typeBool
                                      getTypeAnnotation
                                      _valueIannotatedTree] $ Right $ Pseudo Void
                  {-# LINE 11414 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 11423 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Return ann_ _valueIoriginalTree
                  {-# LINE 11428 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11433 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11444 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11449 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_ReturnNext_1 _lhsIlib _lhsIenv expr_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_ReturnNext_1 _lhsIlib _lhsIenv expr_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _exprOlib :: LocalIdentifierBindings
              _exprOenv :: Environment
              expr_1 :: T_Expression_1 
              _exprIoriginalTree :: Expression
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11467 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11472 "AstInternal.hs" #-}
              ( _exprIoriginalTree,expr_1) =
                  (expr_ )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_1 _exprOenv _exprOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIannotatedTree
                  {-# LINE 11481 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11486 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIoriginalTree
                  {-# LINE 11491 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11496 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11507 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11512 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_ReturnQuery_1 _lhsIlib _lhsIenv sel_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_ReturnQuery_1 _lhsIlib _lhsIenv sel_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _selOlib :: LocalIdentifierBindings
              _selOenv :: Environment
              sel_1 :: T_SelectExpression_1 
              _selIoriginalTree :: SelectExpression
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11530 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11535 "AstInternal.hs" #-}
              ( _selIoriginalTree,sel_1) =
                  (sel_ )
              ( _selIannotatedTree,_selIlibUpdates) =
                  (sel_1 _selOenv _selOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIannotatedTree
                  {-# LINE 11544 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11549 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIoriginalTree
                  {-# LINE 11554 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11559 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_SelectStatement ann_ ex_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _exOlib :: LocalIdentifierBindings
              _exOenv :: Environment
              ex_1 :: T_SelectExpression_1 
              _exIoriginalTree :: SelectExpression
              _exIannotatedTree :: SelectExpression
              _exIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/SelectStatement.ag"(line 17, column 9)
              _envUpdates =
                  {-# LINE 17 "./TypeChecking/SelectStatement.ag" #-}
                  []
                  {-# LINE 11576 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 11581 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11586 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11591 "AstInternal.hs" #-}
              ( _exIoriginalTree,ex_1) =
                  (ex_ )
              ( _exIannotatedTree,_exIlibUpdates) =
                  (ex_1 _exOenv _exOlib )
              -- "./TypeChecking/SelectLists.ag"(line 79, column 9)
              _libUpdates =
                  {-# LINE 79 "./TypeChecking/SelectLists.ag" #-}
                  _exIlibUpdates
                  {-# LINE 11600 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11605 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_SelectStatement_1 ann_ _exIannotatedTree _envUpdates _exIoriginalTree )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_SelectStatement_1 ann_ _exIannotatedTree _envUpdates _exIoriginalTree  =
    (\ _lhsIinProducedEnv ->
         (let _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/SelectStatement.ag"(line 16, column 9)
              _backTree =
                  {-# LINE 16 "./TypeChecking/SelectStatement.ag" #-}
                  SelectStatement ann_ _exIannotatedTree
                  {-# LINE 11617 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 15, column 9)
              _statementInfo =
                  {-# LINE 15 "./TypeChecking/SelectStatement.ag" #-}
                  [SelectInfo $ getTypeAnnotation _exIannotatedTree]
                  {-# LINE 11622 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 14, column 9)
              _tpe =
                  {-# LINE 14 "./TypeChecking/SelectStatement.ag" #-}
                  dependsOnRTpe [getTypeAnnotation _exIannotatedTree] $ Right $ Pseudo Void
                  {-# LINE 11627 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 11636 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SelectStatement ann_ _exIoriginalTree
                  {-# LINE 11641 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11646 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11657 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11662 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_Truncate_1 _lhsIlib _lhsIenv cascade_ restartIdentity_ tables_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_Truncate_1 _lhsIlib _lhsIenv cascade_ restartIdentity_ tables_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _cascadeOlib :: LocalIdentifierBindings
              _cascadeOenv :: Environment
              _cascadeIannotatedTree :: Cascade
              _cascadeIoriginalTree :: Cascade
              _restartIdentityOlib :: LocalIdentifierBindings
              _restartIdentityOenv :: Environment
              _restartIdentityIannotatedTree :: RestartIdentity
              _restartIdentityIoriginalTree :: RestartIdentity
              tables_1 :: T_StringList_1 
              _tablesIoriginalTree :: StringList
              _tablesOlib :: LocalIdentifierBindings
              _tablesOenv :: Environment
              _tablesIannotatedTree :: StringList
              _tablesIstrings :: ([String])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _cascadeOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11688 "AstInternal.hs" #-}
              -- copy rule (down)
              _cascadeOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11693 "AstInternal.hs" #-}
              ( _cascadeIannotatedTree,_cascadeIoriginalTree) =
                  (cascade_ _cascadeOenv _cascadeOlib )
              -- copy rule (down)
              _restartIdentityOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11700 "AstInternal.hs" #-}
              -- copy rule (down)
              _restartIdentityOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11705 "AstInternal.hs" #-}
              ( _restartIdentityIannotatedTree,_restartIdentityIoriginalTree) =
                  (restartIdentity_ _restartIdentityOenv _restartIdentityOlib )
              ( _tablesIoriginalTree,tables_1) =
                  (tables_ )
              -- copy rule (down)
              _tablesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11714 "AstInternal.hs" #-}
              -- copy rule (down)
              _tablesOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11719 "AstInternal.hs" #-}
              ( _tablesIannotatedTree,_tablesIstrings) =
                  (tables_1 _tablesOenv _tablesOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Truncate ann_ _tablesIannotatedTree _restartIdentityIannotatedTree _cascadeIannotatedTree
                  {-# LINE 11726 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11731 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Truncate ann_ _tablesIoriginalTree _restartIdentityIoriginalTree _cascadeIoriginalTree
                  {-# LINE 11736 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11741 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Update ann_ table_ assigns_ whr_ returning_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Dml.ag"(line 79, column 9)
              _envUpdates =
                  {-# LINE 79 "./TypeChecking/Dml.ag" #-}
                  []
                  {-# LINE 11752 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 65, column 9)
              _lhsOenvUpdates =
                  {-# LINE 65 "./TypeChecking/Statements.ag" #-}
                  _envUpdates
                  {-# LINE 11757 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _libUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11762 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOlibUpdates =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11767 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_Update_1 _lhsIenv table_ _lhsIlib returning_ whr_ assigns_ ann_ _envUpdates )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_Update_1 _lhsIenv table_ _lhsIlib returning_ whr_ assigns_ ann_ _envUpdates  =
    (\ _lhsIinProducedEnv ->
         (let _returningOenv :: Environment
              _whrOenv :: Environment
              _assignsOenv :: Environment
              _returningOlib :: LocalIdentifierBindings
              _assignsOlib :: LocalIdentifierBindings
              _whrOlib :: LocalIdentifierBindings
              _returningIannotatedTree :: MaybeSelectList
              _returningIlistType :: (Maybe [(String,Type)])
              _returningIoriginalTree :: MaybeSelectList
              whr_1 :: T_MaybeBoolExpression_1 
              _whrIoriginalTree :: MaybeBoolExpression
              _whrIannotatedTree :: MaybeBoolExpression
              _assignsIannotatedTree :: SetClauseList
              _assignsIoriginalTree :: SetClauseList
              _assignsIpairs :: ([(String,Type)])
              _assignsIrowSetErrors :: ([TypeError])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _returningOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11795 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11800 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11805 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 87, column 9)
              _lib =
                  {-# LINE 87 "./TypeChecking/Dml.ag" #-}
                  fromRight _lhsIlib $ do
                  ct <- envCompositeAttrs _lhsIenv
                                          relationComposites
                                          table_
                  updateBindings _lhsIlib _lhsIenv [LibStackIDs [("", ct)]]
                  {-# LINE 11814 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 95, column 9)
              _returningOlib =
                  {-# LINE 95 "./TypeChecking/Dml.ag" #-}
                  _lib
                  {-# LINE 11819 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 94, column 9)
              _assignsOlib =
                  {-# LINE 94 "./TypeChecking/Dml.ag" #-}
                  _lib
                  {-# LINE 11824 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 93, column 9)
              _whrOlib =
                  {-# LINE 93 "./TypeChecking/Dml.ag" #-}
                  _lib
                  {-# LINE 11829 "AstInternal.hs" #-}
              ( _returningIannotatedTree,_returningIlistType,_returningIoriginalTree) =
                  (returning_ _returningOenv _returningOlib )
              ( _whrIoriginalTree,whr_1) =
                  (whr_ )
              ( _whrIannotatedTree) =
                  (whr_1 _whrOenv _whrOlib )
              ( _assignsIannotatedTree,_assignsIoriginalTree,_assignsIpairs,_assignsIrowSetErrors) =
                  (assigns_ _assignsOenv _assignsOlib )
              -- "./TypeChecking/Dml.ag"(line 74, column 9)
              _backTree =
                  {-# LINE 74 "./TypeChecking/Dml.ag" #-}
                  Update ann_
                         table_
                         _assignsIannotatedTree
                         _whrIannotatedTree
                         _returningIannotatedTree
                  {-# LINE 11846 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 68, column 9)
              _columnTypes =
                  {-# LINE 68 "./TypeChecking/Dml.ag" #-}
                  checkColumnConsistency _lhsIenv
                                         table_
                                         (map fst _assignsIpairs)
                                         _assignsIpairs
                  {-# LINE 11854 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 65, column 9)
              _statementInfo =
                  {-# LINE 65 "./TypeChecking/Dml.ag" #-}
                  leftToEmpty (\ct -> [UpdateInfo table_ ct _returningIlistType]) _columnTypes
                  {-# LINE 11859 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml.ag"(line 58, column 9)
              _tpe =
                  {-# LINE 58 "./TypeChecking/Dml.ag" #-}
                  do
                  checkRelationExists _lhsIenv table_
                  dependsOnRTpe (map snd _assignsIpairs) $ do
                    _columnTypes
                    liftErrors _assignsIrowSetErrors
                    return $ Pseudo Void
                  {-# LINE 11869 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _lhsOannotatedTree =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  annTypesAndErrors _backTree
                    (tpeToT _tpe    )
                    (getErrors _tpe    )
                    $ Just (map StatementInfoA _statementInfo     ++
                            [EnvUpdates _envUpdates    ])
                  {-# LINE 11878 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Update ann_ table_ _assignsIoriginalTree _whrIoriginalTree _returningIoriginalTree
                  {-# LINE 11883 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11888 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_WhileStatement ann_ expr_ sts_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOenvUpdates :: ([EnvironmentUpdate])
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOenvUpdates =
                  {-# LINE 85 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11899 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11904 "AstInternal.hs" #-}
              ( sem_Statement_1) =
                  (sem_Statement_WhileStatement_1 _lhsIlib _lhsIenv sts_ expr_ ann_ )
          in  ( _lhsOenvUpdates,_lhsOlibUpdates,sem_Statement_1)))
sem_Statement_WhileStatement_1 _lhsIlib _lhsIenv sts_ expr_ ann_  =
    (\ _lhsIinProducedEnv ->
         (let _stsOlib :: LocalIdentifierBindings
              _stsOenv :: Environment
              _exprOlib :: LocalIdentifierBindings
              _exprOenv :: Environment
              _stsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _stsOenvUpdates :: ([EnvironmentUpdate])
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedEnv :: Environment
              _stsIproducedLib :: LocalIdentifierBindings
              expr_1 :: T_Expression_1 
              _exprIoriginalTree :: Expression
              _exprIannotatedTree :: Expression
              _exprIliftedColumnName :: String
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11930 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11935 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11940 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 11945 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 107, column 9)
              _stsOlibUpdates =
                  {-# LINE 107 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11950 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 106, column 9)
              _stsOenvUpdates =
                  {-# LINE 106 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11955 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedEnv,_stsIproducedLib) =
                  (sts_ _stsOenv _stsOenvUpdates _stsOlib _stsOlibUpdates )
              ( _exprIoriginalTree,expr_1) =
                  (expr_ )
              ( _exprIannotatedTree,_exprIliftedColumnName) =
                  (expr_1 _exprOenv _exprOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  WhileStatement ann_ _exprIannotatedTree _stsIannotatedTree
                  {-# LINE 11966 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11971 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  WhileStatement ann_ _exprIoriginalTree _stsIoriginalTree
                  {-# LINE 11976 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11981 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- StatementList -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         envUpdates           : [EnvironmentUpdate]
         lib                  : LocalIdentifierBindings
         libUpdates           : [LocalIdentifierBindingsUpdate]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         producedEnv          : Environment
         producedLib          : LocalIdentifierBindings
   alternatives:
      alternative Cons:
         child hd             : Statement 
         child tl             : StatementList 
         visit 0:
            local newLib      : _
            local newEnv      : _
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
            local newEnv      : _
            local newLib      : _
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
                        LocalIdentifierBindings ->
                        ([LocalIdentifierBindingsUpdate]) ->
                        ( StatementList,StatementList,Environment,LocalIdentifierBindings)
data Inh_StatementList  = Inh_StatementList {env_Inh_StatementList :: Environment,envUpdates_Inh_StatementList :: [EnvironmentUpdate],lib_Inh_StatementList :: LocalIdentifierBindings,libUpdates_Inh_StatementList :: [LocalIdentifierBindingsUpdate]}
data Syn_StatementList  = Syn_StatementList {annotatedTree_Syn_StatementList :: StatementList,originalTree_Syn_StatementList :: StatementList,producedEnv_Syn_StatementList :: Environment,producedLib_Syn_StatementList :: LocalIdentifierBindings}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIenv _lhsIenvUpdates _lhsIlib _lhsIlibUpdates )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedEnv,_lhsOproducedLib) =
             (sem _lhsIenv _lhsIenvUpdates _lhsIlib _lhsIlibUpdates )
     in  (Syn_StatementList _lhsOannotatedTree _lhsOoriginalTree _lhsOproducedEnv _lhsOproducedLib ))
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIenvUpdates
       _lhsIlib
       _lhsIlibUpdates ->
         (let _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              hd_1 :: T_Statement_1 
              _hdIenvUpdates :: ([EnvironmentUpdate])
              _hdIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _tlOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _tlOenvUpdates :: ([EnvironmentUpdate])
              _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _tlIannotatedTree :: StatementList
              _tlIoriginalTree :: StatementList
              _tlIproducedEnv :: Environment
              _tlIproducedLib :: LocalIdentifierBindings
              _hdOinProducedEnv :: Environment
              _hdIannotatedTree :: Statement
              _hdIoriginalTree :: Statement
              _lhsOannotatedTree :: StatementList
              _lhsOoriginalTree :: StatementList
              _lhsOproducedEnv :: Environment
              _lhsOproducedLib :: LocalIdentifierBindings
              -- "./TypeChecking/Statements.ag"(line 37, column 9)
              _newLib =
                  {-# LINE 37 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIlib $ updateBindings _lhsIlib _lhsIenv _lhsIlibUpdates
                  {-# LINE 12062 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 41, column 9)
              _hdOlib =
                  {-# LINE 41 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 12067 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 36, column 9)
              _newEnv =
                  {-# LINE 36 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIenv $ updateEnvironment _lhsIenv _lhsIenvUpdates
                  {-# LINE 12072 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 39, column 9)
              _hdOenv =
                  {-# LINE 39 "./TypeChecking/Statements.ag" #-}
                  _newEnv
                  {-# LINE 12077 "AstInternal.hs" #-}
              ( _hdIenvUpdates,_hdIlibUpdates,hd_1) =
                  (hd_ _hdOenv _hdOlib )
              -- "./TypeChecking/Statements.ag"(line 51, column 9)
              _tlOlibUpdates =
                  {-# LINE 51 "./TypeChecking/Statements.ag" #-}
                  _hdIlibUpdates
                  {-# LINE 12084 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 50, column 9)
              _tlOenvUpdates =
                  {-# LINE 50 "./TypeChecking/Statements.ag" #-}
                  _hdIenvUpdates
                  {-# LINE 12089 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 42, column 9)
              _tlOlib =
                  {-# LINE 42 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 12094 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 40, column 9)
              _tlOenv =
                  {-# LINE 40 "./TypeChecking/Statements.ag" #-}
                  _newEnv
                  {-# LINE 12099 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIproducedEnv,_tlIproducedLib) =
                  (tl_ _tlOenv _tlOenvUpdates _tlOlib _tlOlibUpdates )
              -- "./TypeChecking/Statements.ag"(line 73, column 12)
              _hdOinProducedEnv =
                  {-# LINE 73 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedEnv
                  {-# LINE 12106 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_1 _hdOinProducedEnv )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 12113 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12118 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 12123 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12128 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 46, column 9)
              _lhsOproducedEnv =
                  {-# LINE 46 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedEnv
                  {-# LINE 12133 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 47, column 9)
              _lhsOproducedLib =
                  {-# LINE 47 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedLib
                  {-# LINE 12138 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedEnv,_lhsOproducedLib)))
sem_StatementList_Nil  =
    (\ _lhsIenv
       _lhsIenvUpdates
       _lhsIlib
       _lhsIlibUpdates ->
         (let _lhsOannotatedTree :: StatementList
              _lhsOoriginalTree :: StatementList
              _lhsOproducedEnv :: Environment
              _lhsOproducedLib :: LocalIdentifierBindings
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 12153 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12158 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 12163 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12168 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 36, column 9)
              _newEnv =
                  {-# LINE 36 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIenv $ updateEnvironment _lhsIenv _lhsIenvUpdates
                  {-# LINE 12173 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 53, column 9)
              _lhsOproducedEnv =
                  {-# LINE 53 "./TypeChecking/Statements.ag" #-}
                  _newEnv
                  {-# LINE 12178 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 37, column 9)
              _newLib =
                  {-# LINE 37 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIlib $ updateBindings _lhsIlib _lhsIenv _lhsIlibUpdates
                  {-# LINE 12183 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 54, column 9)
              _lhsOproducedLib =
                  {-# LINE 54 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 12188 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedEnv,_lhsOproducedLib)))
-- StringList --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         strings              : [String]
   alternatives:
      alternative Cons:
         child hd             : {String}
         child tl             : StringList 
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
      alternative Nil:
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
-}
type StringList  = [(String)]
-- cata
sem_StringList :: StringList  ->
                  T_StringList 
sem_StringList list  =
    (Prelude.foldr sem_StringList_Cons sem_StringList_Nil list )
-- semantic domain
type T_StringList  = ( StringList,T_StringList_1 )
type T_StringList_1  = Environment ->
                       LocalIdentifierBindings ->
                       ( StringList,([String]))
data Inh_StringList  = Inh_StringList {env_Inh_StringList :: Environment,lib_Inh_StringList :: LocalIdentifierBindings}
data Syn_StringList  = Syn_StringList {annotatedTree_Syn_StringList :: StringList,originalTree_Syn_StringList :: StringList,strings_Syn_StringList :: [String]}
wrap_StringList :: T_StringList  ->
                   Inh_StringList  ->
                   Syn_StringList 
wrap_StringList sem (Inh_StringList _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOstrings) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_StringList _lhsOannotatedTree _lhsOoriginalTree _lhsOstrings ))
sem_StringList_Cons hd_ tl_  =
    (let tl_1 :: T_StringList_1 
         _tlIoriginalTree :: StringList
         _lhsOoriginalTree :: StringList
         ( _tlIoriginalTree,tl_1) =
             (tl_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             (:) hd_ _tlIoriginalTree
             {-# LINE 12248 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 12253 "AstInternal.hs" #-}
         ( sem_StringList_1) =
             (sem_StringList_Cons_1 tl_1 hd_ )
     in  ( _lhsOoriginalTree,sem_StringList_1))
sem_StringList_Cons_1 tl_1 hd_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _tlIannotatedTree :: StringList
              _tlIstrings :: ([String])
              _lhsOannotatedTree :: StringList
              _lhsOstrings :: ([String])
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12270 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 12275 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIstrings) =
                  (tl_1 _tlOenv _tlOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) hd_ _tlIannotatedTree
                  {-# LINE 12282 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12287 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 67, column 10)
              _lhsOstrings =
                  {-# LINE 67 "./TypeChecking/Misc.ag" #-}
                  hd_ : _tlIstrings
                  {-# LINE 12292 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOstrings)))
sem_StringList_Nil  =
    (let _lhsOoriginalTree :: StringList
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             []
             {-# LINE 12300 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 12305 "AstInternal.hs" #-}
         ( sem_StringList_1) =
             (sem_StringList_Nil_1 )
     in  ( _lhsOoriginalTree,sem_StringList_1))
sem_StringList_Nil_1  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: StringList
              _lhsOstrings :: ([String])
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 12318 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12323 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 68, column 9)
              _lhsOstrings =
                  {-# LINE 68 "./TypeChecking/Misc.ag" #-}
                  []
                  {-# LINE 12328 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOstrings)))
-- StringStringListPair ----------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : {String}
         child x2             : StringList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type StringStringListPair  = ( (String),(StringList))
-- cata
sem_StringStringListPair :: StringStringListPair  ->
                            T_StringStringListPair 
sem_StringStringListPair ( x1,x2)  =
    (sem_StringStringListPair_Tuple x1 (sem_StringList x2 ) )
-- semantic domain
type T_StringStringListPair  = Environment ->
                               LocalIdentifierBindings ->
                               ( StringStringListPair,StringStringListPair)
data Inh_StringStringListPair  = Inh_StringStringListPair {env_Inh_StringStringListPair :: Environment,lib_Inh_StringStringListPair :: LocalIdentifierBindings}
data Syn_StringStringListPair  = Syn_StringStringListPair {annotatedTree_Syn_StringStringListPair :: StringStringListPair,originalTree_Syn_StringStringListPair :: StringStringListPair}
wrap_StringStringListPair :: T_StringStringListPair  ->
                             Inh_StringStringListPair  ->
                             Syn_StringStringListPair 
wrap_StringStringListPair sem (Inh_StringStringListPair _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_StringStringListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_StringStringListPair_Tuple x1_ x2_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let x2_1 :: T_StringList_1 
              _x2IoriginalTree :: StringList
              _x2Olib :: LocalIdentifierBindings
              _x2Oenv :: Environment
              _x2IannotatedTree :: StringList
              _x2Istrings :: ([String])
              _lhsOannotatedTree :: StringStringListPair
              _lhsOoriginalTree :: StringStringListPair
              ( _x2IoriginalTree,x2_1) =
                  (x2_ )
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12383 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 12388 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2Istrings) =
                  (x2_1 _x2Oenv _x2Olib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (x1_,_x2IannotatedTree)
                  {-# LINE 12395 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12400 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (x1_,_x2IoriginalTree)
                  {-# LINE 12405 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12410 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- StringStringListPairList ------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : StringStringListPair 
         child tl             : StringStringListPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type StringStringListPairList  = [(StringStringListPair)]
-- cata
sem_StringStringListPairList :: StringStringListPairList  ->
                                T_StringStringListPairList 
sem_StringStringListPairList list  =
    (Prelude.foldr sem_StringStringListPairList_Cons sem_StringStringListPairList_Nil (Prelude.map sem_StringStringListPair list) )
-- semantic domain
type T_StringStringListPairList  = Environment ->
                                   LocalIdentifierBindings ->
                                   ( StringStringListPairList,StringStringListPairList)
data Inh_StringStringListPairList  = Inh_StringStringListPairList {env_Inh_StringStringListPairList :: Environment,lib_Inh_StringStringListPairList :: LocalIdentifierBindings}
data Syn_StringStringListPairList  = Syn_StringStringListPairList {annotatedTree_Syn_StringStringListPairList :: StringStringListPairList,originalTree_Syn_StringStringListPairList :: StringStringListPairList}
wrap_StringStringListPairList :: T_StringStringListPairList  ->
                                 Inh_StringStringListPairList  ->
                                 Syn_StringStringListPairList 
wrap_StringStringListPairList sem (Inh_StringStringListPairList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_StringStringListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_StringStringListPairList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOlib :: LocalIdentifierBindings
              _tlOenv :: Environment
              _tlIannotatedTree :: StringStringListPairList
              _tlIoriginalTree :: StringStringListPairList
              _hdOlib :: LocalIdentifierBindings
              _hdOenv :: Environment
              _hdIannotatedTree :: StringStringListPair
              _hdIoriginalTree :: StringStringListPair
              _lhsOannotatedTree :: StringStringListPairList
              _lhsOoriginalTree :: StringStringListPairList
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12469 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 12474 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOenv _tlOlib )
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12481 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 12486 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 12493 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12498 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 12503 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12508 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_StringStringListPairList_Nil  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: StringStringListPairList
              _lhsOoriginalTree :: StringStringListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 12519 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12524 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 12529 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12534 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- TableRef ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         idLookups            : [(String,Type)]
         qidLookups           : [(String,[(String,Type)])]
         qstarExpansion       : [(String,[(String,Type)])]
         starExpansion        : [(String,Type)]
   visit 2:
      inherited attribute:
         jlibUpdates          : [LocalIdentifierBindingsUpdate]
      synthesized attributes:
         annotatedTree        : SELF 
         libUpdates           : [LocalIdentifierBindingsUpdate]
   alternatives:
      alternative JoinedTref:
         child ann            : {Annotation}
         child tbl            : TableRef 
         child nat            : Natural 
         child joinType       : JoinType 
         child tbl1           : TableRef 
         child onExpr         : OnExpr 
         visit 0:
            local originalTree : _
         visit 1:
            local ejoinAttrs  : _
            local joinAttrs   : _
            local joinNames   : _
            local removeJoinAttrs : _
            local idLookups   : _
            local qidLookups  : _
            local qstarExpansion : _
            local starExpansion : _
         visit 2:
            local errs        : _
            local libUpdates  : _
            local newLib      : _
            local backTree    : _
            intra ejoinAttrs  : _
            intra qstarExpansion : _
            intra starExpansion : _
            intra qidLookups  : _
            intra idLookups   : _
      alternative SubTref:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         child alias          : {String}
         visit 0:
            local originalTree : _
         visit 1:
            local selectAttrs : _
            local idLookups   : _
            local qidLookups  : _
            local qstarExpansion : _
            local starExpansion : _
         visit 2:
            local backTree    : _
            local errs        : _
            local libUpdates  : _
            intra selectAttrs : _
            intra qstarExpansion : _
            intra starExpansion : _
            intra qidLookups  : _
            intra idLookups   : _
      alternative Tref:
         child ann            : {Annotation}
         child tbl            : {String}
         visit 0:
            local originalTree : _
         visit 1:
            local relType     : _
            local relType1    : _
            local sAttrs      : _
            local pAttrs      : _
            local idLookups   : _
            local alias1      : _
            local qidLookups  : _
            local qstarExpansion : _
            local starExpansion : _
         visit 2:
            local backTree    : _
            local errs        : _
            local libUpdates  : _
            intra relType     : _
            intra qstarExpansion : _
            intra starExpansion : _
            intra qidLookups  : _
            intra idLookups   : _
      alternative TrefAlias:
         child ann            : {Annotation}
         child tbl            : {String}
         child alias          : {String}
         visit 0:
            local originalTree : _
         visit 1:
            local relType     : _
            local relType1    : _
            local sAttrs      : _
            local pAttrs      : _
            local idLookups   : _
            local alias1      : _
            local qidLookups  : _
            local qstarExpansion : _
            local starExpansion : _
         visit 2:
            local backTree    : _
            local errs        : _
            local libUpdates  : _
            intra relType     : _
            intra qstarExpansion : _
            intra starExpansion : _
            intra qidLookups  : _
            intra idLookups   : _
      alternative TrefFun:
         child ann            : {Annotation}
         child fn             : Expression 
         visit 0:
            local originalTree : _
         visit 1:
            local alias1      : _
            local eqfunIdens  : _
            local qfunIdens   : _
            local funIdens    : _
            local idLookups   : _
            local alias2      : _
            local qidLookups  : _
            local qstarExpansion : _
            local starExpansion : _
         visit 2:
            local backTree    : _
            local errs        : _
            local libUpdates  : _
            intra eqfunIdens  : _
            intra qstarExpansion : _
            intra starExpansion : _
            intra qidLookups  : _
            intra idLookups   : _
      alternative TrefFunAlias:
         child ann            : {Annotation}
         child fn             : Expression 
         child alias          : {String}
         visit 0:
            local originalTree : _
         visit 1:
            local alias1      : _
            local eqfunIdens  : _
            local qfunIdens   : _
            local funIdens    : _
            local idLookups   : _
            local alias2      : _
            local qidLookups  : _
            local qstarExpansion : _
            local starExpansion : _
         visit 2:
            local backTree    : _
            local errs        : _
            local libUpdates  : _
            intra eqfunIdens  : _
            intra qstarExpansion : _
            intra starExpansion : _
            intra qidLookups  : _
            intra idLookups   : _
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
type T_TableRef  = ( TableRef,T_TableRef_1 )
type T_TableRef_1  = Environment ->
                     LocalIdentifierBindings ->
                     ( ([(String,Type)]),([(String,[(String,Type)])]),([(String,[(String,Type)])]),([(String,Type)]),T_TableRef_2 )
type T_TableRef_2  = ([LocalIdentifierBindingsUpdate]) ->
                     ( TableRef,([LocalIdentifierBindingsUpdate]))
data Inh_TableRef  = Inh_TableRef {env_Inh_TableRef :: Environment,jlibUpdates_Inh_TableRef :: [LocalIdentifierBindingsUpdate],lib_Inh_TableRef :: LocalIdentifierBindings}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef,idLookups_Syn_TableRef :: [(String,Type)],libUpdates_Syn_TableRef :: [LocalIdentifierBindingsUpdate],originalTree_Syn_TableRef :: TableRef,qidLookups_Syn_TableRef :: [(String,[(String,Type)])],qstarExpansion_Syn_TableRef :: [(String,[(String,Type)])],starExpansion_Syn_TableRef :: [(String,Type)]}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIenv _lhsIjlibUpdates _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOidLookups,_lhsOqidLookups,_lhsOqstarExpansion,_lhsOstarExpansion,sem_2) =
             (sem_1 _lhsIenv _lhsIlib )
         ( _lhsOannotatedTree,_lhsOlibUpdates) =
             (sem_2 _lhsIjlibUpdates )
     in  (Syn_TableRef _lhsOannotatedTree _lhsOidLookups _lhsOlibUpdates _lhsOoriginalTree _lhsOqidLookups _lhsOqstarExpansion _lhsOstarExpansion ))
sem_TableRef_JoinedTref ann_ tbl_ nat_ joinType_ tbl1_ onExpr_  =
    (let onExpr_1 :: T_OnExpr_1 
         _onExprIoriginalTree :: OnExpr
         tbl1_1 :: T_TableRef_1 
         _tbl1IoriginalTree :: TableRef
         joinType_1 :: T_JoinType_1 
         _joinTypeIoriginalTree :: JoinType
         nat_1 :: T_Natural_1 
         _natIoriginalTree :: Natural
         tbl_1 :: T_TableRef_1 
         _tblIoriginalTree :: TableRef
         _lhsOoriginalTree :: TableRef
         ( _onExprIoriginalTree,onExpr_1) =
             (onExpr_ )
         ( _tbl1IoriginalTree,tbl1_1) =
             (tbl1_ )
         ( _joinTypeIoriginalTree,joinType_1) =
             (joinType_ )
         ( _natIoriginalTree,nat_1) =
             (nat_ )
         ( _tblIoriginalTree,tbl_1) =
             (tbl_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             JoinedTref ann_ _tblIoriginalTree _natIoriginalTree _joinTypeIoriginalTree _tbl1IoriginalTree _onExprIoriginalTree
             {-# LINE 12773 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 12778 "AstInternal.hs" #-}
         ( sem_TableRef_1) =
             (sem_TableRef_JoinedTref_1 tbl1_1 nat_1 tbl_1 _onExprIoriginalTree onExpr_1 joinType_1 ann_ )
     in  ( _lhsOoriginalTree,sem_TableRef_1))
sem_TableRef_JoinedTref_1 tbl1_1 nat_1 tbl_1 _onExprIoriginalTree onExpr_1 joinType_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tbl1Olib :: LocalIdentifierBindings
              _tbl1Oenv :: Environment
              _tblOlib :: LocalIdentifierBindings
              _tblOenv :: Environment
              tbl1_2 :: T_TableRef_2 
              _tbl1IidLookups :: ([(String,Type)])
              _tbl1IqidLookups :: ([(String,[(String,Type)])])
              _tbl1IqstarExpansion :: ([(String,[(String,Type)])])
              _tbl1IstarExpansion :: ([(String,Type)])
              _natOlib :: LocalIdentifierBindings
              _natOenv :: Environment
              _natIannotatedTree :: Natural
              tbl_2 :: T_TableRef_2 
              _tblIidLookups :: ([(String,Type)])
              _tblIqidLookups :: ([(String,[(String,Type)])])
              _tblIqstarExpansion :: ([(String,[(String,Type)])])
              _tblIstarExpansion :: ([(String,Type)])
              _lhsOidLookups :: ([(String,Type)])
              _lhsOqidLookups :: ([(String,[(String,Type)])])
              _lhsOqstarExpansion :: ([(String,[(String,Type)])])
              _lhsOstarExpansion :: ([(String,Type)])
              -- copy rule (down)
              _tbl1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12810 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1Oenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 12815 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12820 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 12825 "AstInternal.hs" #-}
              ( _tbl1IidLookups,_tbl1IqidLookups,_tbl1IqstarExpansion,_tbl1IstarExpansion,tbl1_2) =
                  (tbl1_1 _tbl1Oenv _tbl1Olib )
              -- copy rule (down)
              _natOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12832 "AstInternal.hs" #-}
              -- copy rule (down)
              _natOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 12837 "AstInternal.hs" #-}
              ( _natIannotatedTree) =
                  (nat_1 _natOenv _natOlib )
              ( _tblIidLookups,_tblIqidLookups,_tblIqstarExpansion,_tblIstarExpansion,tbl_2) =
                  (tbl_1 _tblOenv _tblOlib )
              -- "./TypeChecking/TableRefs.ag"(line 142, column 9)
              _ejoinAttrs =
                  {-# LINE 142 "./TypeChecking/TableRefs.ag" #-}
                  do
                  let jns = case (_natIannotatedTree, _onExprIoriginalTree) of
                                (Natural, _) -> commonFieldNames
                                (_,Just (JoinUsing _ s)) -> s
                                _ -> []
                      tjtsm = map (flip lookup _tblIidLookups) jns
                      t1jtsm = map (flip lookup _tbl1IidLookups) jns
                  errorWhen (not $ null $ filter (==Nothing) $ tjtsm ++ t1jtsm)
                            [MissingJoinAttribute]
                  let tjts = catMaybes tjtsm
                      t1jts = catMaybes t1jtsm
                      resolvedTypes :: [Either [TypeError] Type]
                      resolvedTypes = map (\(a,b) -> resolveResultSetType _lhsIenv [a,b]) $ zip tjts t1jts
                  liftErrors $ concat $ lefts resolvedTypes
                  return $ zip jns $ rights resolvedTypes
                  where
                    commonFieldNames = intersect (f _tblIstarExpansion) (f _tbl1IstarExpansion)
                                       where f = map fst
                  {-# LINE 12863 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 167, column 9)
              _joinAttrs =
                  {-# LINE 167 "./TypeChecking/TableRefs.ag" #-}
                  fromRight [] _ejoinAttrs
                  {-# LINE 12868 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 166, column 9)
              _joinNames =
                  {-# LINE 166 "./TypeChecking/TableRefs.ag" #-}
                  map fst _joinAttrs
                  {-# LINE 12873 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 133, column 9)
              _removeJoinAttrs =
                  {-# LINE 133 "./TypeChecking/TableRefs.ag" #-}
                  filter (\(n,_) -> n `notElem` _joinNames    )
                  {-# LINE 12878 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 172, column 9)
              _idLookups =
                  {-# LINE 172 "./TypeChecking/TableRefs.ag" #-}
                  _joinAttrs     ++
                    _removeJoinAttrs     _tblIidLookups ++
                    _removeJoinAttrs     _tbl1IidLookups
                  {-# LINE 12885 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 235, column 9)
              _lhsOidLookups =
                  {-# LINE 235 "./TypeChecking/TableRefs.ag" #-}
                  _idLookups
                  {-# LINE 12890 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 175, column 9)
              _qidLookups =
                  {-# LINE 175 "./TypeChecking/TableRefs.ag" #-}
                  _tblIqidLookups ++ _tbl1IqidLookups
                  {-# LINE 12895 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 236, column 9)
              _lhsOqidLookups =
                  {-# LINE 236 "./TypeChecking/TableRefs.ag" #-}
                  _qidLookups
                  {-# LINE 12900 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 179, column 9)
              _qstarExpansion =
                  {-# LINE 179 "./TypeChecking/TableRefs.ag" #-}
                  _tblIqstarExpansion ++ _tbl1IqstarExpansion
                  {-# LINE 12905 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 238, column 9)
              _lhsOqstarExpansion =
                  {-# LINE 238 "./TypeChecking/TableRefs.ag" #-}
                  _qstarExpansion
                  {-# LINE 12910 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 176, column 9)
              _starExpansion =
                  {-# LINE 176 "./TypeChecking/TableRefs.ag" #-}
                  _joinAttrs     ++
                    _removeJoinAttrs     _tblIstarExpansion ++
                    _removeJoinAttrs     _tbl1IstarExpansion
                  {-# LINE 12917 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 237, column 9)
              _lhsOstarExpansion =
                  {-# LINE 237 "./TypeChecking/TableRefs.ag" #-}
                  _starExpansion
                  {-# LINE 12922 "AstInternal.hs" #-}
              ( sem_TableRef_2) =
                  (sem_TableRef_JoinedTref_2 _lhsIenv _ejoinAttrs _qstarExpansion _starExpansion _qidLookups _idLookups _lhsIlib onExpr_1 tbl1_2 joinType_1 tbl_2 ann_ _natIannotatedTree )
          in  ( _lhsOidLookups,_lhsOqidLookups,_lhsOqstarExpansion,_lhsOstarExpansion,sem_TableRef_2)))
sem_TableRef_JoinedTref_2 _lhsIenv _ejoinAttrs _qstarExpansion _starExpansion _qidLookups _idLookups _lhsIlib onExpr_1 tbl1_2 joinType_1 tbl_2 ann_ _natIannotatedTree  =
    (\ _lhsIjlibUpdates ->
         (let _onExprOenv :: Environment
              _tbl1OjlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _tblOjlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _onExprOlib :: LocalIdentifierBindings
              _onExprIannotatedTree :: OnExpr
              _tbl1IannotatedTree :: TableRef
              _tbl1IlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _joinTypeOlib :: LocalIdentifierBindings
              _joinTypeOenv :: Environment
              _joinTypeIannotatedTree :: JoinType
              _tblIannotatedTree :: TableRef
              _tblIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- copy rule (down)
              _onExprOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 12946 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 132, column 9)
              _errs =
                  {-# LINE 132 "./TypeChecking/TableRefs.ag" #-}
                  fromLeft [] _ejoinAttrs
                  {-# LINE 12951 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 69, column 9)
              _libUpdates =
                  {-# LINE 69 "./TypeChecking/TableRefs.ag" #-}
                  if null _errs
                    then [LibStackIDs $ ("", _idLookups    ): _qidLookups
                         ,LibSetStarExpansion $ ("", _starExpansion    ): _qstarExpansion    ]
                    else []
                  {-# LINE 12959 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 189, column 9)
              _tbl1OjlibUpdates =
                  {-# LINE 189 "./TypeChecking/TableRefs.ag" #-}
                  _libUpdates
                  {-# LINE 12964 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 188, column 9)
              _tblOjlibUpdates =
                  {-# LINE 188 "./TypeChecking/TableRefs.ag" #-}
                  _libUpdates
                  {-# LINE 12969 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 183, column 9)
              _newLib =
                  {-# LINE 183 "./TypeChecking/TableRefs.ag" #-}
                  case updateBindings _lhsIlib _lhsIenv (_libUpdates     ++ _lhsIjlibUpdates) of
                    Left x -> error $ show x
                    Right e ->                                      e
                  {-# LINE 12976 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 186, column 9)
              _onExprOlib =
                  {-# LINE 186 "./TypeChecking/TableRefs.ag" #-}
                  _newLib
                  {-# LINE 12981 "AstInternal.hs" #-}
              ( _onExprIannotatedTree) =
                  (onExpr_1 _onExprOenv _onExprOlib )
              ( _tbl1IannotatedTree,_tbl1IlibUpdates) =
                  (tbl1_2 _tbl1OjlibUpdates )
              -- copy rule (down)
              _joinTypeOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12990 "AstInternal.hs" #-}
              -- copy rule (down)
              _joinTypeOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 12995 "AstInternal.hs" #-}
              ( _joinTypeIannotatedTree) =
                  (joinType_1 _joinTypeOenv _joinTypeOlib )
              ( _tblIannotatedTree,_tblIlibUpdates) =
                  (tbl_2 _tblOjlibUpdates )
              -- "./TypeChecking/TableRefs.ag"(line 255, column 9)
              _backTree =
                  {-# LINE 255 "./TypeChecking/TableRefs.ag" #-}
                  JoinedTref ann_
                             _tblIannotatedTree
                             _natIannotatedTree
                             _joinTypeIannotatedTree
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
                  {-# LINE 13009 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 33, column 9)
              _lhsOannotatedTree =
                  {-# LINE 33 "./TypeChecking/TableRefs.ag" #-}
                  updateAnnotation (map TypeErrorA _errs     ++) _backTree
                  {-# LINE 13014 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 234, column 9)
              _lhsOlibUpdates =
                  {-# LINE 234 "./TypeChecking/TableRefs.ag" #-}
                  _libUpdates
                  {-# LINE 13019 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
sem_TableRef_SubTref ann_ sel_ alias_  =
    (let sel_1 :: T_SelectExpression_1 
         _selIoriginalTree :: SelectExpression
         _lhsOoriginalTree :: TableRef
         ( _selIoriginalTree,sel_1) =
             (sel_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             SubTref ann_ _selIoriginalTree alias_
             {-# LINE 13031 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 13036 "AstInternal.hs" #-}
         ( sem_TableRef_1) =
             (sem_TableRef_SubTref_1 sel_1 alias_ ann_ )
     in  ( _lhsOoriginalTree,sem_TableRef_1))
sem_TableRef_SubTref_1 sel_1 alias_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _selOlib :: LocalIdentifierBindings
              _selOenv :: Environment
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalIdentifierBindingsUpdate])
              _lhsOidLookups :: ([(String,Type)])
              _lhsOqidLookups :: ([(String,[(String,Type)])])
              _lhsOqstarExpansion :: ([(String,[(String,Type)])])
              _lhsOstarExpansion :: ([(String,Type)])
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13055 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 13060 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIlibUpdates) =
                  (sel_1 _selOenv _selOlib )
              -- "./TypeChecking/TableRefs.ag"(line 89, column 9)
              _selectAttrs =
                  {-# LINE 89 "./TypeChecking/TableRefs.ag" #-}
                  unwrapSetOfComposite (getTypeAnnotation _selIannotatedTree)
                  {-# LINE 13067 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _idLookups =
                  {-# LINE 91 "./TypeChecking/TableRefs.ag" #-}
                  fromRight [] _selectAttrs
                  {-# LINE 13072 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 235, column 9)
              _lhsOidLookups =
                  {-# LINE 235 "./TypeChecking/TableRefs.ag" #-}
                  _idLookups
                  {-# LINE 13077 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 92, column 9)
              _qidLookups =
                  {-# LINE 92 "./TypeChecking/TableRefs.ag" #-}
                  [(alias_, _idLookups    )]
                  {-# LINE 13082 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 236, column 9)
              _lhsOqidLookups =
                  {-# LINE 236 "./TypeChecking/TableRefs.ag" #-}
                  _qidLookups
                  {-# LINE 13087 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 94, column 9)
              _qstarExpansion =
                  {-# LINE 94 "./TypeChecking/TableRefs.ag" #-}
                  _qidLookups
                  {-# LINE 13092 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 238, column 9)
              _lhsOqstarExpansion =
                  {-# LINE 238 "./TypeChecking/TableRefs.ag" #-}
                  _qstarExpansion
                  {-# LINE 13097 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 93, column 9)
              _starExpansion =
                  {-# LINE 93 "./TypeChecking/TableRefs.ag" #-}
                  _idLookups
                  {-# LINE 13102 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 237, column 9)
              _lhsOstarExpansion =
                  {-# LINE 237 "./TypeChecking/TableRefs.ag" #-}
                  _starExpansion
                  {-# LINE 13107 "AstInternal.hs" #-}
              ( sem_TableRef_2) =
                  (sem_TableRef_SubTref_2 alias_ ann_ _selIannotatedTree _selectAttrs _qstarExpansion _starExpansion _qidLookups _idLookups )
          in  ( _lhsOidLookups,_lhsOqidLookups,_lhsOqstarExpansion,_lhsOstarExpansion,sem_TableRef_2)))
sem_TableRef_SubTref_2 alias_ ann_ _selIannotatedTree _selectAttrs _qstarExpansion _starExpansion _qidLookups _idLookups  =
    (\ _lhsIjlibUpdates ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/TableRefs.ag"(line 244, column 9)
              _backTree =
                  {-# LINE 244 "./TypeChecking/TableRefs.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 13119 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 86, column 9)
              _errs =
                  {-# LINE 86 "./TypeChecking/TableRefs.ag" #-}
                  case _selectAttrs     of
                          Left e -> e
                          Right _ -> []
                  {-# LINE 13126 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 33, column 9)
              _lhsOannotatedTree =
                  {-# LINE 33 "./TypeChecking/TableRefs.ag" #-}
                  updateAnnotation (map TypeErrorA _errs     ++) _backTree
                  {-# LINE 13131 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 69, column 9)
              _libUpdates =
                  {-# LINE 69 "./TypeChecking/TableRefs.ag" #-}
                  if null _errs
                    then [LibStackIDs $ ("", _idLookups    ): _qidLookups
                         ,LibSetStarExpansion $ ("", _starExpansion    ): _qstarExpansion    ]
                    else []
                  {-# LINE 13139 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 234, column 9)
              _lhsOlibUpdates =
                  {-# LINE 234 "./TypeChecking/TableRefs.ag" #-}
                  _libUpdates
                  {-# LINE 13144 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
sem_TableRef_Tref ann_ tbl_  =
    (let _lhsOoriginalTree :: TableRef
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             Tref ann_ tbl_
             {-# LINE 13152 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 13157 "AstInternal.hs" #-}
         ( sem_TableRef_1) =
             (sem_TableRef_Tref_1 tbl_ ann_ )
     in  ( _lhsOoriginalTree,sem_TableRef_1))
sem_TableRef_Tref_1 tbl_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOidLookups :: ([(String,Type)])
              _lhsOqidLookups :: ([(String,[(String,Type)])])
              _lhsOqstarExpansion :: ([(String,[(String,Type)])])
              _lhsOstarExpansion :: ([(String,Type)])
              -- "./TypeChecking/TableRefs.ag"(line 100, column 9)
              _relType =
                  {-# LINE 100 "./TypeChecking/TableRefs.ag" #-}
                  envCompositeAttrsPair _lhsIenv [] tbl_
                  {-# LINE 13172 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 101, column 9)
              _relType1 =
                  {-# LINE 101 "./TypeChecking/TableRefs.ag" #-}
                  fromRight ([],[]) _relType
                  {-# LINE 13177 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 103, column 9)
              _sAttrs =
                  {-# LINE 103 "./TypeChecking/TableRefs.ag" #-}
                  snd _relType1
                  {-# LINE 13182 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 102, column 9)
              _pAttrs =
                  {-# LINE 102 "./TypeChecking/TableRefs.ag" #-}
                  fst _relType1
                  {-# LINE 13187 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 105, column 9)
              _idLookups =
                  {-# LINE 105 "./TypeChecking/TableRefs.ag" #-}
                  _pAttrs     ++ _sAttrs
                  {-# LINE 13192 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 235, column 9)
              _lhsOidLookups =
                  {-# LINE 235 "./TypeChecking/TableRefs.ag" #-}
                  _idLookups
                  {-# LINE 13197 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 110, column 9)
              _alias1 =
                  {-# LINE 110 "./TypeChecking/TableRefs.ag" #-}
                  tbl_
                  {-# LINE 13202 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 106, column 9)
              _qidLookups =
                  {-# LINE 106 "./TypeChecking/TableRefs.ag" #-}
                  [(_alias1    , _idLookups    )]
                  {-# LINE 13207 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 236, column 9)
              _lhsOqidLookups =
                  {-# LINE 236 "./TypeChecking/TableRefs.ag" #-}
                  _qidLookups
                  {-# LINE 13212 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 108, column 9)
              _qstarExpansion =
                  {-# LINE 108 "./TypeChecking/TableRefs.ag" #-}
                  [(_alias1    , _pAttrs    )]
                  {-# LINE 13217 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 238, column 9)
              _lhsOqstarExpansion =
                  {-# LINE 238 "./TypeChecking/TableRefs.ag" #-}
                  _qstarExpansion
                  {-# LINE 13222 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 107, column 9)
              _starExpansion =
                  {-# LINE 107 "./TypeChecking/TableRefs.ag" #-}
                  _pAttrs
                  {-# LINE 13227 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 237, column 9)
              _lhsOstarExpansion =
                  {-# LINE 237 "./TypeChecking/TableRefs.ag" #-}
                  _starExpansion
                  {-# LINE 13232 "AstInternal.hs" #-}
              ( sem_TableRef_2) =
                  (sem_TableRef_Tref_2 tbl_ ann_ _relType _qstarExpansion _starExpansion _qidLookups _idLookups )
          in  ( _lhsOidLookups,_lhsOqidLookups,_lhsOqstarExpansion,_lhsOstarExpansion,sem_TableRef_2)))
sem_TableRef_Tref_2 tbl_ ann_ _relType _qstarExpansion _starExpansion _qidLookups _idLookups  =
    (\ _lhsIjlibUpdates ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/TableRefs.ag"(line 246, column 9)
              _backTree =
                  {-# LINE 246 "./TypeChecking/TableRefs.ag" #-}
                  Tref ann_ tbl_
                  {-# LINE 13244 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 97, column 9)
              _errs =
                  {-# LINE 97 "./TypeChecking/TableRefs.ag" #-}
                  case _relType     of
                    Left e -> e
                    Right _ -> []
                  {-# LINE 13251 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 33, column 9)
              _lhsOannotatedTree =
                  {-# LINE 33 "./TypeChecking/TableRefs.ag" #-}
                  updateAnnotation (map TypeErrorA _errs     ++) _backTree
                  {-# LINE 13256 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 69, column 9)
              _libUpdates =
                  {-# LINE 69 "./TypeChecking/TableRefs.ag" #-}
                  if null _errs
                    then [LibStackIDs $ ("", _idLookups    ): _qidLookups
                         ,LibSetStarExpansion $ ("", _starExpansion    ): _qstarExpansion    ]
                    else []
                  {-# LINE 13264 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 234, column 9)
              _lhsOlibUpdates =
                  {-# LINE 234 "./TypeChecking/TableRefs.ag" #-}
                  _libUpdates
                  {-# LINE 13269 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
sem_TableRef_TrefAlias ann_ tbl_ alias_  =
    (let _lhsOoriginalTree :: TableRef
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             TrefAlias ann_ tbl_ alias_
             {-# LINE 13277 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 13282 "AstInternal.hs" #-}
         ( sem_TableRef_1) =
             (sem_TableRef_TrefAlias_1 tbl_ alias_ ann_ )
     in  ( _lhsOoriginalTree,sem_TableRef_1))
sem_TableRef_TrefAlias_1 tbl_ alias_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOidLookups :: ([(String,Type)])
              _lhsOqidLookups :: ([(String,[(String,Type)])])
              _lhsOqstarExpansion :: ([(String,[(String,Type)])])
              _lhsOstarExpansion :: ([(String,Type)])
              -- "./TypeChecking/TableRefs.ag"(line 100, column 9)
              _relType =
                  {-# LINE 100 "./TypeChecking/TableRefs.ag" #-}
                  envCompositeAttrsPair _lhsIenv [] tbl_
                  {-# LINE 13297 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 101, column 9)
              _relType1 =
                  {-# LINE 101 "./TypeChecking/TableRefs.ag" #-}
                  fromRight ([],[]) _relType
                  {-# LINE 13302 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 103, column 9)
              _sAttrs =
                  {-# LINE 103 "./TypeChecking/TableRefs.ag" #-}
                  snd _relType1
                  {-# LINE 13307 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 102, column 9)
              _pAttrs =
                  {-# LINE 102 "./TypeChecking/TableRefs.ag" #-}
                  fst _relType1
                  {-# LINE 13312 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 105, column 9)
              _idLookups =
                  {-# LINE 105 "./TypeChecking/TableRefs.ag" #-}
                  _pAttrs     ++ _sAttrs
                  {-# LINE 13317 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 235, column 9)
              _lhsOidLookups =
                  {-# LINE 235 "./TypeChecking/TableRefs.ag" #-}
                  _idLookups
                  {-# LINE 13322 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 112, column 9)
              _alias1 =
                  {-# LINE 112 "./TypeChecking/TableRefs.ag" #-}
                  alias_
                  {-# LINE 13327 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 106, column 9)
              _qidLookups =
                  {-# LINE 106 "./TypeChecking/TableRefs.ag" #-}
                  [(_alias1    , _idLookups    )]
                  {-# LINE 13332 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 236, column 9)
              _lhsOqidLookups =
                  {-# LINE 236 "./TypeChecking/TableRefs.ag" #-}
                  _qidLookups
                  {-# LINE 13337 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 108, column 9)
              _qstarExpansion =
                  {-# LINE 108 "./TypeChecking/TableRefs.ag" #-}
                  [(_alias1    , _pAttrs    )]
                  {-# LINE 13342 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 238, column 9)
              _lhsOqstarExpansion =
                  {-# LINE 238 "./TypeChecking/TableRefs.ag" #-}
                  _qstarExpansion
                  {-# LINE 13347 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 107, column 9)
              _starExpansion =
                  {-# LINE 107 "./TypeChecking/TableRefs.ag" #-}
                  _pAttrs
                  {-# LINE 13352 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 237, column 9)
              _lhsOstarExpansion =
                  {-# LINE 237 "./TypeChecking/TableRefs.ag" #-}
                  _starExpansion
                  {-# LINE 13357 "AstInternal.hs" #-}
              ( sem_TableRef_2) =
                  (sem_TableRef_TrefAlias_2 alias_ tbl_ ann_ _relType _qstarExpansion _starExpansion _qidLookups _idLookups )
          in  ( _lhsOidLookups,_lhsOqidLookups,_lhsOqstarExpansion,_lhsOstarExpansion,sem_TableRef_2)))
sem_TableRef_TrefAlias_2 alias_ tbl_ ann_ _relType _qstarExpansion _starExpansion _qidLookups _idLookups  =
    (\ _lhsIjlibUpdates ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/TableRefs.ag"(line 248, column 9)
              _backTree =
                  {-# LINE 248 "./TypeChecking/TableRefs.ag" #-}
                  TrefAlias ann_ tbl_ alias_
                  {-# LINE 13369 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 97, column 9)
              _errs =
                  {-# LINE 97 "./TypeChecking/TableRefs.ag" #-}
                  case _relType     of
                    Left e -> e
                    Right _ -> []
                  {-# LINE 13376 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 33, column 9)
              _lhsOannotatedTree =
                  {-# LINE 33 "./TypeChecking/TableRefs.ag" #-}
                  updateAnnotation (map TypeErrorA _errs     ++) _backTree
                  {-# LINE 13381 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 69, column 9)
              _libUpdates =
                  {-# LINE 69 "./TypeChecking/TableRefs.ag" #-}
                  if null _errs
                    then [LibStackIDs $ ("", _idLookups    ): _qidLookups
                         ,LibSetStarExpansion $ ("", _starExpansion    ): _qstarExpansion    ]
                    else []
                  {-# LINE 13389 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 234, column 9)
              _lhsOlibUpdates =
                  {-# LINE 234 "./TypeChecking/TableRefs.ag" #-}
                  _libUpdates
                  {-# LINE 13394 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
sem_TableRef_TrefFun ann_ fn_  =
    (let fn_1 :: T_Expression_1 
         _fnIoriginalTree :: Expression
         _lhsOoriginalTree :: TableRef
         ( _fnIoriginalTree,fn_1) =
             (fn_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             TrefFun ann_ _fnIoriginalTree
             {-# LINE 13406 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 13411 "AstInternal.hs" #-}
         ( sem_TableRef_1) =
             (sem_TableRef_TrefFun_1 fn_1 ann_ )
     in  ( _lhsOoriginalTree,sem_TableRef_1))
sem_TableRef_TrefFun_1 fn_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _fnOlib :: LocalIdentifierBindings
              _fnOenv :: Environment
              _fnIannotatedTree :: Expression
              _fnIliftedColumnName :: String
              _lhsOidLookups :: ([(String,Type)])
              _lhsOqidLookups :: ([(String,[(String,Type)])])
              _lhsOqstarExpansion :: ([(String,[(String,Type)])])
              _lhsOstarExpansion :: ([(String,Type)])
              -- copy rule (down)
              _fnOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13430 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 13435 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 128, column 9)
              _alias1 =
                  {-# LINE 128 "./TypeChecking/TableRefs.ag" #-}
                  ""
                  {-# LINE 13440 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIliftedColumnName) =
                  (fn_1 _fnOenv _fnOlib )
              -- "./TypeChecking/TableRefs.ag"(line 118, column 9)
              _eqfunIdens =
                  {-# LINE 118 "./TypeChecking/TableRefs.ag" #-}
                  funIdens _lhsIenv _alias1     _fnIannotatedTree
                  {-# LINE 13447 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 119, column 9)
              _qfunIdens =
                  {-# LINE 119 "./TypeChecking/TableRefs.ag" #-}
                  fromRight ("",[]) _eqfunIdens
                  {-# LINE 13452 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 121, column 9)
              _funIdens =
                  {-# LINE 121 "./TypeChecking/TableRefs.ag" #-}
                  snd _qfunIdens
                  {-# LINE 13457 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 123, column 9)
              _idLookups =
                  {-# LINE 123 "./TypeChecking/TableRefs.ag" #-}
                  _funIdens
                  {-# LINE 13462 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 235, column 9)
              _lhsOidLookups =
                  {-# LINE 235 "./TypeChecking/TableRefs.ag" #-}
                  _idLookups
                  {-# LINE 13467 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 120, column 9)
              _alias2 =
                  {-# LINE 120 "./TypeChecking/TableRefs.ag" #-}
                  fst _qfunIdens
                  {-# LINE 13472 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 124, column 9)
              _qidLookups =
                  {-# LINE 124 "./TypeChecking/TableRefs.ag" #-}
                  [(_alias2, _idLookups    )]
                  {-# LINE 13477 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 236, column 9)
              _lhsOqidLookups =
                  {-# LINE 236 "./TypeChecking/TableRefs.ag" #-}
                  _qidLookups
                  {-# LINE 13482 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 126, column 9)
              _qstarExpansion =
                  {-# LINE 126 "./TypeChecking/TableRefs.ag" #-}
                  _qidLookups
                  {-# LINE 13487 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 238, column 9)
              _lhsOqstarExpansion =
                  {-# LINE 238 "./TypeChecking/TableRefs.ag" #-}
                  _qstarExpansion
                  {-# LINE 13492 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 125, column 9)
              _starExpansion =
                  {-# LINE 125 "./TypeChecking/TableRefs.ag" #-}
                  _idLookups
                  {-# LINE 13497 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 237, column 9)
              _lhsOstarExpansion =
                  {-# LINE 237 "./TypeChecking/TableRefs.ag" #-}
                  _starExpansion
                  {-# LINE 13502 "AstInternal.hs" #-}
              ( sem_TableRef_2) =
                  (sem_TableRef_TrefFun_2 ann_ _fnIannotatedTree _eqfunIdens _qstarExpansion _starExpansion _qidLookups _idLookups )
          in  ( _lhsOidLookups,_lhsOqidLookups,_lhsOqstarExpansion,_lhsOstarExpansion,sem_TableRef_2)))
sem_TableRef_TrefFun_2 ann_ _fnIannotatedTree _eqfunIdens _qstarExpansion _starExpansion _qidLookups _idLookups  =
    (\ _lhsIjlibUpdates ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/TableRefs.ag"(line 251, column 9)
              _backTree =
                  {-# LINE 251 "./TypeChecking/TableRefs.ag" #-}
                  TrefFun ann_ _fnIannotatedTree
                  {-# LINE 13514 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 115, column 9)
              _errs =
                  {-# LINE 115 "./TypeChecking/TableRefs.ag" #-}
                  case _eqfunIdens of
                    Left e -> e
                    Right _ -> []
                  {-# LINE 13521 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 33, column 9)
              _lhsOannotatedTree =
                  {-# LINE 33 "./TypeChecking/TableRefs.ag" #-}
                  updateAnnotation (map TypeErrorA _errs     ++) _backTree
                  {-# LINE 13526 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 69, column 9)
              _libUpdates =
                  {-# LINE 69 "./TypeChecking/TableRefs.ag" #-}
                  if null _errs
                    then [LibStackIDs $ ("", _idLookups    ): _qidLookups
                         ,LibSetStarExpansion $ ("", _starExpansion    ): _qstarExpansion    ]
                    else []
                  {-# LINE 13534 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 234, column 9)
              _lhsOlibUpdates =
                  {-# LINE 234 "./TypeChecking/TableRefs.ag" #-}
                  _libUpdates
                  {-# LINE 13539 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
sem_TableRef_TrefFunAlias ann_ fn_ alias_  =
    (let fn_1 :: T_Expression_1 
         _fnIoriginalTree :: Expression
         _lhsOoriginalTree :: TableRef
         ( _fnIoriginalTree,fn_1) =
             (fn_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             TrefFunAlias ann_ _fnIoriginalTree alias_
             {-# LINE 13551 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 13556 "AstInternal.hs" #-}
         ( sem_TableRef_1) =
             (sem_TableRef_TrefFunAlias_1 alias_ fn_1 ann_ )
     in  ( _lhsOoriginalTree,sem_TableRef_1))
sem_TableRef_TrefFunAlias_1 alias_ fn_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _fnOlib :: LocalIdentifierBindings
              _fnOenv :: Environment
              _fnIannotatedTree :: Expression
              _fnIliftedColumnName :: String
              _lhsOidLookups :: ([(String,Type)])
              _lhsOqidLookups :: ([(String,[(String,Type)])])
              _lhsOqstarExpansion :: ([(String,[(String,Type)])])
              _lhsOstarExpansion :: ([(String,Type)])
              -- copy rule (down)
              _fnOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13575 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 13580 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 130, column 9)
              _alias1 =
                  {-# LINE 130 "./TypeChecking/TableRefs.ag" #-}
                  alias_
                  {-# LINE 13585 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIliftedColumnName) =
                  (fn_1 _fnOenv _fnOlib )
              -- "./TypeChecking/TableRefs.ag"(line 118, column 9)
              _eqfunIdens =
                  {-# LINE 118 "./TypeChecking/TableRefs.ag" #-}
                  funIdens _lhsIenv _alias1     _fnIannotatedTree
                  {-# LINE 13592 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 119, column 9)
              _qfunIdens =
                  {-# LINE 119 "./TypeChecking/TableRefs.ag" #-}
                  fromRight ("",[]) _eqfunIdens
                  {-# LINE 13597 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 121, column 9)
              _funIdens =
                  {-# LINE 121 "./TypeChecking/TableRefs.ag" #-}
                  snd _qfunIdens
                  {-# LINE 13602 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 123, column 9)
              _idLookups =
                  {-# LINE 123 "./TypeChecking/TableRefs.ag" #-}
                  _funIdens
                  {-# LINE 13607 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 235, column 9)
              _lhsOidLookups =
                  {-# LINE 235 "./TypeChecking/TableRefs.ag" #-}
                  _idLookups
                  {-# LINE 13612 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 120, column 9)
              _alias2 =
                  {-# LINE 120 "./TypeChecking/TableRefs.ag" #-}
                  fst _qfunIdens
                  {-# LINE 13617 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 124, column 9)
              _qidLookups =
                  {-# LINE 124 "./TypeChecking/TableRefs.ag" #-}
                  [(_alias2, _idLookups    )]
                  {-# LINE 13622 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 236, column 9)
              _lhsOqidLookups =
                  {-# LINE 236 "./TypeChecking/TableRefs.ag" #-}
                  _qidLookups
                  {-# LINE 13627 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 126, column 9)
              _qstarExpansion =
                  {-# LINE 126 "./TypeChecking/TableRefs.ag" #-}
                  _qidLookups
                  {-# LINE 13632 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 238, column 9)
              _lhsOqstarExpansion =
                  {-# LINE 238 "./TypeChecking/TableRefs.ag" #-}
                  _qstarExpansion
                  {-# LINE 13637 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 125, column 9)
              _starExpansion =
                  {-# LINE 125 "./TypeChecking/TableRefs.ag" #-}
                  _idLookups
                  {-# LINE 13642 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 237, column 9)
              _lhsOstarExpansion =
                  {-# LINE 237 "./TypeChecking/TableRefs.ag" #-}
                  _starExpansion
                  {-# LINE 13647 "AstInternal.hs" #-}
              ( sem_TableRef_2) =
                  (sem_TableRef_TrefFunAlias_2 alias_ ann_ _fnIannotatedTree _eqfunIdens _qstarExpansion _starExpansion _qidLookups _idLookups )
          in  ( _lhsOidLookups,_lhsOqidLookups,_lhsOqstarExpansion,_lhsOstarExpansion,sem_TableRef_2)))
sem_TableRef_TrefFunAlias_2 alias_ ann_ _fnIannotatedTree _eqfunIdens _qstarExpansion _starExpansion _qidLookups _idLookups  =
    (\ _lhsIjlibUpdates ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalIdentifierBindingsUpdate])
              -- "./TypeChecking/TableRefs.ag"(line 253, column 9)
              _backTree =
                  {-# LINE 253 "./TypeChecking/TableRefs.ag" #-}
                  TrefFunAlias ann_ _fnIannotatedTree alias_
                  {-# LINE 13659 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 115, column 9)
              _errs =
                  {-# LINE 115 "./TypeChecking/TableRefs.ag" #-}
                  case _eqfunIdens of
                    Left e -> e
                    Right _ -> []
                  {-# LINE 13666 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 33, column 9)
              _lhsOannotatedTree =
                  {-# LINE 33 "./TypeChecking/TableRefs.ag" #-}
                  updateAnnotation (map TypeErrorA _errs     ++) _backTree
                  {-# LINE 13671 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 69, column 9)
              _libUpdates =
                  {-# LINE 69 "./TypeChecking/TableRefs.ag" #-}
                  if null _errs
                    then [LibStackIDs $ ("", _idLookups    ): _qidLookups
                         ,LibSetStarExpansion $ ("", _starExpansion    ): _qstarExpansion    ]
                    else []
                  {-# LINE 13679 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 234, column 9)
              _lhsOlibUpdates =
                  {-# LINE 234 "./TypeChecking/TableRefs.ag" #-}
                  _libUpdates
                  {-# LINE 13684 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates)))
-- TypeAttributeDef --------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         attrName             : String
         namedType            : Type
         originalTree         : SELF 
   alternatives:
      alternative TypeAttDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                           LocalIdentifierBindings ->
                           ( TypeAttributeDef,String,Type,TypeAttributeDef)
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {env_Inh_TypeAttributeDef :: Environment,lib_Inh_TypeAttributeDef :: LocalIdentifierBindings}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef,attrName_Syn_TypeAttributeDef :: String,namedType_Syn_TypeAttributeDef :: Type,originalTree_Syn_TypeAttributeDef :: TypeAttributeDef}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_TypeAttributeDef _lhsOannotatedTree _lhsOattrName _lhsOnamedType _lhsOoriginalTree ))
sem_TypeAttributeDef_TypeAttDef ann_ name_ typ_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _typOenv :: Environment
              typ_1 :: T_TypeName_1 
              _typIoriginalTree :: TypeName
              _typOlib :: LocalIdentifierBindings
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: TypeAttributeDef
              _lhsOattrName :: String
              _lhsOnamedType :: Type
              _lhsOoriginalTree :: TypeAttributeDef
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 13743 "AstInternal.hs" #-}
              ( _typIoriginalTree,typ_1) =
                  (typ_ )
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13750 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_1 _typOenv _typOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIannotatedTree
                  {-# LINE 13757 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13762 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 40, column 9)
              _lhsOattrName =
                  {-# LINE 40 "./TypeChecking/MiscCreates.ag" #-}
                  name_
                  {-# LINE 13767 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 41, column 9)
              _lhsOnamedType =
                  {-# LINE 41 "./TypeChecking/MiscCreates.ag" #-}
                  _typInamedType
                  {-# LINE 13772 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIoriginalTree
                  {-# LINE 13777 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13782 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType,_lhsOoriginalTree)))
-- TypeAttributeDefList ----------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         attrs                : [(String, Type)]
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : TypeAttributeDef 
         child tl             : TypeAttributeDefList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type TypeAttributeDefList  = [(TypeAttributeDef)]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = Environment ->
                               LocalIdentifierBindings ->
                               ( TypeAttributeDefList,([(String, Type)]),TypeAttributeDefList)
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {env_Inh_TypeAttributeDefList :: Environment,lib_Inh_TypeAttributeDefList :: LocalIdentifierBindings}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,attrs_Syn_TypeAttributeDefList :: [(String, Type)],originalTree_Syn_TypeAttributeDefList :: TypeAttributeDefList}
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_TypeAttributeDefList _lhsOannotatedTree _lhsOattrs _lhsOoriginalTree ))
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlOlib :: LocalIdentifierBindings
              _tlIannotatedTree :: TypeAttributeDefList
              _tlIattrs :: ([(String, Type)])
              _tlIoriginalTree :: TypeAttributeDefList
              _hdOlib :: LocalIdentifierBindings
              _hdIannotatedTree :: TypeAttributeDef
              _hdIattrName :: String
              _hdInamedType :: Type
              _hdIoriginalTree :: TypeAttributeDef
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOattrs :: ([(String, Type)])
              _lhsOoriginalTree :: TypeAttributeDefList
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 13846 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 13851 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13856 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIattrs,_tlIoriginalTree) =
                  (tl_ _tlOenv _tlOlib )
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13863 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIattrName,_hdInamedType,_hdIoriginalTree) =
                  (hd_ _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 13870 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13875 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 46, column 12)
              _lhsOattrs =
                  {-# LINE 46 "./TypeChecking/MiscCreates.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 13880 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 13885 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13890 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree)))
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOattrs :: ([(String, Type)])
              _lhsOoriginalTree :: TypeAttributeDefList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 13902 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13907 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 47, column 11)
              _lhsOattrs =
                  {-# LINE 47 "./TypeChecking/MiscCreates.ag" #-}
                  []
                  {-# LINE 13912 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 13917 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13922 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree)))
-- TypeName ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         namedType            : Type
   alternatives:
      alternative ArrayTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative PrecTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         child prec           : {Integer}
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative SetOfTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local originalTree : _
         visit 1:
            local backTree    : _
            local tpe         : _
      alternative SimpleTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         visit 0:
            local originalTree : _
         visit 1:
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
type T_TypeName  = ( TypeName,T_TypeName_1 )
type T_TypeName_1  = Environment ->
                     LocalIdentifierBindings ->
                     ( TypeName,Type)
data Inh_TypeName  = Inh_TypeName {env_Inh_TypeName :: Environment,lib_Inh_TypeName :: LocalIdentifierBindings}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName,namedType_Syn_TypeName :: Type,originalTree_Syn_TypeName :: TypeName}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIenv _lhsIlib )  =
    (let ( _lhsOoriginalTree,sem_1) =
             (sem )
         ( _lhsOannotatedTree,_lhsOnamedType) =
             (sem_1 _lhsIenv _lhsIlib )
     in  (Syn_TypeName _lhsOannotatedTree _lhsOnamedType _lhsOoriginalTree ))
sem_TypeName_ArrayTypeName ann_ typ_  =
    (let typ_1 :: T_TypeName_1 
         _typIoriginalTree :: TypeName
         _lhsOoriginalTree :: TypeName
         ( _typIoriginalTree,typ_1) =
             (typ_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             ArrayTypeName ann_ _typIoriginalTree
             {-# LINE 14013 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 14018 "AstInternal.hs" #-}
         ( sem_TypeName_1) =
             (sem_TypeName_ArrayTypeName_1 typ_1 ann_ )
     in  ( _lhsOoriginalTree,sem_TypeName_1))
sem_TypeName_ArrayTypeName_1 typ_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _typOenv :: Environment
              _typOlib :: LocalIdentifierBindings
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: TypeName
              _lhsOnamedType :: Type
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 14035 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14040 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_1 _typOenv _typOlib )
              -- "./TypeChecking/Misc.ag"(line 31, column 9)
              _backTree =
                  {-# LINE 31 "./TypeChecking/Misc.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 14047 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 30, column 9)
              _tpe =
                  {-# LINE 30 "./TypeChecking/Misc.ag" #-}
                  dependsOnRTpe [_typInamedType] $ Right $ ArrayType _typInamedType
                  {-# LINE 14052 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  updateAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 14059 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  tpeToT _tpe
                  {-# LINE 14064 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
sem_TypeName_PrecTypeName ann_ tn_ prec_  =
    (let _lhsOoriginalTree :: TypeName
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             PrecTypeName ann_ tn_ prec_
             {-# LINE 14072 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 14077 "AstInternal.hs" #-}
         ( sem_TypeName_1) =
             (sem_TypeName_PrecTypeName_1 prec_ tn_ ann_ )
     in  ( _lhsOoriginalTree,sem_TypeName_1))
sem_TypeName_PrecTypeName_1 prec_ tn_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: TypeName
              _lhsOnamedType :: Type
              -- "./TypeChecking/Misc.ag"(line 37, column 9)
              _backTree =
                  {-# LINE 37 "./TypeChecking/Misc.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 14090 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 36, column 9)
              _tpe =
                  {-# LINE 36 "./TypeChecking/Misc.ag" #-}
                  envLookupType _lhsIenv $ canonicalizeTypeName tn_
                  {-# LINE 14095 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  updateAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 14102 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  tpeToT _tpe
                  {-# LINE 14107 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
sem_TypeName_SetOfTypeName ann_ typ_  =
    (let typ_1 :: T_TypeName_1 
         _typIoriginalTree :: TypeName
         _lhsOoriginalTree :: TypeName
         ( _typIoriginalTree,typ_1) =
             (typ_ )
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             SetOfTypeName ann_ _typIoriginalTree
             {-# LINE 14119 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 14124 "AstInternal.hs" #-}
         ( sem_TypeName_1) =
             (sem_TypeName_SetOfTypeName_1 typ_1 ann_ )
     in  ( _lhsOoriginalTree,sem_TypeName_1))
sem_TypeName_SetOfTypeName_1 typ_1 ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _typOenv :: Environment
              _typOlib :: LocalIdentifierBindings
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: TypeName
              _lhsOnamedType :: Type
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 14141 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14146 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_1 _typOenv _typOlib )
              -- "./TypeChecking/Misc.ag"(line 34, column 9)
              _backTree =
                  {-# LINE 34 "./TypeChecking/Misc.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 14153 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 33, column 9)
              _tpe =
                  {-# LINE 33 "./TypeChecking/Misc.ag" #-}
                  dependsOnRTpe [_typInamedType] $ Right $ SetOfType _typInamedType
                  {-# LINE 14158 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  updateAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 14165 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  tpeToT _tpe
                  {-# LINE 14170 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
sem_TypeName_SimpleTypeName ann_ tn_  =
    (let _lhsOoriginalTree :: TypeName
         -- self rule
         _originalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             SimpleTypeName ann_ tn_
             {-# LINE 14178 "AstInternal.hs" #-}
         -- self rule
         _lhsOoriginalTree =
             {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 14183 "AstInternal.hs" #-}
         ( sem_TypeName_1) =
             (sem_TypeName_SimpleTypeName_1 tn_ ann_ )
     in  ( _lhsOoriginalTree,sem_TypeName_1))
sem_TypeName_SimpleTypeName_1 tn_ ann_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: TypeName
              _lhsOnamedType :: Type
              -- "./TypeChecking/Misc.ag"(line 28, column 9)
              _backTree =
                  {-# LINE 28 "./TypeChecking/Misc.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 14196 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 27, column 9)
              _tpe =
                  {-# LINE 27 "./TypeChecking/Misc.ag" #-}
                  envLookupType _lhsIenv $ canonicalizeTypeName tn_
                  {-# LINE 14201 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  updateAnnotation
                    ((map TypeErrorA $ getErrors _tpe    ) ++)
                    _backTree
                  {-# LINE 14208 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  tpeToT _tpe
                  {-# LINE 14213 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType)))
-- VarDef ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         def                  : (String,Type)
         originalTree         : SELF 
   alternatives:
      alternative VarDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         child value          : {Maybe Expression}
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                 LocalIdentifierBindings ->
                 ( VarDef,((String,Type)),VarDef)
data Inh_VarDef  = Inh_VarDef {env_Inh_VarDef :: Environment,lib_Inh_VarDef :: LocalIdentifierBindings}
data Syn_VarDef  = Syn_VarDef {annotatedTree_Syn_VarDef :: VarDef,def_Syn_VarDef :: (String,Type),originalTree_Syn_VarDef :: VarDef}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOdef,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_VarDef _lhsOannotatedTree _lhsOdef _lhsOoriginalTree ))
sem_VarDef_VarDef ann_ name_ typ_ value_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _typOenv :: Environment
              typ_1 :: T_TypeName_1 
              _typIoriginalTree :: TypeName
              _typOlib :: LocalIdentifierBindings
              _typIannotatedTree :: TypeName
              _typInamedType :: Type
              _lhsOannotatedTree :: VarDef
              _lhsOdef :: ((String,Type))
              _lhsOoriginalTree :: VarDef
              -- copy rule (down)
              _typOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 14271 "AstInternal.hs" #-}
              ( _typIoriginalTree,typ_1) =
                  (typ_ )
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14278 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType) =
                  (typ_1 _typOenv _typOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIannotatedTree value_
                  {-# LINE 14285 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14290 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 131, column 14)
              _lhsOdef =
                  {-# LINE 131 "./TypeChecking/CreateFunction.ag" #-}
                  (name_, if _typInamedType == Pseudo Record then PgRecord Nothing else _typInamedType)
                  {-# LINE 14295 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIoriginalTree value_
                  {-# LINE 14300 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14305 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdef,_lhsOoriginalTree)))
-- VarDefList --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         defs                 : [(String,Type)]
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : VarDef 
         child tl             : VarDefList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type VarDefList  = [(VarDef)]
-- cata
sem_VarDefList :: VarDefList  ->
                  T_VarDefList 
sem_VarDefList list  =
    (Prelude.foldr sem_VarDefList_Cons sem_VarDefList_Nil (Prelude.map sem_VarDef list) )
-- semantic domain
type T_VarDefList  = Environment ->
                     LocalIdentifierBindings ->
                     ( VarDefList,([(String,Type)]),VarDefList)
data Inh_VarDefList  = Inh_VarDefList {env_Inh_VarDefList :: Environment,lib_Inh_VarDefList :: LocalIdentifierBindings}
data Syn_VarDefList  = Syn_VarDefList {annotatedTree_Syn_VarDefList :: VarDefList,defs_Syn_VarDefList :: [(String,Type)],originalTree_Syn_VarDefList :: VarDefList}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOdefs,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_VarDefList _lhsOannotatedTree _lhsOdefs _lhsOoriginalTree ))
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _tlOenv :: Environment
              _hdOenv :: Environment
              _tlOlib :: LocalIdentifierBindings
              _tlIannotatedTree :: VarDefList
              _tlIdefs :: ([(String,Type)])
              _tlIoriginalTree :: VarDefList
              _hdOlib :: LocalIdentifierBindings
              _hdIannotatedTree :: VarDef
              _hdIdef :: ((String,Type))
              _hdIoriginalTree :: VarDef
              _lhsOannotatedTree :: VarDefList
              _lhsOdefs :: ([(String,Type)])
              _lhsOoriginalTree :: VarDefList
              -- copy rule (down)
              _tlOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 14368 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOenv =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIenv
                  {-# LINE 14373 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14378 "AstInternal.hs" #-}
              ( _tlIannotatedTree,_tlIdefs,_tlIoriginalTree) =
                  (tl_ _tlOenv _tlOlib )
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14385 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIdef,_hdIoriginalTree) =
                  (hd_ _hdOenv _hdOlib )
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 14392 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14397 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 134, column 12)
              _lhsOdefs =
                  {-# LINE 134 "./TypeChecking/CreateFunction.ag" #-}
                  _hdIdef : _tlIdefs
                  {-# LINE 14402 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 14407 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14412 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOoriginalTree)))
sem_VarDefList_Nil  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: VarDefList
              _lhsOdefs :: ([(String,Type)])
              _lhsOoriginalTree :: VarDefList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 14424 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14429 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 135, column 11)
              _lhsOdefs =
                  {-# LINE 135 "./TypeChecking/CreateFunction.ag" #-}
                  []
                  {-# LINE 14434 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 14439 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14444 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOoriginalTree)))
-- Volatility --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         env                  : Environment
         lib                  : LocalIdentifierBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Immutable:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Stable:
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Volatile:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
                     LocalIdentifierBindings ->
                     ( Volatility,Volatility)
data Inh_Volatility  = Inh_Volatility {env_Inh_Volatility :: Environment,lib_Inh_Volatility :: LocalIdentifierBindings}
data Syn_Volatility  = Syn_Volatility {annotatedTree_Syn_Volatility :: Volatility,originalTree_Syn_Volatility :: Volatility}
wrap_Volatility :: T_Volatility  ->
                   Inh_Volatility  ->
                   Syn_Volatility 
wrap_Volatility sem (Inh_Volatility _lhsIenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIenv _lhsIlib )
     in  (Syn_Volatility _lhsOannotatedTree _lhsOoriginalTree ))
sem_Volatility_Immutable  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Volatility
              _lhsOoriginalTree :: Volatility
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Immutable
                  {-# LINE 14504 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14509 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Immutable
                  {-# LINE 14514 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14519 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Volatility_Stable  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Volatility
              _lhsOoriginalTree :: Volatility
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Stable
                  {-# LINE 14530 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14535 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Stable
                  {-# LINE 14540 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14545 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Volatility_Volatile  =
    (\ _lhsIenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Volatility
              _lhsOoriginalTree :: Volatility
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Volatile
                  {-# LINE 14556 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14561 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Volatile
                  {-# LINE 14566 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14571 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))