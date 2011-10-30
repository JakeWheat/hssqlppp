

-- UUAGC 0.9.39.1 (src/Database/HsSqlPpp/Internals/AstInternal.ag)
module Database.HsSqlPpp.Internals.AstInternal(
    -- {-# LANGUAGE DeriveDataTypeable,ScopedTypeVariables #-}
    --from the ag files:
    --ast nodes
    Statement (..)
   ,QueryExpr (..)
   ,WithQueryList
   ,WithQuery(..)
   ,FnBody (..)
   ,SetClause (..)
   ,SetClauseList
   ,TableRef (..)
   ,TableAlias(..)
   ,JoinExpr (..)
   ,JoinType (..)
   ,SelectList (..)
   ,SelectItem (..)
   ,CopySource (..)
   ,AttributeDef (..)
   ,RowConstraint (..)
   ,AlterTableAction(..)
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
   ,Replace(..)
   ,RestartIdentity (..)
   ,ScalarExpr (..)
   ,Name(..)
   ,NameComponent(..)
   ,ncStr
   ,IntervalField(..)
   ,ExtractField(..)
   ,FrameClause(..)
   ,InList (..)
   ,LiftFlavour(..)
   ,TriggerWhen(..)
   ,TriggerEvent(..)
   ,TriggerFire(..)
   ,StatementList
   ,ScalarExprListStatementListPairList
   ,ScalarExprListStatementListPair
   ,ScalarExprList
   ,ParamDefList
   ,AttributeDefList
   ,ConstraintList
   ,TypeAttributeDefList
   ,TypeNameList
   ,NameTypeNameListPair
   ,NameTypeNameListPairList
   ,ScalarExprStatementListPairList
   --,SetClauseList
   ,CaseScalarExprListScalarExprPairList
   ,MaybeScalarExpr
   ,TableRefList
   ,ScalarExprListList
   ,SelectItemList
   ,OnExpr
   ,RowConstraintList
   ,VarDefList
   ,ScalarExprStatementListPair
   ,CaseScalarExprListScalarExprPair
   ,ScalarExprDirectionPair
   ,ScalarExprDirectionPairList
   ,MaybeBoolExpr
   ,MaybeSelectList
   ,SetValue(..)
   ,AlterTableActionList
   ,NameComponentList
   ,MaybeNameComponentList
   -- typechecking
   ,typeCheckStatements
   ,typeCheckParameterizedStatement
   ,typeCheckScalarExpr
   ,typeCheckQueryExpr
   --,fixUpIdentifiers
   --,fixUpIdentifiersQE
   --,fixUpIdentifiersSE
) where

import Data.Maybe
import Data.Either
import Data.List
import Control.Applicative
import Data.Data
import Data.Char
import Control.Monad.State
import Control.Arrow

import Data.Generics.Uniplate.Data
import Debug.Trace
--import Text.Groom


import Database.HsSqlPpp.Internals.TypeType
import Database.HsSqlPpp.Internals.TypeChecking.TypeConversion
import Database.HsSqlPpp.Internals.AstAnnotation
import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
import Database.HsSqlPpp.Utils.Utils

{-# LINE 330 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}


data NameComponent = Nmc String
                   | QNmc String -- quoted
                     deriving (Data,Eq,Show,Typeable)
-- this is a transition function
-- it should be removed when ready, since all the code
-- should be working with NameComponents directly
ncStr :: NameComponent -> String
ncStr (Nmc n) = n
ncStr (QNmc n) = n
{-# LINE 128 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 399 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data JoinType = Inner | LeftOuter| RightOuter | FullOuter | Cross
                deriving (Show,Eq,Typeable,Data)
{-# LINE 134 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 411 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data CopySource = CopyFilename String
                | Stdin
                  deriving (Show,Eq,Typeable,Data)
{-# LINE 141 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 468 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data SetValue
    = SetStr Annotation String
    | SetId Annotation String
    | SetNum Annotation Double
      deriving (Show,Eq,Typeable,Data)


data TriggerWhen = TriggerBefore | TriggerAfter
                   deriving (Show,Eq,Typeable,Data)
data TriggerEvent = TInsert| TUpdate | TDelete | AntiTriggerEvent String
                    deriving (Show,Eq,Typeable,Data)
data TriggerFire = EachRow | EachStatement
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 158 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 497 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data RaiseType = RNotice | RException | RError
                 deriving (Show,Eq,Typeable,Data)

data CombineType = Except | Union | Intersect | UnionAll
                   deriving (Show,Eq,Typeable,Data)

data Volatility = Volatile | Stable | Immutable
                  deriving (Show,Eq,Typeable,Data)

data Language = Sql | Plpgsql
                deriving (Show,Eq,Typeable,Data)
{-# LINE 173 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 516 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data DropType = Table
              | Domain
              | View
              | Type
                deriving (Show,Eq,Typeable,Data)

data Cascade = Cascade | Restrict
               deriving (Show,Eq,Typeable,Data)

data Direction = Asc | Desc
                 deriving (Show,Eq,Typeable,Data)

data Distinct = Distinct | Dupes
                deriving (Show,Eq,Typeable,Data)

data Natural = Natural | Unnatural
               deriving (Show,Eq,Typeable,Data)

data IfExists = Require | IfExists
                deriving (Show,Eq,Typeable,Data)

data Replace = Replace | NoReplace
               deriving (Show,Eq,Typeable,Data)

data RestartIdentity = RestartIdentity | ContinueIdentity
                       deriving (Show,Eq,Typeable,Data)

{-# LINE 204 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 613 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data LiftFlavour = LiftAny | LiftAll
                   deriving (Show,Eq,Typeable,Data)

data IntervalField = IntervalYear
                   | IntervalMonth
                   | IntervalDay
                   | IntervalHour
                   | IntervalMinute
                   | IntervalSecond
                   | IntervalYearToMonth
                   | IntervalDayToHour
                   | IntervalDayToMinute
                   | IntervalDayToSecond
                   | IntervalHourToMinute
                   | IntervalHourToSecond
                   | IntervalMinuteToSecond
                     deriving (Show,Eq,Typeable,Data)

data ExtractField = ExtractCentury
                  | ExtractDay
                  | ExtractDecade
                  | ExtractDow
                  | ExtractDoy
                  | ExtractEpoch
                  | ExtractHour
                  | ExtractIsodow
                  | ExtractIsoyear
                  | ExtractMicroseconds
                  | ExtractMillennium
                  | ExtractMilliseconds
                  | ExtractMinute
                  | ExtractMonth
                  | ExtractQuarter
                  | ExtractSecond
                  | ExtractTimezone
                  | ExtractTimezoneHour
                  | ExtractTimezoneMinute
                  | ExtractWeek
                  | ExtractYear
                    deriving (Show,Eq,Typeable,Data)

{-# LINE 249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 661 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data FrameClause = FrameUnboundedPreceding
                 | FrameUnboundedFull
                 | FrameRowsUnboundedPreceding
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 257 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 3 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}


-- | Takes an ast, checks against catalog passed, and adds
--   annotations, including types, type errors, and statement info.
--   Returns the updated catalog as well as the annotated ast.
typeCheckStatements :: Catalog -> [Statement] -> (Catalog,[Statement])
typeCheckStatements cat sts =
    let t = sem_Root (Root sts)
        ta = wrap_Root t Inh_Root {cat_Inh_Root = cat
                                  {-,lib_Inh_Root = emptyBindings
                                  ,idenv_Inh_Root = emptyIDEnv "tcs"-}}
        tl = annotatedTree_Syn_Root ta
        cat1 = undefined --producedCat_Syn_Root ta
    in case tl of
         Root r -> (cat1,r)

typeCheckQueryExpr :: Catalog -> QueryExpr -> QueryExpr
typeCheckQueryExpr cat qe =
   let (_,[QueryStatement _ qe']) = typeCheckStatements cat [QueryStatement emptyAnnotation qe]
   in qe'

-- | Unfinished version of type check which can type check an
-- individual statement with ? or positional arg placeholders in
-- it. Will error if the statement isn't select, update, insert or
-- delete. For use in type checking embedded parameterized
-- statements. Does all typechecking and annotation that the regular
-- typecheck does.
typeCheckParameterizedStatement :: Catalog -> Statement -> Either String Statement
typeCheckParameterizedStatement cat st =
    case st of
      QueryStatement _ _ -> tc
      Insert _ _ _ _ _ -> tc
      Update _ _ _ _ _ _ -> tc
      Delete _ _ _ _ _ -> tc
      _ -> Left "requires select, update, insert or delete statement"
    where
      tc = let tl = typeCheckStatements cat [st]
           in case tl of
                (_,[st1]) -> Right st1
                _ -> error "impossible happened in typeCheckPS!"


-- | Testing utility, mainly used to check an expression for type errors
-- or to get its type.
typeCheckScalarExpr :: Catalog -> ScalarExpr -> ScalarExpr
typeCheckScalarExpr cat ex =
    let t = sem_ScalarExprRoot (ScalarExprRoot ex)
        rt = (annotatedTree_Syn_ScalarExprRoot
              (wrap_ScalarExprRoot t Inh_ScalarExprRoot {cat_Inh_ScalarExprRoot = cat
                                                        {-,lib_Inh_ScalarExprRoot = emptyBindings
                                                        ,idenv_Inh_ScalarExprRoot = emptyIDEnv "tcse"-}}))
    in case rt of
         ScalarExprRoot e -> e

{-# LINE 314 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
-- AlterTableAction --------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative AddConstraint:
         child ann            : {Annotation}
         child con            : Constraint 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative AlterColumnDefault:
         child ann            : {Annotation}
         child nm             : {NameComponent}
         child def            : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data AlterTableAction  = AddConstraint (Annotation) (Constraint ) 
                       | AlterColumnDefault (Annotation) (NameComponent) (ScalarExpr ) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_AlterTableAction :: AlterTableAction  ->
                        T_AlterTableAction 
sem_AlterTableAction (AddConstraint _ann _con )  =
    (sem_AlterTableAction_AddConstraint _ann (sem_Constraint _con ) )
sem_AlterTableAction (AlterColumnDefault _ann _nm _def )  =
    (sem_AlterTableAction_AlterColumnDefault _ann _nm (sem_ScalarExpr _def ) )
-- semantic domain
type T_AlterTableAction  = Catalog ->
                           ( AlterTableAction ,AlterTableAction )
data Inh_AlterTableAction  = Inh_AlterTableAction {cat_Inh_AlterTableAction :: Catalog}
data Syn_AlterTableAction  = Syn_AlterTableAction {annotatedTree_Syn_AlterTableAction :: AlterTableAction ,originalTree_Syn_AlterTableAction :: AlterTableAction }
wrap_AlterTableAction :: T_AlterTableAction  ->
                         Inh_AlterTableAction  ->
                         Syn_AlterTableAction 
wrap_AlterTableAction sem (Inh_AlterTableAction _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_AlterTableAction _lhsOannotatedTree _lhsOoriginalTree ))
sem_AlterTableAction_AddConstraint :: Annotation ->
                                      T_Constraint  ->
                                      T_AlterTableAction 
sem_AlterTableAction_AddConstraint ann_ con_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: AlterTableAction 
              _lhsOoriginalTree :: AlterTableAction 
              _conOcat :: Catalog
              _conIannotatedTree :: Constraint 
              _conIoriginalTree :: Constraint 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AddConstraint ann_ _conIannotatedTree
                   {-# LINE 373 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AddConstraint ann_ _conIoriginalTree
                   {-# LINE 379 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 385 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 391 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _conOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 397 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _conIannotatedTree,_conIoriginalTree) =
                  con_ _conOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_AlterTableAction_AlterColumnDefault :: Annotation ->
                                           NameComponent ->
                                           T_ScalarExpr  ->
                                           T_AlterTableAction 
sem_AlterTableAction_AlterColumnDefault ann_ nm_ def_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: AlterTableAction 
              _lhsOoriginalTree :: AlterTableAction 
              _defOcat :: Catalog
              _defIannotatedTree :: ScalarExpr 
              _defIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterColumnDefault ann_ nm_ _defIannotatedTree
                   {-# LINE 417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterColumnDefault ann_ nm_ _defIoriginalTree
                   {-# LINE 423 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 429 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 435 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _defOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 441 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _defIannotatedTree,_defIoriginalTree) =
                  def_ _defOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- AlterTableActionList ----------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : AlterTableAction 
         child tl             : AlterTableActionList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type AlterTableActionList  = [AlterTableAction ]
-- cata
sem_AlterTableActionList :: AlterTableActionList  ->
                            T_AlterTableActionList 
sem_AlterTableActionList list  =
    (Prelude.foldr sem_AlterTableActionList_Cons sem_AlterTableActionList_Nil (Prelude.map sem_AlterTableAction list) )
-- semantic domain
type T_AlterTableActionList  = Catalog ->
                               ( AlterTableActionList ,AlterTableActionList )
data Inh_AlterTableActionList  = Inh_AlterTableActionList {cat_Inh_AlterTableActionList :: Catalog}
data Syn_AlterTableActionList  = Syn_AlterTableActionList {annotatedTree_Syn_AlterTableActionList :: AlterTableActionList ,originalTree_Syn_AlterTableActionList :: AlterTableActionList }
wrap_AlterTableActionList :: T_AlterTableActionList  ->
                             Inh_AlterTableActionList  ->
                             Syn_AlterTableActionList 
wrap_AlterTableActionList sem (Inh_AlterTableActionList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_AlterTableActionList _lhsOannotatedTree _lhsOoriginalTree ))
sem_AlterTableActionList_Cons :: T_AlterTableAction  ->
                                 T_AlterTableActionList  ->
                                 T_AlterTableActionList 
sem_AlterTableActionList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: AlterTableActionList 
              _lhsOoriginalTree :: AlterTableActionList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: AlterTableAction 
              _hdIoriginalTree :: AlterTableAction 
              _tlIannotatedTree :: AlterTableActionList 
              _tlIoriginalTree :: AlterTableActionList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 500 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 506 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 512 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 518 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 530 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_AlterTableActionList_Nil :: T_AlterTableActionList 
sem_AlterTableActionList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: AlterTableActionList 
              _lhsOoriginalTree :: AlterTableActionList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 546 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 552 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 558 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 564 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- AttributeDef ------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative AttributeDef:
         child ann            : {Annotation}
         child name           : {NameComponent}
         child typ            : TypeName 
         child def            : MaybeScalarExpr 
         child cons           : RowConstraintList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data AttributeDef  = AttributeDef (Annotation) (NameComponent) (TypeName ) (MaybeScalarExpr ) (RowConstraintList ) 
                   deriving ( Data,Eq,Show,Typeable)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _ann _name _typ _def _cons )  =
    (sem_AttributeDef_AttributeDef _ann _name (sem_TypeName _typ ) (sem_MaybeScalarExpr _def ) (sem_RowConstraintList _cons ) )
-- semantic domain
type T_AttributeDef  = Catalog ->
                       ( AttributeDef ,AttributeDef )
data Inh_AttributeDef  = Inh_AttributeDef {cat_Inh_AttributeDef :: Catalog}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef ,originalTree_Syn_AttributeDef :: AttributeDef }
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_AttributeDef _lhsOannotatedTree _lhsOoriginalTree ))
sem_AttributeDef_AttributeDef :: Annotation ->
                                 NameComponent ->
                                 T_TypeName  ->
                                 T_MaybeScalarExpr  ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef ann_ name_ typ_ def_ cons_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: AttributeDef 
              _lhsOoriginalTree :: AttributeDef 
              _typOcat :: Catalog
              _defOcat :: Catalog
              _consOcat :: Catalog
              _typIannotatedTree :: TypeName 
              _typIoriginalTree :: TypeName 
              _defIannotatedTree :: MaybeScalarExpr 
              _defIoriginalTree :: MaybeScalarExpr 
              _consIannotatedTree :: RowConstraintList 
              _consIoriginalTree :: RowConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AttributeDef ann_ name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
                   {-# LINE 627 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AttributeDef ann_ name_ _typIoriginalTree _defIoriginalTree _consIoriginalTree
                   {-# LINE 633 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 639 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 645 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 651 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _defOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 657 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _consOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 663 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _typIannotatedTree,_typIoriginalTree) =
                  typ_ _typOcat 
              ( _defIannotatedTree,_defIoriginalTree) =
                  def_ _defOcat 
              ( _consIannotatedTree,_consIoriginalTree) =
                  cons_ _consOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- AttributeDefList --------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
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
type AttributeDefList  = [AttributeDef ]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = Catalog ->
                           ( AttributeDefList ,AttributeDefList )
data Inh_AttributeDefList  = Inh_AttributeDefList {cat_Inh_AttributeDefList :: Catalog}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList ,originalTree_Syn_AttributeDefList :: AttributeDefList }
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_AttributeDefList _lhsOannotatedTree _lhsOoriginalTree ))
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: AttributeDefList 
              _lhsOoriginalTree :: AttributeDefList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: AttributeDef 
              _hdIoriginalTree :: AttributeDef 
              _tlIannotatedTree :: AttributeDefList 
              _tlIoriginalTree :: AttributeDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 726 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 732 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 738 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 744 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 750 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 756 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: AttributeDefList 
              _lhsOoriginalTree :: AttributeDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 772 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 778 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 784 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 790 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- CaseScalarExprListScalarExprPair ----------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExprList 
         child x2             : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type CaseScalarExprListScalarExprPair  = ( ScalarExprList ,ScalarExpr )
-- cata
sem_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair  ->
                                        T_CaseScalarExprListScalarExprPair 
sem_CaseScalarExprListScalarExprPair ( x1,x2)  =
    (sem_CaseScalarExprListScalarExprPair_Tuple (sem_ScalarExprList x1 ) (sem_ScalarExpr x2 ) )
-- semantic domain
type T_CaseScalarExprListScalarExprPair  = Catalog ->
                                           ( CaseScalarExprListScalarExprPair ,CaseScalarExprListScalarExprPair )
data Inh_CaseScalarExprListScalarExprPair  = Inh_CaseScalarExprListScalarExprPair {cat_Inh_CaseScalarExprListScalarExprPair :: Catalog}
data Syn_CaseScalarExprListScalarExprPair  = Syn_CaseScalarExprListScalarExprPair {annotatedTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair ,originalTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair }
wrap_CaseScalarExprListScalarExprPair :: T_CaseScalarExprListScalarExprPair  ->
                                         Inh_CaseScalarExprListScalarExprPair  ->
                                         Syn_CaseScalarExprListScalarExprPair 
wrap_CaseScalarExprListScalarExprPair sem (Inh_CaseScalarExprListScalarExprPair _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_CaseScalarExprListScalarExprPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_CaseScalarExprListScalarExprPair_Tuple :: T_ScalarExprList  ->
                                              T_ScalarExpr  ->
                                              T_CaseScalarExprListScalarExprPair 
sem_CaseScalarExprListScalarExprPair_Tuple x1_ x2_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: CaseScalarExprListScalarExprPair 
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPair 
              _x1Ocat :: Catalog
              _x2Ocat :: Catalog
              _x1IannotatedTree :: ScalarExprList 
              _x1IoriginalTree :: ScalarExprList 
              _x2IannotatedTree :: ScalarExpr 
              _x2IoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IannotatedTree,_x2IannotatedTree)
                   {-# LINE 843 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IoriginalTree,_x2IoriginalTree)
                   {-# LINE 849 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 855 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 861 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 867 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 873 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _x1IannotatedTree,_x1IoriginalTree) =
                  x1_ _x1Ocat 
              ( _x2IannotatedTree,_x2IoriginalTree) =
                  x2_ _x2Ocat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- CaseScalarExprListScalarExprPairList ------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : CaseScalarExprListScalarExprPair 
         child tl             : CaseScalarExprListScalarExprPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type CaseScalarExprListScalarExprPairList  = [CaseScalarExprListScalarExprPair ]
-- cata
sem_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList  ->
                                            T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList list  =
    (Prelude.foldr sem_CaseScalarExprListScalarExprPairList_Cons sem_CaseScalarExprListScalarExprPairList_Nil (Prelude.map sem_CaseScalarExprListScalarExprPair list) )
-- semantic domain
type T_CaseScalarExprListScalarExprPairList  = Catalog ->
                                               ( CaseScalarExprListScalarExprPairList ,CaseScalarExprListScalarExprPairList )
data Inh_CaseScalarExprListScalarExprPairList  = Inh_CaseScalarExprListScalarExprPairList {cat_Inh_CaseScalarExprListScalarExprPairList :: Catalog}
data Syn_CaseScalarExprListScalarExprPairList  = Syn_CaseScalarExprListScalarExprPairList {annotatedTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList ,originalTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList }
wrap_CaseScalarExprListScalarExprPairList :: T_CaseScalarExprListScalarExprPairList  ->
                                             Inh_CaseScalarExprListScalarExprPairList  ->
                                             Syn_CaseScalarExprListScalarExprPairList 
wrap_CaseScalarExprListScalarExprPairList sem (Inh_CaseScalarExprListScalarExprPairList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_CaseScalarExprListScalarExprPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_CaseScalarExprListScalarExprPairList_Cons :: T_CaseScalarExprListScalarExprPair  ->
                                                 T_CaseScalarExprListScalarExprPairList  ->
                                                 T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: CaseScalarExprListScalarExprPairList 
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPairList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: CaseScalarExprListScalarExprPair 
              _hdIoriginalTree :: CaseScalarExprListScalarExprPair 
              _tlIannotatedTree :: CaseScalarExprListScalarExprPairList 
              _tlIoriginalTree :: CaseScalarExprListScalarExprPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 934 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 940 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 946 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 952 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 958 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 964 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_CaseScalarExprListScalarExprPairList_Nil :: T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: CaseScalarExprListScalarExprPairList 
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 980 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 986 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 992 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 998 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- Constraint --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative CheckConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative PrimaryKeyConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child x              : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ReferenceConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child atts           : {[NameComponent]}
         child table          : Name 
         child tableAtts      : {[NameComponent]}
         child onUpdate       : {Cascade}
         child onDelete       : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative UniqueConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child x              : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data Constraint  = CheckConstraint (Annotation) (String) (ScalarExpr ) 
                 | PrimaryKeyConstraint (Annotation) (String) (([NameComponent])) 
                 | ReferenceConstraint (Annotation) (String) (([NameComponent])) (Name ) (([NameComponent])) (Cascade) (Cascade) 
                 | UniqueConstraint (Annotation) (String) (([NameComponent])) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Constraint :: Constraint  ->
                  T_Constraint 
sem_Constraint (CheckConstraint _ann _name _expr )  =
    (sem_Constraint_CheckConstraint _ann _name (sem_ScalarExpr _expr ) )
sem_Constraint (PrimaryKeyConstraint _ann _name _x )  =
    (sem_Constraint_PrimaryKeyConstraint _ann _name _x )
sem_Constraint (ReferenceConstraint _ann _name _atts _table _tableAtts _onUpdate _onDelete )  =
    (sem_Constraint_ReferenceConstraint _ann _name _atts (sem_Name _table ) _tableAtts _onUpdate _onDelete )
sem_Constraint (UniqueConstraint _ann _name _x )  =
    (sem_Constraint_UniqueConstraint _ann _name _x )
-- semantic domain
type T_Constraint  = Catalog ->
                     ( Constraint ,Constraint )
data Inh_Constraint  = Inh_Constraint {cat_Inh_Constraint :: Catalog}
data Syn_Constraint  = Syn_Constraint {annotatedTree_Syn_Constraint :: Constraint ,originalTree_Syn_Constraint :: Constraint }
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_Constraint _lhsOannotatedTree _lhsOoriginalTree ))
sem_Constraint_CheckConstraint :: Annotation ->
                                  String ->
                                  T_ScalarExpr  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Constraint 
              _lhsOoriginalTree :: Constraint 
              _exprOcat :: Catalog
              _exprIannotatedTree :: ScalarExpr 
              _exprIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CheckConstraint ann_ name_ _exprIannotatedTree
                   {-# LINE 1085 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CheckConstraint ann_ name_ _exprIoriginalTree
                   {-# LINE 1091 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1097 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1103 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1109 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Constraint_PrimaryKeyConstraint :: Annotation ->
                                       String ->
                                       ([NameComponent]) ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint ann_ name_ x_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Constraint 
              _lhsOoriginalTree :: Constraint 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PrimaryKeyConstraint ann_ name_ x_
                   {-# LINE 1126 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PrimaryKeyConstraint ann_ name_ x_
                   {-# LINE 1132 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1138 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1144 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Constraint_ReferenceConstraint :: Annotation ->
                                      String ->
                                      ([NameComponent]) ->
                                      T_Name  ->
                                      ([NameComponent]) ->
                                      Cascade ->
                                      Cascade ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Constraint 
              _lhsOoriginalTree :: Constraint 
              _tableOcat :: Catalog
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReferenceConstraint ann_ name_ atts_ _tableIannotatedTree tableAtts_ onUpdate_ onDelete_
                   {-# LINE 1166 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReferenceConstraint ann_ name_ atts_ _tableIoriginalTree tableAtts_ onUpdate_ onDelete_
                   {-# LINE 1172 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1178 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1184 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1190 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Constraint_UniqueConstraint :: Annotation ->
                                   String ->
                                   ([NameComponent]) ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint ann_ name_ x_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Constraint 
              _lhsOoriginalTree :: Constraint 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   UniqueConstraint ann_ name_ x_
                   {-# LINE 1207 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   UniqueConstraint ann_ name_ x_
                   {-# LINE 1213 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1219 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1225 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ConstraintList ----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
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
type ConstraintList  = [Constraint ]
-- cata
sem_ConstraintList :: ConstraintList  ->
                      T_ConstraintList 
sem_ConstraintList list  =
    (Prelude.foldr sem_ConstraintList_Cons sem_ConstraintList_Nil (Prelude.map sem_Constraint list) )
-- semantic domain
type T_ConstraintList  = Catalog ->
                         ( ConstraintList ,ConstraintList )
data Inh_ConstraintList  = Inh_ConstraintList {cat_Inh_ConstraintList :: Catalog}
data Syn_ConstraintList  = Syn_ConstraintList {annotatedTree_Syn_ConstraintList :: ConstraintList ,originalTree_Syn_ConstraintList :: ConstraintList }
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ConstraintList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ConstraintList 
              _lhsOoriginalTree :: ConstraintList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: Constraint 
              _hdIoriginalTree :: Constraint 
              _tlIannotatedTree :: ConstraintList 
              _tlIoriginalTree :: ConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 1282 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 1288 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1294 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1300 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1306 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1312 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ConstraintList 
              _lhsOoriginalTree :: ConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1328 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1334 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1340 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1346 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- FnBody ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative PlpgsqlFnBody:
         child ann            : {Annotation}
         child blk            : Statement 
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
data FnBody  = PlpgsqlFnBody (Annotation) (Statement ) 
             | SqlFnBody (Annotation) (StatementList ) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_FnBody :: FnBody  ->
              T_FnBody 
sem_FnBody (PlpgsqlFnBody _ann _blk )  =
    (sem_FnBody_PlpgsqlFnBody _ann (sem_Statement _blk ) )
sem_FnBody (SqlFnBody _ann _sts )  =
    (sem_FnBody_SqlFnBody _ann (sem_StatementList _sts ) )
-- semantic domain
type T_FnBody  = Catalog ->
                 ( FnBody ,FnBody )
data Inh_FnBody  = Inh_FnBody {cat_Inh_FnBody :: Catalog}
data Syn_FnBody  = Syn_FnBody {annotatedTree_Syn_FnBody :: FnBody ,originalTree_Syn_FnBody :: FnBody }
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_FnBody _lhsOannotatedTree _lhsOoriginalTree ))
sem_FnBody_PlpgsqlFnBody :: Annotation ->
                            T_Statement  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody ann_ blk_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: FnBody 
              _lhsOoriginalTree :: FnBody 
              _blkOcat :: Catalog
              _blkIannotatedTree :: Statement 
              _blkIoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PlpgsqlFnBody ann_ _blkIannotatedTree
                   {-# LINE 1406 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PlpgsqlFnBody ann_ _blkIoriginalTree
                   {-# LINE 1412 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1418 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1424 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _blkOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1430 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _blkIannotatedTree,_blkIoriginalTree) =
                  blk_ _blkOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_FnBody_SqlFnBody :: Annotation ->
                        T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody ann_ sts_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: FnBody 
              _lhsOoriginalTree :: FnBody 
              _stsOcat :: Catalog
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SqlFnBody ann_ _stsIannotatedTree
                   {-# LINE 1449 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SqlFnBody ann_ _stsIoriginalTree
                   {-# LINE 1455 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1467 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1473 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- InList ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative InList:
         child ann            : {Annotation}
         child exprs          : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative InQueryExpr:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data InList  = InList (Annotation) (ScalarExprList ) 
             | InQueryExpr (Annotation) (QueryExpr ) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_InList :: InList  ->
              T_InList 
sem_InList (InList _ann _exprs )  =
    (sem_InList_InList _ann (sem_ScalarExprList _exprs ) )
sem_InList (InQueryExpr _ann _sel )  =
    (sem_InList_InQueryExpr _ann (sem_QueryExpr _sel ) )
-- semantic domain
type T_InList  = Catalog ->
                 ( InList ,InList )
data Inh_InList  = Inh_InList {cat_Inh_InList :: Catalog}
data Syn_InList  = Syn_InList {annotatedTree_Syn_InList :: InList ,originalTree_Syn_InList :: InList }
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_InList _lhsOannotatedTree _lhsOoriginalTree ))
sem_InList_InList :: Annotation ->
                     T_ScalarExprList  ->
                     T_InList 
sem_InList_InList ann_ exprs_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: InList 
              _lhsOoriginalTree :: InList 
              _exprsOcat :: Catalog
              _exprsIannotatedTree :: ScalarExprList 
              _exprsIoriginalTree :: ScalarExprList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InList ann_ _exprsIannotatedTree
                   {-# LINE 1535 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InList ann_ _exprsIoriginalTree
                   {-# LINE 1541 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1547 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1553 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1559 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprsIannotatedTree,_exprsIoriginalTree) =
                  exprs_ _exprsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_InList_InQueryExpr :: Annotation ->
                          T_QueryExpr  ->
                          T_InList 
sem_InList_InQueryExpr ann_ sel_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: InList 
              _lhsOoriginalTree :: InList 
              _selOcat :: Catalog
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InQueryExpr ann_ _selIannotatedTree
                   {-# LINE 1578 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InQueryExpr ann_ _selIoriginalTree
                   {-# LINE 1584 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1590 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1596 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1602 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _selIannotatedTree,_selIoriginalTree) =
                  sel_ _selOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- JoinExpr ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative JoinOn:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative JoinUsing:
         child ann            : {Annotation}
         child x              : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data JoinExpr  = JoinOn (Annotation) (ScalarExpr ) 
               | JoinUsing (Annotation) (([NameComponent])) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_JoinExpr :: JoinExpr  ->
                T_JoinExpr 
sem_JoinExpr (JoinOn _ann _expr )  =
    (sem_JoinExpr_JoinOn _ann (sem_ScalarExpr _expr ) )
sem_JoinExpr (JoinUsing _ann _x )  =
    (sem_JoinExpr_JoinUsing _ann _x )
-- semantic domain
type T_JoinExpr  = Catalog ->
                   ( JoinExpr ,JoinExpr )
data Inh_JoinExpr  = Inh_JoinExpr {cat_Inh_JoinExpr :: Catalog}
data Syn_JoinExpr  = Syn_JoinExpr {annotatedTree_Syn_JoinExpr :: JoinExpr ,originalTree_Syn_JoinExpr :: JoinExpr }
wrap_JoinExpr :: T_JoinExpr  ->
                 Inh_JoinExpr  ->
                 Syn_JoinExpr 
wrap_JoinExpr sem (Inh_JoinExpr _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_JoinExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_JoinExpr_JoinOn :: Annotation ->
                       T_ScalarExpr  ->
                       T_JoinExpr 
sem_JoinExpr_JoinOn ann_ expr_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: JoinExpr 
              _lhsOoriginalTree :: JoinExpr 
              _exprOcat :: Catalog
              _exprIannotatedTree :: ScalarExpr 
              _exprIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinOn ann_ _exprIannotatedTree
                   {-# LINE 1664 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinOn ann_ _exprIoriginalTree
                   {-# LINE 1670 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1676 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1682 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1688 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_JoinExpr_JoinUsing :: Annotation ->
                          ([NameComponent]) ->
                          T_JoinExpr 
sem_JoinExpr_JoinUsing ann_ x_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: JoinExpr 
              _lhsOoriginalTree :: JoinExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinUsing ann_ x_
                   {-# LINE 1704 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinUsing ann_ x_
                   {-# LINE 1710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1716 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1722 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- MaybeBoolExpr -----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type MaybeBoolExpr  = Maybe ScalarExpr 
-- cata
sem_MaybeBoolExpr :: MaybeBoolExpr  ->
                     T_MaybeBoolExpr 
sem_MaybeBoolExpr (Prelude.Just x )  =
    (sem_MaybeBoolExpr_Just (sem_ScalarExpr x ) )
sem_MaybeBoolExpr Prelude.Nothing  =
    sem_MaybeBoolExpr_Nothing
-- semantic domain
type T_MaybeBoolExpr  = Catalog ->
                        ( MaybeBoolExpr ,MaybeBoolExpr )
data Inh_MaybeBoolExpr  = Inh_MaybeBoolExpr {cat_Inh_MaybeBoolExpr :: Catalog}
data Syn_MaybeBoolExpr  = Syn_MaybeBoolExpr {annotatedTree_Syn_MaybeBoolExpr :: MaybeBoolExpr ,originalTree_Syn_MaybeBoolExpr :: MaybeBoolExpr }
wrap_MaybeBoolExpr :: T_MaybeBoolExpr  ->
                      Inh_MaybeBoolExpr  ->
                      Syn_MaybeBoolExpr 
wrap_MaybeBoolExpr sem (Inh_MaybeBoolExpr _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_MaybeBoolExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_MaybeBoolExpr_Just :: T_ScalarExpr  ->
                          T_MaybeBoolExpr 
sem_MaybeBoolExpr_Just just_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: MaybeBoolExpr 
              _lhsOoriginalTree :: MaybeBoolExpr 
              _justOcat :: Catalog
              _justIannotatedTree :: ScalarExpr 
              _justIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIannotatedTree
                   {-# LINE 1776 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIoriginalTree
                   {-# LINE 1782 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1788 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1794 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1800 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _justIannotatedTree,_justIoriginalTree) =
                  just_ _justOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_MaybeBoolExpr_Nothing :: T_MaybeBoolExpr 
sem_MaybeBoolExpr_Nothing  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: MaybeBoolExpr 
              _lhsOoriginalTree :: MaybeBoolExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 1814 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 1820 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1826 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1832 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- MaybeNameComponentList --------------------------------------
{-
   alternatives:
      alternative Just:
         child just           : NameComponentList 
      alternative Nothing:
-}
type MaybeNameComponentList  = Maybe NameComponentList 
-- cata
sem_MaybeNameComponentList :: MaybeNameComponentList  ->
                              T_MaybeNameComponentList 
sem_MaybeNameComponentList (Prelude.Just x )  =
    (sem_MaybeNameComponentList_Just (sem_NameComponentList x ) )
sem_MaybeNameComponentList Prelude.Nothing  =
    sem_MaybeNameComponentList_Nothing
-- semantic domain
type T_MaybeNameComponentList  = ( )
data Inh_MaybeNameComponentList  = Inh_MaybeNameComponentList {}
data Syn_MaybeNameComponentList  = Syn_MaybeNameComponentList {}
wrap_MaybeNameComponentList :: T_MaybeNameComponentList  ->
                               Inh_MaybeNameComponentList  ->
                               Syn_MaybeNameComponentList 
wrap_MaybeNameComponentList sem (Inh_MaybeNameComponentList )  =
    (let ( ) = sem 
     in  (Syn_MaybeNameComponentList ))
sem_MaybeNameComponentList_Just :: T_NameComponentList  ->
                                   T_MaybeNameComponentList 
sem_MaybeNameComponentList_Just just_  =
    (let 
     in  ( ))
sem_MaybeNameComponentList_Nothing :: T_MaybeNameComponentList 
sem_MaybeNameComponentList_Nothing  =
    (let 
     in  ( ))
-- MaybeScalarExpr ---------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type MaybeScalarExpr  = Maybe ScalarExpr 
-- cata
sem_MaybeScalarExpr :: MaybeScalarExpr  ->
                       T_MaybeScalarExpr 
sem_MaybeScalarExpr (Prelude.Just x )  =
    (sem_MaybeScalarExpr_Just (sem_ScalarExpr x ) )
sem_MaybeScalarExpr Prelude.Nothing  =
    sem_MaybeScalarExpr_Nothing
-- semantic domain
type T_MaybeScalarExpr  = Catalog ->
                          ( MaybeScalarExpr ,MaybeScalarExpr )
data Inh_MaybeScalarExpr  = Inh_MaybeScalarExpr {cat_Inh_MaybeScalarExpr :: Catalog}
data Syn_MaybeScalarExpr  = Syn_MaybeScalarExpr {annotatedTree_Syn_MaybeScalarExpr :: MaybeScalarExpr ,originalTree_Syn_MaybeScalarExpr :: MaybeScalarExpr }
wrap_MaybeScalarExpr :: T_MaybeScalarExpr  ->
                        Inh_MaybeScalarExpr  ->
                        Syn_MaybeScalarExpr 
wrap_MaybeScalarExpr sem (Inh_MaybeScalarExpr _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_MaybeScalarExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_MaybeScalarExpr_Just :: T_ScalarExpr  ->
                            T_MaybeScalarExpr 
sem_MaybeScalarExpr_Just just_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: MaybeScalarExpr 
              _lhsOoriginalTree :: MaybeScalarExpr 
              _justOcat :: Catalog
              _justIannotatedTree :: ScalarExpr 
              _justIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIannotatedTree
                   {-# LINE 1920 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIoriginalTree
                   {-# LINE 1926 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1932 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1938 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1944 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _justIannotatedTree,_justIoriginalTree) =
                  just_ _justOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_MaybeScalarExpr_Nothing :: T_MaybeScalarExpr 
sem_MaybeScalarExpr_Nothing  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: MaybeScalarExpr 
              _lhsOoriginalTree :: MaybeScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 1958 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 1964 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1970 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1976 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- MaybeSelectList ---------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
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
type MaybeSelectList  = Maybe SelectList 
-- cata
sem_MaybeSelectList :: MaybeSelectList  ->
                       T_MaybeSelectList 
sem_MaybeSelectList (Prelude.Just x )  =
    (sem_MaybeSelectList_Just (sem_SelectList x ) )
sem_MaybeSelectList Prelude.Nothing  =
    sem_MaybeSelectList_Nothing
-- semantic domain
type T_MaybeSelectList  = Catalog ->
                          ( MaybeSelectList ,MaybeSelectList )
data Inh_MaybeSelectList  = Inh_MaybeSelectList {cat_Inh_MaybeSelectList :: Catalog}
data Syn_MaybeSelectList  = Syn_MaybeSelectList {annotatedTree_Syn_MaybeSelectList :: MaybeSelectList ,originalTree_Syn_MaybeSelectList :: MaybeSelectList }
wrap_MaybeSelectList :: T_MaybeSelectList  ->
                        Inh_MaybeSelectList  ->
                        Syn_MaybeSelectList 
wrap_MaybeSelectList sem (Inh_MaybeSelectList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_MaybeSelectList _lhsOannotatedTree _lhsOoriginalTree ))
sem_MaybeSelectList_Just :: T_SelectList  ->
                            T_MaybeSelectList 
sem_MaybeSelectList_Just just_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: MaybeSelectList 
              _lhsOoriginalTree :: MaybeSelectList 
              _justOcat :: Catalog
              _justIannotatedTree :: SelectList 
              _justIoriginalTree :: SelectList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIannotatedTree
                   {-# LINE 2030 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIoriginalTree
                   {-# LINE 2036 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2042 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2048 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2054 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _justIannotatedTree,_justIoriginalTree) =
                  just_ _justOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_MaybeSelectList_Nothing :: T_MaybeSelectList 
sem_MaybeSelectList_Nothing  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: MaybeSelectList 
              _lhsOoriginalTree :: MaybeSelectList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2068 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2074 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2080 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2086 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- Name --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Name:
         child ann            : {Annotation}
         child is             : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data Name  = Name (Annotation) (([NameComponent])) 
           deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Name :: Name  ->
            T_Name 
sem_Name (Name _ann _is )  =
    (sem_Name_Name _ann _is )
-- semantic domain
type T_Name  = Catalog ->
               ( Name ,Name )
data Inh_Name  = Inh_Name {cat_Inh_Name :: Catalog}
data Syn_Name  = Syn_Name {annotatedTree_Syn_Name :: Name ,originalTree_Syn_Name :: Name }
wrap_Name :: T_Name  ->
             Inh_Name  ->
             Syn_Name 
wrap_Name sem (Inh_Name _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_Name _lhsOannotatedTree _lhsOoriginalTree ))
sem_Name_Name :: Annotation ->
                 ([NameComponent]) ->
                 T_Name 
sem_Name_Name ann_ is_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Name 
              _lhsOoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Name ann_ is_
                   {-# LINE 2134 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Name ann_ is_
                   {-# LINE 2140 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2146 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2152 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- NameComponentList -------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : {NameComponent}
         child tl             : NameComponentList 
      alternative Nil:
-}
type NameComponentList  = [(NameComponent)]
-- cata
sem_NameComponentList :: NameComponentList  ->
                         T_NameComponentList 
sem_NameComponentList list  =
    (Prelude.foldr sem_NameComponentList_Cons sem_NameComponentList_Nil list )
-- semantic domain
type T_NameComponentList  = ( )
data Inh_NameComponentList  = Inh_NameComponentList {}
data Syn_NameComponentList  = Syn_NameComponentList {}
wrap_NameComponentList :: T_NameComponentList  ->
                          Inh_NameComponentList  ->
                          Syn_NameComponentList 
wrap_NameComponentList sem (Inh_NameComponentList )  =
    (let ( ) = sem 
     in  (Syn_NameComponentList ))
sem_NameComponentList_Cons :: NameComponent ->
                              T_NameComponentList  ->
                              T_NameComponentList 
sem_NameComponentList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_NameComponentList_Nil :: T_NameComponentList 
sem_NameComponentList_Nil  =
    (let 
     in  ( ))
-- NameTypeNameListPair ----------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : Name 
         child x2             : TypeNameList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type NameTypeNameListPair  = ( Name ,TypeNameList )
-- cata
sem_NameTypeNameListPair :: NameTypeNameListPair  ->
                            T_NameTypeNameListPair 
sem_NameTypeNameListPair ( x1,x2)  =
    (sem_NameTypeNameListPair_Tuple (sem_Name x1 ) (sem_TypeNameList x2 ) )
-- semantic domain
type T_NameTypeNameListPair  = Catalog ->
                               ( NameTypeNameListPair ,NameTypeNameListPair )
data Inh_NameTypeNameListPair  = Inh_NameTypeNameListPair {cat_Inh_NameTypeNameListPair :: Catalog}
data Syn_NameTypeNameListPair  = Syn_NameTypeNameListPair {annotatedTree_Syn_NameTypeNameListPair :: NameTypeNameListPair ,originalTree_Syn_NameTypeNameListPair :: NameTypeNameListPair }
wrap_NameTypeNameListPair :: T_NameTypeNameListPair  ->
                             Inh_NameTypeNameListPair  ->
                             Syn_NameTypeNameListPair 
wrap_NameTypeNameListPair sem (Inh_NameTypeNameListPair _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_NameTypeNameListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_NameTypeNameListPair_Tuple :: T_Name  ->
                                  T_TypeNameList  ->
                                  T_NameTypeNameListPair 
sem_NameTypeNameListPair_Tuple x1_ x2_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: NameTypeNameListPair 
              _lhsOoriginalTree :: NameTypeNameListPair 
              _x1Ocat :: Catalog
              _x2Ocat :: Catalog
              _x1IannotatedTree :: Name 
              _x1IoriginalTree :: Name 
              _x2IannotatedTree :: TypeNameList 
              _x2IoriginalTree :: TypeNameList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IannotatedTree,_x2IannotatedTree)
                   {-# LINE 2239 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IoriginalTree,_x2IoriginalTree)
                   {-# LINE 2245 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2251 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2257 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2263 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2269 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _x1IannotatedTree,_x1IoriginalTree) =
                  x1_ _x1Ocat 
              ( _x2IannotatedTree,_x2IoriginalTree) =
                  x2_ _x2Ocat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- NameTypeNameListPairList ------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : NameTypeNameListPair 
         child tl             : NameTypeNameListPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type NameTypeNameListPairList  = [NameTypeNameListPair ]
-- cata
sem_NameTypeNameListPairList :: NameTypeNameListPairList  ->
                                T_NameTypeNameListPairList 
sem_NameTypeNameListPairList list  =
    (Prelude.foldr sem_NameTypeNameListPairList_Cons sem_NameTypeNameListPairList_Nil (Prelude.map sem_NameTypeNameListPair list) )
-- semantic domain
type T_NameTypeNameListPairList  = Catalog ->
                                   ( NameTypeNameListPairList ,NameTypeNameListPairList )
data Inh_NameTypeNameListPairList  = Inh_NameTypeNameListPairList {cat_Inh_NameTypeNameListPairList :: Catalog}
data Syn_NameTypeNameListPairList  = Syn_NameTypeNameListPairList {annotatedTree_Syn_NameTypeNameListPairList :: NameTypeNameListPairList ,originalTree_Syn_NameTypeNameListPairList :: NameTypeNameListPairList }
wrap_NameTypeNameListPairList :: T_NameTypeNameListPairList  ->
                                 Inh_NameTypeNameListPairList  ->
                                 Syn_NameTypeNameListPairList 
wrap_NameTypeNameListPairList sem (Inh_NameTypeNameListPairList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_NameTypeNameListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_NameTypeNameListPairList_Cons :: T_NameTypeNameListPair  ->
                                     T_NameTypeNameListPairList  ->
                                     T_NameTypeNameListPairList 
sem_NameTypeNameListPairList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: NameTypeNameListPairList 
              _lhsOoriginalTree :: NameTypeNameListPairList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: NameTypeNameListPair 
              _hdIoriginalTree :: NameTypeNameListPair 
              _tlIannotatedTree :: NameTypeNameListPairList 
              _tlIoriginalTree :: NameTypeNameListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 2330 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 2336 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2342 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2348 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2354 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2360 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_NameTypeNameListPairList_Nil :: T_NameTypeNameListPairList 
sem_NameTypeNameListPairList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: NameTypeNameListPairList 
              _lhsOoriginalTree :: NameTypeNameListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 2376 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 2382 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2388 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2394 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- OnExpr ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : JoinExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type OnExpr  = Maybe JoinExpr 
-- cata
sem_OnExpr :: OnExpr  ->
              T_OnExpr 
sem_OnExpr (Prelude.Just x )  =
    (sem_OnExpr_Just (sem_JoinExpr x ) )
sem_OnExpr Prelude.Nothing  =
    sem_OnExpr_Nothing
-- semantic domain
type T_OnExpr  = Catalog ->
                 ( OnExpr ,OnExpr )
data Inh_OnExpr  = Inh_OnExpr {cat_Inh_OnExpr :: Catalog}
data Syn_OnExpr  = Syn_OnExpr {annotatedTree_Syn_OnExpr :: OnExpr ,originalTree_Syn_OnExpr :: OnExpr }
wrap_OnExpr :: T_OnExpr  ->
               Inh_OnExpr  ->
               Syn_OnExpr 
wrap_OnExpr sem (Inh_OnExpr _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_OnExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_OnExpr_Just :: T_JoinExpr  ->
                   T_OnExpr 
sem_OnExpr_Just just_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: OnExpr 
              _lhsOoriginalTree :: OnExpr 
              _justOcat :: Catalog
              _justIannotatedTree :: JoinExpr 
              _justIoriginalTree :: JoinExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIannotatedTree
                   {-# LINE 2448 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIoriginalTree
                   {-# LINE 2454 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2460 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2466 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2472 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _justIannotatedTree,_justIoriginalTree) =
                  just_ _justOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: OnExpr 
              _lhsOoriginalTree :: OnExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2486 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2492 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2498 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2504 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ParamDef ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative ParamDef:
         child ann            : {Annotation}
         child name           : {NameComponent}
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
data ParamDef  = ParamDef (Annotation) (NameComponent) (TypeName ) 
               | ParamDefTp (Annotation) (TypeName ) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_ParamDef :: ParamDef  ->
                T_ParamDef 
sem_ParamDef (ParamDef _ann _name _typ )  =
    (sem_ParamDef_ParamDef _ann _name (sem_TypeName _typ ) )
sem_ParamDef (ParamDefTp _ann _typ )  =
    (sem_ParamDef_ParamDefTp _ann (sem_TypeName _typ ) )
-- semantic domain
type T_ParamDef  = Catalog ->
                   ( ParamDef ,ParamDef )
data Inh_ParamDef  = Inh_ParamDef {cat_Inh_ParamDef :: Catalog}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef ,originalTree_Syn_ParamDef :: ParamDef }
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ParamDef _lhsOannotatedTree _lhsOoriginalTree ))
sem_ParamDef_ParamDef :: Annotation ->
                         NameComponent ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef ann_ name_ typ_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ParamDef 
              _lhsOoriginalTree :: ParamDef 
              _typOcat :: Catalog
              _typIannotatedTree :: TypeName 
              _typIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamDef ann_ name_ _typIannotatedTree
                   {-# LINE 2566 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamDef ann_ name_ _typIoriginalTree
                   {-# LINE 2572 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2578 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2584 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2590 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _typIannotatedTree,_typIoriginalTree) =
                  typ_ _typOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ParamDef_ParamDefTp :: Annotation ->
                           T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp ann_ typ_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ParamDef 
              _lhsOoriginalTree :: ParamDef 
              _typOcat :: Catalog
              _typIannotatedTree :: TypeName 
              _typIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamDefTp ann_ _typIannotatedTree
                   {-# LINE 2609 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamDefTp ann_ _typIoriginalTree
                   {-# LINE 2615 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2621 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2627 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2633 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _typIannotatedTree,_typIoriginalTree) =
                  typ_ _typOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ParamDefList ------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
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
type ParamDefList  = [ParamDef ]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = Catalog ->
                       ( ParamDefList ,ParamDefList )
data Inh_ParamDefList  = Inh_ParamDefList {cat_Inh_ParamDefList :: Catalog}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList ,originalTree_Syn_ParamDefList :: ParamDefList }
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ParamDefList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ParamDefList 
              _lhsOoriginalTree :: ParamDefList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: ParamDef 
              _hdIoriginalTree :: ParamDef 
              _tlIannotatedTree :: ParamDefList 
              _tlIoriginalTree :: ParamDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 2692 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 2698 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2704 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2716 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2722 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ParamDefList 
              _lhsOoriginalTree :: ParamDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 2738 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 2744 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2750 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2756 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- QueryExpr ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative CombineQueryExpr:
         child ann            : {Annotation}
         child ctype          : {CombineType}
         child sel1           : QueryExpr 
         child sel2           : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Select:
         child ann            : {Annotation}
         child selDistinct    : {Distinct}
         child selSelectList  : SelectList 
         child selTref        : TableRefList 
         child selWhere       : MaybeBoolExpr 
         child selGroupBy     : ScalarExprList 
         child selHaving      : MaybeBoolExpr 
         child selOrderBy     : ScalarExprDirectionPairList 
         child selLimit       : MaybeScalarExpr 
         child selOffset      : MaybeScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Values:
         child ann            : {Annotation}
         child vll            : ScalarExprListList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative WithQueryExpr:
         child ann            : {Annotation}
         child withs          : WithQueryList 
         child ex             : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data QueryExpr  = CombineQueryExpr (Annotation) (CombineType) (QueryExpr ) (QueryExpr ) 
                | Select (Annotation) (Distinct) (SelectList ) (TableRefList ) (MaybeBoolExpr ) (ScalarExprList ) (MaybeBoolExpr ) (ScalarExprDirectionPairList ) (MaybeScalarExpr ) (MaybeScalarExpr ) 
                | Values (Annotation) (ScalarExprListList ) 
                | WithQueryExpr (Annotation) (WithQueryList ) (QueryExpr ) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_QueryExpr :: QueryExpr  ->
                 T_QueryExpr 
sem_QueryExpr (CombineQueryExpr _ann _ctype _sel1 _sel2 )  =
    (sem_QueryExpr_CombineQueryExpr _ann _ctype (sem_QueryExpr _sel1 ) (sem_QueryExpr _sel2 ) )
sem_QueryExpr (Select _ann _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selLimit _selOffset )  =
    (sem_QueryExpr_Select _ann _selDistinct (sem_SelectList _selSelectList ) (sem_TableRefList _selTref ) (sem_MaybeBoolExpr _selWhere ) (sem_ScalarExprList _selGroupBy ) (sem_MaybeBoolExpr _selHaving ) (sem_ScalarExprDirectionPairList _selOrderBy ) (sem_MaybeScalarExpr _selLimit ) (sem_MaybeScalarExpr _selOffset ) )
sem_QueryExpr (Values _ann _vll )  =
    (sem_QueryExpr_Values _ann (sem_ScalarExprListList _vll ) )
sem_QueryExpr (WithQueryExpr _ann _withs _ex )  =
    (sem_QueryExpr_WithQueryExpr _ann (sem_WithQueryList _withs ) (sem_QueryExpr _ex ) )
-- semantic domain
type T_QueryExpr  = Catalog ->
                    ( QueryExpr ,QueryExpr )
data Inh_QueryExpr  = Inh_QueryExpr {cat_Inh_QueryExpr :: Catalog}
data Syn_QueryExpr  = Syn_QueryExpr {annotatedTree_Syn_QueryExpr :: QueryExpr ,originalTree_Syn_QueryExpr :: QueryExpr }
wrap_QueryExpr :: T_QueryExpr  ->
                  Inh_QueryExpr  ->
                  Syn_QueryExpr 
wrap_QueryExpr sem (Inh_QueryExpr _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_QueryExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_QueryExpr_CombineQueryExpr :: Annotation ->
                                  CombineType ->
                                  T_QueryExpr  ->
                                  T_QueryExpr  ->
                                  T_QueryExpr 
sem_QueryExpr_CombineQueryExpr ann_ ctype_ sel1_ sel2_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: QueryExpr 
              _lhsOoriginalTree :: QueryExpr 
              _sel1Ocat :: Catalog
              _sel2Ocat :: Catalog
              _sel1IannotatedTree :: QueryExpr 
              _sel1IoriginalTree :: QueryExpr 
              _sel2IannotatedTree :: QueryExpr 
              _sel2IoriginalTree :: QueryExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CombineQueryExpr ann_ ctype_ _sel1IannotatedTree _sel2IannotatedTree
                   {-# LINE 2850 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CombineQueryExpr ann_ ctype_ _sel1IoriginalTree _sel2IoriginalTree
                   {-# LINE 2856 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2862 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2868 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sel1Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2874 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sel2Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2880 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _sel1IannotatedTree,_sel1IoriginalTree) =
                  sel1_ _sel1Ocat 
              ( _sel2IannotatedTree,_sel2IoriginalTree) =
                  sel2_ _sel2Ocat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_QueryExpr_Select :: Annotation ->
                        Distinct ->
                        T_SelectList  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_ScalarExprList  ->
                        T_MaybeBoolExpr  ->
                        T_ScalarExprDirectionPairList  ->
                        T_MaybeScalarExpr  ->
                        T_MaybeScalarExpr  ->
                        T_QueryExpr 
sem_QueryExpr_Select ann_ selDistinct_ selSelectList_ selTref_ selWhere_ selGroupBy_ selHaving_ selOrderBy_ selLimit_ selOffset_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: QueryExpr 
              _lhsOoriginalTree :: QueryExpr 
              _selSelectListOcat :: Catalog
              _selTrefOcat :: Catalog
              _selWhereOcat :: Catalog
              _selGroupByOcat :: Catalog
              _selHavingOcat :: Catalog
              _selOrderByOcat :: Catalog
              _selLimitOcat :: Catalog
              _selOffsetOcat :: Catalog
              _selSelectListIannotatedTree :: SelectList 
              _selSelectListIoriginalTree :: SelectList 
              _selTrefIannotatedTree :: TableRefList 
              _selTrefIoriginalTree :: TableRefList 
              _selWhereIannotatedTree :: MaybeBoolExpr 
              _selWhereIoriginalTree :: MaybeBoolExpr 
              _selGroupByIannotatedTree :: ScalarExprList 
              _selGroupByIoriginalTree :: ScalarExprList 
              _selHavingIannotatedTree :: MaybeBoolExpr 
              _selHavingIoriginalTree :: MaybeBoolExpr 
              _selOrderByIannotatedTree :: ScalarExprDirectionPairList 
              _selOrderByIoriginalTree :: ScalarExprDirectionPairList 
              _selLimitIannotatedTree :: MaybeScalarExpr 
              _selLimitIoriginalTree :: MaybeScalarExpr 
              _selOffsetIannotatedTree :: MaybeScalarExpr 
              _selOffsetIoriginalTree :: MaybeScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Select ann_ selDistinct_ _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
                   {-# LINE 2930 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Select ann_ selDistinct_ _selSelectListIoriginalTree _selTrefIoriginalTree _selWhereIoriginalTree _selGroupByIoriginalTree _selHavingIoriginalTree _selOrderByIoriginalTree _selLimitIoriginalTree _selOffsetIoriginalTree
                   {-# LINE 2936 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2942 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2948 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selSelectListOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2954 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selTrefOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2960 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selWhereOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2966 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selGroupByOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2972 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selHavingOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2978 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOrderByOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2984 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selLimitOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2990 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOffsetOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2996 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _selSelectListIannotatedTree,_selSelectListIoriginalTree) =
                  selSelectList_ _selSelectListOcat 
              ( _selTrefIannotatedTree,_selTrefIoriginalTree) =
                  selTref_ _selTrefOcat 
              ( _selWhereIannotatedTree,_selWhereIoriginalTree) =
                  selWhere_ _selWhereOcat 
              ( _selGroupByIannotatedTree,_selGroupByIoriginalTree) =
                  selGroupBy_ _selGroupByOcat 
              ( _selHavingIannotatedTree,_selHavingIoriginalTree) =
                  selHaving_ _selHavingOcat 
              ( _selOrderByIannotatedTree,_selOrderByIoriginalTree) =
                  selOrderBy_ _selOrderByOcat 
              ( _selLimitIannotatedTree,_selLimitIoriginalTree) =
                  selLimit_ _selLimitOcat 
              ( _selOffsetIannotatedTree,_selOffsetIoriginalTree) =
                  selOffset_ _selOffsetOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_QueryExpr_Values :: Annotation ->
                        T_ScalarExprListList  ->
                        T_QueryExpr 
sem_QueryExpr_Values ann_ vll_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: QueryExpr 
              _lhsOoriginalTree :: QueryExpr 
              _vllOcat :: Catalog
              _vllIannotatedTree :: ScalarExprListList 
              _vllIoriginalTree :: ScalarExprListList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Values ann_ _vllIannotatedTree
                   {-# LINE 3029 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Values ann_ _vllIoriginalTree
                   {-# LINE 3035 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3041 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3047 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _vllOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3053 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _vllIannotatedTree,_vllIoriginalTree) =
                  vll_ _vllOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_QueryExpr_WithQueryExpr :: Annotation ->
                               T_WithQueryList  ->
                               T_QueryExpr  ->
                               T_QueryExpr 
sem_QueryExpr_WithQueryExpr ann_ withs_ ex_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: QueryExpr 
              _lhsOoriginalTree :: QueryExpr 
              _withsOcat :: Catalog
              _exOcat :: Catalog
              _withsIannotatedTree :: WithQueryList 
              _withsIoriginalTree :: WithQueryList 
              _exIannotatedTree :: QueryExpr 
              _exIoriginalTree :: QueryExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WithQueryExpr ann_ _withsIannotatedTree _exIannotatedTree
                   {-# LINE 3076 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WithQueryExpr ann_ _withsIoriginalTree _exIoriginalTree
                   {-# LINE 3082 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3094 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _withsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3100 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _withsIannotatedTree,_withsIoriginalTree) =
                  withs_ _withsOcat 
              ( _exIannotatedTree,_exIoriginalTree) =
                  ex_ _exOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- Root --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Root:
         child statements     : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data Root  = Root (StatementList ) 
           deriving ( Show)
-- cata
sem_Root :: Root  ->
            T_Root 
sem_Root (Root _statements )  =
    (sem_Root_Root (sem_StatementList _statements ) )
-- semantic domain
type T_Root  = Catalog ->
               ( Root ,Root )
data Inh_Root  = Inh_Root {cat_Inh_Root :: Catalog}
data Syn_Root  = Syn_Root {annotatedTree_Syn_Root :: Root ,originalTree_Syn_Root :: Root }
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_Root _lhsOannotatedTree _lhsOoriginalTree ))
sem_Root_Root :: T_StatementList  ->
                 T_Root 
sem_Root_Root statements_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Root 
              _lhsOoriginalTree :: Root 
              _statementsOcat :: Catalog
              _statementsIannotatedTree :: StatementList 
              _statementsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Root _statementsIannotatedTree
                   {-# LINE 3159 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Root _statementsIoriginalTree
                   {-# LINE 3165 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3171 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3177 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _statementsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3183 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _statementsIannotatedTree,_statementsIoriginalTree) =
                  statements_ _statementsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- RowConstraint -----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative NotNullConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative NullConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowCheckConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowPrimaryKeyConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowReferenceConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child table          : Name 
         child att            : {Maybe NameComponent}
         child onUpdate       : {Cascade}
         child onDelete       : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowUniqueConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data RowConstraint  = NotNullConstraint (Annotation) (String) 
                    | NullConstraint (Annotation) (String) 
                    | RowCheckConstraint (Annotation) (String) (ScalarExpr ) 
                    | RowPrimaryKeyConstraint (Annotation) (String) 
                    | RowReferenceConstraint (Annotation) (String) (Name ) ((Maybe NameComponent)) (Cascade) (Cascade) 
                    | RowUniqueConstraint (Annotation) (String) 
                    deriving ( Data,Eq,Show,Typeable)
-- cata
sem_RowConstraint :: RowConstraint  ->
                     T_RowConstraint 
sem_RowConstraint (NotNullConstraint _ann _name )  =
    (sem_RowConstraint_NotNullConstraint _ann _name )
sem_RowConstraint (NullConstraint _ann _name )  =
    (sem_RowConstraint_NullConstraint _ann _name )
sem_RowConstraint (RowCheckConstraint _ann _name _expr )  =
    (sem_RowConstraint_RowCheckConstraint _ann _name (sem_ScalarExpr _expr ) )
sem_RowConstraint (RowPrimaryKeyConstraint _ann _name )  =
    (sem_RowConstraint_RowPrimaryKeyConstraint _ann _name )
sem_RowConstraint (RowReferenceConstraint _ann _name _table _att _onUpdate _onDelete )  =
    (sem_RowConstraint_RowReferenceConstraint _ann _name (sem_Name _table ) _att _onUpdate _onDelete )
sem_RowConstraint (RowUniqueConstraint _ann _name )  =
    (sem_RowConstraint_RowUniqueConstraint _ann _name )
-- semantic domain
type T_RowConstraint  = Catalog ->
                        ( RowConstraint ,RowConstraint )
data Inh_RowConstraint  = Inh_RowConstraint {cat_Inh_RowConstraint :: Catalog}
data Syn_RowConstraint  = Syn_RowConstraint {annotatedTree_Syn_RowConstraint :: RowConstraint ,originalTree_Syn_RowConstraint :: RowConstraint }
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_RowConstraint _lhsOannotatedTree _lhsOoriginalTree ))
sem_RowConstraint_NotNullConstraint :: Annotation ->
                                       String ->
                                       T_RowConstraint 
sem_RowConstraint_NotNullConstraint ann_ name_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NotNullConstraint ann_ name_
                   {-# LINE 3283 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NotNullConstraint ann_ name_
                   {-# LINE 3289 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3295 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3301 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_NullConstraint :: Annotation ->
                                    String ->
                                    T_RowConstraint 
sem_RowConstraint_NullConstraint ann_ name_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullConstraint ann_ name_
                   {-# LINE 3315 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullConstraint ann_ name_
                   {-# LINE 3321 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3327 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3333 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowCheckConstraint :: Annotation ->
                                        String ->
                                        T_ScalarExpr  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              _exprOcat :: Catalog
              _exprIannotatedTree :: ScalarExpr 
              _exprIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowCheckConstraint ann_ name_ _exprIannotatedTree
                   {-# LINE 3351 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowCheckConstraint ann_ name_ _exprIoriginalTree
                   {-# LINE 3357 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3363 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3369 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowPrimaryKeyConstraint :: Annotation ->
                                             String ->
                                             T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint ann_ name_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowPrimaryKeyConstraint ann_ name_
                   {-# LINE 3391 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowPrimaryKeyConstraint ann_ name_
                   {-# LINE 3397 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3403 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3409 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowReferenceConstraint :: Annotation ->
                                            String ->
                                            T_Name  ->
                                            (Maybe NameComponent) ->
                                            Cascade ->
                                            Cascade ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              _tableOcat :: Catalog
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowReferenceConstraint ann_ name_ _tableIannotatedTree att_ onUpdate_ onDelete_
                   {-# LINE 3430 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowReferenceConstraint ann_ name_ _tableIoriginalTree att_ onUpdate_ onDelete_
                   {-# LINE 3436 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3442 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3448 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3454 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowUniqueConstraint :: Annotation ->
                                         String ->
                                         T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint ann_ name_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowUniqueConstraint ann_ name_
                   {-# LINE 3470 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowUniqueConstraint ann_ name_
                   {-# LINE 3476 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3482 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3488 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- RowConstraintList -------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
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
type RowConstraintList  = [RowConstraint ]
-- cata
sem_RowConstraintList :: RowConstraintList  ->
                         T_RowConstraintList 
sem_RowConstraintList list  =
    (Prelude.foldr sem_RowConstraintList_Cons sem_RowConstraintList_Nil (Prelude.map sem_RowConstraint list) )
-- semantic domain
type T_RowConstraintList  = Catalog ->
                            ( RowConstraintList ,RowConstraintList )
data Inh_RowConstraintList  = Inh_RowConstraintList {cat_Inh_RowConstraintList :: Catalog}
data Syn_RowConstraintList  = Syn_RowConstraintList {annotatedTree_Syn_RowConstraintList :: RowConstraintList ,originalTree_Syn_RowConstraintList :: RowConstraintList }
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_RowConstraintList _lhsOannotatedTree _lhsOoriginalTree ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: RowConstraintList 
              _lhsOoriginalTree :: RowConstraintList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: RowConstraint 
              _hdIoriginalTree :: RowConstraint 
              _tlIannotatedTree :: RowConstraintList 
              _tlIoriginalTree :: RowConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 3545 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 3551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3557 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3563 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3569 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3575 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: RowConstraintList 
              _lhsOoriginalTree :: RowConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 3591 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 3597 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3603 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3609 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ScalarExpr --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative AggregateFn:
         child ann            : {Annotation}
         child aggDistinct    : {Distinct}
         child fn             : ScalarExpr 
         child orderBy        : ScalarExprDirectionPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative AntiScalarExpr:
         child string         : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative BooleanLit:
         child ann            : {Annotation}
         child b              : {Bool}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Case:
         child ann            : {Annotation}
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CaseSimple:
         child ann            : {Annotation}
         child value          : ScalarExpr 
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Cast:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         child tn             : TypeName 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Exists:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Extract:
         child ann            : {Annotation}
         child field          : {ExtractField}
         child e              : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative FunCall:
         child ann            : {Annotation}
         child funName        : Name 
         child args           : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Identifier:
         child ann            : {Annotation}
         child i              : {NameComponent}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative InPredicate:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         child i              : {Bool}
         child list           : InList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Interval:
         child ann            : {Annotation}
         child value          : {String}
         child field          : {IntervalField}
         child prec           : {Maybe Int}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative LiftOperator:
         child ann            : {Annotation}
         child oper           : {String}
         child flav           : {LiftFlavour}
         child args           : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative NullLit:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative NumberLit:
         child ann            : {Annotation}
         child d              : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Placeholder:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative PositionalArg:
         child ann            : {Annotation}
         child p              : {Integer}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative QIdentifier:
         child ann            : {Annotation}
         child is             : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative QStar:
         child ann            : {Annotation}
         child q              : {NameComponent}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ScalarSubQuery:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Star:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative StringLit:
         child ann            : {Annotation}
         child value          : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative TypedStringLit:
         child ann            : {Annotation}
         child tn             : TypeName 
         child value          : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative WindowFn:
         child ann            : {Annotation}
         child fn             : ScalarExpr 
         child partitionBy    : ScalarExprList 
         child orderBy        : ScalarExprDirectionPairList 
         child frm            : {FrameClause}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data ScalarExpr  = AggregateFn (Annotation) (Distinct) (ScalarExpr ) (ScalarExprDirectionPairList ) 
                 | AntiScalarExpr (String) 
                 | BooleanLit (Annotation) (Bool) 
                 | Case (Annotation) (CaseScalarExprListScalarExprPairList ) (MaybeScalarExpr ) 
                 | CaseSimple (Annotation) (ScalarExpr ) (CaseScalarExprListScalarExprPairList ) (MaybeScalarExpr ) 
                 | Cast (Annotation) (ScalarExpr ) (TypeName ) 
                 | Exists (Annotation) (QueryExpr ) 
                 | Extract (Annotation) (ExtractField) (ScalarExpr ) 
                 | FunCall (Annotation) (Name ) (ScalarExprList ) 
                 | Identifier (Annotation) (NameComponent) 
                 | InPredicate (Annotation) (ScalarExpr ) (Bool) (InList ) 
                 | Interval (Annotation) (String) (IntervalField) ((Maybe Int)) 
                 | LiftOperator (Annotation) (String) (LiftFlavour) (ScalarExprList ) 
                 | NullLit (Annotation) 
                 | NumberLit (Annotation) (String) 
                 | Placeholder (Annotation) 
                 | PositionalArg (Annotation) (Integer) 
                 | QIdentifier (Annotation) (([NameComponent])) 
                 | QStar (Annotation) (NameComponent) 
                 | ScalarSubQuery (Annotation) (QueryExpr ) 
                 | Star (Annotation) 
                 | StringLit (Annotation) (String) 
                 | TypedStringLit (Annotation) (TypeName ) (String) 
                 | WindowFn (Annotation) (ScalarExpr ) (ScalarExprList ) (ScalarExprDirectionPairList ) (FrameClause) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_ScalarExpr :: ScalarExpr  ->
                  T_ScalarExpr 
sem_ScalarExpr (AggregateFn _ann _aggDistinct _fn _orderBy )  =
    (sem_ScalarExpr_AggregateFn _ann _aggDistinct (sem_ScalarExpr _fn ) (sem_ScalarExprDirectionPairList _orderBy ) )
sem_ScalarExpr (AntiScalarExpr _string )  =
    (sem_ScalarExpr_AntiScalarExpr _string )
sem_ScalarExpr (BooleanLit _ann _b )  =
    (sem_ScalarExpr_BooleanLit _ann _b )
sem_ScalarExpr (Case _ann _cases _els )  =
    (sem_ScalarExpr_Case _ann (sem_CaseScalarExprListScalarExprPairList _cases ) (sem_MaybeScalarExpr _els ) )
sem_ScalarExpr (CaseSimple _ann _value _cases _els )  =
    (sem_ScalarExpr_CaseSimple _ann (sem_ScalarExpr _value ) (sem_CaseScalarExprListScalarExprPairList _cases ) (sem_MaybeScalarExpr _els ) )
sem_ScalarExpr (Cast _ann _expr _tn )  =
    (sem_ScalarExpr_Cast _ann (sem_ScalarExpr _expr ) (sem_TypeName _tn ) )
sem_ScalarExpr (Exists _ann _sel )  =
    (sem_ScalarExpr_Exists _ann (sem_QueryExpr _sel ) )
sem_ScalarExpr (Extract _ann _field _e )  =
    (sem_ScalarExpr_Extract _ann _field (sem_ScalarExpr _e ) )
sem_ScalarExpr (FunCall _ann _funName _args )  =
    (sem_ScalarExpr_FunCall _ann (sem_Name _funName ) (sem_ScalarExprList _args ) )
sem_ScalarExpr (Identifier _ann _i )  =
    (sem_ScalarExpr_Identifier _ann _i )
sem_ScalarExpr (InPredicate _ann _expr _i _list )  =
    (sem_ScalarExpr_InPredicate _ann (sem_ScalarExpr _expr ) _i (sem_InList _list ) )
sem_ScalarExpr (Interval _ann _value _field _prec )  =
    (sem_ScalarExpr_Interval _ann _value _field _prec )
sem_ScalarExpr (LiftOperator _ann _oper _flav _args )  =
    (sem_ScalarExpr_LiftOperator _ann _oper _flav (sem_ScalarExprList _args ) )
sem_ScalarExpr (NullLit _ann )  =
    (sem_ScalarExpr_NullLit _ann )
sem_ScalarExpr (NumberLit _ann _d )  =
    (sem_ScalarExpr_NumberLit _ann _d )
sem_ScalarExpr (Placeholder _ann )  =
    (sem_ScalarExpr_Placeholder _ann )
sem_ScalarExpr (PositionalArg _ann _p )  =
    (sem_ScalarExpr_PositionalArg _ann _p )
sem_ScalarExpr (QIdentifier _ann _is )  =
    (sem_ScalarExpr_QIdentifier _ann _is )
sem_ScalarExpr (QStar _ann _q )  =
    (sem_ScalarExpr_QStar _ann _q )
sem_ScalarExpr (ScalarSubQuery _ann _sel )  =
    (sem_ScalarExpr_ScalarSubQuery _ann (sem_QueryExpr _sel ) )
sem_ScalarExpr (Star _ann )  =
    (sem_ScalarExpr_Star _ann )
sem_ScalarExpr (StringLit _ann _value )  =
    (sem_ScalarExpr_StringLit _ann _value )
sem_ScalarExpr (TypedStringLit _ann _tn _value )  =
    (sem_ScalarExpr_TypedStringLit _ann (sem_TypeName _tn ) _value )
sem_ScalarExpr (WindowFn _ann _fn _partitionBy _orderBy _frm )  =
    (sem_ScalarExpr_WindowFn _ann (sem_ScalarExpr _fn ) (sem_ScalarExprList _partitionBy ) (sem_ScalarExprDirectionPairList _orderBy ) _frm )
-- semantic domain
type T_ScalarExpr  = Catalog ->
                     ( ScalarExpr ,ScalarExpr )
data Inh_ScalarExpr  = Inh_ScalarExpr {cat_Inh_ScalarExpr :: Catalog}
data Syn_ScalarExpr  = Syn_ScalarExpr {annotatedTree_Syn_ScalarExpr :: ScalarExpr ,originalTree_Syn_ScalarExpr :: ScalarExpr }
wrap_ScalarExpr :: T_ScalarExpr  ->
                   Inh_ScalarExpr  ->
                   Syn_ScalarExpr 
wrap_ScalarExpr sem (Inh_ScalarExpr _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ScalarExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExpr_AggregateFn :: Annotation ->
                              Distinct ->
                              T_ScalarExpr  ->
                              T_ScalarExprDirectionPairList  ->
                              T_ScalarExpr 
sem_ScalarExpr_AggregateFn ann_ aggDistinct_ fn_ orderBy_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _fnOcat :: Catalog
              _orderByOcat :: Catalog
              _fnIannotatedTree :: ScalarExpr 
              _fnIoriginalTree :: ScalarExpr 
              _orderByIannotatedTree :: ScalarExprDirectionPairList 
              _orderByIoriginalTree :: ScalarExprDirectionPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AggregateFn ann_ aggDistinct_ _fnIannotatedTree _orderByIannotatedTree
                   {-# LINE 3886 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AggregateFn ann_ aggDistinct_ _fnIoriginalTree _orderByIoriginalTree
                   {-# LINE 3892 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3898 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3904 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3910 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _orderByOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3916 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _fnIannotatedTree,_fnIoriginalTree) =
                  fn_ _fnOcat 
              ( _orderByIannotatedTree,_orderByIoriginalTree) =
                  orderBy_ _orderByOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_AntiScalarExpr :: String ->
                                 T_ScalarExpr 
sem_ScalarExpr_AntiScalarExpr string_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiScalarExpr string_
                   {-# LINE 3933 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiScalarExpr string_
                   {-# LINE 3939 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3945 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3951 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_BooleanLit :: Annotation ->
                             Bool ->
                             T_ScalarExpr 
sem_ScalarExpr_BooleanLit ann_ b_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   BooleanLit ann_ b_
                   {-# LINE 3965 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   BooleanLit ann_ b_
                   {-# LINE 3971 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3977 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3983 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_Case :: Annotation ->
                       T_CaseScalarExprListScalarExprPairList  ->
                       T_MaybeScalarExpr  ->
                       T_ScalarExpr 
sem_ScalarExpr_Case ann_ cases_ els_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _casesOcat :: Catalog
              _elsOcat :: Catalog
              _casesIannotatedTree :: CaseScalarExprListScalarExprPairList 
              _casesIoriginalTree :: CaseScalarExprListScalarExprPairList 
              _elsIannotatedTree :: MaybeScalarExpr 
              _elsIoriginalTree :: MaybeScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Case ann_ _casesIannotatedTree _elsIannotatedTree
                   {-# LINE 4004 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Case ann_ _casesIoriginalTree _elsIoriginalTree
                   {-# LINE 4010 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4016 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4022 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4028 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4034 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  cases_ _casesOcat 
              ( _elsIannotatedTree,_elsIoriginalTree) =
                  els_ _elsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_CaseSimple :: Annotation ->
                             T_ScalarExpr  ->
                             T_CaseScalarExprListScalarExprPairList  ->
                             T_MaybeScalarExpr  ->
                             T_ScalarExpr 
sem_ScalarExpr_CaseSimple ann_ value_ cases_ els_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _valueOcat :: Catalog
              _casesOcat :: Catalog
              _elsOcat :: Catalog
              _valueIannotatedTree :: ScalarExpr 
              _valueIoriginalTree :: ScalarExpr 
              _casesIannotatedTree :: CaseScalarExprListScalarExprPairList 
              _casesIoriginalTree :: CaseScalarExprListScalarExprPairList 
              _elsIannotatedTree :: MaybeScalarExpr 
              _elsIoriginalTree :: MaybeScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                   {-# LINE 4063 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseSimple ann_ _valueIoriginalTree _casesIoriginalTree _elsIoriginalTree
                   {-# LINE 4069 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4075 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4081 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valueOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4087 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4093 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4099 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _valueIannotatedTree,_valueIoriginalTree) =
                  value_ _valueOcat 
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  cases_ _casesOcat 
              ( _elsIannotatedTree,_elsIoriginalTree) =
                  els_ _elsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_Cast :: Annotation ->
                       T_ScalarExpr  ->
                       T_TypeName  ->
                       T_ScalarExpr 
sem_ScalarExpr_Cast ann_ expr_ tn_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _exprOcat :: Catalog
              _tnOcat :: Catalog
              _exprIannotatedTree :: ScalarExpr 
              _exprIoriginalTree :: ScalarExpr 
              _tnIannotatedTree :: TypeName 
              _tnIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Cast ann_ _exprIannotatedTree _tnIannotatedTree
                   {-# LINE 4126 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Cast ann_ _exprIoriginalTree _tnIoriginalTree
                   {-# LINE 4132 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4138 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4144 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4150 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4156 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
              ( _tnIannotatedTree,_tnIoriginalTree) =
                  tn_ _tnOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_Exists :: Annotation ->
                         T_QueryExpr  ->
                         T_ScalarExpr 
sem_ScalarExpr_Exists ann_ sel_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _selOcat :: Catalog
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Exists ann_ _selIannotatedTree
                   {-# LINE 4177 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Exists ann_ _selIoriginalTree
                   {-# LINE 4183 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4189 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4195 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4201 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _selIannotatedTree,_selIoriginalTree) =
                  sel_ _selOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_Extract :: Annotation ->
                          ExtractField ->
                          T_ScalarExpr  ->
                          T_ScalarExpr 
sem_ScalarExpr_Extract ann_ field_ e_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _eOcat :: Catalog
              _eIannotatedTree :: ScalarExpr 
              _eIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Extract ann_ field_ _eIannotatedTree
                   {-# LINE 4221 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Extract ann_ field_ _eIoriginalTree
                   {-# LINE 4227 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4233 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4239 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _eOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4245 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _eIannotatedTree,_eIoriginalTree) =
                  e_ _eOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_FunCall :: Annotation ->
                          T_Name  ->
                          T_ScalarExprList  ->
                          T_ScalarExpr 
sem_ScalarExpr_FunCall ann_ funName_ args_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _funNameOcat :: Catalog
              _argsOcat :: Catalog
              _funNameIannotatedTree :: Name 
              _funNameIoriginalTree :: Name 
              _argsIannotatedTree :: ScalarExprList 
              _argsIoriginalTree :: ScalarExprList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   FunCall ann_ _funNameIannotatedTree _argsIannotatedTree
                   {-# LINE 4268 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   FunCall ann_ _funNameIoriginalTree _argsIoriginalTree
                   {-# LINE 4274 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4280 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4286 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _funNameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4292 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4298 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _funNameIannotatedTree,_funNameIoriginalTree) =
                  funName_ _funNameOcat 
              ( _argsIannotatedTree,_argsIoriginalTree) =
                  args_ _argsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_Identifier :: Annotation ->
                             NameComponent ->
                             T_ScalarExpr 
sem_ScalarExpr_Identifier ann_ i_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Identifier ann_ i_
                   {-# LINE 4316 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Identifier ann_ i_
                   {-# LINE 4322 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4328 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4334 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_InPredicate :: Annotation ->
                              T_ScalarExpr  ->
                              Bool ->
                              T_InList  ->
                              T_ScalarExpr 
sem_ScalarExpr_InPredicate ann_ expr_ i_ list_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _exprOcat :: Catalog
              _listOcat :: Catalog
              _exprIannotatedTree :: ScalarExpr 
              _exprIoriginalTree :: ScalarExpr 
              _listIannotatedTree :: InList 
              _listIoriginalTree :: InList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                   {-# LINE 4356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InPredicate ann_ _exprIoriginalTree i_ _listIoriginalTree
                   {-# LINE 4362 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4368 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4374 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4380 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _listOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4386 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
              ( _listIannotatedTree,_listIoriginalTree) =
                  list_ _listOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_Interval :: Annotation ->
                           String ->
                           IntervalField ->
                           (Maybe Int) ->
                           T_ScalarExpr 
sem_ScalarExpr_Interval ann_ value_ field_ prec_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Interval ann_ value_ field_ prec_
                   {-# LINE 4406 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Interval ann_ value_ field_ prec_
                   {-# LINE 4412 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4418 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4424 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_LiftOperator :: Annotation ->
                               String ->
                               LiftFlavour ->
                               T_ScalarExprList  ->
                               T_ScalarExpr 
sem_ScalarExpr_LiftOperator ann_ oper_ flav_ args_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _argsOcat :: Catalog
              _argsIannotatedTree :: ScalarExprList 
              _argsIoriginalTree :: ScalarExprList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   LiftOperator ann_ oper_ flav_ _argsIannotatedTree
                   {-# LINE 4443 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   LiftOperator ann_ oper_ flav_ _argsIoriginalTree
                   {-# LINE 4449 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4455 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4467 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _argsIannotatedTree,_argsIoriginalTree) =
                  args_ _argsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_NullLit :: Annotation ->
                          T_ScalarExpr 
sem_ScalarExpr_NullLit ann_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullLit ann_
                   {-# LINE 4482 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullLit ann_
                   {-# LINE 4488 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4494 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4500 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_NumberLit :: Annotation ->
                            String ->
                            T_ScalarExpr 
sem_ScalarExpr_NumberLit ann_ d_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NumberLit ann_ d_
                   {-# LINE 4514 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NumberLit ann_ d_
                   {-# LINE 4520 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4526 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4532 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_Placeholder :: Annotation ->
                              T_ScalarExpr 
sem_ScalarExpr_Placeholder ann_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Placeholder ann_
                   {-# LINE 4545 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Placeholder ann_
                   {-# LINE 4551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4557 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4563 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_PositionalArg :: Annotation ->
                                Integer ->
                                T_ScalarExpr 
sem_ScalarExpr_PositionalArg ann_ p_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PositionalArg ann_ p_
                   {-# LINE 4577 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PositionalArg ann_ p_
                   {-# LINE 4583 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4589 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4595 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_QIdentifier :: Annotation ->
                              ([NameComponent]) ->
                              T_ScalarExpr 
sem_ScalarExpr_QIdentifier ann_ is_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QIdentifier ann_ is_
                   {-# LINE 4609 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QIdentifier ann_ is_
                   {-# LINE 4615 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4621 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4627 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_QStar :: Annotation ->
                        NameComponent ->
                        T_ScalarExpr 
sem_ScalarExpr_QStar ann_ q_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QStar ann_ q_
                   {-# LINE 4641 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QStar ann_ q_
                   {-# LINE 4647 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4653 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4659 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_ScalarSubQuery :: Annotation ->
                                 T_QueryExpr  ->
                                 T_ScalarExpr 
sem_ScalarExpr_ScalarSubQuery ann_ sel_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _selOcat :: Catalog
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ScalarSubQuery ann_ _selIannotatedTree
                   {-# LINE 4676 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ScalarSubQuery ann_ _selIoriginalTree
                   {-# LINE 4682 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4688 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4694 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4700 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _selIannotatedTree,_selIoriginalTree) =
                  sel_ _selOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_Star :: Annotation ->
                       T_ScalarExpr 
sem_ScalarExpr_Star ann_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Star ann_
                   {-# LINE 4715 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Star ann_
                   {-# LINE 4721 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4727 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4733 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_StringLit :: Annotation ->
                            String ->
                            T_ScalarExpr 
sem_ScalarExpr_StringLit ann_ value_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   StringLit ann_ value_
                   {-# LINE 4747 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   StringLit ann_ value_
                   {-# LINE 4753 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4759 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4765 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_TypedStringLit :: Annotation ->
                                 T_TypeName  ->
                                 String ->
                                 T_ScalarExpr 
sem_ScalarExpr_TypedStringLit ann_ tn_ value_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _tnOcat :: Catalog
              _tnIannotatedTree :: TypeName 
              _tnIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TypedStringLit ann_ _tnIannotatedTree value_
                   {-# LINE 4783 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TypedStringLit ann_ _tnIoriginalTree value_
                   {-# LINE 4789 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4795 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4801 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4807 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _tnIannotatedTree,_tnIoriginalTree) =
                  tn_ _tnOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExpr_WindowFn :: Annotation ->
                           T_ScalarExpr  ->
                           T_ScalarExprList  ->
                           T_ScalarExprDirectionPairList  ->
                           FrameClause ->
                           T_ScalarExpr 
sem_ScalarExpr_WindowFn ann_ fn_ partitionBy_ orderBy_ frm_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _fnOcat :: Catalog
              _partitionByOcat :: Catalog
              _orderByOcat :: Catalog
              _fnIannotatedTree :: ScalarExpr 
              _fnIoriginalTree :: ScalarExpr 
              _partitionByIannotatedTree :: ScalarExprList 
              _partitionByIoriginalTree :: ScalarExprList 
              _orderByIannotatedTree :: ScalarExprDirectionPairList 
              _orderByIoriginalTree :: ScalarExprDirectionPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree frm_
                   {-# LINE 4835 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WindowFn ann_ _fnIoriginalTree _partitionByIoriginalTree _orderByIoriginalTree frm_
                   {-# LINE 4841 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4847 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4853 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4859 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _partitionByOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4865 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _orderByOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4871 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _fnIannotatedTree,_fnIoriginalTree) =
                  fn_ _fnOcat 
              ( _partitionByIannotatedTree,_partitionByIoriginalTree) =
                  partitionBy_ _partitionByOcat 
              ( _orderByIannotatedTree,_orderByIoriginalTree) =
                  orderBy_ _orderByOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ScalarExprDirectionPair -------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExpr 
         child x2             : {Direction}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ScalarExprDirectionPair  = ( ScalarExpr ,(Direction))
-- cata
sem_ScalarExprDirectionPair :: ScalarExprDirectionPair  ->
                               T_ScalarExprDirectionPair 
sem_ScalarExprDirectionPair ( x1,x2)  =
    (sem_ScalarExprDirectionPair_Tuple (sem_ScalarExpr x1 ) x2 )
-- semantic domain
type T_ScalarExprDirectionPair  = Catalog ->
                                  ( ScalarExprDirectionPair ,ScalarExprDirectionPair )
data Inh_ScalarExprDirectionPair  = Inh_ScalarExprDirectionPair {cat_Inh_ScalarExprDirectionPair :: Catalog}
data Syn_ScalarExprDirectionPair  = Syn_ScalarExprDirectionPair {annotatedTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair ,originalTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair }
wrap_ScalarExprDirectionPair :: T_ScalarExprDirectionPair  ->
                                Inh_ScalarExprDirectionPair  ->
                                Syn_ScalarExprDirectionPair 
wrap_ScalarExprDirectionPair sem (Inh_ScalarExprDirectionPair _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ScalarExprDirectionPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprDirectionPair_Tuple :: T_ScalarExpr  ->
                                     Direction ->
                                     T_ScalarExprDirectionPair 
sem_ScalarExprDirectionPair_Tuple x1_ x2_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprDirectionPair 
              _lhsOoriginalTree :: ScalarExprDirectionPair 
              _x1Ocat :: Catalog
              _x1IannotatedTree :: ScalarExpr 
              _x1IoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IannotatedTree,x2_)
                   {-# LINE 4927 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IoriginalTree,x2_)
                   {-# LINE 4933 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4939 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4945 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4951 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _x1IannotatedTree,_x1IoriginalTree) =
                  x1_ _x1Ocat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ScalarExprDirectionPairList ---------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExprDirectionPair 
         child tl             : ScalarExprDirectionPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ScalarExprDirectionPairList  = [ScalarExprDirectionPair ]
-- cata
sem_ScalarExprDirectionPairList :: ScalarExprDirectionPairList  ->
                                   T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList list  =
    (Prelude.foldr sem_ScalarExprDirectionPairList_Cons sem_ScalarExprDirectionPairList_Nil (Prelude.map sem_ScalarExprDirectionPair list) )
-- semantic domain
type T_ScalarExprDirectionPairList  = Catalog ->
                                      ( ScalarExprDirectionPairList ,ScalarExprDirectionPairList )
data Inh_ScalarExprDirectionPairList  = Inh_ScalarExprDirectionPairList {cat_Inh_ScalarExprDirectionPairList :: Catalog}
data Syn_ScalarExprDirectionPairList  = Syn_ScalarExprDirectionPairList {annotatedTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList ,originalTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList }
wrap_ScalarExprDirectionPairList :: T_ScalarExprDirectionPairList  ->
                                    Inh_ScalarExprDirectionPairList  ->
                                    Syn_ScalarExprDirectionPairList 
wrap_ScalarExprDirectionPairList sem (Inh_ScalarExprDirectionPairList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ScalarExprDirectionPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprDirectionPairList_Cons :: T_ScalarExprDirectionPair  ->
                                        T_ScalarExprDirectionPairList  ->
                                        T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprDirectionPairList 
              _lhsOoriginalTree :: ScalarExprDirectionPairList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: ScalarExprDirectionPair 
              _hdIoriginalTree :: ScalarExprDirectionPair 
              _tlIannotatedTree :: ScalarExprDirectionPairList 
              _tlIoriginalTree :: ScalarExprDirectionPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 5010 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 5016 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5022 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5028 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5034 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5040 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExprDirectionPairList_Nil :: T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprDirectionPairList 
              _lhsOoriginalTree :: ScalarExprDirectionPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5056 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5062 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5068 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5074 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ScalarExprList ----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExpr 
         child tl             : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ScalarExprList  = [ScalarExpr ]
-- cata
sem_ScalarExprList :: ScalarExprList  ->
                      T_ScalarExprList 
sem_ScalarExprList list  =
    (Prelude.foldr sem_ScalarExprList_Cons sem_ScalarExprList_Nil (Prelude.map sem_ScalarExpr list) )
-- semantic domain
type T_ScalarExprList  = Catalog ->
                         ( ScalarExprList ,ScalarExprList )
data Inh_ScalarExprList  = Inh_ScalarExprList {cat_Inh_ScalarExprList :: Catalog}
data Syn_ScalarExprList  = Syn_ScalarExprList {annotatedTree_Syn_ScalarExprList :: ScalarExprList ,originalTree_Syn_ScalarExprList :: ScalarExprList }
wrap_ScalarExprList :: T_ScalarExprList  ->
                       Inh_ScalarExprList  ->
                       Syn_ScalarExprList 
wrap_ScalarExprList sem (Inh_ScalarExprList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ScalarExprList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprList_Cons :: T_ScalarExpr  ->
                           T_ScalarExprList  ->
                           T_ScalarExprList 
sem_ScalarExprList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprList 
              _lhsOoriginalTree :: ScalarExprList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: ScalarExpr 
              _hdIoriginalTree :: ScalarExpr 
              _tlIannotatedTree :: ScalarExprList 
              _tlIoriginalTree :: ScalarExprList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 5131 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 5137 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5143 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5149 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5155 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5161 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExprList_Nil :: T_ScalarExprList 
sem_ScalarExprList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprList 
              _lhsOoriginalTree :: ScalarExprList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5177 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5183 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5189 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5195 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ScalarExprListList ------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExprList 
         child tl             : ScalarExprListList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ScalarExprListList  = [ScalarExprList ]
-- cata
sem_ScalarExprListList :: ScalarExprListList  ->
                          T_ScalarExprListList 
sem_ScalarExprListList list  =
    (Prelude.foldr sem_ScalarExprListList_Cons sem_ScalarExprListList_Nil (Prelude.map sem_ScalarExprList list) )
-- semantic domain
type T_ScalarExprListList  = Catalog ->
                             ( ScalarExprListList ,ScalarExprListList )
data Inh_ScalarExprListList  = Inh_ScalarExprListList {cat_Inh_ScalarExprListList :: Catalog}
data Syn_ScalarExprListList  = Syn_ScalarExprListList {annotatedTree_Syn_ScalarExprListList :: ScalarExprListList ,originalTree_Syn_ScalarExprListList :: ScalarExprListList }
wrap_ScalarExprListList :: T_ScalarExprListList  ->
                           Inh_ScalarExprListList  ->
                           Syn_ScalarExprListList 
wrap_ScalarExprListList sem (Inh_ScalarExprListList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ScalarExprListList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprListList_Cons :: T_ScalarExprList  ->
                               T_ScalarExprListList  ->
                               T_ScalarExprListList 
sem_ScalarExprListList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprListList 
              _lhsOoriginalTree :: ScalarExprListList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: ScalarExprList 
              _hdIoriginalTree :: ScalarExprList 
              _tlIannotatedTree :: ScalarExprListList 
              _tlIoriginalTree :: ScalarExprListList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 5252 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 5258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5264 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5270 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5276 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5282 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExprListList_Nil :: T_ScalarExprListList 
sem_ScalarExprListList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprListList 
              _lhsOoriginalTree :: ScalarExprListList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5298 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5304 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5310 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5316 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ScalarExprListStatementListPair -----------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExprList 
         child x2             : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ScalarExprListStatementListPair  = ( ScalarExprList ,StatementList )
-- cata
sem_ScalarExprListStatementListPair :: ScalarExprListStatementListPair  ->
                                       T_ScalarExprListStatementListPair 
sem_ScalarExprListStatementListPair ( x1,x2)  =
    (sem_ScalarExprListStatementListPair_Tuple (sem_ScalarExprList x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ScalarExprListStatementListPair  = Catalog ->
                                          ( ScalarExprListStatementListPair ,ScalarExprListStatementListPair )
data Inh_ScalarExprListStatementListPair  = Inh_ScalarExprListStatementListPair {cat_Inh_ScalarExprListStatementListPair :: Catalog}
data Syn_ScalarExprListStatementListPair  = Syn_ScalarExprListStatementListPair {annotatedTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair ,originalTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair }
wrap_ScalarExprListStatementListPair :: T_ScalarExprListStatementListPair  ->
                                        Inh_ScalarExprListStatementListPair  ->
                                        Syn_ScalarExprListStatementListPair 
wrap_ScalarExprListStatementListPair sem (Inh_ScalarExprListStatementListPair _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ScalarExprListStatementListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprListStatementListPair_Tuple :: T_ScalarExprList  ->
                                             T_StatementList  ->
                                             T_ScalarExprListStatementListPair 
sem_ScalarExprListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprListStatementListPair 
              _lhsOoriginalTree :: ScalarExprListStatementListPair 
              _x1Ocat :: Catalog
              _x2Ocat :: Catalog
              _x1IannotatedTree :: ScalarExprList 
              _x1IoriginalTree :: ScalarExprList 
              _x2IannotatedTree :: StatementList 
              _x2IoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IannotatedTree,_x2IannotatedTree)
                   {-# LINE 5369 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IoriginalTree,_x2IoriginalTree)
                   {-# LINE 5375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5381 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5387 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5393 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _x1IannotatedTree,_x1IoriginalTree) =
                  x1_ _x1Ocat 
              ( _x2IannotatedTree,_x2IoriginalTree) =
                  x2_ _x2Ocat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ScalarExprListStatementListPairList -------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExprListStatementListPair 
         child tl             : ScalarExprListStatementListPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ScalarExprListStatementListPairList  = [ScalarExprListStatementListPair ]
-- cata
sem_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList  ->
                                           T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList list  =
    (Prelude.foldr sem_ScalarExprListStatementListPairList_Cons sem_ScalarExprListStatementListPairList_Nil (Prelude.map sem_ScalarExprListStatementListPair list) )
-- semantic domain
type T_ScalarExprListStatementListPairList  = Catalog ->
                                              ( ScalarExprListStatementListPairList ,ScalarExprListStatementListPairList )
data Inh_ScalarExprListStatementListPairList  = Inh_ScalarExprListStatementListPairList {cat_Inh_ScalarExprListStatementListPairList :: Catalog}
data Syn_ScalarExprListStatementListPairList  = Syn_ScalarExprListStatementListPairList {annotatedTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList ,originalTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList }
wrap_ScalarExprListStatementListPairList :: T_ScalarExprListStatementListPairList  ->
                                            Inh_ScalarExprListStatementListPairList  ->
                                            Syn_ScalarExprListStatementListPairList 
wrap_ScalarExprListStatementListPairList sem (Inh_ScalarExprListStatementListPairList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ScalarExprListStatementListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprListStatementListPairList_Cons :: T_ScalarExprListStatementListPair  ->
                                                T_ScalarExprListStatementListPairList  ->
                                                T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprListStatementListPairList 
              _lhsOoriginalTree :: ScalarExprListStatementListPairList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: ScalarExprListStatementListPair 
              _hdIoriginalTree :: ScalarExprListStatementListPair 
              _tlIannotatedTree :: ScalarExprListStatementListPairList 
              _tlIoriginalTree :: ScalarExprListStatementListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 5460 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 5466 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5472 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5478 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5484 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5490 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExprListStatementListPairList_Nil :: T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprListStatementListPairList 
              _lhsOoriginalTree :: ScalarExprListStatementListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5506 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5512 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5518 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ScalarExprRoot ----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative ScalarExprRoot:
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data ScalarExprRoot  = ScalarExprRoot (ScalarExpr ) 
                     deriving ( Show)
-- cata
sem_ScalarExprRoot :: ScalarExprRoot  ->
                      T_ScalarExprRoot 
sem_ScalarExprRoot (ScalarExprRoot _expr )  =
    (sem_ScalarExprRoot_ScalarExprRoot (sem_ScalarExpr _expr ) )
-- semantic domain
type T_ScalarExprRoot  = Catalog ->
                         ( ScalarExprRoot ,ScalarExprRoot )
data Inh_ScalarExprRoot  = Inh_ScalarExprRoot {cat_Inh_ScalarExprRoot :: Catalog}
data Syn_ScalarExprRoot  = Syn_ScalarExprRoot {annotatedTree_Syn_ScalarExprRoot :: ScalarExprRoot ,originalTree_Syn_ScalarExprRoot :: ScalarExprRoot }
wrap_ScalarExprRoot :: T_ScalarExprRoot  ->
                       Inh_ScalarExprRoot  ->
                       Syn_ScalarExprRoot 
wrap_ScalarExprRoot sem (Inh_ScalarExprRoot _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ScalarExprRoot _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprRoot_ScalarExprRoot :: T_ScalarExpr  ->
                                     T_ScalarExprRoot 
sem_ScalarExprRoot_ScalarExprRoot expr_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprRoot 
              _lhsOoriginalTree :: ScalarExprRoot 
              _exprOcat :: Catalog
              _exprIannotatedTree :: ScalarExpr 
              _exprIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ScalarExprRoot _exprIannotatedTree
                   {-# LINE 5573 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ScalarExprRoot _exprIoriginalTree
                   {-# LINE 5579 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5585 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5591 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5597 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ScalarExprStatementListPair ---------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExpr 
         child x2             : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ScalarExprStatementListPair  = ( ScalarExpr ,StatementList )
-- cata
sem_ScalarExprStatementListPair :: ScalarExprStatementListPair  ->
                                   T_ScalarExprStatementListPair 
sem_ScalarExprStatementListPair ( x1,x2)  =
    (sem_ScalarExprStatementListPair_Tuple (sem_ScalarExpr x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ScalarExprStatementListPair  = Catalog ->
                                      ( ScalarExprStatementListPair ,ScalarExprStatementListPair )
data Inh_ScalarExprStatementListPair  = Inh_ScalarExprStatementListPair {cat_Inh_ScalarExprStatementListPair :: Catalog}
data Syn_ScalarExprStatementListPair  = Syn_ScalarExprStatementListPair {annotatedTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair ,originalTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair }
wrap_ScalarExprStatementListPair :: T_ScalarExprStatementListPair  ->
                                    Inh_ScalarExprStatementListPair  ->
                                    Syn_ScalarExprStatementListPair 
wrap_ScalarExprStatementListPair sem (Inh_ScalarExprStatementListPair _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ScalarExprStatementListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprStatementListPair_Tuple :: T_ScalarExpr  ->
                                         T_StatementList  ->
                                         T_ScalarExprStatementListPair 
sem_ScalarExprStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprStatementListPair 
              _lhsOoriginalTree :: ScalarExprStatementListPair 
              _x1Ocat :: Catalog
              _x2Ocat :: Catalog
              _x1IannotatedTree :: ScalarExpr 
              _x1IoriginalTree :: ScalarExpr 
              _x2IannotatedTree :: StatementList 
              _x2IoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IannotatedTree,_x2IannotatedTree)
                   {-# LINE 5652 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IoriginalTree,_x2IoriginalTree)
                   {-# LINE 5658 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5664 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5670 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5676 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5682 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _x1IannotatedTree,_x1IoriginalTree) =
                  x1_ _x1Ocat 
              ( _x2IannotatedTree,_x2IoriginalTree) =
                  x2_ _x2Ocat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ScalarExprStatementListPairList -----------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExprStatementListPair 
         child tl             : ScalarExprStatementListPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ScalarExprStatementListPairList  = [ScalarExprStatementListPair ]
-- cata
sem_ScalarExprStatementListPairList :: ScalarExprStatementListPairList  ->
                                       T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList list  =
    (Prelude.foldr sem_ScalarExprStatementListPairList_Cons sem_ScalarExprStatementListPairList_Nil (Prelude.map sem_ScalarExprStatementListPair list) )
-- semantic domain
type T_ScalarExprStatementListPairList  = Catalog ->
                                          ( ScalarExprStatementListPairList ,ScalarExprStatementListPairList )
data Inh_ScalarExprStatementListPairList  = Inh_ScalarExprStatementListPairList {cat_Inh_ScalarExprStatementListPairList :: Catalog}
data Syn_ScalarExprStatementListPairList  = Syn_ScalarExprStatementListPairList {annotatedTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList ,originalTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList }
wrap_ScalarExprStatementListPairList :: T_ScalarExprStatementListPairList  ->
                                        Inh_ScalarExprStatementListPairList  ->
                                        Syn_ScalarExprStatementListPairList 
wrap_ScalarExprStatementListPairList sem (Inh_ScalarExprStatementListPairList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_ScalarExprStatementListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprStatementListPairList_Cons :: T_ScalarExprStatementListPair  ->
                                            T_ScalarExprStatementListPairList  ->
                                            T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprStatementListPairList 
              _lhsOoriginalTree :: ScalarExprStatementListPairList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: ScalarExprStatementListPair 
              _hdIoriginalTree :: ScalarExprStatementListPair 
              _tlIannotatedTree :: ScalarExprStatementListPairList 
              _tlIoriginalTree :: ScalarExprStatementListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 5743 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 5749 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5755 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5761 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5767 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5773 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExprStatementListPairList_Nil :: T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: ScalarExprStatementListPairList 
              _lhsOoriginalTree :: ScalarExprStatementListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5789 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5795 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5801 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5807 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- SelectItem --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative SelExp:
         child ann            : {Annotation}
         child ex             : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SelectItem:
         child ann            : {Annotation}
         child ex             : ScalarExpr 
         child name           : {NameComponent}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data SelectItem  = SelExp (Annotation) (ScalarExpr ) 
                 | SelectItem (Annotation) (ScalarExpr ) (NameComponent) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectItem :: SelectItem  ->
                  T_SelectItem 
sem_SelectItem (SelExp _ann _ex )  =
    (sem_SelectItem_SelExp _ann (sem_ScalarExpr _ex ) )
sem_SelectItem (SelectItem _ann _ex _name )  =
    (sem_SelectItem_SelectItem _ann (sem_ScalarExpr _ex ) _name )
-- semantic domain
type T_SelectItem  = Catalog ->
                     ( SelectItem ,SelectItem )
data Inh_SelectItem  = Inh_SelectItem {cat_Inh_SelectItem :: Catalog}
data Syn_SelectItem  = Syn_SelectItem {annotatedTree_Syn_SelectItem :: SelectItem ,originalTree_Syn_SelectItem :: SelectItem }
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_SelectItem _lhsOannotatedTree _lhsOoriginalTree ))
sem_SelectItem_SelExp :: Annotation ->
                         T_ScalarExpr  ->
                         T_SelectItem 
sem_SelectItem_SelExp ann_ ex_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: SelectItem 
              _lhsOoriginalTree :: SelectItem 
              _exOcat :: Catalog
              _exIannotatedTree :: ScalarExpr 
              _exIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelExp ann_ _exIannotatedTree
                   {-# LINE 5868 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelExp ann_ _exIoriginalTree
                   {-# LINE 5874 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5880 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5886 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5892 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exIannotatedTree,_exIoriginalTree) =
                  ex_ _exOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_SelectItem_SelectItem :: Annotation ->
                             T_ScalarExpr  ->
                             NameComponent ->
                             T_SelectItem 
sem_SelectItem_SelectItem ann_ ex_ name_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: SelectItem 
              _lhsOoriginalTree :: SelectItem 
              _exOcat :: Catalog
              _exIannotatedTree :: ScalarExpr 
              _exIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelectItem ann_ _exIannotatedTree name_
                   {-# LINE 5912 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelectItem ann_ _exIoriginalTree name_
                   {-# LINE 5918 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5924 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5930 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5936 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exIannotatedTree,_exIoriginalTree) =
                  ex_ _exOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- SelectItemList ----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : SelectItem 
         child tl             : SelectItemList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type SelectItemList  = [SelectItem ]
-- cata
sem_SelectItemList :: SelectItemList  ->
                      T_SelectItemList 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList  = Catalog ->
                         ( SelectItemList ,SelectItemList )
data Inh_SelectItemList  = Inh_SelectItemList {cat_Inh_SelectItemList :: Catalog}
data Syn_SelectItemList  = Syn_SelectItemList {annotatedTree_Syn_SelectItemList :: SelectItemList ,originalTree_Syn_SelectItemList :: SelectItemList }
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_SelectItemList _lhsOannotatedTree _lhsOoriginalTree ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: SelectItemList 
              _lhsOoriginalTree :: SelectItemList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: SelectItem 
              _hdIoriginalTree :: SelectItem 
              _tlIannotatedTree :: SelectItemList 
              _tlIoriginalTree :: SelectItemList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 5995 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 6001 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6007 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6013 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6019 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6025 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: SelectItemList 
              _lhsOoriginalTree :: SelectItemList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 6041 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 6047 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6053 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6059 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- SelectList --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative SelectList:
         child ann            : {Annotation}
         child items          : SelectItemList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data SelectList  = SelectList (Annotation) (SelectItemList ) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _ann _items )  =
    (sem_SelectList_SelectList _ann (sem_SelectItemList _items ) )
-- semantic domain
type T_SelectList  = Catalog ->
                     ( SelectList ,SelectList )
data Inh_SelectList  = Inh_SelectList {cat_Inh_SelectList :: Catalog}
data Syn_SelectList  = Syn_SelectList {annotatedTree_Syn_SelectList :: SelectList ,originalTree_Syn_SelectList :: SelectList }
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_SelectList _lhsOannotatedTree _lhsOoriginalTree ))
sem_SelectList_SelectList :: Annotation ->
                             T_SelectItemList  ->
                             T_SelectList 
sem_SelectList_SelectList ann_ items_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: SelectList 
              _lhsOoriginalTree :: SelectList 
              _itemsOcat :: Catalog
              _itemsIannotatedTree :: SelectItemList 
              _itemsIoriginalTree :: SelectItemList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelectList ann_ _itemsIannotatedTree
                   {-# LINE 6110 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelectList ann_ _itemsIoriginalTree
                   {-# LINE 6116 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6122 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6128 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _itemsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6134 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _itemsIannotatedTree,_itemsIoriginalTree) =
                  items_ _itemsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- SetClause ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative MultiSetClause:
         child ann            : {Annotation}
         child setTargets     : {[NameComponent]}
         child ex             : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SetClause:
         child ann            : {Annotation}
         child setTarget      : {NameComponent}
         child ex             : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data SetClause  = MultiSetClause (Annotation) (([NameComponent])) (ScalarExpr ) 
                | SetClause (Annotation) (NameComponent) (ScalarExpr ) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SetClause :: SetClause  ->
                 T_SetClause 
sem_SetClause (MultiSetClause _ann _setTargets _ex )  =
    (sem_SetClause_MultiSetClause _ann _setTargets (sem_ScalarExpr _ex ) )
sem_SetClause (SetClause _ann _setTarget _ex )  =
    (sem_SetClause_SetClause _ann _setTarget (sem_ScalarExpr _ex ) )
-- semantic domain
type T_SetClause  = Catalog ->
                    ( SetClause ,SetClause )
data Inh_SetClause  = Inh_SetClause {cat_Inh_SetClause :: Catalog}
data Syn_SetClause  = Syn_SetClause {annotatedTree_Syn_SetClause :: SetClause ,originalTree_Syn_SetClause :: SetClause }
wrap_SetClause :: T_SetClause  ->
                  Inh_SetClause  ->
                  Syn_SetClause 
wrap_SetClause sem (Inh_SetClause _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_SetClause _lhsOannotatedTree _lhsOoriginalTree ))
sem_SetClause_MultiSetClause :: Annotation ->
                                ([NameComponent]) ->
                                T_ScalarExpr  ->
                                T_SetClause 
sem_SetClause_MultiSetClause ann_ setTargets_ ex_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: SetClause 
              _lhsOoriginalTree :: SetClause 
              _exOcat :: Catalog
              _exIannotatedTree :: ScalarExpr 
              _exIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   MultiSetClause ann_ setTargets_ _exIannotatedTree
                   {-# LINE 6199 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   MultiSetClause ann_ setTargets_ _exIoriginalTree
                   {-# LINE 6205 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6211 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6217 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6223 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exIannotatedTree,_exIoriginalTree) =
                  ex_ _exOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_SetClause_SetClause :: Annotation ->
                           NameComponent ->
                           T_ScalarExpr  ->
                           T_SetClause 
sem_SetClause_SetClause ann_ setTarget_ ex_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: SetClause 
              _lhsOoriginalTree :: SetClause 
              _exOcat :: Catalog
              _exIannotatedTree :: ScalarExpr 
              _exIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SetClause ann_ setTarget_ _exIannotatedTree
                   {-# LINE 6243 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SetClause ann_ setTarget_ _exIoriginalTree
                   {-# LINE 6249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6261 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exIannotatedTree,_exIoriginalTree) =
                  ex_ _exOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- SetClauseList -----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
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
type SetClauseList  = [SetClause ]
-- cata
sem_SetClauseList :: SetClauseList  ->
                     T_SetClauseList 
sem_SetClauseList list  =
    (Prelude.foldr sem_SetClauseList_Cons sem_SetClauseList_Nil (Prelude.map sem_SetClause list) )
-- semantic domain
type T_SetClauseList  = Catalog ->
                        ( SetClauseList ,SetClauseList )
data Inh_SetClauseList  = Inh_SetClauseList {cat_Inh_SetClauseList :: Catalog}
data Syn_SetClauseList  = Syn_SetClauseList {annotatedTree_Syn_SetClauseList :: SetClauseList ,originalTree_Syn_SetClauseList :: SetClauseList }
wrap_SetClauseList :: T_SetClauseList  ->
                      Inh_SetClauseList  ->
                      Syn_SetClauseList 
wrap_SetClauseList sem (Inh_SetClauseList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_SetClauseList _lhsOannotatedTree _lhsOoriginalTree ))
sem_SetClauseList_Cons :: T_SetClause  ->
                          T_SetClauseList  ->
                          T_SetClauseList 
sem_SetClauseList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: SetClauseList 
              _lhsOoriginalTree :: SetClauseList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: SetClause 
              _hdIoriginalTree :: SetClause 
              _tlIannotatedTree :: SetClauseList 
              _tlIoriginalTree :: SetClauseList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 6326 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 6332 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6344 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: SetClauseList 
              _lhsOoriginalTree :: SetClauseList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 6372 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 6378 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6384 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6390 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- Statement ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative AlterSequence:
         child ann            : {Annotation}
         child name           : Name 
         child ownedBy        : Name 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative AlterTable:
         child ann            : {Annotation}
         child name           : Name 
         child actions        : AlterTableActionList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative AntiStatement:
         child string         : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Assignment:
         child ann            : {Annotation}
         child target         : Name 
         child value          : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Block:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child vars           : VarDefList 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CaseStatement:
         child ann            : {Annotation}
         child cases          : ScalarExprListStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CaseStatementSimple:
         child ann            : {Annotation}
         child val            : ScalarExpr 
         child cases          : ScalarExprListStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ContinueStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Copy:
         child ann            : {Annotation}
         child table          : Name 
         child targetCols     : {[NameComponent]}
         child source         : {CopySource}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CopyData:
         child ann            : {Annotation}
         child insData        : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateDomain:
         child ann            : {Annotation}
         child name           : Name 
         child typ            : TypeName 
         child constraintName : {String}
         child check          : MaybeBoolExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateFunction:
         child ann            : {Annotation}
         child name           : Name 
         child params         : ParamDefList 
         child rettype        : TypeName 
         child rep            : {Replace}
         child lang           : {Language}
         child body           : FnBody 
         child vol            : {Volatility}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateLanguage:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateSequence:
         child ann            : {Annotation}
         child name           : Name 
         child incr           : {Integer}
         child min            : {Integer}
         child max            : {Integer}
         child start          : {Integer}
         child cache          : {Integer}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateTable:
         child ann            : {Annotation}
         child name           : Name 
         child atts           : AttributeDefList 
         child cons           : ConstraintList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateTableAs:
         child ann            : {Annotation}
         child name           : Name 
         child expr           : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateTrigger:
         child ann            : {Annotation}
         child name           : {NameComponent}
         child wh             : {TriggerWhen}
         child events         : {[TriggerEvent]}
         child tbl            : Name 
         child firing         : {TriggerFire}
         child fnName         : Name 
         child fnArgs         : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateType:
         child ann            : {Annotation}
         child name           : Name 
         child atts           : TypeAttributeDefList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateView:
         child ann            : {Annotation}
         child name           : Name 
         child colNames       : {MaybeNameComponentList}
         child expr           : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Delete:
         child ann            : {Annotation}
         child table          : Name 
         child using          : TableRefList 
         child whr            : MaybeBoolExpr 
         child returning      : MaybeSelectList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative DropFunction:
         child ann            : {Annotation}
         child ifE            : {IfExists}
         child sigs           : NameTypeNameListPairList 
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative DropSomething:
         child ann            : {Annotation}
         child dropType       : {DropType}
         child ifE            : {IfExists}
         child names          : {[Name]}
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Execute:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ExitStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ForIntegerStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child var            : {NameComponent}
         child from           : ScalarExpr 
         child to             : ScalarExpr 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ForQueryStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child var            : {NameComponent}
         child sel            : QueryExpr 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative If:
         child ann            : {Annotation}
         child cases          : ScalarExprStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Insert:
         child ann            : {Annotation}
         child table          : Name 
         child targetCols     : {[NameComponent]}
         child insData        : QueryExpr 
         child returning      : MaybeSelectList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Into:
         child ann            : {Annotation}
         child strict         : {Bool}
         child into           : {[Name]}
         child stmt           : Statement 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative LoopStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Notify:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative NullStatement:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Perform:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative QueryStatement:
         child ann            : {Annotation}
         child ex             : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Raise:
         child ann            : {Annotation}
         child level          : {RaiseType}
         child message        : {String}
         child args           : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Return:
         child ann            : {Annotation}
         child value          : MaybeScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ReturnNext:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ReturnQuery:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Set:
         child ann            : {Annotation}
         child name           : {String}
         child values         : {[SetValue]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Truncate:
         child ann            : {Annotation}
         child tables         : {[Name]}
         child restartIdentity : {RestartIdentity}
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Update:
         child ann            : {Annotation}
         child table          : Name 
         child assigns        : SetClauseList 
         child fromList       : TableRefList 
         child whr            : MaybeBoolExpr 
         child returning      : MaybeSelectList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative WhileStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child expr           : ScalarExpr 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data Statement  = AlterSequence (Annotation) (Name ) (Name ) 
                | AlterTable (Annotation) (Name ) (AlterTableActionList ) 
                | AntiStatement (String) 
                | Assignment (Annotation) (Name ) (ScalarExpr ) 
                | Block (Annotation) ((Maybe String)) (VarDefList ) (StatementList ) 
                | CaseStatement (Annotation) (ScalarExprListStatementListPairList ) (StatementList ) 
                | CaseStatementSimple (Annotation) (ScalarExpr ) (ScalarExprListStatementListPairList ) (StatementList ) 
                | ContinueStatement (Annotation) ((Maybe String)) 
                | Copy (Annotation) (Name ) (([NameComponent])) (CopySource) 
                | CopyData (Annotation) (String) 
                | CreateDomain (Annotation) (Name ) (TypeName ) (String) (MaybeBoolExpr ) 
                | CreateFunction (Annotation) (Name ) (ParamDefList ) (TypeName ) (Replace) (Language) (FnBody ) (Volatility) 
                | CreateLanguage (Annotation) (String) 
                | CreateSequence (Annotation) (Name ) (Integer) (Integer) (Integer) (Integer) (Integer) 
                | CreateTable (Annotation) (Name ) (AttributeDefList ) (ConstraintList ) 
                | CreateTableAs (Annotation) (Name ) (QueryExpr ) 
                | CreateTrigger (Annotation) (NameComponent) (TriggerWhen) (([TriggerEvent])) (Name ) (TriggerFire) (Name ) (ScalarExprList ) 
                | CreateType (Annotation) (Name ) (TypeAttributeDefList ) 
                | CreateView (Annotation) (Name ) (MaybeNameComponentList) (QueryExpr ) 
                | Delete (Annotation) (Name ) (TableRefList ) (MaybeBoolExpr ) (MaybeSelectList ) 
                | DropFunction (Annotation) (IfExists) (NameTypeNameListPairList ) (Cascade) 
                | DropSomething (Annotation) (DropType) (IfExists) (([Name])) (Cascade) 
                | Execute (Annotation) (ScalarExpr ) 
                | ExitStatement (Annotation) ((Maybe String)) 
                | ForIntegerStatement (Annotation) ((Maybe String)) (NameComponent) (ScalarExpr ) (ScalarExpr ) (StatementList ) 
                | ForQueryStatement (Annotation) ((Maybe String)) (NameComponent) (QueryExpr ) (StatementList ) 
                | If (Annotation) (ScalarExprStatementListPairList ) (StatementList ) 
                | Insert (Annotation) (Name ) (([NameComponent])) (QueryExpr ) (MaybeSelectList ) 
                | Into (Annotation) (Bool) (([Name])) (Statement ) 
                | LoopStatement (Annotation) ((Maybe String)) (StatementList ) 
                | Notify (Annotation) (String) 
                | NullStatement (Annotation) 
                | Perform (Annotation) (ScalarExpr ) 
                | QueryStatement (Annotation) (QueryExpr ) 
                | Raise (Annotation) (RaiseType) (String) (ScalarExprList ) 
                | Return (Annotation) (MaybeScalarExpr ) 
                | ReturnNext (Annotation) (ScalarExpr ) 
                | ReturnQuery (Annotation) (QueryExpr ) 
                | Set (Annotation) (String) (([SetValue])) 
                | Truncate (Annotation) (([Name])) (RestartIdentity) (Cascade) 
                | Update (Annotation) (Name ) (SetClauseList ) (TableRefList ) (MaybeBoolExpr ) (MaybeSelectList ) 
                | WhileStatement (Annotation) ((Maybe String)) (ScalarExpr ) (StatementList ) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (AlterSequence _ann _name _ownedBy )  =
    (sem_Statement_AlterSequence _ann (sem_Name _name ) (sem_Name _ownedBy ) )
sem_Statement (AlterTable _ann _name _actions )  =
    (sem_Statement_AlterTable _ann (sem_Name _name ) (sem_AlterTableActionList _actions ) )
sem_Statement (AntiStatement _string )  =
    (sem_Statement_AntiStatement _string )
sem_Statement (Assignment _ann _target _value )  =
    (sem_Statement_Assignment _ann (sem_Name _target ) (sem_ScalarExpr _value ) )
sem_Statement (Block _ann _lb _vars _sts )  =
    (sem_Statement_Block _ann _lb (sem_VarDefList _vars ) (sem_StatementList _sts ) )
sem_Statement (CaseStatement _ann _cases _els )  =
    (sem_Statement_CaseStatement _ann (sem_ScalarExprListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (CaseStatementSimple _ann _val _cases _els )  =
    (sem_Statement_CaseStatementSimple _ann (sem_ScalarExpr _val ) (sem_ScalarExprListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (ContinueStatement _ann _lb )  =
    (sem_Statement_ContinueStatement _ann _lb )
sem_Statement (Copy _ann _table _targetCols _source )  =
    (sem_Statement_Copy _ann (sem_Name _table ) _targetCols _source )
sem_Statement (CopyData _ann _insData )  =
    (sem_Statement_CopyData _ann _insData )
sem_Statement (CreateDomain _ann _name _typ _constraintName _check )  =
    (sem_Statement_CreateDomain _ann (sem_Name _name ) (sem_TypeName _typ ) _constraintName (sem_MaybeBoolExpr _check ) )
sem_Statement (CreateFunction _ann _name _params _rettype _rep _lang _body _vol )  =
    (sem_Statement_CreateFunction _ann (sem_Name _name ) (sem_ParamDefList _params ) (sem_TypeName _rettype ) _rep _lang (sem_FnBody _body ) _vol )
sem_Statement (CreateLanguage _ann _name )  =
    (sem_Statement_CreateLanguage _ann _name )
sem_Statement (CreateSequence _ann _name _incr _min _max _start _cache )  =
    (sem_Statement_CreateSequence _ann (sem_Name _name ) _incr _min _max _start _cache )
sem_Statement (CreateTable _ann _name _atts _cons )  =
    (sem_Statement_CreateTable _ann (sem_Name _name ) (sem_AttributeDefList _atts ) (sem_ConstraintList _cons ) )
sem_Statement (CreateTableAs _ann _name _expr )  =
    (sem_Statement_CreateTableAs _ann (sem_Name _name ) (sem_QueryExpr _expr ) )
sem_Statement (CreateTrigger _ann _name _wh _events _tbl _firing _fnName _fnArgs )  =
    (sem_Statement_CreateTrigger _ann _name _wh _events (sem_Name _tbl ) _firing (sem_Name _fnName ) (sem_ScalarExprList _fnArgs ) )
sem_Statement (CreateType _ann _name _atts )  =
    (sem_Statement_CreateType _ann (sem_Name _name ) (sem_TypeAttributeDefList _atts ) )
sem_Statement (CreateView _ann _name _colNames _expr )  =
    (sem_Statement_CreateView _ann (sem_Name _name ) _colNames (sem_QueryExpr _expr ) )
sem_Statement (Delete _ann _table _using _whr _returning )  =
    (sem_Statement_Delete _ann (sem_Name _table ) (sem_TableRefList _using ) (sem_MaybeBoolExpr _whr ) (sem_MaybeSelectList _returning ) )
sem_Statement (DropFunction _ann _ifE _sigs _cascade )  =
    (sem_Statement_DropFunction _ann _ifE (sem_NameTypeNameListPairList _sigs ) _cascade )
sem_Statement (DropSomething _ann _dropType _ifE _names _cascade )  =
    (sem_Statement_DropSomething _ann _dropType _ifE _names _cascade )
sem_Statement (Execute _ann _expr )  =
    (sem_Statement_Execute _ann (sem_ScalarExpr _expr ) )
sem_Statement (ExitStatement _ann _lb )  =
    (sem_Statement_ExitStatement _ann _lb )
sem_Statement (ForIntegerStatement _ann _lb _var _from _to _sts )  =
    (sem_Statement_ForIntegerStatement _ann _lb _var (sem_ScalarExpr _from ) (sem_ScalarExpr _to ) (sem_StatementList _sts ) )
sem_Statement (ForQueryStatement _ann _lb _var _sel _sts )  =
    (sem_Statement_ForQueryStatement _ann _lb _var (sem_QueryExpr _sel ) (sem_StatementList _sts ) )
sem_Statement (If _ann _cases _els )  =
    (sem_Statement_If _ann (sem_ScalarExprStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (Insert _ann _table _targetCols _insData _returning )  =
    (sem_Statement_Insert _ann (sem_Name _table ) _targetCols (sem_QueryExpr _insData ) (sem_MaybeSelectList _returning ) )
sem_Statement (Into _ann _strict _into _stmt )  =
    (sem_Statement_Into _ann _strict _into (sem_Statement _stmt ) )
sem_Statement (LoopStatement _ann _lb _sts )  =
    (sem_Statement_LoopStatement _ann _lb (sem_StatementList _sts ) )
sem_Statement (Notify _ann _name )  =
    (sem_Statement_Notify _ann _name )
sem_Statement (NullStatement _ann )  =
    (sem_Statement_NullStatement _ann )
sem_Statement (Perform _ann _expr )  =
    (sem_Statement_Perform _ann (sem_ScalarExpr _expr ) )
sem_Statement (QueryStatement _ann _ex )  =
    (sem_Statement_QueryStatement _ann (sem_QueryExpr _ex ) )
sem_Statement (Raise _ann _level _message _args )  =
    (sem_Statement_Raise _ann _level _message (sem_ScalarExprList _args ) )
sem_Statement (Return _ann _value )  =
    (sem_Statement_Return _ann (sem_MaybeScalarExpr _value ) )
sem_Statement (ReturnNext _ann _expr )  =
    (sem_Statement_ReturnNext _ann (sem_ScalarExpr _expr ) )
sem_Statement (ReturnQuery _ann _sel )  =
    (sem_Statement_ReturnQuery _ann (sem_QueryExpr _sel ) )
sem_Statement (Set _ann _name _values )  =
    (sem_Statement_Set _ann _name _values )
sem_Statement (Truncate _ann _tables _restartIdentity _cascade )  =
    (sem_Statement_Truncate _ann _tables _restartIdentity _cascade )
sem_Statement (Update _ann _table _assigns _fromList _whr _returning )  =
    (sem_Statement_Update _ann (sem_Name _table ) (sem_SetClauseList _assigns ) (sem_TableRefList _fromList ) (sem_MaybeBoolExpr _whr ) (sem_MaybeSelectList _returning ) )
sem_Statement (WhileStatement _ann _lb _expr _sts )  =
    (sem_Statement_WhileStatement _ann _lb (sem_ScalarExpr _expr ) (sem_StatementList _sts ) )
-- semantic domain
type T_Statement  = Catalog ->
                    ( Statement ,Statement )
data Inh_Statement  = Inh_Statement {cat_Inh_Statement :: Catalog}
data Syn_Statement  = Syn_Statement {annotatedTree_Syn_Statement :: Statement ,originalTree_Syn_Statement :: Statement }
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_Statement _lhsOannotatedTree _lhsOoriginalTree ))
sem_Statement_AlterSequence :: Annotation ->
                               T_Name  ->
                               T_Name  ->
                               T_Statement 
sem_Statement_AlterSequence ann_ name_ ownedBy_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _nameOcat :: Catalog
              _ownedByOcat :: Catalog
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _ownedByIannotatedTree :: Name 
              _ownedByIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterSequence ann_ _nameIannotatedTree _ownedByIannotatedTree
                   {-# LINE 6881 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterSequence ann_ _nameIoriginalTree _ownedByIoriginalTree
                   {-# LINE 6887 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6893 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6899 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6905 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _ownedByOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6911 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat 
              ( _ownedByIannotatedTree,_ownedByIoriginalTree) =
                  ownedBy_ _ownedByOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_AlterTable :: Annotation ->
                            T_Name  ->
                            T_AlterTableActionList  ->
                            T_Statement 
sem_Statement_AlterTable ann_ name_ actions_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _nameOcat :: Catalog
              _actionsOcat :: Catalog
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _actionsIannotatedTree :: AlterTableActionList 
              _actionsIoriginalTree :: AlterTableActionList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterTable ann_ _nameIannotatedTree _actionsIannotatedTree
                   {-# LINE 6936 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterTable ann_ _nameIoriginalTree _actionsIoriginalTree
                   {-# LINE 6942 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6948 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6954 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6960 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _actionsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6966 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat 
              ( _actionsIannotatedTree,_actionsIoriginalTree) =
                  actions_ _actionsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_AntiStatement :: String ->
                               T_Statement 
sem_Statement_AntiStatement string_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiStatement string_
                   {-# LINE 6983 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiStatement string_
                   {-# LINE 6989 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6995 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7001 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Assignment :: Annotation ->
                            T_Name  ->
                            T_ScalarExpr  ->
                            T_Statement 
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _targetOcat :: Catalog
              _valueOcat :: Catalog
              _targetIannotatedTree :: Name 
              _targetIoriginalTree :: Name 
              _valueIannotatedTree :: ScalarExpr 
              _valueIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Assignment ann_ _targetIannotatedTree _valueIannotatedTree
                   {-# LINE 7022 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Assignment ann_ _targetIoriginalTree _valueIoriginalTree
                   {-# LINE 7028 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7034 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7040 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _targetOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7046 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valueOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7052 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _targetIannotatedTree,_targetIoriginalTree) =
                  target_ _targetOcat 
              ( _valueIannotatedTree,_valueIoriginalTree) =
                  value_ _valueOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Block :: Annotation ->
                       (Maybe String) ->
                       T_VarDefList  ->
                       T_StatementList  ->
                       T_Statement 
sem_Statement_Block ann_ lb_ vars_ sts_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _varsOcat :: Catalog
              _stsOcat :: Catalog
              _varsIannotatedTree :: VarDefList 
              _varsIoriginalTree :: VarDefList 
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Block ann_ lb_ _varsIannotatedTree _stsIannotatedTree
                   {-# LINE 7078 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Block ann_ lb_ _varsIoriginalTree _stsIoriginalTree
                   {-# LINE 7084 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7090 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7096 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _varsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7102 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7108 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _varsIannotatedTree,_varsIoriginalTree) =
                  vars_ _varsOcat 
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CaseStatement :: Annotation ->
                               T_ScalarExprListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement ann_ cases_ els_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _casesOcat :: Catalog
              _elsOcat :: Catalog
              _casesIannotatedTree :: ScalarExprListStatementListPairList 
              _casesIoriginalTree :: ScalarExprListStatementListPairList 
              _elsIannotatedTree :: StatementList 
              _elsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseStatement ann_ _casesIannotatedTree _elsIannotatedTree
                   {-# LINE 7133 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseStatement ann_ _casesIoriginalTree _elsIoriginalTree
                   {-# LINE 7139 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7145 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7151 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7157 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7163 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  cases_ _casesOcat 
              ( _elsIannotatedTree,_elsIoriginalTree) =
                  els_ _elsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CaseStatementSimple :: Annotation ->
                                     T_ScalarExpr  ->
                                     T_ScalarExprListStatementListPairList  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_CaseStatementSimple ann_ val_ cases_ els_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _valOcat :: Catalog
              _casesOcat :: Catalog
              _elsOcat :: Catalog
              _valIannotatedTree :: ScalarExpr 
              _valIoriginalTree :: ScalarExpr 
              _casesIannotatedTree :: ScalarExprListStatementListPairList 
              _casesIoriginalTree :: ScalarExprListStatementListPairList 
              _elsIannotatedTree :: StatementList 
              _elsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseStatementSimple ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                   {-# LINE 7192 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseStatementSimple ann_ _valIoriginalTree _casesIoriginalTree _elsIoriginalTree
                   {-# LINE 7198 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7204 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7210 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7216 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7222 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7228 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _valIannotatedTree,_valIoriginalTree) =
                  val_ _valOcat 
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  cases_ _casesOcat 
              ( _elsIannotatedTree,_elsIoriginalTree) =
                  els_ _elsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ContinueStatement :: Annotation ->
                                   (Maybe String) ->
                                   T_Statement 
sem_Statement_ContinueStatement ann_ lb_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ContinueStatement ann_ lb_
                   {-# LINE 7248 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ContinueStatement ann_ lb_
                   {-# LINE 7254 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7260 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7266 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Copy :: Annotation ->
                      T_Name  ->
                      ([NameComponent]) ->
                      CopySource ->
                      T_Statement 
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _tableOcat :: Catalog
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Copy ann_ _tableIannotatedTree targetCols_ source_
                   {-# LINE 7285 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Copy ann_ _tableIoriginalTree targetCols_ source_
                   {-# LINE 7291 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7297 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7303 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7309 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CopyData :: Annotation ->
                          String ->
                          T_Statement 
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CopyData ann_ insData_
                   {-# LINE 7325 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CopyData ann_ insData_
                   {-# LINE 7331 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7337 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7343 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateDomain :: Annotation ->
                              T_Name  ->
                              T_TypeName  ->
                              String ->
                              T_MaybeBoolExpr  ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ constraintName_ check_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _nameOcat :: Catalog
              _typOcat :: Catalog
              _checkOcat :: Catalog
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _typIannotatedTree :: TypeName 
              _typIoriginalTree :: TypeName 
              _checkIannotatedTree :: MaybeBoolExpr 
              _checkIoriginalTree :: MaybeBoolExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateDomain ann_ _nameIannotatedTree _typIannotatedTree constraintName_ _checkIannotatedTree
                   {-# LINE 7369 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateDomain ann_ _nameIoriginalTree _typIoriginalTree constraintName_ _checkIoriginalTree
                   {-# LINE 7375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7381 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7387 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7393 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _checkOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7405 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat 
              ( _typIannotatedTree,_typIoriginalTree) =
                  typ_ _typOcat 
              ( _checkIannotatedTree,_checkIoriginalTree) =
                  check_ _checkOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateFunction :: Annotation ->
                                T_Name  ->
                                T_ParamDefList  ->
                                T_TypeName  ->
                                Replace ->
                                Language ->
                                T_FnBody  ->
                                Volatility ->
                                T_Statement 
sem_Statement_CreateFunction ann_ name_ params_ rettype_ rep_ lang_ body_ vol_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _nameOcat :: Catalog
              _paramsOcat :: Catalog
              _rettypeOcat :: Catalog
              _bodyOcat :: Catalog
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _paramsIannotatedTree :: ParamDefList 
              _paramsIoriginalTree :: ParamDefList 
              _rettypeIannotatedTree :: TypeName 
              _rettypeIoriginalTree :: TypeName 
              _bodyIannotatedTree :: FnBody 
              _bodyIoriginalTree :: FnBody 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateFunction ann_ _nameIannotatedTree _paramsIannotatedTree _rettypeIannotatedTree rep_ lang_ _bodyIannotatedTree vol_
                   {-# LINE 7443 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateFunction ann_ _nameIoriginalTree _paramsIoriginalTree _rettypeIoriginalTree rep_ lang_ _bodyIoriginalTree vol_
                   {-# LINE 7449 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7455 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7467 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _paramsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7473 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _rettypeOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7479 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _bodyOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7485 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat 
              ( _paramsIannotatedTree,_paramsIoriginalTree) =
                  params_ _paramsOcat 
              ( _rettypeIannotatedTree,_rettypeIoriginalTree) =
                  rettype_ _rettypeOcat 
              ( _bodyIannotatedTree,_bodyIoriginalTree) =
                  body_ _bodyOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateLanguage :: Annotation ->
                                String ->
                                T_Statement 
sem_Statement_CreateLanguage ann_ name_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateLanguage ann_ name_
                   {-# LINE 7507 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateLanguage ann_ name_
                   {-# LINE 7513 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7519 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7525 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateSequence :: Annotation ->
                                T_Name  ->
                                Integer ->
                                Integer ->
                                Integer ->
                                Integer ->
                                Integer ->
                                T_Statement 
sem_Statement_CreateSequence ann_ name_ incr_ min_ max_ start_ cache_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _nameOcat :: Catalog
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateSequence ann_ _nameIannotatedTree incr_ min_ max_ start_ cache_
                   {-# LINE 7547 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateSequence ann_ _nameIoriginalTree incr_ min_ max_ start_ cache_
                   {-# LINE 7553 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7559 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7565 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7571 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateTable :: Annotation ->
                             T_Name  ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _nameOcat :: Catalog
              _attsOcat :: Catalog
              _consOcat :: Catalog
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _attsIannotatedTree :: AttributeDefList 
              _attsIoriginalTree :: AttributeDefList 
              _consIannotatedTree :: ConstraintList 
              _consIoriginalTree :: ConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTable ann_ _nameIannotatedTree _attsIannotatedTree _consIannotatedTree
                   {-# LINE 7598 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTable ann_ _nameIoriginalTree _attsIoriginalTree _consIoriginalTree
                   {-# LINE 7604 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7610 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7616 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7622 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _attsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7628 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _consOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7634 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat 
              ( _attsIannotatedTree,_attsIoriginalTree) =
                  atts_ _attsOcat 
              ( _consIannotatedTree,_consIoriginalTree) =
                  cons_ _consOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateTableAs :: Annotation ->
                               T_Name  ->
                               T_QueryExpr  ->
                               T_Statement 
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _nameOcat :: Catalog
              _exprOcat :: Catalog
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _exprIannotatedTree :: QueryExpr 
              _exprIoriginalTree :: QueryExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTableAs ann_ _nameIannotatedTree _exprIannotatedTree
                   {-# LINE 7661 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTableAs ann_ _nameIoriginalTree _exprIoriginalTree
                   {-# LINE 7667 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7673 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7679 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7685 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7691 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat 
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateTrigger :: Annotation ->
                               NameComponent ->
                               TriggerWhen ->
                               ([TriggerEvent]) ->
                               T_Name  ->
                               TriggerFire ->
                               T_Name  ->
                               T_ScalarExprList  ->
                               T_Statement 
sem_Statement_CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ fnArgs_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _tblOcat :: Catalog
              _fnNameOcat :: Catalog
              _fnArgsOcat :: Catalog
              _tblIannotatedTree :: Name 
              _tblIoriginalTree :: Name 
              _fnNameIannotatedTree :: Name 
              _fnNameIoriginalTree :: Name 
              _fnArgsIannotatedTree :: ScalarExprList 
              _fnArgsIoriginalTree :: ScalarExprList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTrigger ann_ name_ wh_ events_ _tblIannotatedTree firing_ _fnNameIannotatedTree _fnArgsIannotatedTree
                   {-# LINE 7724 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTrigger ann_ name_ wh_ events_ _tblIoriginalTree firing_ _fnNameIoriginalTree _fnArgsIoriginalTree
                   {-# LINE 7730 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7736 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7742 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tblOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7748 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnNameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7754 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnArgsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7760 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _tblIannotatedTree,_tblIoriginalTree) =
                  tbl_ _tblOcat 
              ( _fnNameIannotatedTree,_fnNameIoriginalTree) =
                  fnName_ _fnNameOcat 
              ( _fnArgsIannotatedTree,_fnArgsIoriginalTree) =
                  fnArgs_ _fnArgsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateType :: Annotation ->
                            T_Name  ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _nameOcat :: Catalog
              _attsOcat :: Catalog
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _attsIannotatedTree :: TypeAttributeDefList 
              _attsIoriginalTree :: TypeAttributeDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateType ann_ _nameIannotatedTree _attsIannotatedTree
                   {-# LINE 7787 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateType ann_ _nameIoriginalTree _attsIoriginalTree
                   {-# LINE 7793 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7799 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7805 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7811 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _attsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7817 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat 
              ( _attsIannotatedTree,_attsIoriginalTree) =
                  atts_ _attsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateView :: Annotation ->
                            T_Name  ->
                            MaybeNameComponentList ->
                            T_QueryExpr  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ colNames_ expr_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _nameOcat :: Catalog
              _exprOcat :: Catalog
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _exprIannotatedTree :: QueryExpr 
              _exprIoriginalTree :: QueryExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateView ann_ _nameIannotatedTree colNames_ _exprIannotatedTree
                   {-# LINE 7843 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateView ann_ _nameIoriginalTree colNames_ _exprIoriginalTree
                   {-# LINE 7849 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7855 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7861 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7867 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7873 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat 
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Delete :: Annotation ->
                        T_Name  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Delete ann_ table_ using_ whr_ returning_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _tableOcat :: Catalog
              _usingOcat :: Catalog
              _whrOcat :: Catalog
              _returningOcat :: Catalog
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              _usingIannotatedTree :: TableRefList 
              _usingIoriginalTree :: TableRefList 
              _whrIannotatedTree :: MaybeBoolExpr 
              _whrIoriginalTree :: MaybeBoolExpr 
              _returningIannotatedTree :: MaybeSelectList 
              _returningIoriginalTree :: MaybeSelectList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Delete ann_ _tableIannotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
                   {-# LINE 7906 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Delete ann_ _tableIoriginalTree _usingIoriginalTree _whrIoriginalTree _returningIoriginalTree
                   {-# LINE 7912 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7918 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7924 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7930 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _usingOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7936 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _whrOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7942 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _returningOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7948 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat 
              ( _usingIannotatedTree,_usingIoriginalTree) =
                  using_ _usingOcat 
              ( _whrIannotatedTree,_whrIoriginalTree) =
                  whr_ _whrOcat 
              ( _returningIannotatedTree,_returningIoriginalTree) =
                  returning_ _returningOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_DropFunction :: Annotation ->
                              IfExists ->
                              T_NameTypeNameListPairList  ->
                              Cascade ->
                              T_Statement 
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _sigsOcat :: Catalog
              _sigsIannotatedTree :: NameTypeNameListPairList 
              _sigsIoriginalTree :: NameTypeNameListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   DropFunction ann_ ifE_ _sigsIannotatedTree cascade_
                   {-# LINE 7975 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   DropFunction ann_ ifE_ _sigsIoriginalTree cascade_
                   {-# LINE 7981 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7987 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7993 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sigsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7999 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _sigsIannotatedTree,_sigsIoriginalTree) =
                  sigs_ _sigsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_DropSomething :: Annotation ->
                               DropType ->
                               IfExists ->
                               ([Name]) ->
                               Cascade ->
                               T_Statement 
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   DropSomething ann_ dropType_ ifE_ names_ cascade_
                   {-# LINE 8018 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   DropSomething ann_ dropType_ ifE_ names_ cascade_
                   {-# LINE 8024 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8030 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8036 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Execute :: Annotation ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _exprOcat :: Catalog
              _exprIannotatedTree :: ScalarExpr 
              _exprIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Execute ann_ _exprIannotatedTree
                   {-# LINE 8053 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Execute ann_ _exprIoriginalTree
                   {-# LINE 8059 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8065 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8071 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8077 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ExitStatement :: Annotation ->
                               (Maybe String) ->
                               T_Statement 
sem_Statement_ExitStatement ann_ lb_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ExitStatement ann_ lb_
                   {-# LINE 8093 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ExitStatement ann_ lb_
                   {-# LINE 8099 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8105 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8111 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ForIntegerStatement :: Annotation ->
                                     (Maybe String) ->
                                     NameComponent ->
                                     T_ScalarExpr  ->
                                     T_ScalarExpr  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement ann_ lb_ var_ from_ to_ sts_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _fromOcat :: Catalog
              _toOcat :: Catalog
              _stsOcat :: Catalog
              _fromIannotatedTree :: ScalarExpr 
              _fromIoriginalTree :: ScalarExpr 
              _toIannotatedTree :: ScalarExpr 
              _toIoriginalTree :: ScalarExpr 
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ForIntegerStatement ann_ lb_ var_ _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                   {-# LINE 8138 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ForIntegerStatement ann_ lb_ var_ _fromIoriginalTree _toIoriginalTree _stsIoriginalTree
                   {-# LINE 8144 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8150 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8156 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fromOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8162 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _toOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8168 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8174 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _fromIannotatedTree,_fromIoriginalTree) =
                  from_ _fromOcat 
              ( _toIannotatedTree,_toIoriginalTree) =
                  to_ _toOcat 
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ForQueryStatement :: Annotation ->
                                   (Maybe String) ->
                                   NameComponent ->
                                   T_QueryExpr  ->
                                   T_StatementList  ->
                                   T_Statement 
sem_Statement_ForQueryStatement ann_ lb_ var_ sel_ sts_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _selOcat :: Catalog
              _stsOcat :: Catalog
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ForQueryStatement ann_ lb_ var_ _selIannotatedTree _stsIannotatedTree
                   {-# LINE 8203 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ForQueryStatement ann_ lb_ var_ _selIoriginalTree _stsIoriginalTree
                   {-# LINE 8209 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8215 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8221 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8227 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8233 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _selIannotatedTree,_selIoriginalTree) =
                  sel_ _selOcat 
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_If :: Annotation ->
                    T_ScalarExprStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _casesOcat :: Catalog
              _elsOcat :: Catalog
              _casesIannotatedTree :: ScalarExprStatementListPairList 
              _casesIoriginalTree :: ScalarExprStatementListPairList 
              _elsIannotatedTree :: StatementList 
              _elsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   If ann_ _casesIannotatedTree _elsIannotatedTree
                   {-# LINE 8258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   If ann_ _casesIoriginalTree _elsIoriginalTree
                   {-# LINE 8264 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8270 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8276 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8282 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8288 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  cases_ _casesOcat 
              ( _elsIannotatedTree,_elsIoriginalTree) =
                  els_ _elsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Insert :: Annotation ->
                        T_Name  ->
                        ([NameComponent]) ->
                        T_QueryExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _tableOcat :: Catalog
              _insDataOcat :: Catalog
              _returningOcat :: Catalog
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              _insDataIannotatedTree :: QueryExpr 
              _insDataIoriginalTree :: QueryExpr 
              _returningIannotatedTree :: MaybeSelectList 
              _returningIoriginalTree :: MaybeSelectList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Insert ann_ _tableIannotatedTree targetCols_ _insDataIannotatedTree _returningIannotatedTree
                   {-# LINE 8318 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Insert ann_ _tableIoriginalTree targetCols_ _insDataIoriginalTree _returningIoriginalTree
                   {-# LINE 8324 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8330 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8336 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8342 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _insDataOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8348 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _returningOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8354 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat 
              ( _insDataIannotatedTree,_insDataIoriginalTree) =
                  insData_ _insDataOcat 
              ( _returningIannotatedTree,_returningIoriginalTree) =
                  returning_ _returningOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Into :: Annotation ->
                      Bool ->
                      ([Name]) ->
                      T_Statement  ->
                      T_Statement 
sem_Statement_Into ann_ strict_ into_ stmt_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _stmtOcat :: Catalog
              _stmtIannotatedTree :: Statement 
              _stmtIoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Into ann_ strict_ into_ _stmtIannotatedTree
                   {-# LINE 8379 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Into ann_ strict_ into_ _stmtIoriginalTree
                   {-# LINE 8385 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8391 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8397 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stmtOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8403 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _stmtIannotatedTree,_stmtIoriginalTree) =
                  stmt_ _stmtOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_LoopStatement :: Annotation ->
                               (Maybe String) ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_LoopStatement ann_ lb_ sts_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _stsOcat :: Catalog
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   LoopStatement ann_ lb_ _stsIannotatedTree
                   {-# LINE 8423 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   LoopStatement ann_ lb_ _stsIoriginalTree
                   {-# LINE 8429 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8435 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8441 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8447 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Notify :: Annotation ->
                        String ->
                        T_Statement 
sem_Statement_Notify ann_ name_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Notify ann_ name_
                   {-# LINE 8463 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Notify ann_ name_
                   {-# LINE 8469 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8475 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8481 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_NullStatement :: Annotation ->
                               T_Statement 
sem_Statement_NullStatement ann_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullStatement ann_
                   {-# LINE 8494 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullStatement ann_
                   {-# LINE 8500 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8506 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8512 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Perform :: Annotation ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _exprOcat :: Catalog
              _exprIannotatedTree :: ScalarExpr 
              _exprIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Perform ann_ _exprIannotatedTree
                   {-# LINE 8529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Perform ann_ _exprIoriginalTree
                   {-# LINE 8535 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8541 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8547 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8553 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_QueryStatement :: Annotation ->
                                T_QueryExpr  ->
                                T_Statement 
sem_Statement_QueryStatement ann_ ex_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _exOcat :: Catalog
              _exIannotatedTree :: QueryExpr 
              _exIoriginalTree :: QueryExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QueryStatement ann_ _exIannotatedTree
                   {-# LINE 8572 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QueryStatement ann_ _exIoriginalTree
                   {-# LINE 8578 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8584 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8590 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8596 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exIannotatedTree,_exIoriginalTree) =
                  ex_ _exOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Raise :: Annotation ->
                       RaiseType ->
                       String ->
                       T_ScalarExprList  ->
                       T_Statement 
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _argsOcat :: Catalog
              _argsIannotatedTree :: ScalarExprList 
              _argsIoriginalTree :: ScalarExprList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Raise ann_ level_ message_ _argsIannotatedTree
                   {-# LINE 8617 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Raise ann_ level_ message_ _argsIoriginalTree
                   {-# LINE 8623 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8629 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8635 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8641 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _argsIannotatedTree,_argsIoriginalTree) =
                  args_ _argsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Return :: Annotation ->
                        T_MaybeScalarExpr  ->
                        T_Statement 
sem_Statement_Return ann_ value_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _valueOcat :: Catalog
              _valueIannotatedTree :: MaybeScalarExpr 
              _valueIoriginalTree :: MaybeScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Return ann_ _valueIannotatedTree
                   {-# LINE 8660 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Return ann_ _valueIoriginalTree
                   {-# LINE 8666 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8672 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8678 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valueOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8684 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _valueIannotatedTree,_valueIoriginalTree) =
                  value_ _valueOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ReturnNext :: Annotation ->
                            T_ScalarExpr  ->
                            T_Statement 
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _exprOcat :: Catalog
              _exprIannotatedTree :: ScalarExpr 
              _exprIoriginalTree :: ScalarExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReturnNext ann_ _exprIannotatedTree
                   {-# LINE 8703 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReturnNext ann_ _exprIoriginalTree
                   {-# LINE 8709 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8715 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8721 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8727 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ReturnQuery :: Annotation ->
                             T_QueryExpr  ->
                             T_Statement 
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _selOcat :: Catalog
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReturnQuery ann_ _selIannotatedTree
                   {-# LINE 8746 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReturnQuery ann_ _selIoriginalTree
                   {-# LINE 8752 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8758 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8764 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8770 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _selIannotatedTree,_selIoriginalTree) =
                  sel_ _selOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Set :: Annotation ->
                     String ->
                     ([SetValue]) ->
                     T_Statement 
sem_Statement_Set ann_ name_ values_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Set ann_ name_ values_
                   {-# LINE 8787 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Set ann_ name_ values_
                   {-# LINE 8793 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8799 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8805 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Truncate :: Annotation ->
                          ([Name]) ->
                          RestartIdentity ->
                          Cascade ->
                          T_Statement 
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Truncate ann_ tables_ restartIdentity_ cascade_
                   {-# LINE 8821 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Truncate ann_ tables_ restartIdentity_ cascade_
                   {-# LINE 8827 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8833 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8839 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Update :: Annotation ->
                        T_Name  ->
                        T_SetClauseList  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Update ann_ table_ assigns_ fromList_ whr_ returning_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _tableOcat :: Catalog
              _assignsOcat :: Catalog
              _fromListOcat :: Catalog
              _whrOcat :: Catalog
              _returningOcat :: Catalog
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              _assignsIannotatedTree :: SetClauseList 
              _assignsIoriginalTree :: SetClauseList 
              _fromListIannotatedTree :: TableRefList 
              _fromListIoriginalTree :: TableRefList 
              _whrIannotatedTree :: MaybeBoolExpr 
              _whrIoriginalTree :: MaybeBoolExpr 
              _returningIannotatedTree :: MaybeSelectList 
              _returningIoriginalTree :: MaybeSelectList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Update ann_ _tableIannotatedTree _assignsIannotatedTree _fromListIannotatedTree _whrIannotatedTree _returningIannotatedTree
                   {-# LINE 8872 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Update ann_ _tableIoriginalTree _assignsIoriginalTree _fromListIoriginalTree _whrIoriginalTree _returningIoriginalTree
                   {-# LINE 8878 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8884 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8890 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8896 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _assignsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8902 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fromListOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8908 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _whrOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8914 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _returningOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8920 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat 
              ( _assignsIannotatedTree,_assignsIoriginalTree) =
                  assigns_ _assignsOcat 
              ( _fromListIannotatedTree,_fromListIoriginalTree) =
                  fromList_ _fromListOcat 
              ( _whrIannotatedTree,_whrIoriginalTree) =
                  whr_ _whrOcat 
              ( _returningIannotatedTree,_returningIoriginalTree) =
                  returning_ _returningOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_WhileStatement :: Annotation ->
                                (Maybe String) ->
                                T_ScalarExpr  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement ann_ lb_ expr_ sts_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _exprOcat :: Catalog
              _stsOcat :: Catalog
              _exprIannotatedTree :: ScalarExpr 
              _exprIoriginalTree :: ScalarExpr 
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WhileStatement ann_ lb_ _exprIannotatedTree _stsIannotatedTree
                   {-# LINE 8952 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WhileStatement ann_ lb_ _exprIoriginalTree _stsIoriginalTree
                   {-# LINE 8958 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8964 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8970 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8976 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8982 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIoriginalTree) =
                  expr_ _exprOcat 
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- StatementList -----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : Statement 
         child tl             : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type StatementList  = [Statement ]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_Statement list) )
-- semantic domain
type T_StatementList  = Catalog ->
                        ( StatementList ,StatementList )
data Inh_StatementList  = Inh_StatementList {cat_Inh_StatementList :: Catalog}
data Syn_StatementList  = Syn_StatementList {annotatedTree_Syn_StatementList :: StatementList ,originalTree_Syn_StatementList :: StatementList }
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_StatementList _lhsOannotatedTree _lhsOoriginalTree ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: StatementList 
              _lhsOoriginalTree :: StatementList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: Statement 
              _hdIoriginalTree :: Statement 
              _tlIannotatedTree :: StatementList 
              _tlIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 9043 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 9049 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9055 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9061 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9067 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9073 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: StatementList 
              _lhsOoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9089 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9095 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9101 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9107 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- TableAlias --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative FullAlias:
         child ann            : {Annotation}
         child tb             : {NameComponent}
         child cols           : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative NoAlias:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative TableAlias:
         child ann            : {Annotation}
         child tb             : {NameComponent}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data TableAlias  = FullAlias (Annotation) (NameComponent) (([NameComponent])) 
                 | NoAlias (Annotation) 
                 | TableAlias (Annotation) (NameComponent) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TableAlias :: TableAlias  ->
                  T_TableAlias 
sem_TableAlias (FullAlias _ann _tb _cols )  =
    (sem_TableAlias_FullAlias _ann _tb _cols )
sem_TableAlias (NoAlias _ann )  =
    (sem_TableAlias_NoAlias _ann )
sem_TableAlias (TableAlias _ann _tb )  =
    (sem_TableAlias_TableAlias _ann _tb )
-- semantic domain
type T_TableAlias  = Catalog ->
                     ( TableAlias ,TableAlias )
data Inh_TableAlias  = Inh_TableAlias {cat_Inh_TableAlias :: Catalog}
data Syn_TableAlias  = Syn_TableAlias {annotatedTree_Syn_TableAlias :: TableAlias ,originalTree_Syn_TableAlias :: TableAlias }
wrap_TableAlias :: T_TableAlias  ->
                   Inh_TableAlias  ->
                   Syn_TableAlias 
wrap_TableAlias sem (Inh_TableAlias _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_TableAlias _lhsOannotatedTree _lhsOoriginalTree ))
sem_TableAlias_FullAlias :: Annotation ->
                            NameComponent ->
                            ([NameComponent]) ->
                            T_TableAlias 
sem_TableAlias_FullAlias ann_ tb_ cols_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TableAlias 
              _lhsOoriginalTree :: TableAlias 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   FullAlias ann_ tb_ cols_
                   {-# LINE 9174 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   FullAlias ann_ tb_ cols_
                   {-# LINE 9180 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9186 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9192 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TableAlias_NoAlias :: Annotation ->
                          T_TableAlias 
sem_TableAlias_NoAlias ann_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TableAlias 
              _lhsOoriginalTree :: TableAlias 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NoAlias ann_
                   {-# LINE 9205 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NoAlias ann_
                   {-# LINE 9211 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9217 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9223 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TableAlias_TableAlias :: Annotation ->
                             NameComponent ->
                             T_TableAlias 
sem_TableAlias_TableAlias ann_ tb_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TableAlias 
              _lhsOoriginalTree :: TableAlias 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TableAlias ann_ tb_
                   {-# LINE 9237 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TableAlias ann_ tb_
                   {-# LINE 9243 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- TableRef ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative FunTref:
         child ann            : {Annotation}
         child fn             : ScalarExpr 
         child alias          : TableAlias 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative JoinTref:
         child ann            : {Annotation}
         child tbl            : TableRef 
         child nat            : {Natural}
         child joinType       : {JoinType}
         child tbl1           : TableRef 
         child onExpr         : OnExpr 
         child alias          : TableAlias 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SubTref:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         child alias          : TableAlias 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Tref:
         child ann            : {Annotation}
         child tbl            : Name 
         child alias          : TableAlias 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data TableRef  = FunTref (Annotation) (ScalarExpr ) (TableAlias ) 
               | JoinTref (Annotation) (TableRef ) (Natural) (JoinType) (TableRef ) (OnExpr ) (TableAlias ) 
               | SubTref (Annotation) (QueryExpr ) (TableAlias ) 
               | Tref (Annotation) (Name ) (TableAlias ) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TableRef :: TableRef  ->
                T_TableRef 
sem_TableRef (FunTref _ann _fn _alias )  =
    (sem_TableRef_FunTref _ann (sem_ScalarExpr _fn ) (sem_TableAlias _alias ) )
sem_TableRef (JoinTref _ann _tbl _nat _joinType _tbl1 _onExpr _alias )  =
    (sem_TableRef_JoinTref _ann (sem_TableRef _tbl ) _nat _joinType (sem_TableRef _tbl1 ) (sem_OnExpr _onExpr ) (sem_TableAlias _alias ) )
sem_TableRef (SubTref _ann _sel _alias )  =
    (sem_TableRef_SubTref _ann (sem_QueryExpr _sel ) (sem_TableAlias _alias ) )
sem_TableRef (Tref _ann _tbl _alias )  =
    (sem_TableRef_Tref _ann (sem_Name _tbl ) (sem_TableAlias _alias ) )
-- semantic domain
type T_TableRef  = Catalog ->
                   ( TableRef ,TableRef )
data Inh_TableRef  = Inh_TableRef {cat_Inh_TableRef :: Catalog}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef ,originalTree_Syn_TableRef :: TableRef }
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_TableRef _lhsOannotatedTree _lhsOoriginalTree ))
sem_TableRef_FunTref :: Annotation ->
                        T_ScalarExpr  ->
                        T_TableAlias  ->
                        T_TableRef 
sem_TableRef_FunTref ann_ fn_ alias_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TableRef 
              _lhsOoriginalTree :: TableRef 
              _fnOcat :: Catalog
              _aliasOcat :: Catalog
              _fnIannotatedTree :: ScalarExpr 
              _fnIoriginalTree :: ScalarExpr 
              _aliasIannotatedTree :: TableAlias 
              _aliasIoriginalTree :: TableAlias 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   FunTref ann_ _fnIannotatedTree _aliasIannotatedTree
                   {-# LINE 9345 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   FunTref ann_ _fnIoriginalTree _aliasIoriginalTree
                   {-# LINE 9351 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9357 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9363 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9369 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _fnIannotatedTree,_fnIoriginalTree) =
                  fn_ _fnOcat 
              ( _aliasIannotatedTree,_aliasIoriginalTree) =
                  alias_ _aliasOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TableRef_JoinTref :: Annotation ->
                         T_TableRef  ->
                         Natural ->
                         JoinType ->
                         T_TableRef  ->
                         T_OnExpr  ->
                         T_TableAlias  ->
                         T_TableRef 
sem_TableRef_JoinTref ann_ tbl_ nat_ joinType_ tbl1_ onExpr_ alias_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TableRef 
              _lhsOoriginalTree :: TableRef 
              _tblOcat :: Catalog
              _tbl1Ocat :: Catalog
              _onExprOcat :: Catalog
              _aliasOcat :: Catalog
              _tblIannotatedTree :: TableRef 
              _tblIoriginalTree :: TableRef 
              _tbl1IannotatedTree :: TableRef 
              _tbl1IoriginalTree :: TableRef 
              _onExprIannotatedTree :: OnExpr 
              _onExprIoriginalTree :: OnExpr 
              _aliasIannotatedTree :: TableAlias 
              _aliasIoriginalTree :: TableAlias 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinTref ann_ _tblIannotatedTree nat_ joinType_ _tbl1IannotatedTree _onExprIannotatedTree _aliasIannotatedTree
                   {-# LINE 9410 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinTref ann_ _tblIoriginalTree nat_ joinType_ _tbl1IoriginalTree _onExprIoriginalTree _aliasIoriginalTree
                   {-# LINE 9416 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9428 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tblOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9434 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tbl1Ocat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9440 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _onExprOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9446 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9452 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _tblIannotatedTree,_tblIoriginalTree) =
                  tbl_ _tblOcat 
              ( _tbl1IannotatedTree,_tbl1IoriginalTree) =
                  tbl1_ _tbl1Ocat 
              ( _onExprIannotatedTree,_onExprIoriginalTree) =
                  onExpr_ _onExprOcat 
              ( _aliasIannotatedTree,_aliasIoriginalTree) =
                  alias_ _aliasOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TableRef_SubTref :: Annotation ->
                        T_QueryExpr  ->
                        T_TableAlias  ->
                        T_TableRef 
sem_TableRef_SubTref ann_ sel_ alias_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TableRef 
              _lhsOoriginalTree :: TableRef 
              _selOcat :: Catalog
              _aliasOcat :: Catalog
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              _aliasIannotatedTree :: TableAlias 
              _aliasIoriginalTree :: TableAlias 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SubTref ann_ _selIannotatedTree _aliasIannotatedTree
                   {-# LINE 9481 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SubTref ann_ _selIoriginalTree _aliasIoriginalTree
                   {-# LINE 9487 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9493 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9499 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9505 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9511 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _selIannotatedTree,_selIoriginalTree) =
                  sel_ _selOcat 
              ( _aliasIannotatedTree,_aliasIoriginalTree) =
                  alias_ _aliasOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TableRef_Tref :: Annotation ->
                     T_Name  ->
                     T_TableAlias  ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_ alias_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TableRef 
              _lhsOoriginalTree :: TableRef 
              _tblOcat :: Catalog
              _aliasOcat :: Catalog
              _tblIannotatedTree :: Name 
              _tblIoriginalTree :: Name 
              _aliasIannotatedTree :: TableAlias 
              _aliasIoriginalTree :: TableAlias 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Tref ann_ _tblIannotatedTree _aliasIannotatedTree
                   {-# LINE 9536 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Tref ann_ _tblIoriginalTree _aliasIoriginalTree
                   {-# LINE 9542 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9548 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9554 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tblOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9560 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9566 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _tblIannotatedTree,_tblIoriginalTree) =
                  tbl_ _tblOcat 
              ( _aliasIannotatedTree,_aliasIoriginalTree) =
                  alias_ _aliasOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- TableRefList ------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : TableRef 
         child tl             : TableRefList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type TableRefList  = [TableRef ]
-- cata
sem_TableRefList :: TableRefList  ->
                    T_TableRefList 
sem_TableRefList list  =
    (Prelude.foldr sem_TableRefList_Cons sem_TableRefList_Nil (Prelude.map sem_TableRef list) )
-- semantic domain
type T_TableRefList  = Catalog ->
                       ( TableRefList ,TableRefList )
data Inh_TableRefList  = Inh_TableRefList {cat_Inh_TableRefList :: Catalog}
data Syn_TableRefList  = Syn_TableRefList {annotatedTree_Syn_TableRefList :: TableRefList ,originalTree_Syn_TableRefList :: TableRefList }
wrap_TableRefList :: T_TableRefList  ->
                     Inh_TableRefList  ->
                     Syn_TableRefList 
wrap_TableRefList sem (Inh_TableRefList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_TableRefList _lhsOannotatedTree _lhsOoriginalTree ))
sem_TableRefList_Cons :: T_TableRef  ->
                         T_TableRefList  ->
                         T_TableRefList 
sem_TableRefList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TableRefList 
              _lhsOoriginalTree :: TableRefList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: TableRef 
              _hdIoriginalTree :: TableRef 
              _tlIannotatedTree :: TableRefList 
              _tlIoriginalTree :: TableRefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 9627 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 9633 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9639 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9645 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9651 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9657 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TableRefList_Nil :: T_TableRefList 
sem_TableRefList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TableRefList 
              _lhsOoriginalTree :: TableRefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9673 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9679 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9685 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9691 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- TypeAttributeDef --------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative TypeAttDef:
         child ann            : {Annotation}
         child name           : {NameComponent}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data TypeAttributeDef  = TypeAttDef (Annotation) (NameComponent) (TypeName ) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TypeAttributeDef :: TypeAttributeDef  ->
                        T_TypeAttributeDef 
sem_TypeAttributeDef (TypeAttDef _ann _name _typ )  =
    (sem_TypeAttributeDef_TypeAttDef _ann _name (sem_TypeName _typ ) )
-- semantic domain
type T_TypeAttributeDef  = Catalog ->
                           ( TypeAttributeDef ,TypeAttributeDef )
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {cat_Inh_TypeAttributeDef :: Catalog}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef ,originalTree_Syn_TypeAttributeDef :: TypeAttributeDef }
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_TypeAttributeDef _lhsOannotatedTree _lhsOoriginalTree ))
sem_TypeAttributeDef_TypeAttDef :: Annotation ->
                                   NameComponent ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef ann_ name_ typ_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TypeAttributeDef 
              _lhsOoriginalTree :: TypeAttributeDef 
              _typOcat :: Catalog
              _typIannotatedTree :: TypeName 
              _typIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TypeAttDef ann_ name_ _typIannotatedTree
                   {-# LINE 9744 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TypeAttDef ann_ name_ _typIoriginalTree
                   {-# LINE 9750 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9756 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9762 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9768 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _typIannotatedTree,_typIoriginalTree) =
                  typ_ _typOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- TypeAttributeDefList ----------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
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
type TypeAttributeDefList  = [TypeAttributeDef ]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = Catalog ->
                               ( TypeAttributeDefList ,TypeAttributeDefList )
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {cat_Inh_TypeAttributeDefList :: Catalog}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList ,originalTree_Syn_TypeAttributeDefList :: TypeAttributeDefList }
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_TypeAttributeDefList _lhsOannotatedTree _lhsOoriginalTree ))
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TypeAttributeDefList 
              _lhsOoriginalTree :: TypeAttributeDefList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: TypeAttributeDef 
              _hdIoriginalTree :: TypeAttributeDef 
              _tlIannotatedTree :: TypeAttributeDefList 
              _tlIoriginalTree :: TypeAttributeDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 9827 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 9833 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9839 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9845 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9851 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9857 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TypeAttributeDefList 
              _lhsOoriginalTree :: TypeAttributeDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9873 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9879 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9885 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9891 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- TypeName ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative ArrayTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Prec2TypeName:
         child ann            : {Annotation}
         child tn             : {String}
         child prec           : {Integer}
         child prec1          : {Integer}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative PrecTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         child prec           : {Integer}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SetOfTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SimpleTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data TypeName  = ArrayTypeName (Annotation) (TypeName ) 
               | Prec2TypeName (Annotation) (String) (Integer) (Integer) 
               | PrecTypeName (Annotation) (String) (Integer) 
               | SetOfTypeName (Annotation) (TypeName ) 
               | SimpleTypeName (Annotation) (String) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TypeName :: TypeName  ->
                T_TypeName 
sem_TypeName (ArrayTypeName _ann _typ )  =
    (sem_TypeName_ArrayTypeName _ann (sem_TypeName _typ ) )
sem_TypeName (Prec2TypeName _ann _tn _prec _prec1 )  =
    (sem_TypeName_Prec2TypeName _ann _tn _prec _prec1 )
sem_TypeName (PrecTypeName _ann _tn _prec )  =
    (sem_TypeName_PrecTypeName _ann _tn _prec )
sem_TypeName (SetOfTypeName _ann _typ )  =
    (sem_TypeName_SetOfTypeName _ann (sem_TypeName _typ ) )
sem_TypeName (SimpleTypeName _ann _tn )  =
    (sem_TypeName_SimpleTypeName _ann _tn )
-- semantic domain
type T_TypeName  = Catalog ->
                   ( TypeName ,TypeName )
data Inh_TypeName  = Inh_TypeName {cat_Inh_TypeName :: Catalog}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName ,originalTree_Syn_TypeName :: TypeName }
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_TypeName _lhsOannotatedTree _lhsOoriginalTree ))
sem_TypeName_ArrayTypeName :: Annotation ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName ann_ typ_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TypeName 
              _lhsOoriginalTree :: TypeName 
              _typOcat :: Catalog
              _typIannotatedTree :: TypeName 
              _typIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ArrayTypeName ann_ _typIannotatedTree
                   {-# LINE 9981 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ArrayTypeName ann_ _typIoriginalTree
                   {-# LINE 9987 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9993 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9999 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10005 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _typIannotatedTree,_typIoriginalTree) =
                  typ_ _typOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TypeName_Prec2TypeName :: Annotation ->
                              String ->
                              Integer ->
                              Integer ->
                              T_TypeName 
sem_TypeName_Prec2TypeName ann_ tn_ prec_ prec1_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TypeName 
              _lhsOoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Prec2TypeName ann_ tn_ prec_ prec1_
                   {-# LINE 10023 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Prec2TypeName ann_ tn_ prec_ prec1_
                   {-# LINE 10029 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10035 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10041 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TypeName_PrecTypeName :: Annotation ->
                             String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName ann_ tn_ prec_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TypeName 
              _lhsOoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PrecTypeName ann_ tn_ prec_
                   {-# LINE 10056 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PrecTypeName ann_ tn_ prec_
                   {-# LINE 10062 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10068 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10074 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TypeName_SetOfTypeName :: Annotation ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName ann_ typ_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TypeName 
              _lhsOoriginalTree :: TypeName 
              _typOcat :: Catalog
              _typIannotatedTree :: TypeName 
              _typIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SetOfTypeName ann_ _typIannotatedTree
                   {-# LINE 10091 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SetOfTypeName ann_ _typIoriginalTree
                   {-# LINE 10097 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10103 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10109 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10115 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _typIannotatedTree,_typIoriginalTree) =
                  typ_ _typOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TypeName_SimpleTypeName :: Annotation ->
                               String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName ann_ tn_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TypeName 
              _lhsOoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SimpleTypeName ann_ tn_
                   {-# LINE 10131 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SimpleTypeName ann_ tn_
                   {-# LINE 10137 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10143 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10149 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- TypeNameList ------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : TypeName 
         child tl             : TypeNameList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type TypeNameList  = [TypeName ]
-- cata
sem_TypeNameList :: TypeNameList  ->
                    T_TypeNameList 
sem_TypeNameList list  =
    (Prelude.foldr sem_TypeNameList_Cons sem_TypeNameList_Nil (Prelude.map sem_TypeName list) )
-- semantic domain
type T_TypeNameList  = Catalog ->
                       ( TypeNameList ,TypeNameList )
data Inh_TypeNameList  = Inh_TypeNameList {cat_Inh_TypeNameList :: Catalog}
data Syn_TypeNameList  = Syn_TypeNameList {annotatedTree_Syn_TypeNameList :: TypeNameList ,originalTree_Syn_TypeNameList :: TypeNameList }
wrap_TypeNameList :: T_TypeNameList  ->
                     Inh_TypeNameList  ->
                     Syn_TypeNameList 
wrap_TypeNameList sem (Inh_TypeNameList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_TypeNameList _lhsOannotatedTree _lhsOoriginalTree ))
sem_TypeNameList_Cons :: T_TypeName  ->
                         T_TypeNameList  ->
                         T_TypeNameList 
sem_TypeNameList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TypeNameList 
              _lhsOoriginalTree :: TypeNameList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: TypeName 
              _hdIoriginalTree :: TypeName 
              _tlIannotatedTree :: TypeNameList 
              _tlIoriginalTree :: TypeNameList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 10206 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 10212 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10218 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10224 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10230 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10236 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TypeNameList_Nil :: T_TypeNameList 
sem_TypeNameList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: TypeNameList 
              _lhsOoriginalTree :: TypeNameList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10252 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10264 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10270 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- VarDef ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative ParamAlias:
         child ann            : {Annotation}
         child name           : {NameComponent}
         child i              : {Integer}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative VarAlias:
         child ann            : {Annotation}
         child name           : {NameComponent}
         child aliased        : Name 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative VarDef:
         child ann            : {Annotation}
         child name           : {NameComponent}
         child typ            : TypeName 
         child value          : {Maybe ScalarExpr}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data VarDef  = ParamAlias (Annotation) (NameComponent) (Integer) 
             | VarAlias (Annotation) (NameComponent) (Name ) 
             | VarDef (Annotation) (NameComponent) (TypeName ) ((Maybe ScalarExpr)) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_VarDef :: VarDef  ->
              T_VarDef 
sem_VarDef (ParamAlias _ann _name _i )  =
    (sem_VarDef_ParamAlias _ann _name _i )
sem_VarDef (VarAlias _ann _name _aliased )  =
    (sem_VarDef_VarAlias _ann _name (sem_Name _aliased ) )
sem_VarDef (VarDef _ann _name _typ _value )  =
    (sem_VarDef_VarDef _ann _name (sem_TypeName _typ ) _value )
-- semantic domain
type T_VarDef  = Catalog ->
                 ( VarDef ,VarDef )
data Inh_VarDef  = Inh_VarDef {cat_Inh_VarDef :: Catalog}
data Syn_VarDef  = Syn_VarDef {annotatedTree_Syn_VarDef :: VarDef ,originalTree_Syn_VarDef :: VarDef }
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_VarDef _lhsOannotatedTree _lhsOoriginalTree ))
sem_VarDef_ParamAlias :: Annotation ->
                         NameComponent ->
                         Integer ->
                         T_VarDef 
sem_VarDef_ParamAlias ann_ name_ i_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: VarDef 
              _lhsOoriginalTree :: VarDef 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamAlias ann_ name_ i_
                   {-# LINE 10341 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamAlias ann_ name_ i_
                   {-# LINE 10347 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10353 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10359 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_VarDef_VarAlias :: Annotation ->
                       NameComponent ->
                       T_Name  ->
                       T_VarDef 
sem_VarDef_VarAlias ann_ name_ aliased_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: VarDef 
              _lhsOoriginalTree :: VarDef 
              _aliasedOcat :: Catalog
              _aliasedIannotatedTree :: Name 
              _aliasedIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   VarAlias ann_ name_ _aliasedIannotatedTree
                   {-# LINE 10377 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   VarAlias ann_ name_ _aliasedIoriginalTree
                   {-# LINE 10383 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10389 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10395 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasedOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10401 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _aliasedIannotatedTree,_aliasedIoriginalTree) =
                  aliased_ _aliasedOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_VarDef_VarDef :: Annotation ->
                     NameComponent ->
                     T_TypeName  ->
                     (Maybe ScalarExpr) ->
                     T_VarDef 
sem_VarDef_VarDef ann_ name_ typ_ value_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: VarDef 
              _lhsOoriginalTree :: VarDef 
              _typOcat :: Catalog
              _typIannotatedTree :: TypeName 
              _typIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   VarDef ann_ name_ _typIannotatedTree value_
                   {-# LINE 10422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   VarDef ann_ name_ _typIoriginalTree value_
                   {-# LINE 10428 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10434 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10440 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10446 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _typIannotatedTree,_typIoriginalTree) =
                  typ_ _typOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- VarDefList --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
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
type VarDefList  = [VarDef ]
-- cata
sem_VarDefList :: VarDefList  ->
                  T_VarDefList 
sem_VarDefList list  =
    (Prelude.foldr sem_VarDefList_Cons sem_VarDefList_Nil (Prelude.map sem_VarDef list) )
-- semantic domain
type T_VarDefList  = Catalog ->
                     ( VarDefList ,VarDefList )
data Inh_VarDefList  = Inh_VarDefList {cat_Inh_VarDefList :: Catalog}
data Syn_VarDefList  = Syn_VarDefList {annotatedTree_Syn_VarDefList :: VarDefList ,originalTree_Syn_VarDefList :: VarDefList }
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_VarDefList _lhsOannotatedTree _lhsOoriginalTree ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: VarDefList 
              _lhsOoriginalTree :: VarDefList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: VarDef 
              _hdIoriginalTree :: VarDef 
              _tlIannotatedTree :: VarDefList 
              _tlIoriginalTree :: VarDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 10505 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 10511 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10517 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10523 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10535 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: VarDefList 
              _lhsOoriginalTree :: VarDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10557 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10563 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10569 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- WithQuery ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative WithQuery:
         child ann            : {Annotation}
         child name           : {NameComponent}
         child colAliases     : {Maybe [NameComponent]}
         child ex             : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data WithQuery  = WithQuery (Annotation) (NameComponent) ((Maybe [NameComponent])) (QueryExpr ) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_WithQuery :: WithQuery  ->
                 T_WithQuery 
sem_WithQuery (WithQuery _ann _name _colAliases _ex )  =
    (sem_WithQuery_WithQuery _ann _name _colAliases (sem_QueryExpr _ex ) )
-- semantic domain
type T_WithQuery  = Catalog ->
                    ( WithQuery ,WithQuery )
data Inh_WithQuery  = Inh_WithQuery {cat_Inh_WithQuery :: Catalog}
data Syn_WithQuery  = Syn_WithQuery {annotatedTree_Syn_WithQuery :: WithQuery ,originalTree_Syn_WithQuery :: WithQuery }
wrap_WithQuery :: T_WithQuery  ->
                  Inh_WithQuery  ->
                  Syn_WithQuery 
wrap_WithQuery sem (Inh_WithQuery _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_WithQuery _lhsOannotatedTree _lhsOoriginalTree ))
sem_WithQuery_WithQuery :: Annotation ->
                           NameComponent ->
                           (Maybe [NameComponent]) ->
                           T_QueryExpr  ->
                           T_WithQuery 
sem_WithQuery_WithQuery ann_ name_ colAliases_ ex_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: WithQuery 
              _lhsOoriginalTree :: WithQuery 
              _exOcat :: Catalog
              _exIannotatedTree :: QueryExpr 
              _exIoriginalTree :: QueryExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WithQuery ann_ name_ colAliases_ _exIannotatedTree
                   {-# LINE 10624 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WithQuery ann_ name_ colAliases_ _exIoriginalTree
                   {-# LINE 10630 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10636 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10642 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10648 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exIannotatedTree,_exIoriginalTree) =
                  ex_ _exOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- WithQueryList -----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : WithQuery 
         child tl             : WithQueryList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type WithQueryList  = [WithQuery ]
-- cata
sem_WithQueryList :: WithQueryList  ->
                     T_WithQueryList 
sem_WithQueryList list  =
    (Prelude.foldr sem_WithQueryList_Cons sem_WithQueryList_Nil (Prelude.map sem_WithQuery list) )
-- semantic domain
type T_WithQueryList  = Catalog ->
                        ( WithQueryList ,WithQueryList )
data Inh_WithQueryList  = Inh_WithQueryList {cat_Inh_WithQueryList :: Catalog}
data Syn_WithQueryList  = Syn_WithQueryList {annotatedTree_Syn_WithQueryList :: WithQueryList ,originalTree_Syn_WithQueryList :: WithQueryList }
wrap_WithQueryList :: T_WithQueryList  ->
                      Inh_WithQueryList  ->
                      Syn_WithQueryList 
wrap_WithQueryList sem (Inh_WithQueryList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_WithQueryList _lhsOannotatedTree _lhsOoriginalTree ))
sem_WithQueryList_Cons :: T_WithQuery  ->
                          T_WithQueryList  ->
                          T_WithQueryList 
sem_WithQueryList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: WithQueryList 
              _lhsOoriginalTree :: WithQueryList 
              _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdIannotatedTree :: WithQuery 
              _hdIoriginalTree :: WithQuery 
              _tlIannotatedTree :: WithQueryList 
              _tlIoriginalTree :: WithQueryList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 10707 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 10713 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10719 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10725 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10731 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10737 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_WithQueryList_Nil :: T_WithQueryList 
sem_WithQueryList_Nil  =
    (\ _lhsIcat ->
         (let _lhsOannotatedTree :: WithQueryList 
              _lhsOoriginalTree :: WithQueryList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10753 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10759 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 67 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10765 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10771 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))