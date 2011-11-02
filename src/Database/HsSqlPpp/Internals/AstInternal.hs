

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
   ,nameComponents
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
   -- annotation
   ,Annotation(..)
   ,SourcePosition
   ,ParameterizedStatementType
   ,getAnnotation
   ,updateAnnotation
   ,emptyAnnotation
   ,atype
   ,setAtype
   ,errs
   ,setErrs
   ,setAsrc


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

import Data.Generics
import Data.Generics.Uniplate.Data
import Debug.Trace
--import Text.Groom


import Database.HsSqlPpp.Internals.TypesInternal

import Database.HsSqlPpp.Internals.TypeChecking.TypeConversion
import Database.HsSqlPpp.Internals.TypeChecking.Environment
import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
import Database.HsSqlPpp.Utils.Utils

{-# LINE 347 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

nameComponents :: Name -> [NameComponent]
nameComponents (Name _ is) = is
{-# LINE 137 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 406 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data JoinType = Inner | LeftOuter| RightOuter | FullOuter | Cross
                deriving (Show,Eq,Typeable,Data)
{-# LINE 143 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 418 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data CopySource = CopyFilename String
                | Stdin
                  deriving (Show,Eq,Typeable,Data)
{-# LINE 150 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 475 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

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
{-# LINE 167 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 504 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data RaiseType = RNotice | RException | RError
                 deriving (Show,Eq,Typeable,Data)

data CombineType = Except | Union | Intersect | UnionAll
                   deriving (Show,Eq,Typeable,Data)

data Volatility = Volatile | Stable | Immutable
                  deriving (Show,Eq,Typeable,Data)

data Language = Sql | Plpgsql
                deriving (Show,Eq,Typeable,Data)
{-# LINE 182 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 523 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

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

{-# LINE 213 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 639 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

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

{-# LINE 258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 687 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data FrameClause = FrameUnboundedPreceding
                 | FrameUnboundedFull
                 | FrameRowsUnboundedPreceding
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 266 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 14 "src/Database/HsSqlPpp/Internals/Annotation.ag" #-}

-- | Represents a source file position, usually set by the parser.
type SourcePosition = (String,Int,Int)

-- | Statement type is used for getting type information for a
-- parameterized statement. The first part is the args that the
-- parameterized statement needs, and the second is the names and types
-- of the output columns. No way to signal that a statement returns
-- exactly one row at the moment
type ParameterizedStatementType = ([Type],[(String,Type)])

{-# LINE 280 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 44 "src/Database/HsSqlPpp/Internals/Annotation.ag" #-}


--some simple wrappers around uniplate for internal use. I'm not sure
--which of these are actually used

-- | An annotation value with no information.
emptyAnnotation :: Annotation
emptyAnnotation = Annotation Nothing Nothing [] Nothing Nothing []

-- | get the annotation for the root element of the tree passed
getAnnotation :: Data a => a -> Annotation
getAnnotation = head . childrenBi

--   | get all the annotations from a tree
--getAnnotations :: Data a => a -> [Annotation]
--getAnnotations = universeBi -- st --[x | x <- universeBi st]

--   | update all the annotations in a tree
--updateAnnotations :: Data a => (Annotation -> Annotation) -> a -> a
--updateAnnotations = transformBi

atype :: Annotation -> Maybe Type
atype (Annotation _ a _ _ _ _) = a

setAtype :: Maybe Type -> Annotation -> Annotation
setAtype a (Annotation s _a e i st c) = Annotation s a e i st c

setAsrc :: Maybe SourcePosition -> Annotation -> Annotation
setAsrc s (Annotation _s a e i st c) = Annotation s a e i st c

errs :: Annotation -> [TypeError]
errs (Annotation _ _ e _ _ _) = e

setErrs :: [TypeError] -> Annotation -> Annotation
setErrs e (Annotation s a _e i st c) = Annotation s a e i st c


--getTypeAnnotation :: Data a => a -> Maybe Type
--getTypeAnnotation = atype . getAnnotation

--don't know how to do this one with uniplate

-- | Update the first annotation in a tree using the function supplied
updateAnnotation :: Data a => (Annotation -> Annotation) -> a -> a
updateAnnotation f = gmapT (mkT f)

{-# LINE 329 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 3 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}


-- | Typechecks the ast, and returns the updated catalog (which
-- includes changes from any ddl statements in the ast).
typeCheckStatements :: Catalog -> [Statement] -> (Catalog,[Statement])
typeCheckStatements cat sts =
    let t = sem_Root (Root sts)
        ta = wrap_Root t Inh_Root {cat_Inh_Root = cat
                                  {-,lib_Inh_Root = emptyBindings
                                  ,idenv_Inh_Root = emptyIDEnv "tcs"-}}
        tl = annotatedTree_Syn_Root ta
        cat1 = cat --producedCat_Syn_Root ta
    in case tl of
         Root r -> (cat1,r)
-- | Typecheck a query expr
typeCheckQueryExpr :: Catalog -> QueryExpr -> QueryExpr
typeCheckQueryExpr cat qe =
   let (_,[QueryStatement _ qe']) = typeCheckStatements cat [QueryStatement emptyAnnotation qe]
   in qe'

-- | Not working yet. Typechecks a statement possibly containing ?
-- placeholders. These are annotated with the 'inferred type', and the
-- stType annotation on the return value can be used to get this info
-- easily. Returns Left if the statement is not a query,insert,update or delete
-- statement
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


-- | type check a scalar expr
typeCheckScalarExpr :: Catalog -> ScalarExpr -> ScalarExpr
typeCheckScalarExpr cat ex =
    let t = sem_ScalarExprRoot (ScalarExprRoot ex)
        rt = (annotatedTree_Syn_ScalarExprRoot
              (wrap_ScalarExprRoot t Inh_ScalarExprRoot {cat_Inh_ScalarExprRoot = cat
                                                        {-,lib_Inh_ScalarExprRoot = emptyBindings
                                                        ,idenv_Inh_ScalarExprRoot = emptyIDEnv "tcse"-}}))
    in case rt of
         ScalarExprRoot e -> e

{-# LINE 383 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 40 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}


-- | thet name to choose for a column in a select list which doesn't
-- have an explicit name
columnName :: ScalarExpr -> String
columnName (Identifier _ i) = nm i
columnName (QIdentifier _ is) = nm $ last is
columnName (App _ f@(Name _ ncs) _) = nm $ last ncs
columnName (Cast _ _ (SimpleTypeName _ (Name _ ncs))) = nm $ last ncs
columnName (WindowApp _ (App _ f@(Name _ ncs) _) _ _ _) = nm $ last ncs
columnName (AggregateApp _ _ (App _ f@(Name _ ncs) _) _) = nm $ last ncs
columnName _ = "?column?"

nm :: NameComponent -> String
nm (Nmc n) = map toLower n
nm (QNmc n) = n
{-# LINE 402 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
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
         child ann            : Annotation 
         child con            : Constraint 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative AlterColumnDefault:
         child ann            : Annotation 
         child nm             : {NameComponent}
         child def            : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data AlterTableAction  = AddConstraint (Annotation ) (Constraint ) 
                       | AlterColumnDefault (Annotation ) (NameComponent) (ScalarExpr ) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_AlterTableAction :: AlterTableAction  ->
                        T_AlterTableAction 
sem_AlterTableAction (AddConstraint _ann _con )  =
    (sem_AlterTableAction_AddConstraint (sem_Annotation _ann ) (sem_Constraint _con ) )
sem_AlterTableAction (AlterColumnDefault _ann _nm _def )  =
    (sem_AlterTableAction_AlterColumnDefault (sem_Annotation _ann ) _nm (sem_ScalarExpr _def ) )
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
sem_AlterTableAction_AddConstraint :: T_Annotation  ->
                                      T_Constraint  ->
                                      T_AlterTableAction 
sem_AlterTableAction_AddConstraint ann_ con_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 454 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _conOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: AlterTableAction.AddConstraint.ann.tpe"
                  {-# LINE 459 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (con_ _conOcat ) of
            { ( _conIannotatedTree,_conIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 468 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   AddConstraint _annIannotatedTree _conIannotatedTree
                                   {-# LINE 475 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 480 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     AddConstraint _annIoriginalTree _conIoriginalTree
                                     {-# LINE 485 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 490 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_AlterTableAction_AlterColumnDefault :: T_Annotation  ->
                                           NameComponent ->
                                           T_ScalarExpr  ->
                                           T_AlterTableAction 
sem_AlterTableAction_AlterColumnDefault ann_ nm_ def_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: AlterTableAction.AlterColumnDefault.def.downEnv"
                 {-# LINE 502 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _defOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 507 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _defOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: AlterTableAction.AlterColumnDefault.ann.tpe"
                   {-# LINE 512 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (def_ _defOcat _defOdownEnv ) of
             { ( _defIannotatedTree,_defIoriginalTree,_defIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    AlterColumnDefault _annIannotatedTree nm_ _defIannotatedTree
                                    {-# LINE 528 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 533 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      AlterColumnDefault _annIoriginalTree nm_ _defIoriginalTree
                                      {-# LINE 538 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 543 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 591 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 596 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 605 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 610 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 615 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 620 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_AlterTableActionList_Nil :: T_AlterTableActionList 
sem_AlterTableActionList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 629 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 634 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 639 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 644 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- Annotation --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         cat                  : Catalog
         tpe                  : Either [TypeError] Type
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Annotation:
         child asrc           : {Maybe SourcePosition}
         child atype          : {Maybe Type}
         child errs           : {[TypeError]}
         child implicitCast   : {Maybe Type}
         child stType         : {Maybe ParameterizedStatementType}
         child catUpd         : {[CatalogUpdate]}
         visit 0:
            local originalTree : _
-}
data Annotation  = Annotation ((Maybe SourcePosition)) ((Maybe Type)) (([TypeError])) ((Maybe Type)) ((Maybe ParameterizedStatementType)) (([CatalogUpdate])) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Annotation :: Annotation  ->
                  T_Annotation 
sem_Annotation (Annotation _asrc _atype _errs _implicitCast _stType _catUpd )  =
    (sem_Annotation_Annotation _asrc _atype _errs _implicitCast _stType _catUpd )
-- semantic domain
type T_Annotation  = ( Annotation ,T_Annotation_1 )
type T_Annotation_1  = Catalog ->
                       (Either [TypeError] Type) ->
                       ( Annotation )
data Inh_Annotation  = Inh_Annotation {cat_Inh_Annotation :: Catalog,tpe_Inh_Annotation :: (Either [TypeError] Type)}
data Syn_Annotation  = Syn_Annotation {annotatedTree_Syn_Annotation :: Annotation ,originalTree_Syn_Annotation :: Annotation }
wrap_Annotation :: T_Annotation  ->
                   Inh_Annotation  ->
                   Syn_Annotation 
wrap_Annotation sem (Inh_Annotation _lhsIcat _lhsItpe )  =
    (let ( _lhsOoriginalTree,sem_1) = sem 
         ( _lhsOannotatedTree) = sem_1 _lhsIcat _lhsItpe 
     in  (Syn_Annotation _lhsOannotatedTree _lhsOoriginalTree ))
sem_Annotation_Annotation :: (Maybe SourcePosition) ->
                             (Maybe Type) ->
                             ([TypeError]) ->
                             (Maybe Type) ->
                             (Maybe ParameterizedStatementType) ->
                             ([CatalogUpdate]) ->
                             T_Annotation 
sem_Annotation_Annotation asrc_ atype_ errs_ implicitCast_ stType_ catUpd_  =
    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
            Annotation asrc_ atype_ errs_ implicitCast_ stType_ catUpd_
            {-# LINE 701 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
            )) of
     { _originalTree ->
     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 706 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
             )) of
      { _lhsOoriginalTree ->
      (case ((let sem_Annotation_Annotation_1 :: T_Annotation_1 
                  sem_Annotation_Annotation_1  =
                      (\ _lhsIcat
                         _lhsItpe ->
                           (case (({-# LINE 76 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   let t = either (const Nothing) Just _lhsItpe
                                       es = either id (const []) _lhsItpe
                                   in Annotation asrc_ t es implicitCast_ stType_ catUpd_
                                   {-# LINE 717 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOannotatedTree ->
                            ( _lhsOannotatedTree) }))
              in  sem_Annotation_Annotation_1)) of
       { ( sem_Annotation_1) ->
       ( _lhsOoriginalTree,sem_Annotation_1) }) }) })
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
         child ann            : Annotation 
         child name           : {NameComponent}
         child typ            : TypeName 
         child def            : MaybeScalarExpr 
         child cons           : RowConstraintList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data AttributeDef  = AttributeDef (Annotation ) (NameComponent) (TypeName ) (MaybeScalarExpr ) (RowConstraintList ) 
                   deriving ( Data,Eq,Show,Typeable)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _ann _name _typ _def _cons )  =
    (sem_AttributeDef_AttributeDef (sem_Annotation _ann ) _name (sem_TypeName _typ ) (sem_MaybeScalarExpr _def ) (sem_RowConstraintList _cons ) )
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
sem_AttributeDef_AttributeDef :: T_Annotation  ->
                                 NameComponent ->
                                 T_TypeName  ->
                                 T_MaybeScalarExpr  ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef ann_ name_ typ_ def_ cons_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 771 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _consOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 776 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _defOcat ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 781 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _typOcat ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: AttributeDef.AttributeDef.ann.tpe"
                    {-# LINE 786 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (cons_ _consOcat ) of
              { ( _consIannotatedTree,_consIoriginalTree) ->
                  (case (def_ _defOcat ) of
                   { ( _defIannotatedTree,_defIoriginalTree) ->
                       (case (typ_ _typOcat ) of
                        { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                            (case (ann_ ) of
                             { ( _annIoriginalTree,ann_1) ->
                                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIcat
                                         {-# LINE 799 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annOcat ->
                                  (case (ann_1 _annOcat _annOtpe ) of
                                   { ( _annIannotatedTree) ->
                                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               AttributeDef _annIannotatedTree name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
                                               {-# LINE 806 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annotatedTree ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _annotatedTree
                                                {-# LINE 811 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOannotatedTree ->
                                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 AttributeDef _annIoriginalTree name_ _typIoriginalTree _defIoriginalTree _consIoriginalTree
                                                 {-# LINE 816 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _originalTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _originalTree
                                                  {-# LINE 821 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOoriginalTree ->
                                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 869 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 874 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 883 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 888 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 893 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 898 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 907 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 912 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 917 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 922 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: CaseScalarExprListScalarExprPair.Tuple.x2.downEnv"
                 {-# LINE 966 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _x2OdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 971 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _x2Ocat ->
           (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: CaseScalarExprListScalarExprPair.Tuple.x1.downEnv"
                   {-# LINE 976 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _x1OdownEnv ->
            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 981 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _x1Ocat ->
             (case (x2_ _x2Ocat _x2OdownEnv ) of
              { ( _x2IannotatedTree,_x2IoriginalTree,_x2IupType) ->
                  (case (x1_ _x1Ocat _x1OdownEnv ) of
                   { ( _x1IannotatedTree,_x1IoriginalTree,_x1IupTypes) ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (_x1IannotatedTree,_x2IannotatedTree)
                               {-# LINE 990 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 995 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (_x1IoriginalTree,_x2IoriginalTree)
                                 {-# LINE 1000 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 1005 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 1053 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1058 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 1067 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 1072 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 1077 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 1082 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_CaseScalarExprListScalarExprPairList_Nil :: T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 1091 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1096 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1101 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 1106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         child ann            : Annotation 
         child name           : {String}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative PrimaryKeyConstraint:
         child ann            : Annotation 
         child name           : {String}
         child x              : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ReferenceConstraint:
         child ann            : Annotation 
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
         child ann            : Annotation 
         child name           : {String}
         child x              : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data Constraint  = CheckConstraint (Annotation ) (String) (ScalarExpr ) 
                 | PrimaryKeyConstraint (Annotation ) (String) (([NameComponent])) 
                 | ReferenceConstraint (Annotation ) (String) (([NameComponent])) (Name ) (([NameComponent])) (Cascade) (Cascade) 
                 | UniqueConstraint (Annotation ) (String) (([NameComponent])) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Constraint :: Constraint  ->
                  T_Constraint 
sem_Constraint (CheckConstraint _ann _name _expr )  =
    (sem_Constraint_CheckConstraint (sem_Annotation _ann ) _name (sem_ScalarExpr _expr ) )
sem_Constraint (PrimaryKeyConstraint _ann _name _x )  =
    (sem_Constraint_PrimaryKeyConstraint (sem_Annotation _ann ) _name _x )
sem_Constraint (ReferenceConstraint _ann _name _atts _table _tableAtts _onUpdate _onDelete )  =
    (sem_Constraint_ReferenceConstraint (sem_Annotation _ann ) _name _atts (sem_Name _table ) _tableAtts _onUpdate _onDelete )
sem_Constraint (UniqueConstraint _ann _name _x )  =
    (sem_Constraint_UniqueConstraint (sem_Annotation _ann ) _name _x )
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
sem_Constraint_CheckConstraint :: T_Annotation  ->
                                  String ->
                                  T_ScalarExpr  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: Constraint.CheckConstraint.expr.downEnv"
                 {-# LINE 1187 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1192 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Constraint.CheckConstraint.ann.tpe"
                   {-# LINE 1197 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (expr_ _exprOcat _exprOdownEnv ) of
             { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 1206 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    CheckConstraint _annIannotatedTree name_ _exprIannotatedTree
                                    {-# LINE 1213 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 1218 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      CheckConstraint _annIoriginalTree name_ _exprIoriginalTree
                                      {-# LINE 1223 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 1228 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_Constraint_PrimaryKeyConstraint :: T_Annotation  ->
                                       String ->
                                       ([NameComponent]) ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint ann_ name_ x_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Constraint.PrimaryKeyConstraint.ann.tpe"
                 {-# LINE 1240 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 1247 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             PrimaryKeyConstraint _annIannotatedTree name_ x_
                             {-# LINE 1254 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 1259 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               PrimaryKeyConstraint _annIoriginalTree name_ x_
                               {-# LINE 1264 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 1269 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_Constraint_ReferenceConstraint :: T_Annotation  ->
                                      String ->
                                      ([NameComponent]) ->
                                      T_Name  ->
                                      ([NameComponent]) ->
                                      Cascade ->
                                      Cascade ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIcat ->
         (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: Constraint.ReferenceConstraint.table.tpe"
                 {-# LINE 1285 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tableOtpe ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Constraint.ReferenceConstraint.ann.tpe"
                  {-# LINE 1290 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (table_ ) of
            { ( _tableIoriginalTree,table_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 1297 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tableOcat ->
                 (case (table_1 _tableOcat _tableOtpe ) of
                  { ( _tableIannotatedTree) ->
                      (case (ann_ ) of
                       { ( _annIoriginalTree,ann_1) ->
                           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _lhsIcat
                                   {-# LINE 1306 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annOcat ->
                            (case (ann_1 _annOcat _annOtpe ) of
                             { ( _annIannotatedTree) ->
                                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         ReferenceConstraint _annIannotatedTree name_ atts_ _tableIannotatedTree tableAtts_ onUpdate_ onDelete_
                                         {-# LINE 1313 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annotatedTree ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _annotatedTree
                                          {-# LINE 1318 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOannotatedTree ->
                                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           ReferenceConstraint _annIoriginalTree name_ atts_ _tableIoriginalTree tableAtts_ onUpdate_ onDelete_
                                           {-# LINE 1323 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _originalTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 1328 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Constraint_UniqueConstraint :: T_Annotation  ->
                                   String ->
                                   ([NameComponent]) ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint ann_ name_ x_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Constraint.UniqueConstraint.ann.tpe"
                 {-# LINE 1340 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 1347 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             UniqueConstraint _annIannotatedTree name_ x_
                             {-# LINE 1354 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 1359 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               UniqueConstraint _annIoriginalTree name_ x_
                               {-# LINE 1364 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 1369 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 1417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 1431 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 1436 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 1441 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 1446 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 1455 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1460 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1465 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 1470 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         child ann            : Annotation 
         child blk            : Statement 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SqlFnBody:
         child ann            : Annotation 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data FnBody  = PlpgsqlFnBody (Annotation ) (Statement ) 
             | SqlFnBody (Annotation ) (StatementList ) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_FnBody :: FnBody  ->
              T_FnBody 
sem_FnBody (PlpgsqlFnBody _ann _blk )  =
    (sem_FnBody_PlpgsqlFnBody (sem_Annotation _ann ) (sem_Statement _blk ) )
sem_FnBody (SqlFnBody _ann _sts )  =
    (sem_FnBody_SqlFnBody (sem_Annotation _ann ) (sem_StatementList _sts ) )
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
sem_FnBody_PlpgsqlFnBody :: T_Annotation  ->
                            T_Statement  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody ann_ blk_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 1524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _blkOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: FnBody.PlpgsqlFnBody.ann.tpe"
                  {-# LINE 1529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (blk_ _blkOcat ) of
            { ( _blkIannotatedTree,_blkIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 1538 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   PlpgsqlFnBody _annIannotatedTree _blkIannotatedTree
                                   {-# LINE 1545 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 1550 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     PlpgsqlFnBody _annIoriginalTree _blkIoriginalTree
                                     {-# LINE 1555 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 1560 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_FnBody_SqlFnBody :: T_Annotation  ->
                        T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody ann_ sts_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 1571 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: FnBody.SqlFnBody.ann.tpe"
                  {-# LINE 1576 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (sts_ _stsOcat ) of
            { ( _stsIannotatedTree,_stsIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 1585 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   SqlFnBody _annIannotatedTree _stsIannotatedTree
                                   {-# LINE 1592 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 1597 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     SqlFnBody _annIoriginalTree _stsIoriginalTree
                                     {-# LINE 1602 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 1607 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
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
         child ann            : Annotation 
         child exprs          : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative InQueryExpr:
         child ann            : Annotation 
         child sel            : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data InList  = InList (Annotation ) (ScalarExprList ) 
             | InQueryExpr (Annotation ) (QueryExpr ) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_InList :: InList  ->
              T_InList 
sem_InList (InList _ann _exprs )  =
    (sem_InList_InList (sem_Annotation _ann ) (sem_ScalarExprList _exprs ) )
sem_InList (InQueryExpr _ann _sel )  =
    (sem_InList_InQueryExpr (sem_Annotation _ann ) (sem_QueryExpr _sel ) )
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
sem_InList_InList :: T_Annotation  ->
                     T_ScalarExprList  ->
                     T_InList 
sem_InList_InList ann_ exprs_  =
    (\ _lhsIcat ->
         (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: InList.InList.exprs.downEnv"
                 {-# LINE 1661 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprsOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1666 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprsOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: InList.InList.ann.tpe"
                   {-# LINE 1671 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (exprs_ _exprsOcat _exprsOdownEnv ) of
             { ( _exprsIannotatedTree,_exprsIoriginalTree,_exprsIupTypes) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 1680 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    InList _annIannotatedTree _exprsIannotatedTree
                                    {-# LINE 1687 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 1692 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      InList _annIoriginalTree _exprsIoriginalTree
                                      {-# LINE 1697 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 1702 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_InList_InQueryExpr :: T_Annotation  ->
                          T_QueryExpr  ->
                          T_InList 
sem_InList_InQueryExpr ann_ sel_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 1713 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: InList.InQueryExpr.ann.tpe"
                  {-# LINE 1718 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (sel_ _selOcat ) of
            { ( _selIannotatedTree,_selIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 1727 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   InQueryExpr _annIannotatedTree _selIannotatedTree
                                   {-# LINE 1734 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 1739 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     InQueryExpr _annIoriginalTree _selIoriginalTree
                                     {-# LINE 1744 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 1749 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
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
         child ann            : Annotation 
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative JoinUsing:
         child ann            : Annotation 
         child x              : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data JoinExpr  = JoinOn (Annotation ) (ScalarExpr ) 
               | JoinUsing (Annotation ) (([NameComponent])) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_JoinExpr :: JoinExpr  ->
                T_JoinExpr 
sem_JoinExpr (JoinOn _ann _expr )  =
    (sem_JoinExpr_JoinOn (sem_Annotation _ann ) (sem_ScalarExpr _expr ) )
sem_JoinExpr (JoinUsing _ann _x )  =
    (sem_JoinExpr_JoinUsing (sem_Annotation _ann ) _x )
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
sem_JoinExpr_JoinOn :: T_Annotation  ->
                       T_ScalarExpr  ->
                       T_JoinExpr 
sem_JoinExpr_JoinOn ann_ expr_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: JoinExpr.JoinOn.expr.downEnv"
                 {-# LINE 1803 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1808 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: JoinExpr.JoinOn.ann.tpe"
                   {-# LINE 1813 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (expr_ _exprOcat _exprOdownEnv ) of
             { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 1822 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    JoinOn _annIannotatedTree _exprIannotatedTree
                                    {-# LINE 1829 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 1834 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      JoinOn _annIoriginalTree _exprIoriginalTree
                                      {-# LINE 1839 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 1844 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_JoinExpr_JoinUsing :: T_Annotation  ->
                          ([NameComponent]) ->
                          T_JoinExpr 
sem_JoinExpr_JoinUsing ann_ x_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: JoinExpr.JoinUsing.ann.tpe"
                 {-# LINE 1855 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 1862 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             JoinUsing _annIannotatedTree x_
                             {-# LINE 1869 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 1874 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               JoinUsing _annIoriginalTree x_
                               {-# LINE 1879 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 1884 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: MaybeBoolExpr.Just.just.downEnv"
                 {-# LINE 1932 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _justOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1937 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _justOcat ->
           (case (just_ _justOcat _justOdownEnv ) of
            { ( _justIannotatedTree,_justIoriginalTree,_justIupType) ->
                (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        Just _justIannotatedTree
                        {-# LINE 1944 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annotatedTree ->
                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _annotatedTree
                         {-# LINE 1949 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _lhsOannotatedTree ->
                  (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          Just _justIoriginalTree
                          {-# LINE 1954 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _originalTree ->
                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           _originalTree
                           {-# LINE 1959 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _lhsOoriginalTree ->
                    ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }))
sem_MaybeBoolExpr_Nothing :: T_MaybeBoolExpr 
sem_MaybeBoolExpr_Nothing  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 Nothing
                 {-# LINE 1968 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1973 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 1978 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 1983 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
    ( )
sem_MaybeNameComponentList_Nothing :: T_MaybeNameComponentList 
sem_MaybeNameComponentList_Nothing  =
    ( )
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
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: MaybeScalarExpr.Just.just.downEnv"
                 {-# LINE 2063 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _justOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2068 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _justOcat ->
           (case (just_ _justOcat _justOdownEnv ) of
            { ( _justIannotatedTree,_justIoriginalTree,_justIupType) ->
                (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        Just _justIannotatedTree
                        {-# LINE 2075 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annotatedTree ->
                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _annotatedTree
                         {-# LINE 2080 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _lhsOannotatedTree ->
                  (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          Just _justIoriginalTree
                          {-# LINE 2085 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _originalTree ->
                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           _originalTree
                           {-# LINE 2090 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _lhsOoriginalTree ->
                    ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }))
sem_MaybeScalarExpr_Nothing :: T_MaybeScalarExpr 
sem_MaybeScalarExpr_Nothing  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 Nothing
                 {-# LINE 2099 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2104 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2109 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 2114 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         (case (({-# LINE 3 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                 error "missing rule: MaybeSelectList.Just.just.downEnv"
                 {-# LINE 2162 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _justOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2167 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _justOcat ->
           (case (just_ _justOcat _justOdownEnv ) of
            { ( _justIannotatedTree,_justIoriginalTree,_justIupType) ->
                (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        Just _justIannotatedTree
                        {-# LINE 2174 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annotatedTree ->
                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _annotatedTree
                         {-# LINE 2179 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _lhsOannotatedTree ->
                  (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          Just _justIoriginalTree
                          {-# LINE 2184 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _originalTree ->
                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           _originalTree
                           {-# LINE 2189 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _lhsOoriginalTree ->
                    ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }))
sem_MaybeSelectList_Nothing :: T_MaybeSelectList 
sem_MaybeSelectList_Nothing  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 Nothing
                 {-# LINE 2198 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2203 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2208 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 2213 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- Name --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         originalTree         : SELF 
   visit 1:
      inherited attributes:
         cat                  : Catalog
         tpe                  : Either [TypeError] Type
      synthesized attribute:
         annotatedTree        : SELF 
   alternatives:
      alternative Name:
         child ann            : Annotation 
         child is             : {[NameComponent]}
         visit 0:
            local originalTree : _
         visit 1:
            local annotatedTree : _
-}
data Name  = Name (Annotation ) (([NameComponent])) 
           deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Name :: Name  ->
            T_Name 
sem_Name (Name _ann _is )  =
    (sem_Name_Name (sem_Annotation _ann ) _is )
-- semantic domain
type T_Name  = ( Name ,T_Name_1 )
type T_Name_1  = Catalog ->
                 (Either [TypeError] Type) ->
                 ( Name )
data Inh_Name  = Inh_Name {cat_Inh_Name :: Catalog,tpe_Inh_Name :: (Either [TypeError] Type)}
data Syn_Name  = Syn_Name {annotatedTree_Syn_Name :: Name ,originalTree_Syn_Name :: Name }
wrap_Name :: T_Name  ->
             Inh_Name  ->
             Syn_Name 
wrap_Name sem (Inh_Name _lhsIcat _lhsItpe )  =
    (let ( _lhsOoriginalTree,sem_1) = sem 
         ( _lhsOannotatedTree) = sem_1 _lhsIcat _lhsItpe 
     in  (Syn_Name _lhsOannotatedTree _lhsOoriginalTree ))
sem_Name_Name :: T_Annotation  ->
                 ([NameComponent]) ->
                 T_Name 
sem_Name_Name ann_ is_  =
    (case (ann_ ) of
     { ( _annIoriginalTree,ann_1) ->
         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 Name _annIoriginalTree is_
                 {-# LINE 2266 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _originalTree ->
          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2271 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOoriginalTree ->
           (case ((let sem_Name_Name_1 :: T_Name_1 
                       sem_Name_Name_1  =
                           (\ _lhsIcat
                              _lhsItpe ->
                                (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsItpe
                                        {-# LINE 2280 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOtpe ->
                                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIcat
                                         {-# LINE 2285 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annOcat ->
                                  (case (ann_1 _annOcat _annOtpe ) of
                                   { ( _annIannotatedTree) ->
                                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               Name _annIannotatedTree is_
                                               {-# LINE 2292 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annotatedTree ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _annotatedTree
                                                {-# LINE 2297 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOannotatedTree ->
                                         ( _lhsOannotatedTree) }) }) }) }) }))
                   in  sem_Name_Name_1)) of
            { ( sem_Name_1) ->
            ( _lhsOoriginalTree,sem_Name_1) }) }) }) })
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
    ( )
sem_NameComponentList_Nil :: T_NameComponentList 
sem_NameComponentList_Nil  =
    ( )
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 2376 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _x2Ocat ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                  error "missing rule: NameTypeNameListPair.Tuple.x1.tpe"
                  {-# LINE 2381 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _x1Otpe ->
           (case (x2_ _x2Ocat ) of
            { ( _x2IannotatedTree,_x2IoriginalTree) ->
                (case (x1_ ) of
                 { ( _x1IoriginalTree,x1_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 2390 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _x1Ocat ->
                      (case (x1_1 _x1Ocat _x1Otpe ) of
                       { ( _x1IannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   (_x1IannotatedTree,_x2IannotatedTree)
                                   {-# LINE 2397 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 2402 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     (_x1IoriginalTree,_x2IoriginalTree)
                                     {-# LINE 2407 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 2412 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 2460 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2465 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 2474 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 2479 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 2484 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 2489 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_NameTypeNameListPairList_Nil :: T_NameTypeNameListPairList 
sem_NameTypeNameListPairList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 2498 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2503 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 2508 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 2513 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 2561 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _justOcat ->
          (case (just_ _justOcat ) of
           { ( _justIannotatedTree,_justIoriginalTree) ->
               (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       Just _justIannotatedTree
                       {-# LINE 2568 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annotatedTree ->
                (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _annotatedTree
                        {-# LINE 2573 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _lhsOannotatedTree ->
                 (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         Just _justIoriginalTree
                         {-# LINE 2578 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _originalTree ->
                  (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _originalTree
                          {-# LINE 2583 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _lhsOoriginalTree ->
                   ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 Nothing
                 {-# LINE 2592 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2597 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2602 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 2607 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         child ann            : Annotation 
         child name           : {NameComponent}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ParamDefTp:
         child ann            : Annotation 
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data ParamDef  = ParamDef (Annotation ) (NameComponent) (TypeName ) 
               | ParamDefTp (Annotation ) (TypeName ) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_ParamDef :: ParamDef  ->
                T_ParamDef 
sem_ParamDef (ParamDef _ann _name _typ )  =
    (sem_ParamDef_ParamDef (sem_Annotation _ann ) _name (sem_TypeName _typ ) )
sem_ParamDef (ParamDefTp _ann _typ )  =
    (sem_ParamDef_ParamDefTp (sem_Annotation _ann ) (sem_TypeName _typ ) )
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
sem_ParamDef_ParamDef :: T_Annotation  ->
                         NameComponent ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef ann_ name_ typ_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 2663 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: ParamDef.ParamDef.ann.tpe"
                  {-# LINE 2668 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (typ_ _typOcat ) of
            { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 2677 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   ParamDef _annIannotatedTree name_ _typIannotatedTree
                                   {-# LINE 2684 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 2689 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     ParamDef _annIoriginalTree name_ _typIoriginalTree
                                     {-# LINE 2694 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 2699 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_ParamDef_ParamDefTp :: T_Annotation  ->
                           T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp ann_ typ_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 2710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: ParamDef.ParamDefTp.ann.tpe"
                  {-# LINE 2715 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (typ_ _typOcat ) of
            { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 2724 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   ParamDefTp _annIannotatedTree _typIannotatedTree
                                   {-# LINE 2731 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 2736 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     ParamDefTp _annIoriginalTree _typIoriginalTree
                                     {-# LINE 2741 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 2746 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 2794 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2799 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 2808 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 2813 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 2818 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 2823 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 2832 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2837 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 2842 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 2847 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         child ann            : Annotation 
         child ctype          : {CombineType}
         child sel1           : QueryExpr 
         child sel2           : QueryExpr 
         visit 0:
            local tpe         : {Either [TypeError] Type}
            local annotatedTree : _
            local originalTree : _
      alternative Select:
         child ann            : Annotation 
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
            local tpe         : {Either [TypeError] Type}
            local annotatedTree : _
            local originalTree : _
      alternative Values:
         child ann            : Annotation 
         child vll            : ScalarExprListList 
         visit 0:
            local tpe         : {Either [TypeError] Type}
            local annotatedTree : _
            local originalTree : _
      alternative WithQueryExpr:
         child ann            : Annotation 
         child withs          : WithQueryList 
         child ex             : QueryExpr 
         visit 0:
            local tpe         : {Either [TypeError] Type}
            local annotatedTree : _
            local originalTree : _
-}
data QueryExpr  = CombineQueryExpr (Annotation ) (CombineType) (QueryExpr ) (QueryExpr ) 
                | Select (Annotation ) (Distinct) (SelectList ) (TableRefList ) (MaybeBoolExpr ) (ScalarExprList ) (MaybeBoolExpr ) (ScalarExprDirectionPairList ) (MaybeScalarExpr ) (MaybeScalarExpr ) 
                | Values (Annotation ) (ScalarExprListList ) 
                | WithQueryExpr (Annotation ) (WithQueryList ) (QueryExpr ) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_QueryExpr :: QueryExpr  ->
                 T_QueryExpr 
sem_QueryExpr (CombineQueryExpr _ann _ctype _sel1 _sel2 )  =
    (sem_QueryExpr_CombineQueryExpr (sem_Annotation _ann ) _ctype (sem_QueryExpr _sel1 ) (sem_QueryExpr _sel2 ) )
sem_QueryExpr (Select _ann _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selLimit _selOffset )  =
    (sem_QueryExpr_Select (sem_Annotation _ann ) _selDistinct (sem_SelectList _selSelectList ) (sem_TableRefList _selTref ) (sem_MaybeBoolExpr _selWhere ) (sem_ScalarExprList _selGroupBy ) (sem_MaybeBoolExpr _selHaving ) (sem_ScalarExprDirectionPairList _selOrderBy ) (sem_MaybeScalarExpr _selLimit ) (sem_MaybeScalarExpr _selOffset ) )
sem_QueryExpr (Values _ann _vll )  =
    (sem_QueryExpr_Values (sem_Annotation _ann ) (sem_ScalarExprListList _vll ) )
sem_QueryExpr (WithQueryExpr _ann _withs _ex )  =
    (sem_QueryExpr_WithQueryExpr (sem_Annotation _ann ) (sem_WithQueryList _withs ) (sem_QueryExpr _ex ) )
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
sem_QueryExpr_CombineQueryExpr :: T_Annotation  ->
                                  CombineType ->
                                  T_QueryExpr  ->
                                  T_QueryExpr  ->
                                  T_QueryExpr 
sem_QueryExpr_CombineQueryExpr ann_ ctype_ sel1_ sel2_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 2936 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _sel2Ocat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2941 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _sel1Ocat ->
           (case (({-# LINE 31 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 2946 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tpe ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _tpe
                    {-# LINE 2951 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (sel2_ _sel2Ocat ) of
              { ( _sel2IannotatedTree,_sel2IoriginalTree) ->
                  (case (sel1_ _sel1Ocat ) of
                   { ( _sel1IannotatedTree,_sel1IoriginalTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 2962 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOcat ->
                             (case (ann_1 _annOcat _annOtpe ) of
                              { ( _annIannotatedTree) ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          CombineQueryExpr _annIannotatedTree ctype_ _sel1IannotatedTree _sel2IannotatedTree
                                          {-# LINE 2969 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annotatedTree ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _annotatedTree
                                           {-# LINE 2974 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOannotatedTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            CombineQueryExpr _annIoriginalTree ctype_ _sel1IoriginalTree _sel2IoriginalTree
                                            {-# LINE 2979 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _originalTree ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _originalTree
                                             {-# LINE 2984 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOoriginalTree ->
                                      ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_QueryExpr_Select :: T_Annotation  ->
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 3003 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOffsetOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3008 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _selLimitOcat ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3013 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _selOrderByOcat ->
            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 3018 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _selHavingOcat ->
             (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     error "missing rule: QueryExpr.Select.selGroupBy.downEnv"
                     {-# LINE 3023 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _selGroupByOdownEnv ->
              (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 3028 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _selGroupByOcat ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 3033 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _selWhereOcat ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 3038 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _selTrefOcat ->
                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 3043 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _selSelectListOcat ->
                  (case (selTref_ _selTrefOcat ) of
                   { ( _selTrefIannotatedTree,_selTrefIoriginalTree,_selTrefIupEnv) ->
                       (case (({-# LINE 19 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                               _selTrefIupEnv
                               {-# LINE 3050 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _selSelectListOdownEnv ->
                        (case (selSelectList_ _selSelectListOcat _selSelectListOdownEnv ) of
                         { ( _selSelectListIannotatedTree,_selSelectListIoriginalTree,_selSelectListIupType) ->
                             (case (({-# LINE 21 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                     maybe (Left []) Right _selSelectListIupType
                                     {-# LINE 3057 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _tpe ->
                              (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _tpe
                                      {-# LINE 3062 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annOtpe ->
                               (case (selOffset_ _selOffsetOcat ) of
                                { ( _selOffsetIannotatedTree,_selOffsetIoriginalTree) ->
                                    (case (selLimit_ _selLimitOcat ) of
                                     { ( _selLimitIannotatedTree,_selLimitIoriginalTree) ->
                                         (case (selOrderBy_ _selOrderByOcat ) of
                                          { ( _selOrderByIannotatedTree,_selOrderByIoriginalTree) ->
                                              (case (selHaving_ _selHavingOcat ) of
                                               { ( _selHavingIannotatedTree,_selHavingIoriginalTree) ->
                                                   (case (selGroupBy_ _selGroupByOcat _selGroupByOdownEnv ) of
                                                    { ( _selGroupByIannotatedTree,_selGroupByIoriginalTree,_selGroupByIupTypes) ->
                                                        (case (selWhere_ _selWhereOcat ) of
                                                         { ( _selWhereIannotatedTree,_selWhereIoriginalTree) ->
                                                             (case (ann_ ) of
                                                              { ( _annIoriginalTree,ann_1) ->
                                                                  (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                          _lhsIcat
                                                                          {-# LINE 3081 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                          )) of
                                                                   { _annOcat ->
                                                                   (case (ann_1 _annOcat _annOtpe ) of
                                                                    { ( _annIannotatedTree) ->
                                                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                                Select _annIannotatedTree selDistinct_ _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
                                                                                {-# LINE 3088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                )) of
                                                                         { _annotatedTree ->
                                                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                                 _annotatedTree
                                                                                 {-# LINE 3093 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                 )) of
                                                                          { _lhsOannotatedTree ->
                                                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                                  Select _annIoriginalTree selDistinct_ _selSelectListIoriginalTree _selTrefIoriginalTree _selWhereIoriginalTree _selGroupByIoriginalTree _selHavingIoriginalTree _selOrderByIoriginalTree _selLimitIoriginalTree _selOffsetIoriginalTree
                                                                                  {-# LINE 3098 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                  )) of
                                                                           { _originalTree ->
                                                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                                   _originalTree
                                                                                   {-# LINE 3103 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                   )) of
                                                                            { _lhsOoriginalTree ->
                                                                            ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_QueryExpr_Values :: T_Annotation  ->
                        T_ScalarExprListList  ->
                        T_QueryExpr 
sem_QueryExpr_Values ann_ vll_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 3114 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _vllOcat ->
          (case (({-# LINE 31 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                  Left []
                  {-# LINE 3119 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tpe ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _tpe
                   {-# LINE 3124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (vll_ _vllOcat ) of
             { ( _vllIannotatedTree,_vllIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 3133 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    Values _annIannotatedTree _vllIannotatedTree
                                    {-# LINE 3140 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 3145 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      Values _annIoriginalTree _vllIoriginalTree
                                      {-# LINE 3150 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 3155 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_QueryExpr_WithQueryExpr :: T_Annotation  ->
                               T_WithQueryList  ->
                               T_QueryExpr  ->
                               T_QueryExpr 
sem_QueryExpr_WithQueryExpr ann_ withs_ ex_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 3167 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3172 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _withsOcat ->
           (case (({-# LINE 31 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 3177 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tpe ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _tpe
                    {-# LINE 3182 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (ex_ _exOcat ) of
              { ( _exIannotatedTree,_exIoriginalTree) ->
                  (case (withs_ _withsOcat ) of
                   { ( _withsIannotatedTree,_withsIoriginalTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 3193 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOcat ->
                             (case (ann_1 _annOcat _annOtpe ) of
                              { ( _annIannotatedTree) ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          WithQueryExpr _annIannotatedTree _withsIannotatedTree _exIannotatedTree
                                          {-# LINE 3200 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annotatedTree ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _annotatedTree
                                           {-# LINE 3205 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOannotatedTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            WithQueryExpr _annIoriginalTree _withsIoriginalTree _exIoriginalTree
                                            {-# LINE 3210 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _originalTree ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _originalTree
                                             {-# LINE 3215 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOoriginalTree ->
                                      ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 3258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _statementsOcat ->
          (case (statements_ _statementsOcat ) of
           { ( _statementsIannotatedTree,_statementsIoriginalTree) ->
               (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       Root _statementsIannotatedTree
                       {-# LINE 3265 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annotatedTree ->
                (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _annotatedTree
                        {-# LINE 3270 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _lhsOannotatedTree ->
                 (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         Root _statementsIoriginalTree
                         {-# LINE 3275 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _originalTree ->
                  (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _originalTree
                          {-# LINE 3280 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _lhsOoriginalTree ->
                   ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }))
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
         child ann            : Annotation 
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative NullConstraint:
         child ann            : Annotation 
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowCheckConstraint:
         child ann            : Annotation 
         child name           : {String}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowPrimaryKeyConstraint:
         child ann            : Annotation 
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowReferenceConstraint:
         child ann            : Annotation 
         child name           : {String}
         child table          : Name 
         child att            : {Maybe NameComponent}
         child onUpdate       : {Cascade}
         child onDelete       : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative RowUniqueConstraint:
         child ann            : Annotation 
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data RowConstraint  = NotNullConstraint (Annotation ) (String) 
                    | NullConstraint (Annotation ) (String) 
                    | RowCheckConstraint (Annotation ) (String) (ScalarExpr ) 
                    | RowPrimaryKeyConstraint (Annotation ) (String) 
                    | RowReferenceConstraint (Annotation ) (String) (Name ) ((Maybe NameComponent)) (Cascade) (Cascade) 
                    | RowUniqueConstraint (Annotation ) (String) 
                    deriving ( Data,Eq,Show,Typeable)
-- cata
sem_RowConstraint :: RowConstraint  ->
                     T_RowConstraint 
sem_RowConstraint (NotNullConstraint _ann _name )  =
    (sem_RowConstraint_NotNullConstraint (sem_Annotation _ann ) _name )
sem_RowConstraint (NullConstraint _ann _name )  =
    (sem_RowConstraint_NullConstraint (sem_Annotation _ann ) _name )
sem_RowConstraint (RowCheckConstraint _ann _name _expr )  =
    (sem_RowConstraint_RowCheckConstraint (sem_Annotation _ann ) _name (sem_ScalarExpr _expr ) )
sem_RowConstraint (RowPrimaryKeyConstraint _ann _name )  =
    (sem_RowConstraint_RowPrimaryKeyConstraint (sem_Annotation _ann ) _name )
sem_RowConstraint (RowReferenceConstraint _ann _name _table _att _onUpdate _onDelete )  =
    (sem_RowConstraint_RowReferenceConstraint (sem_Annotation _ann ) _name (sem_Name _table ) _att _onUpdate _onDelete )
sem_RowConstraint (RowUniqueConstraint _ann _name )  =
    (sem_RowConstraint_RowUniqueConstraint (sem_Annotation _ann ) _name )
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
sem_RowConstraint_NotNullConstraint :: T_Annotation  ->
                                       String ->
                                       T_RowConstraint 
sem_RowConstraint_NotNullConstraint ann_ name_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: RowConstraint.NotNullConstraint.ann.tpe"
                 {-# LINE 3375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 3382 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             NotNullConstraint _annIannotatedTree name_
                             {-# LINE 3389 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 3394 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               NotNullConstraint _annIoriginalTree name_
                               {-# LINE 3399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 3404 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_RowConstraint_NullConstraint :: T_Annotation  ->
                                    String ->
                                    T_RowConstraint 
sem_RowConstraint_NullConstraint ann_ name_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: RowConstraint.NullConstraint.ann.tpe"
                 {-# LINE 3415 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 3422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             NullConstraint _annIannotatedTree name_
                             {-# LINE 3429 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 3434 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               NullConstraint _annIoriginalTree name_
                               {-# LINE 3439 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 3444 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_RowConstraint_RowCheckConstraint :: T_Annotation  ->
                                        String ->
                                        T_ScalarExpr  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: RowConstraint.RowCheckConstraint.expr.downEnv"
                 {-# LINE 3456 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: RowConstraint.RowCheckConstraint.ann.tpe"
                   {-# LINE 3466 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (expr_ _exprOcat _exprOdownEnv ) of
             { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 3475 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    RowCheckConstraint _annIannotatedTree name_ _exprIannotatedTree
                                    {-# LINE 3482 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 3487 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      RowCheckConstraint _annIoriginalTree name_ _exprIoriginalTree
                                      {-# LINE 3492 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 3497 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_RowConstraint_RowPrimaryKeyConstraint :: T_Annotation  ->
                                             String ->
                                             T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint ann_ name_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: RowConstraint.RowPrimaryKeyConstraint.ann.tpe"
                 {-# LINE 3508 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 3515 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             RowPrimaryKeyConstraint _annIannotatedTree name_
                             {-# LINE 3522 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 3527 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               RowPrimaryKeyConstraint _annIoriginalTree name_
                               {-# LINE 3532 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 3537 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_RowConstraint_RowReferenceConstraint :: T_Annotation  ->
                                            String ->
                                            T_Name  ->
                                            (Maybe NameComponent) ->
                                            Cascade ->
                                            Cascade ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIcat ->
         (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: RowConstraint.RowReferenceConstraint.table.tpe"
                 {-# LINE 3552 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tableOtpe ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: RowConstraint.RowReferenceConstraint.ann.tpe"
                  {-# LINE 3557 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (table_ ) of
            { ( _tableIoriginalTree,table_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 3564 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tableOcat ->
                 (case (table_1 _tableOcat _tableOtpe ) of
                  { ( _tableIannotatedTree) ->
                      (case (ann_ ) of
                       { ( _annIoriginalTree,ann_1) ->
                           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _lhsIcat
                                   {-# LINE 3573 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annOcat ->
                            (case (ann_1 _annOcat _annOtpe ) of
                             { ( _annIannotatedTree) ->
                                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         RowReferenceConstraint _annIannotatedTree name_ _tableIannotatedTree att_ onUpdate_ onDelete_
                                         {-# LINE 3580 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annotatedTree ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _annotatedTree
                                          {-# LINE 3585 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOannotatedTree ->
                                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           RowReferenceConstraint _annIoriginalTree name_ _tableIoriginalTree att_ onUpdate_ onDelete_
                                           {-# LINE 3590 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _originalTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 3595 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_RowConstraint_RowUniqueConstraint :: T_Annotation  ->
                                         String ->
                                         T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint ann_ name_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: RowConstraint.RowUniqueConstraint.ann.tpe"
                 {-# LINE 3606 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 3613 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             RowUniqueConstraint _annIannotatedTree name_
                             {-# LINE 3620 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 3625 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               RowUniqueConstraint _annIoriginalTree name_
                               {-# LINE 3630 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 3635 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 3683 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3688 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 3697 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 3702 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 3707 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 3712 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 3721 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3726 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 3731 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 3736 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- ScalarExpr --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         upType               : Maybe Type
   alternatives:
      alternative AggregateApp:
         child ann            : Annotation 
         child aggDistinct    : {Distinct}
         child fn             : ScalarExpr 
         child orderBy        : ScalarExprDirectionPairList 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative AntiScalarExpr:
         child string         : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative App:
         child ann            : Annotation 
         child funName        : Name 
         child args           : ScalarExprList 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative BinaryOp:
         child ann            : Annotation 
         child opName         : Name 
         child arg0           : ScalarExpr 
         child arg1           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative BooleanLit:
         child ann            : Annotation 
         child b              : {Bool}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Case:
         child ann            : Annotation 
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative CaseSimple:
         child ann            : Annotation 
         child value          : ScalarExpr 
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Cast:
         child ann            : Annotation 
         child expr           : ScalarExpr 
         child tn             : TypeName 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Exists:
         child ann            : Annotation 
         child sel            : QueryExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Extract:
         child ann            : Annotation 
         child field          : {ExtractField}
         child e              : ScalarExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Identifier:
         child ann            : Annotation 
         child i              : {NameComponent}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative InPredicate:
         child ann            : Annotation 
         child expr           : ScalarExpr 
         child i              : {Bool}
         child list           : InList 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Interval:
         child ann            : Annotation 
         child value          : {String}
         child field          : {IntervalField}
         child prec           : {Maybe Int}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative LiftApp:
         child ann            : Annotation 
         child oper           : Name 
         child flav           : {LiftFlavour}
         child args           : ScalarExprList 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative NullLit:
         child ann            : Annotation 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative NumberLit:
         child ann            : Annotation 
         child d              : {String}
         visit 0:
            local digChars    : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Placeholder:
         child ann            : Annotation 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative PositionalArg:
         child ann            : Annotation 
         child p              : {Integer}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative PostfixOp:
         child ann            : Annotation 
         child opName         : Name 
         child arg            : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative PrefixOp:
         child ann            : Annotation 
         child opName         : Name 
         child arg            : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative QIdentifier:
         child ann            : Annotation 
         child is             : {[NameComponent]}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative QStar:
         child ann            : Annotation 
         child q              : {NameComponent}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative ScalarSubQuery:
         child ann            : Annotation 
         child sel            : QueryExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative SpecialOp:
         child ann            : Annotation 
         child opName         : Name 
         child args           : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Star:
         child ann            : Annotation 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative StringLit:
         child ann            : Annotation 
         child value          : {String}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative TypedStringLit:
         child ann            : Annotation 
         child tn             : TypeName 
         child value          : {String}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative WindowApp:
         child ann            : Annotation 
         child fn             : ScalarExpr 
         child partitionBy    : ScalarExprList 
         child orderBy        : ScalarExprDirectionPairList 
         child frm            : {FrameClause}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
-}
data ScalarExpr  = AggregateApp (Annotation ) (Distinct) (ScalarExpr ) (ScalarExprDirectionPairList ) 
                 | AntiScalarExpr (String) 
                 | App (Annotation ) (Name ) (ScalarExprList ) 
                 | BinaryOp (Annotation ) (Name ) (ScalarExpr ) (ScalarExpr ) 
                 | BooleanLit (Annotation ) (Bool) 
                 | Case (Annotation ) (CaseScalarExprListScalarExprPairList ) (MaybeScalarExpr ) 
                 | CaseSimple (Annotation ) (ScalarExpr ) (CaseScalarExprListScalarExprPairList ) (MaybeScalarExpr ) 
                 | Cast (Annotation ) (ScalarExpr ) (TypeName ) 
                 | Exists (Annotation ) (QueryExpr ) 
                 | Extract (Annotation ) (ExtractField) (ScalarExpr ) 
                 | Identifier (Annotation ) (NameComponent) 
                 | InPredicate (Annotation ) (ScalarExpr ) (Bool) (InList ) 
                 | Interval (Annotation ) (String) (IntervalField) ((Maybe Int)) 
                 | LiftApp (Annotation ) (Name ) (LiftFlavour) (ScalarExprList ) 
                 | NullLit (Annotation ) 
                 | NumberLit (Annotation ) (String) 
                 | Placeholder (Annotation ) 
                 | PositionalArg (Annotation ) (Integer) 
                 | PostfixOp (Annotation ) (Name ) (ScalarExpr ) 
                 | PrefixOp (Annotation ) (Name ) (ScalarExpr ) 
                 | QIdentifier (Annotation ) (([NameComponent])) 
                 | QStar (Annotation ) (NameComponent) 
                 | ScalarSubQuery (Annotation ) (QueryExpr ) 
                 | SpecialOp (Annotation ) (Name ) (ScalarExprList ) 
                 | Star (Annotation ) 
                 | StringLit (Annotation ) (String) 
                 | TypedStringLit (Annotation ) (TypeName ) (String) 
                 | WindowApp (Annotation ) (ScalarExpr ) (ScalarExprList ) (ScalarExprDirectionPairList ) (FrameClause) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_ScalarExpr :: ScalarExpr  ->
                  T_ScalarExpr 
sem_ScalarExpr (AggregateApp _ann _aggDistinct _fn _orderBy )  =
    (sem_ScalarExpr_AggregateApp (sem_Annotation _ann ) _aggDistinct (sem_ScalarExpr _fn ) (sem_ScalarExprDirectionPairList _orderBy ) )
sem_ScalarExpr (AntiScalarExpr _string )  =
    (sem_ScalarExpr_AntiScalarExpr _string )
sem_ScalarExpr (App _ann _funName _args )  =
    (sem_ScalarExpr_App (sem_Annotation _ann ) (sem_Name _funName ) (sem_ScalarExprList _args ) )
sem_ScalarExpr (BinaryOp _ann _opName _arg0 _arg1 )  =
    (sem_ScalarExpr_BinaryOp (sem_Annotation _ann ) (sem_Name _opName ) (sem_ScalarExpr _arg0 ) (sem_ScalarExpr _arg1 ) )
sem_ScalarExpr (BooleanLit _ann _b )  =
    (sem_ScalarExpr_BooleanLit (sem_Annotation _ann ) _b )
sem_ScalarExpr (Case _ann _cases _els )  =
    (sem_ScalarExpr_Case (sem_Annotation _ann ) (sem_CaseScalarExprListScalarExprPairList _cases ) (sem_MaybeScalarExpr _els ) )
sem_ScalarExpr (CaseSimple _ann _value _cases _els )  =
    (sem_ScalarExpr_CaseSimple (sem_Annotation _ann ) (sem_ScalarExpr _value ) (sem_CaseScalarExprListScalarExprPairList _cases ) (sem_MaybeScalarExpr _els ) )
sem_ScalarExpr (Cast _ann _expr _tn )  =
    (sem_ScalarExpr_Cast (sem_Annotation _ann ) (sem_ScalarExpr _expr ) (sem_TypeName _tn ) )
sem_ScalarExpr (Exists _ann _sel )  =
    (sem_ScalarExpr_Exists (sem_Annotation _ann ) (sem_QueryExpr _sel ) )
sem_ScalarExpr (Extract _ann _field _e )  =
    (sem_ScalarExpr_Extract (sem_Annotation _ann ) _field (sem_ScalarExpr _e ) )
sem_ScalarExpr (Identifier _ann _i )  =
    (sem_ScalarExpr_Identifier (sem_Annotation _ann ) _i )
sem_ScalarExpr (InPredicate _ann _expr _i _list )  =
    (sem_ScalarExpr_InPredicate (sem_Annotation _ann ) (sem_ScalarExpr _expr ) _i (sem_InList _list ) )
sem_ScalarExpr (Interval _ann _value _field _prec )  =
    (sem_ScalarExpr_Interval (sem_Annotation _ann ) _value _field _prec )
sem_ScalarExpr (LiftApp _ann _oper _flav _args )  =
    (sem_ScalarExpr_LiftApp (sem_Annotation _ann ) (sem_Name _oper ) _flav (sem_ScalarExprList _args ) )
sem_ScalarExpr (NullLit _ann )  =
    (sem_ScalarExpr_NullLit (sem_Annotation _ann ) )
sem_ScalarExpr (NumberLit _ann _d )  =
    (sem_ScalarExpr_NumberLit (sem_Annotation _ann ) _d )
sem_ScalarExpr (Placeholder _ann )  =
    (sem_ScalarExpr_Placeholder (sem_Annotation _ann ) )
sem_ScalarExpr (PositionalArg _ann _p )  =
    (sem_ScalarExpr_PositionalArg (sem_Annotation _ann ) _p )
sem_ScalarExpr (PostfixOp _ann _opName _arg )  =
    (sem_ScalarExpr_PostfixOp (sem_Annotation _ann ) (sem_Name _opName ) (sem_ScalarExpr _arg ) )
sem_ScalarExpr (PrefixOp _ann _opName _arg )  =
    (sem_ScalarExpr_PrefixOp (sem_Annotation _ann ) (sem_Name _opName ) (sem_ScalarExpr _arg ) )
sem_ScalarExpr (QIdentifier _ann _is )  =
    (sem_ScalarExpr_QIdentifier (sem_Annotation _ann ) _is )
sem_ScalarExpr (QStar _ann _q )  =
    (sem_ScalarExpr_QStar (sem_Annotation _ann ) _q )
sem_ScalarExpr (ScalarSubQuery _ann _sel )  =
    (sem_ScalarExpr_ScalarSubQuery (sem_Annotation _ann ) (sem_QueryExpr _sel ) )
sem_ScalarExpr (SpecialOp _ann _opName _args )  =
    (sem_ScalarExpr_SpecialOp (sem_Annotation _ann ) (sem_Name _opName ) (sem_ScalarExprList _args ) )
sem_ScalarExpr (Star _ann )  =
    (sem_ScalarExpr_Star (sem_Annotation _ann ) )
sem_ScalarExpr (StringLit _ann _value )  =
    (sem_ScalarExpr_StringLit (sem_Annotation _ann ) _value )
sem_ScalarExpr (TypedStringLit _ann _tn _value )  =
    (sem_ScalarExpr_TypedStringLit (sem_Annotation _ann ) (sem_TypeName _tn ) _value )
sem_ScalarExpr (WindowApp _ann _fn _partitionBy _orderBy _frm )  =
    (sem_ScalarExpr_WindowApp (sem_Annotation _ann ) (sem_ScalarExpr _fn ) (sem_ScalarExprList _partitionBy ) (sem_ScalarExprDirectionPairList _orderBy ) _frm )
-- semantic domain
type T_ScalarExpr  = Catalog ->
                     Environment ->
                     ( ScalarExpr ,ScalarExpr ,(Maybe Type))
data Inh_ScalarExpr  = Inh_ScalarExpr {cat_Inh_ScalarExpr :: Catalog,downEnv_Inh_ScalarExpr :: Environment}
data Syn_ScalarExpr  = Syn_ScalarExpr {annotatedTree_Syn_ScalarExpr :: ScalarExpr ,originalTree_Syn_ScalarExpr :: ScalarExpr ,upType_Syn_ScalarExpr :: (Maybe Type)}
wrap_ScalarExpr :: T_ScalarExpr  ->
                   Inh_ScalarExpr  ->
                   Syn_ScalarExpr 
wrap_ScalarExpr sem (Inh_ScalarExpr _lhsIcat _lhsIdownEnv )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) = sem _lhsIcat _lhsIdownEnv 
     in  (Syn_ScalarExpr _lhsOannotatedTree _lhsOoriginalTree _lhsOupType ))
sem_ScalarExpr_AggregateApp :: T_Annotation  ->
                               Distinct ->
                               T_ScalarExpr  ->
                               T_ScalarExprDirectionPairList  ->
                               T_ScalarExpr 
sem_ScalarExpr_AggregateApp ann_ aggDistinct_ fn_ orderBy_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 4073 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _orderByOcat ->
          (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 4078 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _fnOdownEnv ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4083 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _fnOcat ->
            (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    Left []
                    {-# LINE 4088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _tpe ->
             (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     _tpe
                     {-# LINE 4093 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (orderBy_ _orderByOcat ) of
               { ( _orderByIannotatedTree,_orderByIoriginalTree) ->
                   (case (fn_ _fnOcat _fnOdownEnv ) of
                    { ( _fnIannotatedTree,_fnIoriginalTree,_fnIupType) ->
                        (case (ann_ ) of
                         { ( _annIoriginalTree,ann_1) ->
                             (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIcat
                                     {-# LINE 4104 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOcat ->
                              (case (ann_1 _annOcat _annOtpe ) of
                               { ( _annIannotatedTree) ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           AggregateApp _annIannotatedTree aggDistinct_ _fnIannotatedTree _orderByIannotatedTree
                                           {-# LINE 4111 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annotatedTree ->
                                    (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _annotatedTree
                                            {-# LINE 4116 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOannotatedTree ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             AggregateApp _annIoriginalTree aggDistinct_ _fnIoriginalTree _orderByIoriginalTree
                                             {-# LINE 4121 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 4126 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                               either (const Nothing) Just _tpe
                                               {-# LINE 4131 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOupType ->
                                        ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_AntiScalarExpr :: String ->
                                 T_ScalarExpr 
sem_ScalarExpr_AntiScalarExpr string_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 AntiScalarExpr string_
                 {-# LINE 4142 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4147 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiScalarExpr string_
                   {-# LINE 4152 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 4157 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             (case (({-# LINE 29 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     error "missing rule: ScalarExpr.AntiScalarExpr.lhs.upType"
                     {-# LINE 4162 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _lhsOupType ->
              ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }))
sem_ScalarExpr_App :: T_Annotation  ->
                      T_Name  ->
                      T_ScalarExprList  ->
                      T_ScalarExpr 
sem_ScalarExpr_App ann_ funName_ args_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 4175 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argsOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4180 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argsOcat ->
           (case (args_ _argsOcat _argsOdownEnv ) of
            { ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) ->
                (case (funName_ ) of
                 { ( _funNameIoriginalTree,funName_1) ->
                     (case (({-# LINE 124 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                             do
                             tys <- mapM (maybe (Left []) Right) _argsIupTypes
                             let Name _ ns = _funNameIoriginalTree
                             (_,rt) <- matchApp _lhsIcat ns tys
                             return rt
                             {-# LINE 4193 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _tpe ->
                      (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                              _tpe
                              {-# LINE 4198 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _funNameOtpe ->
                       (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                               _tpe
                               {-# LINE 4203 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOtpe ->
                        (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 4208 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _funNameOcat ->
                         (case (funName_1 _funNameOcat _funNameOtpe ) of
                          { ( _funNameIannotatedTree) ->
                              (case (ann_ ) of
                               { ( _annIoriginalTree,ann_1) ->
                                   (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _lhsIcat
                                           {-# LINE 4217 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annOcat ->
                                    (case (ann_1 _annOcat _annOtpe ) of
                                     { ( _annIannotatedTree) ->
                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 App _annIannotatedTree _funNameIannotatedTree _argsIannotatedTree
                                                 {-# LINE 4224 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _annotatedTree ->
                                          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _annotatedTree
                                                  {-# LINE 4229 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOannotatedTree ->
                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   App _annIoriginalTree _funNameIoriginalTree _argsIoriginalTree
                                                   {-# LINE 4234 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _originalTree ->
                                            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _originalTree
                                                    {-# LINE 4239 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOoriginalTree ->
                                             (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                     either (const Nothing) Just _tpe
                                                     {-# LINE 4244 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOupType ->
                                              ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_BinaryOp :: T_Annotation  ->
                           T_Name  ->
                           T_ScalarExpr  ->
                           T_ScalarExpr  ->
                           T_ScalarExpr 
sem_ScalarExpr_BinaryOp ann_ opName_ arg0_ arg1_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 4258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _arg1OdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4263 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _arg1Ocat ->
           (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 4268 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _arg0OdownEnv ->
            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 4273 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _arg0Ocat ->
             (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                     error "missing rule: ScalarExpr.BinaryOp.opName.tpe"
                     {-# LINE 4278 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _opNameOtpe ->
              (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      error "missing rule: ScalarExpr.BinaryOp.ann.tpe"
                      {-# LINE 4283 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (arg1_ _arg1Ocat _arg1OdownEnv ) of
                { ( _arg1IannotatedTree,_arg1IoriginalTree,_arg1IupType) ->
                    (case (arg0_ _arg0Ocat _arg0OdownEnv ) of
                     { ( _arg0IannotatedTree,_arg0IoriginalTree,_arg0IupType) ->
                         (case (opName_ ) of
                          { ( _opNameIoriginalTree,opName_1) ->
                              (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIcat
                                      {-# LINE 4294 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _opNameOcat ->
                               (case (opName_1 _opNameOcat _opNameOtpe ) of
                                { ( _opNameIannotatedTree) ->
                                    (case (ann_ ) of
                                     { ( _annIoriginalTree,ann_1) ->
                                         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _lhsIcat
                                                 {-# LINE 4303 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _annOcat ->
                                          (case (ann_1 _annOcat _annOtpe ) of
                                           { ( _annIannotatedTree) ->
                                               (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                       BinaryOp _annIannotatedTree _opNameIannotatedTree _arg0IannotatedTree _arg1IannotatedTree
                                                       {-# LINE 4310 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _annotatedTree ->
                                                (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                        _annotatedTree
                                                        {-# LINE 4315 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOannotatedTree ->
                                                 (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         BinaryOp _annIoriginalTree _opNameIoriginalTree _arg0IoriginalTree _arg1IoriginalTree
                                                         {-# LINE 4320 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _originalTree ->
                                                  (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                          _originalTree
                                                          {-# LINE 4325 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _lhsOoriginalTree ->
                                                   (case (({-# LINE 29 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                           _arg1IupType
                                                           {-# LINE 4330 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _lhsOupType ->
                                                    ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_BooleanLit :: T_Annotation  ->
                             Bool ->
                             T_ScalarExpr 
sem_ScalarExpr_BooleanLit ann_ b_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Right typeBool
                 {-# LINE 4342 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 4347 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 4354 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              BooleanLit _annIannotatedTree b_
                              {-# LINE 4361 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 4366 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                BooleanLit _annIoriginalTree b_
                                {-# LINE 4371 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 4376 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 4381 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOupType ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Case :: T_Annotation  ->
                       T_CaseScalarExprListScalarExprPairList  ->
                       T_MaybeScalarExpr  ->
                       T_ScalarExpr 
sem_ScalarExpr_Case ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 4394 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elsOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _casesOcat ->
           (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 4404 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tpe ->
            (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    _tpe
                    {-# LINE 4409 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (els_ _elsOcat ) of
              { ( _elsIannotatedTree,_elsIoriginalTree) ->
                  (case (cases_ _casesOcat ) of
                   { ( _casesIannotatedTree,_casesIoriginalTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 4420 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOcat ->
                             (case (ann_1 _annOcat _annOtpe ) of
                              { ( _annIannotatedTree) ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          Case _annIannotatedTree _casesIannotatedTree _elsIannotatedTree
                                          {-# LINE 4427 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annotatedTree ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _annotatedTree
                                           {-# LINE 4432 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOannotatedTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            Case _annIoriginalTree _casesIoriginalTree _elsIoriginalTree
                                            {-# LINE 4437 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _originalTree ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _originalTree
                                             {-# LINE 4442 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOoriginalTree ->
                                      (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                              either (const Nothing) Just _tpe
                                              {-# LINE 4447 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOupType ->
                                       ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_CaseSimple :: T_Annotation  ->
                             T_ScalarExpr  ->
                             T_CaseScalarExprListScalarExprPairList  ->
                             T_MaybeScalarExpr  ->
                             T_ScalarExpr 
sem_ScalarExpr_CaseSimple ann_ value_ cases_ els_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 4461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elsOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4466 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _casesOcat ->
           (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 4471 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _valueOdownEnv ->
            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 4476 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _valueOcat ->
             (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     Left []
                     {-# LINE 4481 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _tpe ->
              (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                      _tpe
                      {-# LINE 4486 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (els_ _elsOcat ) of
                { ( _elsIannotatedTree,_elsIoriginalTree) ->
                    (case (cases_ _casesOcat ) of
                     { ( _casesIannotatedTree,_casesIoriginalTree) ->
                         (case (value_ _valueOcat _valueOdownEnv ) of
                          { ( _valueIannotatedTree,_valueIoriginalTree,_valueIupType) ->
                              (case (ann_ ) of
                               { ( _annIoriginalTree,ann_1) ->
                                   (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _lhsIcat
                                           {-# LINE 4499 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annOcat ->
                                    (case (ann_1 _annOcat _annOtpe ) of
                                     { ( _annIannotatedTree) ->
                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 CaseSimple _annIannotatedTree _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                                                 {-# LINE 4506 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _annotatedTree ->
                                          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _annotatedTree
                                                  {-# LINE 4511 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOannotatedTree ->
                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   CaseSimple _annIoriginalTree _valueIoriginalTree _casesIoriginalTree _elsIoriginalTree
                                                   {-# LINE 4516 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _originalTree ->
                                            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _originalTree
                                                    {-# LINE 4521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOoriginalTree ->
                                             (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                     either (const Nothing) Just _tpe
                                                     {-# LINE 4526 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOupType ->
                                              ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Cast :: T_Annotation  ->
                       T_ScalarExpr  ->
                       T_TypeName  ->
                       T_ScalarExpr 
sem_ScalarExpr_Cast ann_ expr_ tn_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 4539 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tnOcat ->
          (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 4544 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOdownEnv ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4549 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (tn_ _tnOcat ) of
             { ( _tnIannotatedTree,_tnInamedType,_tnIoriginalTree) ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                         maybe (Left []) Right _tnInamedType
                         {-# LINE 4556 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tpe ->
                  (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                          _tpe
                          {-# LINE 4561 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _annOtpe ->
                   (case (expr_ _exprOcat _exprOdownEnv ) of
                    { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                        (case (ann_ ) of
                         { ( _annIoriginalTree,ann_1) ->
                             (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIcat
                                     {-# LINE 4570 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOcat ->
                              (case (ann_1 _annOcat _annOtpe ) of
                               { ( _annIannotatedTree) ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           Cast _annIannotatedTree _exprIannotatedTree _tnIannotatedTree
                                           {-# LINE 4577 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annotatedTree ->
                                    (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _annotatedTree
                                            {-# LINE 4582 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOannotatedTree ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             Cast _annIoriginalTree _exprIoriginalTree _tnIoriginalTree
                                             {-# LINE 4587 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 4592 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                               either (const Nothing) Just _tpe
                                               {-# LINE 4597 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOupType ->
                                        ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Exists :: T_Annotation  ->
                         T_QueryExpr  ->
                         T_ScalarExpr 
sem_ScalarExpr_Exists ann_ sel_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 4609 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOcat ->
          (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  Left []
                  {-# LINE 4614 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tpe ->
           (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 4619 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (sel_ _selOcat ) of
             { ( _selIannotatedTree,_selIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 4628 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    Exists _annIannotatedTree _selIannotatedTree
                                    {-# LINE 4635 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 4640 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      Exists _annIoriginalTree _selIoriginalTree
                                      {-# LINE 4645 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 4650 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                        either (const Nothing) Just _tpe
                                        {-# LINE 4655 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOupType ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Extract :: T_Annotation  ->
                          ExtractField ->
                          T_ScalarExpr  ->
                          T_ScalarExpr 
sem_ScalarExpr_Extract ann_ field_ e_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 4668 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _eOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4673 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _eOcat ->
           (case (e_ _eOcat _eOdownEnv ) of
            { ( _eIannotatedTree,_eIoriginalTree,_eIupType) ->
                (case (({-# LINE 94 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                        do
                        x <- maybe (Left []) Right _eIupType
                        if x == typeDate
                          then Right typeFloat8
                          else Left [NoMatchingOperator "extract" [x]]
                        {-# LINE 4684 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tpe ->
                 (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                         _tpe
                         {-# LINE 4689 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOtpe ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 4696 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     Extract _annIannotatedTree field_ _eIannotatedTree
                                     {-# LINE 4703 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 4708 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       Extract _annIoriginalTree field_ _eIoriginalTree
                                       {-# LINE 4713 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 4718 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                         either (const Nothing) Just _tpe
                                         {-# LINE 4723 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOupType ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Identifier :: T_Annotation  ->
                             NameComponent ->
                             T_ScalarExpr 
sem_ScalarExpr_Identifier ann_ i_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 109 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 envLookupIdentifier [i_] _lhsIdownEnv
                 {-# LINE 4735 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 4740 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 4747 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              Identifier _annIannotatedTree i_
                              {-# LINE 4754 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 4759 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                Identifier _annIoriginalTree i_
                                {-# LINE 4764 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 4769 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 4774 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOupType ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_InPredicate :: T_Annotation  ->
                              T_ScalarExpr  ->
                              Bool ->
                              T_InList  ->
                              T_ScalarExpr 
sem_ScalarExpr_InPredicate ann_ expr_ i_ list_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 4788 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _listOcat ->
          (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 4793 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOdownEnv ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4798 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    Left []
                    {-# LINE 4803 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _tpe ->
             (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     _tpe
                     {-# LINE 4808 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (list_ _listOcat ) of
               { ( _listIannotatedTree,_listIoriginalTree) ->
                   (case (expr_ _exprOcat _exprOdownEnv ) of
                    { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                        (case (ann_ ) of
                         { ( _annIoriginalTree,ann_1) ->
                             (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIcat
                                     {-# LINE 4819 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOcat ->
                              (case (ann_1 _annOcat _annOtpe ) of
                               { ( _annIannotatedTree) ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           InPredicate _annIannotatedTree _exprIannotatedTree i_ _listIannotatedTree
                                           {-# LINE 4826 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annotatedTree ->
                                    (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _annotatedTree
                                            {-# LINE 4831 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOannotatedTree ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             InPredicate _annIoriginalTree _exprIoriginalTree i_ _listIoriginalTree
                                             {-# LINE 4836 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 4841 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                               either (const Nothing) Just _tpe
                                               {-# LINE 4846 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOupType ->
                                        ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Interval :: T_Annotation  ->
                           String ->
                           IntervalField ->
                           (Maybe Int) ->
                           T_ScalarExpr 
sem_ScalarExpr_Interval ann_ value_ field_ prec_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Right $ ScalarType "interval"
                 {-# LINE 4860 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 4865 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 4872 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              Interval _annIannotatedTree value_ field_ prec_
                              {-# LINE 4879 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 4884 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                Interval _annIoriginalTree value_ field_ prec_
                                {-# LINE 4889 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 4894 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 4899 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOupType ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_LiftApp :: T_Annotation  ->
                          T_Name  ->
                          LiftFlavour ->
                          T_ScalarExprList  ->
                          T_ScalarExpr 
sem_ScalarExpr_LiftApp ann_ oper_ flav_ args_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 4913 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argsOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4918 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argsOcat ->
           (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 4923 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tpe ->
            (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                    _tpe
                    {-# LINE 4928 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _operOtpe ->
             (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     _tpe
                     {-# LINE 4933 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (args_ _argsOcat _argsOdownEnv ) of
               { ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) ->
                   (case (oper_ ) of
                    { ( _operIoriginalTree,oper_1) ->
                        (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 4942 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _operOcat ->
                         (case (oper_1 _operOcat _operOtpe ) of
                          { ( _operIannotatedTree) ->
                              (case (ann_ ) of
                               { ( _annIoriginalTree,ann_1) ->
                                   (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _lhsIcat
                                           {-# LINE 4951 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annOcat ->
                                    (case (ann_1 _annOcat _annOtpe ) of
                                     { ( _annIannotatedTree) ->
                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 LiftApp _annIannotatedTree _operIannotatedTree flav_ _argsIannotatedTree
                                                 {-# LINE 4958 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _annotatedTree ->
                                          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _annotatedTree
                                                  {-# LINE 4963 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOannotatedTree ->
                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   LiftApp _annIoriginalTree _operIoriginalTree flav_ _argsIoriginalTree
                                                   {-# LINE 4968 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _originalTree ->
                                            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _originalTree
                                                    {-# LINE 4973 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOoriginalTree ->
                                             (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                     either (const Nothing) Just _tpe
                                                     {-# LINE 4978 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOupType ->
                                              ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_NullLit :: T_Annotation  ->
                          T_ScalarExpr 
sem_ScalarExpr_NullLit ann_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 80 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Right UnknownType
                 {-# LINE 4989 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 4994 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 5001 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              NullLit _annIannotatedTree
                              {-# LINE 5008 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 5013 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                NullLit _annIoriginalTree
                                {-# LINE 5018 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 5023 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 5028 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOupType ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_NumberLit :: T_Annotation  ->
                            String ->
                            T_ScalarExpr 
sem_ScalarExpr_NumberLit ann_ d_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 74 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 concatMap show [(0::Int)..9]
                 {-# LINE 5040 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _digChars ->
          (case (({-# LINE 71 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  Right $ if all (`elem` _digChars    ) d_
                          then typeInt
                          else typeNumeric
                  {-# LINE 5047 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tpe ->
           (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 5052 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (ann_ ) of
             { ( _annIoriginalTree,ann_1) ->
                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 5059 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               NumberLit _annIannotatedTree d_
                               {-# LINE 5066 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 5071 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 NumberLit _annIoriginalTree d_
                                 {-# LINE 5076 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 5081 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                   either (const Nothing) Just _tpe
                                   {-# LINE 5086 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOupType ->
                            ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Placeholder :: T_Annotation  ->
                              T_ScalarExpr 
sem_ScalarExpr_Placeholder ann_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 101 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Right UnknownType
                 {-# LINE 5097 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 5102 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 5109 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              Placeholder _annIannotatedTree
                              {-# LINE 5116 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 5121 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                Placeholder _annIoriginalTree
                                {-# LINE 5126 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 5131 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 5136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOupType ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_PositionalArg :: T_Annotation  ->
                                Integer ->
                                T_ScalarExpr 
sem_ScalarExpr_PositionalArg ann_ p_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Left []
                 {-# LINE 5148 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 5153 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 5160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              PositionalArg _annIannotatedTree p_
                              {-# LINE 5167 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 5172 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                PositionalArg _annIoriginalTree p_
                                {-# LINE 5177 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 5182 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 5187 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOupType ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_PostfixOp :: T_Annotation  ->
                            T_Name  ->
                            T_ScalarExpr  ->
                            T_ScalarExpr 
sem_ScalarExpr_PostfixOp ann_ opName_ arg_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 5200 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5205 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argOcat ->
           (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: ScalarExpr.PostfixOp.opName.tpe"
                   {-# LINE 5210 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _opNameOtpe ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: ScalarExpr.PostfixOp.ann.tpe"
                    {-# LINE 5215 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (arg_ _argOcat _argOdownEnv ) of
              { ( _argIannotatedTree,_argIoriginalTree,_argIupType) ->
                  (case (opName_ ) of
                   { ( _opNameIoriginalTree,opName_1) ->
                       (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 5224 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _opNameOcat ->
                        (case (opName_1 _opNameOcat _opNameOtpe ) of
                         { ( _opNameIannotatedTree) ->
                             (case (ann_ ) of
                              { ( _annIoriginalTree,ann_1) ->
                                  (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 5233 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annOcat ->
                                   (case (ann_1 _annOcat _annOtpe ) of
                                    { ( _annIannotatedTree) ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                PostfixOp _annIannotatedTree _opNameIannotatedTree _argIannotatedTree
                                                {-# LINE 5240 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _annotatedTree ->
                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _annotatedTree
                                                 {-# LINE 5245 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _lhsOannotatedTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  PostfixOp _annIoriginalTree _opNameIoriginalTree _argIoriginalTree
                                                  {-# LINE 5250 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _originalTree ->
                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _originalTree
                                                   {-# LINE 5255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOoriginalTree ->
                                            (case (({-# LINE 29 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                    _argIupType
                                                    {-# LINE 5260 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOupType ->
                                             ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_PrefixOp :: T_Annotation  ->
                           T_Name  ->
                           T_ScalarExpr  ->
                           T_ScalarExpr 
sem_ScalarExpr_PrefixOp ann_ opName_ arg_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 5273 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5278 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argOcat ->
           (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: ScalarExpr.PrefixOp.opName.tpe"
                   {-# LINE 5283 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _opNameOtpe ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: ScalarExpr.PrefixOp.ann.tpe"
                    {-# LINE 5288 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (arg_ _argOcat _argOdownEnv ) of
              { ( _argIannotatedTree,_argIoriginalTree,_argIupType) ->
                  (case (opName_ ) of
                   { ( _opNameIoriginalTree,opName_1) ->
                       (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 5297 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _opNameOcat ->
                        (case (opName_1 _opNameOcat _opNameOtpe ) of
                         { ( _opNameIannotatedTree) ->
                             (case (ann_ ) of
                              { ( _annIoriginalTree,ann_1) ->
                                  (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 5306 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annOcat ->
                                   (case (ann_1 _annOcat _annOtpe ) of
                                    { ( _annIannotatedTree) ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                PrefixOp _annIannotatedTree _opNameIannotatedTree _argIannotatedTree
                                                {-# LINE 5313 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _annotatedTree ->
                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _annotatedTree
                                                 {-# LINE 5318 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _lhsOannotatedTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  PrefixOp _annIoriginalTree _opNameIoriginalTree _argIoriginalTree
                                                  {-# LINE 5323 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _originalTree ->
                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _originalTree
                                                   {-# LINE 5328 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOoriginalTree ->
                                            (case (({-# LINE 29 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                    _argIupType
                                                    {-# LINE 5333 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOupType ->
                                             ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_QIdentifier :: T_Annotation  ->
                              ([NameComponent]) ->
                              T_ScalarExpr 
sem_ScalarExpr_QIdentifier ann_ is_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Left []
                 {-# LINE 5345 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 5350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 5357 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              QIdentifier _annIannotatedTree is_
                              {-# LINE 5364 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 5369 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                QIdentifier _annIoriginalTree is_
                                {-# LINE 5374 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 5379 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 5384 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOupType ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_QStar :: T_Annotation  ->
                        NameComponent ->
                        T_ScalarExpr 
sem_ScalarExpr_QStar ann_ q_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Left []
                 {-# LINE 5396 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 5401 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 5408 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              QStar _annIannotatedTree q_
                              {-# LINE 5415 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 5420 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                QStar _annIoriginalTree q_
                                {-# LINE 5425 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 5430 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 5435 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOupType ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_ScalarSubQuery :: T_Annotation  ->
                                 T_QueryExpr  ->
                                 T_ScalarExpr 
sem_ScalarExpr_ScalarSubQuery ann_ sel_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 5447 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOcat ->
          (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  Left []
                  {-# LINE 5452 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tpe ->
           (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 5457 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (sel_ _selOcat ) of
             { ( _selIannotatedTree,_selIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 5466 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    ScalarSubQuery _annIannotatedTree _selIannotatedTree
                                    {-# LINE 5473 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 5478 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      ScalarSubQuery _annIoriginalTree _selIoriginalTree
                                      {-# LINE 5483 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 5488 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                        either (const Nothing) Just _tpe
                                        {-# LINE 5493 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOupType ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_SpecialOp :: T_Annotation  ->
                            T_Name  ->
                            T_ScalarExprList  ->
                            T_ScalarExpr 
sem_ScalarExpr_SpecialOp ann_ opName_ args_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 5506 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argsOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5511 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argsOcat ->
           (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: ScalarExpr.SpecialOp.opName.tpe"
                   {-# LINE 5516 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _opNameOtpe ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: ScalarExpr.SpecialOp.ann.tpe"
                    {-# LINE 5521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (args_ _argsOcat _argsOdownEnv ) of
              { ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) ->
                  (case (opName_ ) of
                   { ( _opNameIoriginalTree,opName_1) ->
                       (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 5530 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _opNameOcat ->
                        (case (opName_1 _opNameOcat _opNameOtpe ) of
                         { ( _opNameIannotatedTree) ->
                             (case (ann_ ) of
                              { ( _annIoriginalTree,ann_1) ->
                                  (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 5539 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annOcat ->
                                   (case (ann_1 _annOcat _annOtpe ) of
                                    { ( _annIannotatedTree) ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                SpecialOp _annIannotatedTree _opNameIannotatedTree _argsIannotatedTree
                                                {-# LINE 5546 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _annotatedTree ->
                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _annotatedTree
                                                 {-# LINE 5551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _lhsOannotatedTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  SpecialOp _annIoriginalTree _opNameIoriginalTree _argsIoriginalTree
                                                  {-# LINE 5556 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _originalTree ->
                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _originalTree
                                                   {-# LINE 5561 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOoriginalTree ->
                                            (case (({-# LINE 29 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                    error "missing rule: ScalarExpr.SpecialOp.lhs.upType"
                                                    {-# LINE 5566 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOupType ->
                                             ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Star :: T_Annotation  ->
                       T_ScalarExpr 
sem_ScalarExpr_Star ann_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Left []
                 {-# LINE 5577 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 5582 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 5589 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              Star _annIannotatedTree
                              {-# LINE 5596 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 5601 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                Star _annIoriginalTree
                                {-# LINE 5606 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 5611 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 5616 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOupType ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_StringLit :: T_Annotation  ->
                            String ->
                            T_ScalarExpr 
sem_ScalarExpr_StringLit ann_ value_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 77 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Right UnknownType
                 {-# LINE 5628 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 5633 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 5640 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              StringLit _annIannotatedTree value_
                              {-# LINE 5647 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 5652 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                StringLit _annIoriginalTree value_
                                {-# LINE 5657 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 5662 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 5667 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOupType ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_TypedStringLit :: T_Annotation  ->
                                 T_TypeName  ->
                                 String ->
                                 T_ScalarExpr 
sem_ScalarExpr_TypedStringLit ann_ tn_ value_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 5680 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tnOcat ->
          (case (tn_ _tnOcat ) of
           { ( _tnIannotatedTree,_tnInamedType,_tnIoriginalTree) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                       maybe (Left []) Right _tnInamedType
                       {-# LINE 5687 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tpe ->
                (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                        _tpe
                        {-# LINE 5692 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 5699 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    TypedStringLit _annIannotatedTree _tnIannotatedTree value_
                                    {-# LINE 5706 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 5711 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      TypedStringLit _annIoriginalTree _tnIoriginalTree value_
                                      {-# LINE 5716 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 5721 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                        either (const Nothing) Just _tpe
                                        {-# LINE 5726 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOupType ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_WindowApp :: T_Annotation  ->
                            T_ScalarExpr  ->
                            T_ScalarExprList  ->
                            T_ScalarExprDirectionPairList  ->
                            FrameClause ->
                            T_ScalarExpr 
sem_ScalarExpr_WindowApp ann_ fn_ partitionBy_ orderBy_ frm_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 5741 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _orderByOcat ->
          (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 5746 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _partitionByOdownEnv ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5751 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _partitionByOcat ->
            (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    _lhsIdownEnv
                    {-# LINE 5756 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _fnOdownEnv ->
             (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 5761 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _fnOcat ->
              (case (({-# LINE 141 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                      Left []
                      {-# LINE 5766 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tpe ->
               (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                       _tpe
                       {-# LINE 5771 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOtpe ->
                (case (orderBy_ _orderByOcat ) of
                 { ( _orderByIannotatedTree,_orderByIoriginalTree) ->
                     (case (partitionBy_ _partitionByOcat _partitionByOdownEnv ) of
                      { ( _partitionByIannotatedTree,_partitionByIoriginalTree,_partitionByIupTypes) ->
                          (case (fn_ _fnOcat _fnOdownEnv ) of
                           { ( _fnIannotatedTree,_fnIoriginalTree,_fnIupType) ->
                               (case (ann_ ) of
                                { ( _annIoriginalTree,ann_1) ->
                                    (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _lhsIcat
                                            {-# LINE 5784 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annOcat ->
                                     (case (ann_1 _annOcat _annOtpe ) of
                                      { ( _annIannotatedTree) ->
                                          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  WindowApp _annIannotatedTree _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree frm_
                                                  {-# LINE 5791 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _annotatedTree ->
                                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _annotatedTree
                                                   {-# LINE 5796 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOannotatedTree ->
                                            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    WindowApp _annIoriginalTree _fnIoriginalTree _partitionByIoriginalTree _orderByIoriginalTree frm_
                                                    {-# LINE 5801 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _originalTree ->
                                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _originalTree
                                                     {-# LINE 5806 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOoriginalTree ->
                                              (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                      either (const Nothing) Just _tpe
                                                      {-# LINE 5811 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOupType ->
                                               ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: ScalarExprDirectionPair.Tuple.x1.downEnv"
                 {-# LINE 5855 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _x1OdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5860 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _x1Ocat ->
           (case (x1_ _x1Ocat _x1OdownEnv ) of
            { ( _x1IannotatedTree,_x1IoriginalTree,_x1IupType) ->
                (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        (_x1IannotatedTree,x2_)
                        {-# LINE 5867 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annotatedTree ->
                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _annotatedTree
                         {-# LINE 5872 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _lhsOannotatedTree ->
                  (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          (_x1IoriginalTree,x2_)
                          {-# LINE 5877 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _originalTree ->
                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           _originalTree
                           {-# LINE 5882 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _lhsOoriginalTree ->
                    ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 5930 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5935 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 5944 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 5949 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 5954 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 5959 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_ScalarExprDirectionPairList_Nil :: T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 5968 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5973 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5978 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 5983 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- ScalarExprList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         upTypes              : [Maybe Type]
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
                         Environment ->
                         ( ScalarExprList ,ScalarExprList ,([Maybe Type]))
data Inh_ScalarExprList  = Inh_ScalarExprList {cat_Inh_ScalarExprList :: Catalog,downEnv_Inh_ScalarExprList :: Environment}
data Syn_ScalarExprList  = Syn_ScalarExprList {annotatedTree_Syn_ScalarExprList :: ScalarExprList ,originalTree_Syn_ScalarExprList :: ScalarExprList ,upTypes_Syn_ScalarExprList :: ([Maybe Type])}
wrap_ScalarExprList :: T_ScalarExprList  ->
                       Inh_ScalarExprList  ->
                       Syn_ScalarExprList 
wrap_ScalarExprList sem (Inh_ScalarExprList _lhsIcat _lhsIdownEnv )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupTypes) = sem _lhsIcat _lhsIdownEnv 
     in  (Syn_ScalarExprList _lhsOannotatedTree _lhsOoriginalTree _lhsOupTypes ))
sem_ScalarExprList_Cons :: T_ScalarExpr  ->
                           T_ScalarExprList  ->
                           T_ScalarExprList 
sem_ScalarExprList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 6035 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6040 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6045 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOdownEnv ->
            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 6050 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOdownEnv ) of
              { ( _tlIannotatedTree,_tlIoriginalTree,_tlIupTypes) ->
                  (case (hd_ _hdOcat _hdOdownEnv ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree,_hdIupType) ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 6059 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 6064 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 6069 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 6074 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                   _hdIupType : _tlIupTypes
                                   {-# LINE 6079 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOupTypes ->
                            ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupTypes) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExprList_Nil :: T_ScalarExprList 
sem_ScalarExprList_Nil  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 6089 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6094 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 6099 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 6104 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             (case (({-# LINE 38 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     []
                     {-# LINE 6109 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _lhsOupTypes ->
              ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupTypes) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 6157 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: ScalarExprListList.Cons.hd.downEnv"
                  {-# LINE 6162 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOdownEnv ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6167 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOcat ->
            (case (tl_ _tlOcat ) of
             { ( _tlIannotatedTree,_tlIoriginalTree) ->
                 (case (hd_ _hdOcat _hdOdownEnv ) of
                  { ( _hdIannotatedTree,_hdIoriginalTree,_hdIupTypes) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              (:) _hdIannotatedTree _tlIannotatedTree
                              {-# LINE 6176 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 6181 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                (:) _hdIoriginalTree _tlIoriginalTree
                                {-# LINE 6186 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 6191 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_ScalarExprListList_Nil :: T_ScalarExprListList 
sem_ScalarExprListList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 6200 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6205 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 6210 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 6215 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 6259 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _x2Ocat ->
          (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: ScalarExprListStatementListPair.Tuple.x1.downEnv"
                  {-# LINE 6264 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _x1OdownEnv ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6269 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _x1Ocat ->
            (case (x2_ _x2Ocat ) of
             { ( _x2IannotatedTree,_x2IoriginalTree) ->
                 (case (x1_ _x1Ocat _x1OdownEnv ) of
                  { ( _x1IannotatedTree,_x1IoriginalTree,_x1IupTypes) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              (_x1IannotatedTree,_x2IannotatedTree)
                              {-# LINE 6278 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 6283 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                (_x1IoriginalTree,_x2IoriginalTree)
                                {-# LINE 6288 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 6293 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 6341 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6346 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 6355 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 6360 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 6365 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 6370 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_ScalarExprListStatementListPairList_Nil :: T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 6379 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6384 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 6389 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 6394 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: ScalarExprRoot.ScalarExprRoot.expr.downEnv"
                 {-# LINE 6437 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6442 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOcat ->
           (case (expr_ _exprOcat _exprOdownEnv ) of
            { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        ScalarExprRoot _exprIannotatedTree
                        {-# LINE 6449 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annotatedTree ->
                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _annotatedTree
                         {-# LINE 6454 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _lhsOannotatedTree ->
                  (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          ScalarExprRoot _exprIoriginalTree
                          {-# LINE 6459 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _originalTree ->
                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           _originalTree
                           {-# LINE 6464 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _lhsOoriginalTree ->
                    ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 6508 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _x2Ocat ->
          (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: ScalarExprStatementListPair.Tuple.x1.downEnv"
                  {-# LINE 6513 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _x1OdownEnv ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6518 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _x1Ocat ->
            (case (x2_ _x2Ocat ) of
             { ( _x2IannotatedTree,_x2IoriginalTree) ->
                 (case (x1_ _x1Ocat _x1OdownEnv ) of
                  { ( _x1IannotatedTree,_x1IoriginalTree,_x1IupType) ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              (_x1IannotatedTree,_x2IannotatedTree)
                              {-# LINE 6527 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 6532 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                (_x1IoriginalTree,_x2IoriginalTree)
                                {-# LINE 6537 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 6542 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 6590 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6595 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 6604 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 6609 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 6614 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 6619 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_ScalarExprStatementListPairList_Nil :: T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 6628 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6633 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 6638 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 6643 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- SelectItem --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         col                  : Maybe (String,Type)
         originalTree         : SELF 
   alternatives:
      alternative SelExp:
         child ann            : Annotation 
         child ex             : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SelectItem:
         child ann            : Annotation 
         child ex             : ScalarExpr 
         child name           : {NameComponent}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data SelectItem  = SelExp (Annotation ) (ScalarExpr ) 
                 | SelectItem (Annotation ) (ScalarExpr ) (NameComponent) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectItem :: SelectItem  ->
                  T_SelectItem 
sem_SelectItem (SelExp _ann _ex )  =
    (sem_SelectItem_SelExp (sem_Annotation _ann ) (sem_ScalarExpr _ex ) )
sem_SelectItem (SelectItem _ann _ex _name )  =
    (sem_SelectItem_SelectItem (sem_Annotation _ann ) (sem_ScalarExpr _ex ) _name )
-- semantic domain
type T_SelectItem  = Catalog ->
                     Environment ->
                     ( SelectItem ,(Maybe (String,Type)),SelectItem )
data Inh_SelectItem  = Inh_SelectItem {cat_Inh_SelectItem :: Catalog,downEnv_Inh_SelectItem :: Environment}
data Syn_SelectItem  = Syn_SelectItem {annotatedTree_Syn_SelectItem :: SelectItem ,col_Syn_SelectItem :: (Maybe (String,Type)),originalTree_Syn_SelectItem :: SelectItem }
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIcat _lhsIdownEnv )  =
    (let ( _lhsOannotatedTree,_lhsOcol,_lhsOoriginalTree) = sem _lhsIcat _lhsIdownEnv 
     in  (Syn_SelectItem _lhsOannotatedTree _lhsOcol _lhsOoriginalTree ))
sem_SelectItem_SelExp :: T_Annotation  ->
                         T_ScalarExpr  ->
                         T_SelectItem 
sem_SelectItem_SelExp ann_ ex_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 6702 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6707 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOcat ->
           (case (({-# LINE 30 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   Left []
                   {-# LINE 6712 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (ex_ _exOcat _exOdownEnv ) of
             { ( _exIannotatedTree,_exIoriginalTree,_exIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 6721 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    SelExp _annIannotatedTree _exIannotatedTree
                                    {-# LINE 6728 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 6733 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 38 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                      fmap (columnName _exIoriginalTree,) _exIupType
                                      {-# LINE 6738 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOcol ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       SelExp _annIoriginalTree _exIoriginalTree
                                       {-# LINE 6743 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 6748 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOcol,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_SelectItem_SelectItem :: T_Annotation  ->
                             T_ScalarExpr  ->
                             NameComponent ->
                             T_SelectItem 
sem_SelectItem_SelectItem ann_ ex_ name_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 6761 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6766 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOcat ->
           (case (({-# LINE 30 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   Left []
                   {-# LINE 6771 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (ex_ _exOcat _exOdownEnv ) of
             { ( _exIannotatedTree,_exIoriginalTree,_exIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 6780 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    SelectItem _annIannotatedTree _exIannotatedTree name_
                                    {-# LINE 6787 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 6792 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 34 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                      fmap (nm name_,) _exIupType
                                      {-# LINE 6797 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOcol ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       SelectItem _annIoriginalTree _exIoriginalTree name_
                                       {-# LINE 6802 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 6807 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOcol,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
-- SelectItemList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         cols                 : [Maybe (String,Type)]
         originalTree         : SELF 
         upType               : Maybe Type
   alternatives:
      alternative Cons:
         child hd             : SelectItem 
         child tl             : SelectItemList 
         visit 0:
            local annotatedTree : _
            local cols        : _
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
                         Environment ->
                         ( SelectItemList ,([Maybe (String,Type)]),SelectItemList ,(Maybe Type))
data Inh_SelectItemList  = Inh_SelectItemList {cat_Inh_SelectItemList :: Catalog,downEnv_Inh_SelectItemList :: Environment}
data Syn_SelectItemList  = Syn_SelectItemList {annotatedTree_Syn_SelectItemList :: SelectItemList ,cols_Syn_SelectItemList :: ([Maybe (String,Type)]),originalTree_Syn_SelectItemList :: SelectItemList ,upType_Syn_SelectItemList :: (Maybe Type)}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIcat _lhsIdownEnv )  =
    (let ( _lhsOannotatedTree,_lhsOcols,_lhsOoriginalTree,_lhsOupType) = sem _lhsIcat _lhsIdownEnv 
     in  (Syn_SelectItemList _lhsOannotatedTree _lhsOcols _lhsOoriginalTree _lhsOupType ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 7 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 6861 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6866 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 12 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6871 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOdownEnv ->
            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 6876 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOdownEnv ) of
              { ( _tlIannotatedTree,_tlIcols,_tlIoriginalTree,_tlIupType) ->
                  (case (hd_ _hdOcat _hdOdownEnv ) of
                   { ( _hdIannotatedTree,_hdIcol,_hdIoriginalTree) ->
                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 6885 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 6890 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                 _hdIcol : _tlIcols
                                 {-# LINE 6895 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _cols ->
                          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                  _cols
                                  {-# LINE 6900 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOcols ->
                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   (:) _hdIoriginalTree _tlIoriginalTree
                                   {-# LINE 6905 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _originalTree ->
                            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 6910 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                     fmap CompositeType $ sequence _cols
                                     {-# LINE 6915 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupType ->
                              ( _lhsOannotatedTree,_lhsOcols,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 6925 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6930 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 21 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   []
                   {-# LINE 6935 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _lhsOcols ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    []
                    {-# LINE 6940 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _originalTree ->
             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _originalTree
                     {-# LINE 6945 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _lhsOoriginalTree ->
              (case (({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                      Nothing
                      {-# LINE 6950 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _lhsOupType ->
               ( _lhsOannotatedTree,_lhsOcols,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }))
-- SelectList --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         upType               : Maybe Type
   alternatives:
      alternative SelectList:
         child ann            : Annotation 
         child items          : SelectItemList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data SelectList  = SelectList (Annotation ) (SelectItemList ) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _ann _items )  =
    (sem_SelectList_SelectList (sem_Annotation _ann ) (sem_SelectItemList _items ) )
-- semantic domain
type T_SelectList  = Catalog ->
                     Environment ->
                     ( SelectList ,SelectList ,(Maybe Type))
data Inh_SelectList  = Inh_SelectList {cat_Inh_SelectList :: Catalog,downEnv_Inh_SelectList :: Environment}
data Syn_SelectList  = Syn_SelectList {annotatedTree_Syn_SelectList :: SelectList ,originalTree_Syn_SelectList :: SelectList ,upType_Syn_SelectList :: (Maybe Type)}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIcat _lhsIdownEnv )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) = sem _lhsIcat _lhsIdownEnv 
     in  (Syn_SelectList _lhsOannotatedTree _lhsOoriginalTree _lhsOupType ))
sem_SelectList_SelectList :: T_Annotation  ->
                             T_SelectItemList  ->
                             T_SelectList 
sem_SelectList_SelectList ann_ items_  =
    (\ _lhsIcat
       _lhsIdownEnv ->
         (case (({-# LINE 7 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                 _lhsIdownEnv
                 {-# LINE 6999 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _itemsOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7004 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _itemsOcat ->
           (case (({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   Left []
                   {-# LINE 7009 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (items_ _itemsOcat _itemsOdownEnv ) of
             { ( _itemsIannotatedTree,_itemsIcols,_itemsIoriginalTree,_itemsIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 7018 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    SelectList _annIannotatedTree _itemsIannotatedTree
                                    {-# LINE 7025 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 7030 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      SelectList _annIoriginalTree _itemsIoriginalTree
                                      {-# LINE 7035 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 7040 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                (case (({-# LINE 17 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                        _itemsIupType
                                        {-# LINE 7045 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOupType ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }))
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
         child ann            : Annotation 
         child setTargets     : {[NameComponent]}
         child ex             : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SetClause:
         child ann            : Annotation 
         child setTarget      : {NameComponent}
         child ex             : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data SetClause  = MultiSetClause (Annotation ) (([NameComponent])) (ScalarExpr ) 
                | SetClause (Annotation ) (NameComponent) (ScalarExpr ) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SetClause :: SetClause  ->
                 T_SetClause 
sem_SetClause (MultiSetClause _ann _setTargets _ex )  =
    (sem_SetClause_MultiSetClause (sem_Annotation _ann ) _setTargets (sem_ScalarExpr _ex ) )
sem_SetClause (SetClause _ann _setTarget _ex )  =
    (sem_SetClause_SetClause (sem_Annotation _ann ) _setTarget (sem_ScalarExpr _ex ) )
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
sem_SetClause_MultiSetClause :: T_Annotation  ->
                                ([NameComponent]) ->
                                T_ScalarExpr  ->
                                T_SetClause 
sem_SetClause_MultiSetClause ann_ setTargets_ ex_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: SetClause.MultiSetClause.ex.downEnv"
                 {-# LINE 7102 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7107 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: SetClause.MultiSetClause.ann.tpe"
                   {-# LINE 7112 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (ex_ _exOcat _exOdownEnv ) of
             { ( _exIannotatedTree,_exIoriginalTree,_exIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 7121 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    MultiSetClause _annIannotatedTree setTargets_ _exIannotatedTree
                                    {-# LINE 7128 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 7133 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      MultiSetClause _annIoriginalTree setTargets_ _exIoriginalTree
                                      {-# LINE 7138 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 7143 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_SetClause_SetClause :: T_Annotation  ->
                           NameComponent ->
                           T_ScalarExpr  ->
                           T_SetClause 
sem_SetClause_SetClause ann_ setTarget_ ex_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: SetClause.SetClause.ex.downEnv"
                 {-# LINE 7155 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: SetClause.SetClause.ann.tpe"
                   {-# LINE 7165 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (ex_ _exOcat _exOdownEnv ) of
             { ( _exIannotatedTree,_exIoriginalTree,_exIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 7174 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    SetClause _annIannotatedTree setTarget_ _exIannotatedTree
                                    {-# LINE 7181 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 7186 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      SetClause _annIoriginalTree setTarget_ _exIoriginalTree
                                      {-# LINE 7191 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 7196 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 7244 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 7258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 7263 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 7268 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 7273 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 7282 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7287 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 7292 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 7297 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         child ann            : Annotation 
         child name           : Name 
         child ownedBy        : Name 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative AlterTable:
         child ann            : Annotation 
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
         child ann            : Annotation 
         child target         : Name 
         child value          : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Block:
         child ann            : Annotation 
         child lb             : {Maybe String}
         child vars           : VarDefList 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CaseStatement:
         child ann            : Annotation 
         child cases          : ScalarExprListStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CaseStatementSimple:
         child ann            : Annotation 
         child val            : ScalarExpr 
         child cases          : ScalarExprListStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ContinueStatement:
         child ann            : Annotation 
         child lb             : {Maybe String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Copy:
         child ann            : Annotation 
         child table          : Name 
         child targetCols     : {[NameComponent]}
         child source         : {CopySource}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CopyData:
         child ann            : Annotation 
         child insData        : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateDomain:
         child ann            : Annotation 
         child name           : Name 
         child typ            : TypeName 
         child constraintName : {String}
         child check          : MaybeBoolExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateFunction:
         child ann            : Annotation 
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
         child ann            : Annotation 
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateSequence:
         child ann            : Annotation 
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
         child ann            : Annotation 
         child name           : Name 
         child atts           : AttributeDefList 
         child cons           : ConstraintList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateTableAs:
         child ann            : Annotation 
         child name           : Name 
         child expr           : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateTrigger:
         child ann            : Annotation 
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
         child ann            : Annotation 
         child name           : Name 
         child atts           : TypeAttributeDefList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CreateView:
         child ann            : Annotation 
         child name           : Name 
         child colNames       : {MaybeNameComponentList}
         child expr           : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Delete:
         child ann            : Annotation 
         child table          : Name 
         child using          : TableRefList 
         child whr            : MaybeBoolExpr 
         child returning      : MaybeSelectList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative DropFunction:
         child ann            : Annotation 
         child ifE            : {IfExists}
         child sigs           : NameTypeNameListPairList 
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative DropSomething:
         child ann            : Annotation 
         child dropType       : {DropType}
         child ifE            : {IfExists}
         child names          : {[Name]}
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Execute:
         child ann            : Annotation 
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ExitStatement:
         child ann            : Annotation 
         child lb             : {Maybe String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ForIntegerStatement:
         child ann            : Annotation 
         child lb             : {Maybe String}
         child var            : {NameComponent}
         child from           : ScalarExpr 
         child to             : ScalarExpr 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ForQueryStatement:
         child ann            : Annotation 
         child lb             : {Maybe String}
         child var            : {NameComponent}
         child sel            : QueryExpr 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative If:
         child ann            : Annotation 
         child cases          : ScalarExprStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Insert:
         child ann            : Annotation 
         child table          : Name 
         child targetCols     : {[NameComponent]}
         child insData        : QueryExpr 
         child returning      : MaybeSelectList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Into:
         child ann            : Annotation 
         child strict         : {Bool}
         child into           : {[Name]}
         child stmt           : Statement 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative LoopStatement:
         child ann            : Annotation 
         child lb             : {Maybe String}
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Notify:
         child ann            : Annotation 
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative NullStatement:
         child ann            : Annotation 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Perform:
         child ann            : Annotation 
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative QueryStatement:
         child ann            : Annotation 
         child ex             : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Raise:
         child ann            : Annotation 
         child level          : {RaiseType}
         child message        : {String}
         child args           : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Return:
         child ann            : Annotation 
         child value          : MaybeScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ReturnNext:
         child ann            : Annotation 
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ReturnQuery:
         child ann            : Annotation 
         child sel            : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Set:
         child ann            : Annotation 
         child name           : {String}
         child values         : {[SetValue]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Truncate:
         child ann            : Annotation 
         child tables         : {[Name]}
         child restartIdentity : {RestartIdentity}
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Update:
         child ann            : Annotation 
         child table          : Name 
         child assigns        : SetClauseList 
         child fromList       : TableRefList 
         child whr            : MaybeBoolExpr 
         child returning      : MaybeSelectList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative WhileStatement:
         child ann            : Annotation 
         child lb             : {Maybe String}
         child expr           : ScalarExpr 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data Statement  = AlterSequence (Annotation ) (Name ) (Name ) 
                | AlterTable (Annotation ) (Name ) (AlterTableActionList ) 
                | AntiStatement (String) 
                | Assignment (Annotation ) (Name ) (ScalarExpr ) 
                | Block (Annotation ) ((Maybe String)) (VarDefList ) (StatementList ) 
                | CaseStatement (Annotation ) (ScalarExprListStatementListPairList ) (StatementList ) 
                | CaseStatementSimple (Annotation ) (ScalarExpr ) (ScalarExprListStatementListPairList ) (StatementList ) 
                | ContinueStatement (Annotation ) ((Maybe String)) 
                | Copy (Annotation ) (Name ) (([NameComponent])) (CopySource) 
                | CopyData (Annotation ) (String) 
                | CreateDomain (Annotation ) (Name ) (TypeName ) (String) (MaybeBoolExpr ) 
                | CreateFunction (Annotation ) (Name ) (ParamDefList ) (TypeName ) (Replace) (Language) (FnBody ) (Volatility) 
                | CreateLanguage (Annotation ) (String) 
                | CreateSequence (Annotation ) (Name ) (Integer) (Integer) (Integer) (Integer) (Integer) 
                | CreateTable (Annotation ) (Name ) (AttributeDefList ) (ConstraintList ) 
                | CreateTableAs (Annotation ) (Name ) (QueryExpr ) 
                | CreateTrigger (Annotation ) (NameComponent) (TriggerWhen) (([TriggerEvent])) (Name ) (TriggerFire) (Name ) (ScalarExprList ) 
                | CreateType (Annotation ) (Name ) (TypeAttributeDefList ) 
                | CreateView (Annotation ) (Name ) (MaybeNameComponentList) (QueryExpr ) 
                | Delete (Annotation ) (Name ) (TableRefList ) (MaybeBoolExpr ) (MaybeSelectList ) 
                | DropFunction (Annotation ) (IfExists) (NameTypeNameListPairList ) (Cascade) 
                | DropSomething (Annotation ) (DropType) (IfExists) (([Name])) (Cascade) 
                | Execute (Annotation ) (ScalarExpr ) 
                | ExitStatement (Annotation ) ((Maybe String)) 
                | ForIntegerStatement (Annotation ) ((Maybe String)) (NameComponent) (ScalarExpr ) (ScalarExpr ) (StatementList ) 
                | ForQueryStatement (Annotation ) ((Maybe String)) (NameComponent) (QueryExpr ) (StatementList ) 
                | If (Annotation ) (ScalarExprStatementListPairList ) (StatementList ) 
                | Insert (Annotation ) (Name ) (([NameComponent])) (QueryExpr ) (MaybeSelectList ) 
                | Into (Annotation ) (Bool) (([Name])) (Statement ) 
                | LoopStatement (Annotation ) ((Maybe String)) (StatementList ) 
                | Notify (Annotation ) (String) 
                | NullStatement (Annotation ) 
                | Perform (Annotation ) (ScalarExpr ) 
                | QueryStatement (Annotation ) (QueryExpr ) 
                | Raise (Annotation ) (RaiseType) (String) (ScalarExprList ) 
                | Return (Annotation ) (MaybeScalarExpr ) 
                | ReturnNext (Annotation ) (ScalarExpr ) 
                | ReturnQuery (Annotation ) (QueryExpr ) 
                | Set (Annotation ) (String) (([SetValue])) 
                | Truncate (Annotation ) (([Name])) (RestartIdentity) (Cascade) 
                | Update (Annotation ) (Name ) (SetClauseList ) (TableRefList ) (MaybeBoolExpr ) (MaybeSelectList ) 
                | WhileStatement (Annotation ) ((Maybe String)) (ScalarExpr ) (StatementList ) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (AlterSequence _ann _name _ownedBy )  =
    (sem_Statement_AlterSequence (sem_Annotation _ann ) (sem_Name _name ) (sem_Name _ownedBy ) )
sem_Statement (AlterTable _ann _name _actions )  =
    (sem_Statement_AlterTable (sem_Annotation _ann ) (sem_Name _name ) (sem_AlterTableActionList _actions ) )
sem_Statement (AntiStatement _string )  =
    (sem_Statement_AntiStatement _string )
sem_Statement (Assignment _ann _target _value )  =
    (sem_Statement_Assignment (sem_Annotation _ann ) (sem_Name _target ) (sem_ScalarExpr _value ) )
sem_Statement (Block _ann _lb _vars _sts )  =
    (sem_Statement_Block (sem_Annotation _ann ) _lb (sem_VarDefList _vars ) (sem_StatementList _sts ) )
sem_Statement (CaseStatement _ann _cases _els )  =
    (sem_Statement_CaseStatement (sem_Annotation _ann ) (sem_ScalarExprListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (CaseStatementSimple _ann _val _cases _els )  =
    (sem_Statement_CaseStatementSimple (sem_Annotation _ann ) (sem_ScalarExpr _val ) (sem_ScalarExprListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (ContinueStatement _ann _lb )  =
    (sem_Statement_ContinueStatement (sem_Annotation _ann ) _lb )
sem_Statement (Copy _ann _table _targetCols _source )  =
    (sem_Statement_Copy (sem_Annotation _ann ) (sem_Name _table ) _targetCols _source )
sem_Statement (CopyData _ann _insData )  =
    (sem_Statement_CopyData (sem_Annotation _ann ) _insData )
sem_Statement (CreateDomain _ann _name _typ _constraintName _check )  =
    (sem_Statement_CreateDomain (sem_Annotation _ann ) (sem_Name _name ) (sem_TypeName _typ ) _constraintName (sem_MaybeBoolExpr _check ) )
sem_Statement (CreateFunction _ann _name _params _rettype _rep _lang _body _vol )  =
    (sem_Statement_CreateFunction (sem_Annotation _ann ) (sem_Name _name ) (sem_ParamDefList _params ) (sem_TypeName _rettype ) _rep _lang (sem_FnBody _body ) _vol )
sem_Statement (CreateLanguage _ann _name )  =
    (sem_Statement_CreateLanguage (sem_Annotation _ann ) _name )
sem_Statement (CreateSequence _ann _name _incr _min _max _start _cache )  =
    (sem_Statement_CreateSequence (sem_Annotation _ann ) (sem_Name _name ) _incr _min _max _start _cache )
sem_Statement (CreateTable _ann _name _atts _cons )  =
    (sem_Statement_CreateTable (sem_Annotation _ann ) (sem_Name _name ) (sem_AttributeDefList _atts ) (sem_ConstraintList _cons ) )
sem_Statement (CreateTableAs _ann _name _expr )  =
    (sem_Statement_CreateTableAs (sem_Annotation _ann ) (sem_Name _name ) (sem_QueryExpr _expr ) )
sem_Statement (CreateTrigger _ann _name _wh _events _tbl _firing _fnName _fnArgs )  =
    (sem_Statement_CreateTrigger (sem_Annotation _ann ) _name _wh _events (sem_Name _tbl ) _firing (sem_Name _fnName ) (sem_ScalarExprList _fnArgs ) )
sem_Statement (CreateType _ann _name _atts )  =
    (sem_Statement_CreateType (sem_Annotation _ann ) (sem_Name _name ) (sem_TypeAttributeDefList _atts ) )
sem_Statement (CreateView _ann _name _colNames _expr )  =
    (sem_Statement_CreateView (sem_Annotation _ann ) (sem_Name _name ) _colNames (sem_QueryExpr _expr ) )
sem_Statement (Delete _ann _table _using _whr _returning )  =
    (sem_Statement_Delete (sem_Annotation _ann ) (sem_Name _table ) (sem_TableRefList _using ) (sem_MaybeBoolExpr _whr ) (sem_MaybeSelectList _returning ) )
sem_Statement (DropFunction _ann _ifE _sigs _cascade )  =
    (sem_Statement_DropFunction (sem_Annotation _ann ) _ifE (sem_NameTypeNameListPairList _sigs ) _cascade )
sem_Statement (DropSomething _ann _dropType _ifE _names _cascade )  =
    (sem_Statement_DropSomething (sem_Annotation _ann ) _dropType _ifE _names _cascade )
sem_Statement (Execute _ann _expr )  =
    (sem_Statement_Execute (sem_Annotation _ann ) (sem_ScalarExpr _expr ) )
sem_Statement (ExitStatement _ann _lb )  =
    (sem_Statement_ExitStatement (sem_Annotation _ann ) _lb )
sem_Statement (ForIntegerStatement _ann _lb _var _from _to _sts )  =
    (sem_Statement_ForIntegerStatement (sem_Annotation _ann ) _lb _var (sem_ScalarExpr _from ) (sem_ScalarExpr _to ) (sem_StatementList _sts ) )
sem_Statement (ForQueryStatement _ann _lb _var _sel _sts )  =
    (sem_Statement_ForQueryStatement (sem_Annotation _ann ) _lb _var (sem_QueryExpr _sel ) (sem_StatementList _sts ) )
sem_Statement (If _ann _cases _els )  =
    (sem_Statement_If (sem_Annotation _ann ) (sem_ScalarExprStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (Insert _ann _table _targetCols _insData _returning )  =
    (sem_Statement_Insert (sem_Annotation _ann ) (sem_Name _table ) _targetCols (sem_QueryExpr _insData ) (sem_MaybeSelectList _returning ) )
sem_Statement (Into _ann _strict _into _stmt )  =
    (sem_Statement_Into (sem_Annotation _ann ) _strict _into (sem_Statement _stmt ) )
sem_Statement (LoopStatement _ann _lb _sts )  =
    (sem_Statement_LoopStatement (sem_Annotation _ann ) _lb (sem_StatementList _sts ) )
sem_Statement (Notify _ann _name )  =
    (sem_Statement_Notify (sem_Annotation _ann ) _name )
sem_Statement (NullStatement _ann )  =
    (sem_Statement_NullStatement (sem_Annotation _ann ) )
sem_Statement (Perform _ann _expr )  =
    (sem_Statement_Perform (sem_Annotation _ann ) (sem_ScalarExpr _expr ) )
sem_Statement (QueryStatement _ann _ex )  =
    (sem_Statement_QueryStatement (sem_Annotation _ann ) (sem_QueryExpr _ex ) )
sem_Statement (Raise _ann _level _message _args )  =
    (sem_Statement_Raise (sem_Annotation _ann ) _level _message (sem_ScalarExprList _args ) )
sem_Statement (Return _ann _value )  =
    (sem_Statement_Return (sem_Annotation _ann ) (sem_MaybeScalarExpr _value ) )
sem_Statement (ReturnNext _ann _expr )  =
    (sem_Statement_ReturnNext (sem_Annotation _ann ) (sem_ScalarExpr _expr ) )
sem_Statement (ReturnQuery _ann _sel )  =
    (sem_Statement_ReturnQuery (sem_Annotation _ann ) (sem_QueryExpr _sel ) )
sem_Statement (Set _ann _name _values )  =
    (sem_Statement_Set (sem_Annotation _ann ) _name _values )
sem_Statement (Truncate _ann _tables _restartIdentity _cascade )  =
    (sem_Statement_Truncate (sem_Annotation _ann ) _tables _restartIdentity _cascade )
sem_Statement (Update _ann _table _assigns _fromList _whr _returning )  =
    (sem_Statement_Update (sem_Annotation _ann ) (sem_Name _table ) (sem_SetClauseList _assigns ) (sem_TableRefList _fromList ) (sem_MaybeBoolExpr _whr ) (sem_MaybeSelectList _returning ) )
sem_Statement (WhileStatement _ann _lb _expr _sts )  =
    (sem_Statement_WhileStatement (sem_Annotation _ann ) _lb (sem_ScalarExpr _expr ) (sem_StatementList _sts ) )
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
sem_Statement_AlterSequence :: T_Annotation  ->
                               T_Name  ->
                               T_Name  ->
                               T_Statement 
sem_Statement_AlterSequence ann_ name_ ownedBy_  =
    (\ _lhsIcat ->
         (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: Statement.AlterSequence.ownedBy.tpe"
                 {-# LINE 7779 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _ownedByOtpe ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                  error "missing rule: Statement.AlterSequence.name.tpe"
                  {-# LINE 7784 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _nameOtpe ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.AlterSequence.ann.tpe"
                   {-# LINE 7789 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (ownedBy_ ) of
             { ( _ownedByIoriginalTree,ownedBy_1) ->
                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 7796 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _ownedByOcat ->
                  (case (ownedBy_1 _ownedByOcat _ownedByOtpe ) of
                   { ( _ownedByIannotatedTree) ->
                       (case (name_ ) of
                        { ( _nameIoriginalTree,name_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 7805 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _nameOcat ->
                             (case (name_1 _nameOcat _nameOtpe ) of
                              { ( _nameIannotatedTree) ->
                                  (case (ann_ ) of
                                   { ( _annIoriginalTree,ann_1) ->
                                       (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _lhsIcat
                                               {-# LINE 7814 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annOcat ->
                                        (case (ann_1 _annOcat _annOtpe ) of
                                         { ( _annIannotatedTree) ->
                                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     AlterSequence _annIannotatedTree _nameIannotatedTree _ownedByIannotatedTree
                                                     {-# LINE 7821 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _annotatedTree ->
                                              (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _annotatedTree
                                                      {-# LINE 7826 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOannotatedTree ->
                                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                       AlterSequence _annIoriginalTree _nameIoriginalTree _ownedByIoriginalTree
                                                       {-# LINE 7831 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _originalTree ->
                                                (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                        _originalTree
                                                        {-# LINE 7836 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOoriginalTree ->
                                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_AlterTable :: T_Annotation  ->
                            T_Name  ->
                            T_AlterTableActionList  ->
                            T_Statement 
sem_Statement_AlterTable ann_ name_ actions_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 7848 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _actionsOcat ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                  error "missing rule: Statement.AlterTable.name.tpe"
                  {-# LINE 7853 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _nameOtpe ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.AlterTable.ann.tpe"
                   {-# LINE 7858 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (actions_ _actionsOcat ) of
             { ( _actionsIannotatedTree,_actionsIoriginalTree) ->
                 (case (name_ ) of
                  { ( _nameIoriginalTree,name_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 7867 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _nameOcat ->
                       (case (name_1 _nameOcat _nameOtpe ) of
                        { ( _nameIannotatedTree) ->
                            (case (ann_ ) of
                             { ( _annIoriginalTree,ann_1) ->
                                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIcat
                                         {-# LINE 7876 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annOcat ->
                                  (case (ann_1 _annOcat _annOtpe ) of
                                   { ( _annIannotatedTree) ->
                                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               AlterTable _annIannotatedTree _nameIannotatedTree _actionsIannotatedTree
                                               {-# LINE 7883 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annotatedTree ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _annotatedTree
                                                {-# LINE 7888 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOannotatedTree ->
                                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 AlterTable _annIoriginalTree _nameIoriginalTree _actionsIoriginalTree
                                                 {-# LINE 7893 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _originalTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _originalTree
                                                  {-# LINE 7898 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOoriginalTree ->
                                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_AntiStatement :: String ->
                               T_Statement 
sem_Statement_AntiStatement string_  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 AntiStatement string_
                 {-# LINE 7908 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7913 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiStatement string_
                   {-# LINE 7918 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 7923 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
sem_Statement_Assignment :: T_Annotation  ->
                            T_Name  ->
                            T_ScalarExpr  ->
                            T_Statement 
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: Statement.Assignment.value.downEnv"
                 {-# LINE 7935 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _valueOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7940 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _valueOcat ->
           (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.Assignment.target.tpe"
                   {-# LINE 7945 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _targetOtpe ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.Assignment.ann.tpe"
                    {-# LINE 7950 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (value_ _valueOcat _valueOdownEnv ) of
              { ( _valueIannotatedTree,_valueIoriginalTree,_valueIupType) ->
                  (case (target_ ) of
                   { ( _targetIoriginalTree,target_1) ->
                       (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 7959 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _targetOcat ->
                        (case (target_1 _targetOcat _targetOtpe ) of
                         { ( _targetIannotatedTree) ->
                             (case (ann_ ) of
                              { ( _annIoriginalTree,ann_1) ->
                                  (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 7968 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annOcat ->
                                   (case (ann_1 _annOcat _annOtpe ) of
                                    { ( _annIannotatedTree) ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                Assignment _annIannotatedTree _targetIannotatedTree _valueIannotatedTree
                                                {-# LINE 7975 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _annotatedTree ->
                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _annotatedTree
                                                 {-# LINE 7980 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _lhsOannotatedTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  Assignment _annIoriginalTree _targetIoriginalTree _valueIoriginalTree
                                                  {-# LINE 7985 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _originalTree ->
                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _originalTree
                                                   {-# LINE 7990 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOoriginalTree ->
                                            ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Block :: T_Annotation  ->
                       (Maybe String) ->
                       T_VarDefList  ->
                       T_StatementList  ->
                       T_Statement 
sem_Statement_Block ann_ lb_ vars_ sts_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8003 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8008 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _varsOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Block.ann.tpe"
                   {-# LINE 8013 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (sts_ _stsOcat ) of
             { ( _stsIannotatedTree,_stsIoriginalTree) ->
                 (case (vars_ _varsOcat ) of
                  { ( _varsIannotatedTree,_varsIoriginalTree) ->
                      (case (ann_ ) of
                       { ( _annIoriginalTree,ann_1) ->
                           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _lhsIcat
                                   {-# LINE 8024 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annOcat ->
                            (case (ann_1 _annOcat _annOtpe ) of
                             { ( _annIannotatedTree) ->
                                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         Block _annIannotatedTree lb_ _varsIannotatedTree _stsIannotatedTree
                                         {-# LINE 8031 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annotatedTree ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _annotatedTree
                                          {-# LINE 8036 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOannotatedTree ->
                                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           Block _annIoriginalTree lb_ _varsIoriginalTree _stsIoriginalTree
                                           {-# LINE 8041 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _originalTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 8046 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CaseStatement :: T_Annotation  ->
                               T_ScalarExprListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement ann_ cases_ els_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8058 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elsOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8063 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _casesOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CaseStatement.ann.tpe"
                   {-# LINE 8068 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (els_ _elsOcat ) of
             { ( _elsIannotatedTree,_elsIoriginalTree) ->
                 (case (cases_ _casesOcat ) of
                  { ( _casesIannotatedTree,_casesIoriginalTree) ->
                      (case (ann_ ) of
                       { ( _annIoriginalTree,ann_1) ->
                           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _lhsIcat
                                   {-# LINE 8079 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annOcat ->
                            (case (ann_1 _annOcat _annOtpe ) of
                             { ( _annIannotatedTree) ->
                                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         CaseStatement _annIannotatedTree _casesIannotatedTree _elsIannotatedTree
                                         {-# LINE 8086 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annotatedTree ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _annotatedTree
                                          {-# LINE 8091 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOannotatedTree ->
                                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           CaseStatement _annIoriginalTree _casesIoriginalTree _elsIoriginalTree
                                           {-# LINE 8096 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _originalTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 8101 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CaseStatementSimple :: T_Annotation  ->
                                     T_ScalarExpr  ->
                                     T_ScalarExprListStatementListPairList  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_CaseStatementSimple ann_ val_ cases_ els_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8114 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elsOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8119 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _casesOcat ->
           (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.CaseStatementSimple.val.downEnv"
                   {-# LINE 8124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _valOdownEnv ->
            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 8129 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _valOcat ->
             (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     error "missing rule: Statement.CaseStatementSimple.ann.tpe"
                     {-# LINE 8134 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (els_ _elsOcat ) of
               { ( _elsIannotatedTree,_elsIoriginalTree) ->
                   (case (cases_ _casesOcat ) of
                    { ( _casesIannotatedTree,_casesIoriginalTree) ->
                        (case (val_ _valOcat _valOdownEnv ) of
                         { ( _valIannotatedTree,_valIoriginalTree,_valIupType) ->
                             (case (ann_ ) of
                              { ( _annIoriginalTree,ann_1) ->
                                  (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 8147 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annOcat ->
                                   (case (ann_1 _annOcat _annOtpe ) of
                                    { ( _annIannotatedTree) ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                CaseStatementSimple _annIannotatedTree _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                                                {-# LINE 8154 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _annotatedTree ->
                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _annotatedTree
                                                 {-# LINE 8159 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _lhsOannotatedTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  CaseStatementSimple _annIoriginalTree _valIoriginalTree _casesIoriginalTree _elsIoriginalTree
                                                  {-# LINE 8164 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _originalTree ->
                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _originalTree
                                                   {-# LINE 8169 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOoriginalTree ->
                                            ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_ContinueStatement :: T_Annotation  ->
                                   (Maybe String) ->
                                   T_Statement 
sem_Statement_ContinueStatement ann_ lb_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.ContinueStatement.ann.tpe"
                 {-# LINE 8180 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 8187 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             ContinueStatement _annIannotatedTree lb_
                             {-# LINE 8194 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 8199 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               ContinueStatement _annIoriginalTree lb_
                               {-# LINE 8204 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 8209 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_Statement_Copy :: T_Annotation  ->
                      T_Name  ->
                      ([NameComponent]) ->
                      CopySource ->
                      T_Statement 
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIcat ->
         (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: Statement.Copy.table.tpe"
                 {-# LINE 8222 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tableOtpe ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.Copy.ann.tpe"
                  {-# LINE 8227 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (table_ ) of
            { ( _tableIoriginalTree,table_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 8234 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tableOcat ->
                 (case (table_1 _tableOcat _tableOtpe ) of
                  { ( _tableIannotatedTree) ->
                      (case (ann_ ) of
                       { ( _annIoriginalTree,ann_1) ->
                           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _lhsIcat
                                   {-# LINE 8243 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annOcat ->
                            (case (ann_1 _annOcat _annOtpe ) of
                             { ( _annIannotatedTree) ->
                                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         Copy _annIannotatedTree _tableIannotatedTree targetCols_ source_
                                         {-# LINE 8250 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annotatedTree ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _annotatedTree
                                          {-# LINE 8255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOannotatedTree ->
                                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           Copy _annIoriginalTree _tableIoriginalTree targetCols_ source_
                                           {-# LINE 8260 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _originalTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 8265 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CopyData :: T_Annotation  ->
                          String ->
                          T_Statement 
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.CopyData.ann.tpe"
                 {-# LINE 8276 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 8283 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             CopyData _annIannotatedTree insData_
                             {-# LINE 8290 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 8295 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               CopyData _annIoriginalTree insData_
                               {-# LINE 8300 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 8305 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_Statement_CreateDomain :: T_Annotation  ->
                              T_Name  ->
                              T_TypeName  ->
                              String ->
                              T_MaybeBoolExpr  ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ constraintName_ check_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8319 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _checkOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8324 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _typOcat ->
           (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateDomain.name.tpe"
                   {-# LINE 8329 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _nameOtpe ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.CreateDomain.ann.tpe"
                    {-# LINE 8334 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (check_ _checkOcat ) of
              { ( _checkIannotatedTree,_checkIoriginalTree) ->
                  (case (typ_ _typOcat ) of
                   { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                       (case (name_ ) of
                        { ( _nameIoriginalTree,name_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 8345 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _nameOcat ->
                             (case (name_1 _nameOcat _nameOtpe ) of
                              { ( _nameIannotatedTree) ->
                                  (case (ann_ ) of
                                   { ( _annIoriginalTree,ann_1) ->
                                       (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _lhsIcat
                                               {-# LINE 8354 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annOcat ->
                                        (case (ann_1 _annOcat _annOtpe ) of
                                         { ( _annIannotatedTree) ->
                                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     CreateDomain _annIannotatedTree _nameIannotatedTree _typIannotatedTree constraintName_ _checkIannotatedTree
                                                     {-# LINE 8361 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _annotatedTree ->
                                              (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _annotatedTree
                                                      {-# LINE 8366 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOannotatedTree ->
                                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                       CreateDomain _annIoriginalTree _nameIoriginalTree _typIoriginalTree constraintName_ _checkIoriginalTree
                                                       {-# LINE 8371 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _originalTree ->
                                                (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                        _originalTree
                                                        {-# LINE 8376 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOoriginalTree ->
                                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateFunction :: T_Annotation  ->
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8393 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _bodyOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8398 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _rettypeOcat ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8403 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _paramsOcat ->
            (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                    error "missing rule: Statement.CreateFunction.name.tpe"
                    {-# LINE 8408 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _nameOtpe ->
             (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     error "missing rule: Statement.CreateFunction.ann.tpe"
                     {-# LINE 8413 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (body_ _bodyOcat ) of
               { ( _bodyIannotatedTree,_bodyIoriginalTree) ->
                   (case (rettype_ _rettypeOcat ) of
                    { ( _rettypeIannotatedTree,_rettypeInamedType,_rettypeIoriginalTree) ->
                        (case (params_ _paramsOcat ) of
                         { ( _paramsIannotatedTree,_paramsIoriginalTree) ->
                             (case (name_ ) of
                              { ( _nameIoriginalTree,name_1) ->
                                  (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 8426 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _nameOcat ->
                                   (case (name_1 _nameOcat _nameOtpe ) of
                                    { ( _nameIannotatedTree) ->
                                        (case (ann_ ) of
                                         { ( _annIoriginalTree,ann_1) ->
                                             (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _lhsIcat
                                                     {-# LINE 8435 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _annOcat ->
                                              (case (ann_1 _annOcat _annOtpe ) of
                                               { ( _annIannotatedTree) ->
                                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                           CreateFunction _annIannotatedTree _nameIannotatedTree _paramsIannotatedTree _rettypeIannotatedTree rep_ lang_ _bodyIannotatedTree vol_
                                                           {-# LINE 8442 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _annotatedTree ->
                                                    (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                            _annotatedTree
                                                            {-# LINE 8447 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                            )) of
                                                     { _lhsOannotatedTree ->
                                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                             CreateFunction _annIoriginalTree _nameIoriginalTree _paramsIoriginalTree _rettypeIoriginalTree rep_ lang_ _bodyIoriginalTree vol_
                                                             {-# LINE 8452 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                             )) of
                                                      { _originalTree ->
                                                      (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                              _originalTree
                                                              {-# LINE 8457 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                              )) of
                                                       { _lhsOoriginalTree ->
                                                       ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateLanguage :: T_Annotation  ->
                                String ->
                                T_Statement 
sem_Statement_CreateLanguage ann_ name_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.CreateLanguage.ann.tpe"
                 {-# LINE 8468 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 8475 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             CreateLanguage _annIannotatedTree name_
                             {-# LINE 8482 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 8487 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               CreateLanguage _annIoriginalTree name_
                               {-# LINE 8492 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 8497 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_Statement_CreateSequence :: T_Annotation  ->
                                T_Name  ->
                                Integer ->
                                Integer ->
                                Integer ->
                                Integer ->
                                Integer ->
                                T_Statement 
sem_Statement_CreateSequence ann_ name_ incr_ min_ max_ start_ cache_  =
    (\ _lhsIcat ->
         (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: Statement.CreateSequence.name.tpe"
                 {-# LINE 8513 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _nameOtpe ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.CreateSequence.ann.tpe"
                  {-# LINE 8518 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (name_ ) of
            { ( _nameIoriginalTree,name_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 8525 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _nameOcat ->
                 (case (name_1 _nameOcat _nameOtpe ) of
                  { ( _nameIannotatedTree) ->
                      (case (ann_ ) of
                       { ( _annIoriginalTree,ann_1) ->
                           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _lhsIcat
                                   {-# LINE 8534 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annOcat ->
                            (case (ann_1 _annOcat _annOtpe ) of
                             { ( _annIannotatedTree) ->
                                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         CreateSequence _annIannotatedTree _nameIannotatedTree incr_ min_ max_ start_ cache_
                                         {-# LINE 8541 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annotatedTree ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _annotatedTree
                                          {-# LINE 8546 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOannotatedTree ->
                                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           CreateSequence _annIoriginalTree _nameIoriginalTree incr_ min_ max_ start_ cache_
                                           {-# LINE 8551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _originalTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 8556 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateTable :: T_Annotation  ->
                             T_Name  ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8569 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _consOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8574 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _attsOcat ->
           (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateTable.name.tpe"
                   {-# LINE 8579 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _nameOtpe ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.CreateTable.ann.tpe"
                    {-# LINE 8584 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (cons_ _consOcat ) of
              { ( _consIannotatedTree,_consIoriginalTree) ->
                  (case (atts_ _attsOcat ) of
                   { ( _attsIannotatedTree,_attsIoriginalTree) ->
                       (case (name_ ) of
                        { ( _nameIoriginalTree,name_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 8595 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _nameOcat ->
                             (case (name_1 _nameOcat _nameOtpe ) of
                              { ( _nameIannotatedTree) ->
                                  (case (ann_ ) of
                                   { ( _annIoriginalTree,ann_1) ->
                                       (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _lhsIcat
                                               {-# LINE 8604 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annOcat ->
                                        (case (ann_1 _annOcat _annOtpe ) of
                                         { ( _annIannotatedTree) ->
                                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     CreateTable _annIannotatedTree _nameIannotatedTree _attsIannotatedTree _consIannotatedTree
                                                     {-# LINE 8611 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _annotatedTree ->
                                              (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _annotatedTree
                                                      {-# LINE 8616 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOannotatedTree ->
                                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                       CreateTable _annIoriginalTree _nameIoriginalTree _attsIoriginalTree _consIoriginalTree
                                                       {-# LINE 8621 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _originalTree ->
                                                (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                        _originalTree
                                                        {-# LINE 8626 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOoriginalTree ->
                                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateTableAs :: T_Annotation  ->
                               T_Name  ->
                               T_QueryExpr  ->
                               T_Statement 
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8638 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOcat ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                  error "missing rule: Statement.CreateTableAs.name.tpe"
                  {-# LINE 8643 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _nameOtpe ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateTableAs.ann.tpe"
                   {-# LINE 8648 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (expr_ _exprOcat ) of
             { ( _exprIannotatedTree,_exprIoriginalTree) ->
                 (case (name_ ) of
                  { ( _nameIoriginalTree,name_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 8657 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _nameOcat ->
                       (case (name_1 _nameOcat _nameOtpe ) of
                        { ( _nameIannotatedTree) ->
                            (case (ann_ ) of
                             { ( _annIoriginalTree,ann_1) ->
                                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIcat
                                         {-# LINE 8666 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annOcat ->
                                  (case (ann_1 _annOcat _annOtpe ) of
                                   { ( _annIannotatedTree) ->
                                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               CreateTableAs _annIannotatedTree _nameIannotatedTree _exprIannotatedTree
                                               {-# LINE 8673 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annotatedTree ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _annotatedTree
                                                {-# LINE 8678 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOannotatedTree ->
                                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 CreateTableAs _annIoriginalTree _nameIoriginalTree _exprIoriginalTree
                                                 {-# LINE 8683 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _originalTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _originalTree
                                                  {-# LINE 8688 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOoriginalTree ->
                                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateTrigger :: T_Annotation  ->
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
         (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: Statement.CreateTrigger.fnArgs.downEnv"
                 {-# LINE 8705 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _fnArgsOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _fnArgsOcat ->
           (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateTrigger.fnName.tpe"
                   {-# LINE 8715 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _fnNameOtpe ->
            (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                    error "missing rule: Statement.CreateTrigger.tbl.tpe"
                    {-# LINE 8720 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _tblOtpe ->
             (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     error "missing rule: Statement.CreateTrigger.ann.tpe"
                     {-# LINE 8725 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (fnArgs_ _fnArgsOcat _fnArgsOdownEnv ) of
               { ( _fnArgsIannotatedTree,_fnArgsIoriginalTree,_fnArgsIupTypes) ->
                   (case (fnName_ ) of
                    { ( _fnNameIoriginalTree,fnName_1) ->
                        (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 8734 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _fnNameOcat ->
                         (case (fnName_1 _fnNameOcat _fnNameOtpe ) of
                          { ( _fnNameIannotatedTree) ->
                              (case (tbl_ ) of
                               { ( _tblIoriginalTree,tbl_1) ->
                                   (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _lhsIcat
                                           {-# LINE 8743 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _tblOcat ->
                                    (case (tbl_1 _tblOcat _tblOtpe ) of
                                     { ( _tblIannotatedTree) ->
                                         (case (ann_ ) of
                                          { ( _annIoriginalTree,ann_1) ->
                                              (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _lhsIcat
                                                      {-# LINE 8752 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _annOcat ->
                                               (case (ann_1 _annOcat _annOtpe ) of
                                                { ( _annIannotatedTree) ->
                                                    (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                            CreateTrigger _annIannotatedTree name_ wh_ events_ _tblIannotatedTree firing_ _fnNameIannotatedTree _fnArgsIannotatedTree
                                                            {-# LINE 8759 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                            )) of
                                                     { _annotatedTree ->
                                                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                             _annotatedTree
                                                             {-# LINE 8764 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                             )) of
                                                      { _lhsOannotatedTree ->
                                                      (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                              CreateTrigger _annIoriginalTree name_ wh_ events_ _tblIoriginalTree firing_ _fnNameIoriginalTree _fnArgsIoriginalTree
                                                              {-# LINE 8769 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                              )) of
                                                       { _originalTree ->
                                                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                               _originalTree
                                                               {-# LINE 8774 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                               )) of
                                                        { _lhsOoriginalTree ->
                                                        ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateType :: T_Annotation  ->
                            T_Name  ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8786 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _attsOcat ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                  error "missing rule: Statement.CreateType.name.tpe"
                  {-# LINE 8791 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _nameOtpe ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateType.ann.tpe"
                   {-# LINE 8796 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (atts_ _attsOcat ) of
             { ( _attsIannotatedTree,_attsIoriginalTree) ->
                 (case (name_ ) of
                  { ( _nameIoriginalTree,name_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 8805 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _nameOcat ->
                       (case (name_1 _nameOcat _nameOtpe ) of
                        { ( _nameIannotatedTree) ->
                            (case (ann_ ) of
                             { ( _annIoriginalTree,ann_1) ->
                                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIcat
                                         {-# LINE 8814 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annOcat ->
                                  (case (ann_1 _annOcat _annOtpe ) of
                                   { ( _annIannotatedTree) ->
                                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               CreateType _annIannotatedTree _nameIannotatedTree _attsIannotatedTree
                                               {-# LINE 8821 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annotatedTree ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _annotatedTree
                                                {-# LINE 8826 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOannotatedTree ->
                                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 CreateType _annIoriginalTree _nameIoriginalTree _attsIoriginalTree
                                                 {-# LINE 8831 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _originalTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _originalTree
                                                  {-# LINE 8836 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOoriginalTree ->
                                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateView :: T_Annotation  ->
                            T_Name  ->
                            MaybeNameComponentList ->
                            T_QueryExpr  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ colNames_ expr_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8849 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOcat ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                  error "missing rule: Statement.CreateView.name.tpe"
                  {-# LINE 8854 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _nameOtpe ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateView.ann.tpe"
                   {-# LINE 8859 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (expr_ _exprOcat ) of
             { ( _exprIannotatedTree,_exprIoriginalTree) ->
                 (case (name_ ) of
                  { ( _nameIoriginalTree,name_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 8868 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _nameOcat ->
                       (case (name_1 _nameOcat _nameOtpe ) of
                        { ( _nameIannotatedTree) ->
                            (case (ann_ ) of
                             { ( _annIoriginalTree,ann_1) ->
                                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIcat
                                         {-# LINE 8877 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annOcat ->
                                  (case (ann_1 _annOcat _annOtpe ) of
                                   { ( _annIannotatedTree) ->
                                       (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               CreateView _annIannotatedTree _nameIannotatedTree colNames_ _exprIannotatedTree
                                               {-# LINE 8884 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annotatedTree ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _annotatedTree
                                                {-# LINE 8889 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOannotatedTree ->
                                         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 CreateView _annIoriginalTree _nameIoriginalTree colNames_ _exprIoriginalTree
                                                 {-# LINE 8894 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _originalTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _originalTree
                                                  {-# LINE 8899 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOoriginalTree ->
                                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Delete :: T_Annotation  ->
                        T_Name  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Delete ann_ table_ using_ whr_ returning_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8913 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _returningOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8918 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _whrOcat ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8923 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _usingOcat ->
            (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                    error "missing rule: Statement.Delete.table.tpe"
                    {-# LINE 8928 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _tableOtpe ->
             (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     error "missing rule: Statement.Delete.ann.tpe"
                     {-# LINE 8933 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (returning_ _returningOcat ) of
               { ( _returningIannotatedTree,_returningIoriginalTree) ->
                   (case (whr_ _whrOcat ) of
                    { ( _whrIannotatedTree,_whrIoriginalTree) ->
                        (case (using_ _usingOcat ) of
                         { ( _usingIannotatedTree,_usingIoriginalTree,_usingIupEnv) ->
                             (case (table_ ) of
                              { ( _tableIoriginalTree,table_1) ->
                                  (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 8946 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _tableOcat ->
                                   (case (table_1 _tableOcat _tableOtpe ) of
                                    { ( _tableIannotatedTree) ->
                                        (case (ann_ ) of
                                         { ( _annIoriginalTree,ann_1) ->
                                             (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _lhsIcat
                                                     {-# LINE 8955 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _annOcat ->
                                              (case (ann_1 _annOcat _annOtpe ) of
                                               { ( _annIannotatedTree) ->
                                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                           Delete _annIannotatedTree _tableIannotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
                                                           {-# LINE 8962 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _annotatedTree ->
                                                    (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                            _annotatedTree
                                                            {-# LINE 8967 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                            )) of
                                                     { _lhsOannotatedTree ->
                                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                             Delete _annIoriginalTree _tableIoriginalTree _usingIoriginalTree _whrIoriginalTree _returningIoriginalTree
                                                             {-# LINE 8972 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                             )) of
                                                      { _originalTree ->
                                                      (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                              _originalTree
                                                              {-# LINE 8977 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                              )) of
                                                       { _lhsOoriginalTree ->
                                                       ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_DropFunction :: T_Annotation  ->
                              IfExists ->
                              T_NameTypeNameListPairList  ->
                              Cascade ->
                              T_Statement 
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 8990 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _sigsOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.DropFunction.ann.tpe"
                  {-# LINE 8995 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (sigs_ _sigsOcat ) of
            { ( _sigsIannotatedTree,_sigsIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 9004 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   DropFunction _annIannotatedTree ifE_ _sigsIannotatedTree cascade_
                                   {-# LINE 9011 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 9016 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     DropFunction _annIoriginalTree ifE_ _sigsIoriginalTree cascade_
                                     {-# LINE 9021 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 9026 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_Statement_DropSomething :: T_Annotation  ->
                               DropType ->
                               IfExists ->
                               ([Name]) ->
                               Cascade ->
                               T_Statement 
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.DropSomething.ann.tpe"
                 {-# LINE 9040 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 9047 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             DropSomething _annIannotatedTree dropType_ ifE_ names_ cascade_
                             {-# LINE 9054 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 9059 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               DropSomething _annIoriginalTree dropType_ ifE_ names_ cascade_
                               {-# LINE 9064 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 9069 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_Statement_Execute :: T_Annotation  ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: Statement.Execute.expr.downEnv"
                 {-# LINE 9080 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9085 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Execute.ann.tpe"
                   {-# LINE 9090 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (expr_ _exprOcat _exprOdownEnv ) of
             { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 9099 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    Execute _annIannotatedTree _exprIannotatedTree
                                    {-# LINE 9106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 9111 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      Execute _annIoriginalTree _exprIoriginalTree
                                      {-# LINE 9116 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 9121 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_ExitStatement :: T_Annotation  ->
                               (Maybe String) ->
                               T_Statement 
sem_Statement_ExitStatement ann_ lb_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.ExitStatement.ann.tpe"
                 {-# LINE 9132 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 9139 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             ExitStatement _annIannotatedTree lb_
                             {-# LINE 9146 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 9151 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               ExitStatement _annIoriginalTree lb_
                               {-# LINE 9156 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 9161 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_Statement_ForIntegerStatement :: T_Annotation  ->
                                     (Maybe String) ->
                                     NameComponent ->
                                     T_ScalarExpr  ->
                                     T_ScalarExpr  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement ann_ lb_ var_ from_ to_ sts_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 9176 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOcat ->
          (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: Statement.ForIntegerStatement.to.downEnv"
                  {-# LINE 9181 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _toOdownEnv ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9186 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _toOcat ->
            (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    error "missing rule: Statement.ForIntegerStatement.from.downEnv"
                    {-# LINE 9191 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _fromOdownEnv ->
             (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 9196 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _fromOcat ->
              (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      error "missing rule: Statement.ForIntegerStatement.ann.tpe"
                      {-# LINE 9201 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (sts_ _stsOcat ) of
                { ( _stsIannotatedTree,_stsIoriginalTree) ->
                    (case (to_ _toOcat _toOdownEnv ) of
                     { ( _toIannotatedTree,_toIoriginalTree,_toIupType) ->
                         (case (from_ _fromOcat _fromOdownEnv ) of
                          { ( _fromIannotatedTree,_fromIoriginalTree,_fromIupType) ->
                              (case (ann_ ) of
                               { ( _annIoriginalTree,ann_1) ->
                                   (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _lhsIcat
                                           {-# LINE 9214 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annOcat ->
                                    (case (ann_1 _annOcat _annOtpe ) of
                                     { ( _annIannotatedTree) ->
                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 ForIntegerStatement _annIannotatedTree lb_ var_ _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                                                 {-# LINE 9221 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _annotatedTree ->
                                          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _annotatedTree
                                                  {-# LINE 9226 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOannotatedTree ->
                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   ForIntegerStatement _annIoriginalTree lb_ var_ _fromIoriginalTree _toIoriginalTree _stsIoriginalTree
                                                   {-# LINE 9231 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _originalTree ->
                                            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _originalTree
                                                    {-# LINE 9236 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOoriginalTree ->
                                             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_ForQueryStatement :: T_Annotation  ->
                                   (Maybe String) ->
                                   NameComponent ->
                                   T_QueryExpr  ->
                                   T_StatementList  ->
                                   T_Statement 
sem_Statement_ForQueryStatement ann_ lb_ var_ sel_ sts_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 9250 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _selOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.ForQueryStatement.ann.tpe"
                   {-# LINE 9260 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (sts_ _stsOcat ) of
             { ( _stsIannotatedTree,_stsIoriginalTree) ->
                 (case (sel_ _selOcat ) of
                  { ( _selIannotatedTree,_selIoriginalTree) ->
                      (case (ann_ ) of
                       { ( _annIoriginalTree,ann_1) ->
                           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _lhsIcat
                                   {-# LINE 9271 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annOcat ->
                            (case (ann_1 _annOcat _annOtpe ) of
                             { ( _annIannotatedTree) ->
                                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         ForQueryStatement _annIannotatedTree lb_ var_ _selIannotatedTree _stsIannotatedTree
                                         {-# LINE 9278 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annotatedTree ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _annotatedTree
                                          {-# LINE 9283 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOannotatedTree ->
                                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           ForQueryStatement _annIoriginalTree lb_ var_ _selIoriginalTree _stsIoriginalTree
                                           {-# LINE 9288 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _originalTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 9293 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_If :: T_Annotation  ->
                    T_ScalarExprStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 9305 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elsOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9310 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _casesOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.If.ann.tpe"
                   {-# LINE 9315 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (els_ _elsOcat ) of
             { ( _elsIannotatedTree,_elsIoriginalTree) ->
                 (case (cases_ _casesOcat ) of
                  { ( _casesIannotatedTree,_casesIoriginalTree) ->
                      (case (ann_ ) of
                       { ( _annIoriginalTree,ann_1) ->
                           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _lhsIcat
                                   {-# LINE 9326 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annOcat ->
                            (case (ann_1 _annOcat _annOtpe ) of
                             { ( _annIannotatedTree) ->
                                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         If _annIannotatedTree _casesIannotatedTree _elsIannotatedTree
                                         {-# LINE 9333 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annotatedTree ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _annotatedTree
                                          {-# LINE 9338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOannotatedTree ->
                                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           If _annIoriginalTree _casesIoriginalTree _elsIoriginalTree
                                           {-# LINE 9343 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _originalTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 9348 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Insert :: T_Annotation  ->
                        T_Name  ->
                        ([NameComponent]) ->
                        T_QueryExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 9362 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _returningOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9367 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _insDataOcat ->
           (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.Insert.table.tpe"
                   {-# LINE 9372 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tableOtpe ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.Insert.ann.tpe"
                    {-# LINE 9377 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (returning_ _returningOcat ) of
              { ( _returningIannotatedTree,_returningIoriginalTree) ->
                  (case (insData_ _insDataOcat ) of
                   { ( _insDataIannotatedTree,_insDataIoriginalTree) ->
                       (case (table_ ) of
                        { ( _tableIoriginalTree,table_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 9388 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _tableOcat ->
                             (case (table_1 _tableOcat _tableOtpe ) of
                              { ( _tableIannotatedTree) ->
                                  (case (ann_ ) of
                                   { ( _annIoriginalTree,ann_1) ->
                                       (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _lhsIcat
                                               {-# LINE 9397 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annOcat ->
                                        (case (ann_1 _annOcat _annOtpe ) of
                                         { ( _annIannotatedTree) ->
                                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     Insert _annIannotatedTree _tableIannotatedTree targetCols_ _insDataIannotatedTree _returningIannotatedTree
                                                     {-# LINE 9404 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _annotatedTree ->
                                              (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _annotatedTree
                                                      {-# LINE 9409 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOannotatedTree ->
                                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                       Insert _annIoriginalTree _tableIoriginalTree targetCols_ _insDataIoriginalTree _returningIoriginalTree
                                                       {-# LINE 9414 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _originalTree ->
                                                (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                        _originalTree
                                                        {-# LINE 9419 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOoriginalTree ->
                                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Into :: T_Annotation  ->
                      Bool ->
                      ([Name]) ->
                      T_Statement  ->
                      T_Statement 
sem_Statement_Into ann_ strict_ into_ stmt_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 9432 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stmtOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.Into.ann.tpe"
                  {-# LINE 9437 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (stmt_ _stmtOcat ) of
            { ( _stmtIannotatedTree,_stmtIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 9446 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   Into _annIannotatedTree strict_ into_ _stmtIannotatedTree
                                   {-# LINE 9453 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 9458 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     Into _annIoriginalTree strict_ into_ _stmtIoriginalTree
                                     {-# LINE 9463 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 9468 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_Statement_LoopStatement :: T_Annotation  ->
                               (Maybe String) ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_LoopStatement ann_ lb_ sts_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 9480 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.LoopStatement.ann.tpe"
                  {-# LINE 9485 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (sts_ _stsOcat ) of
            { ( _stsIannotatedTree,_stsIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 9494 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   LoopStatement _annIannotatedTree lb_ _stsIannotatedTree
                                   {-# LINE 9501 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 9506 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     LoopStatement _annIoriginalTree lb_ _stsIoriginalTree
                                     {-# LINE 9511 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 9516 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Notify :: T_Annotation  ->
                        String ->
                        T_Statement 
sem_Statement_Notify ann_ name_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.Notify.ann.tpe"
                 {-# LINE 9527 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 9534 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             Notify _annIannotatedTree name_
                             {-# LINE 9541 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 9546 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               Notify _annIoriginalTree name_
                               {-# LINE 9551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 9556 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_Statement_NullStatement :: T_Annotation  ->
                               T_Statement 
sem_Statement_NullStatement ann_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.NullStatement.ann.tpe"
                 {-# LINE 9566 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 9573 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             NullStatement _annIannotatedTree
                             {-# LINE 9580 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 9585 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               NullStatement _annIoriginalTree
                               {-# LINE 9590 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 9595 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_Statement_Perform :: T_Annotation  ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: Statement.Perform.expr.downEnv"
                 {-# LINE 9606 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9611 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Perform.ann.tpe"
                   {-# LINE 9616 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (expr_ _exprOcat _exprOdownEnv ) of
             { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 9625 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    Perform _annIannotatedTree _exprIannotatedTree
                                    {-# LINE 9632 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 9637 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      Perform _annIoriginalTree _exprIoriginalTree
                                      {-# LINE 9642 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 9647 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_QueryStatement :: T_Annotation  ->
                                T_QueryExpr  ->
                                T_Statement 
sem_Statement_QueryStatement ann_ ex_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 9658 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.QueryStatement.ann.tpe"
                  {-# LINE 9663 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ex_ _exOcat ) of
            { ( _exIannotatedTree,_exIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 9672 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   QueryStatement _annIannotatedTree _exIannotatedTree
                                   {-# LINE 9679 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 9684 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     QueryStatement _annIoriginalTree _exIoriginalTree
                                     {-# LINE 9689 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 9694 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Raise :: T_Annotation  ->
                       RaiseType ->
                       String ->
                       T_ScalarExprList  ->
                       T_Statement 
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIcat ->
         (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: Statement.Raise.args.downEnv"
                 {-# LINE 9707 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argsOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9712 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argsOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Raise.ann.tpe"
                   {-# LINE 9717 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (args_ _argsOcat _argsOdownEnv ) of
             { ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 9726 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    Raise _annIannotatedTree level_ message_ _argsIannotatedTree
                                    {-# LINE 9733 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 9738 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      Raise _annIoriginalTree level_ message_ _argsIoriginalTree
                                      {-# LINE 9743 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 9748 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Return :: T_Annotation  ->
                        T_MaybeScalarExpr  ->
                        T_Statement 
sem_Statement_Return ann_ value_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 9759 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _valueOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.Return.ann.tpe"
                  {-# LINE 9764 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (value_ _valueOcat ) of
            { ( _valueIannotatedTree,_valueIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 9773 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   Return _annIannotatedTree _valueIannotatedTree
                                   {-# LINE 9780 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 9785 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     Return _annIoriginalTree _valueIoriginalTree
                                     {-# LINE 9790 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 9795 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_Statement_ReturnNext :: T_Annotation  ->
                            T_ScalarExpr  ->
                            T_Statement 
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: Statement.ReturnNext.expr.downEnv"
                 {-# LINE 9806 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9811 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOcat ->
           (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.ReturnNext.ann.tpe"
                   {-# LINE 9816 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (expr_ _exprOcat _exprOdownEnv ) of
             { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 9825 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    ReturnNext _annIannotatedTree _exprIannotatedTree
                                    {-# LINE 9832 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 9837 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      ReturnNext _annIoriginalTree _exprIoriginalTree
                                      {-# LINE 9842 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _originalTree ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _originalTree
                                       {-# LINE 9847 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOoriginalTree ->
                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_ReturnQuery :: T_Annotation  ->
                             T_QueryExpr  ->
                             T_Statement 
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 9858 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.ReturnQuery.ann.tpe"
                  {-# LINE 9863 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (sel_ _selOcat ) of
            { ( _selIannotatedTree,_selIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 9872 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   ReturnQuery _annIannotatedTree _selIannotatedTree
                                   {-# LINE 9879 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 9884 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     ReturnQuery _annIoriginalTree _selIoriginalTree
                                     {-# LINE 9889 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 9894 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Set :: T_Annotation  ->
                     String ->
                     ([SetValue]) ->
                     T_Statement 
sem_Statement_Set ann_ name_ values_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.Set.ann.tpe"
                 {-# LINE 9906 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 9913 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             Set _annIannotatedTree name_ values_
                             {-# LINE 9920 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 9925 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               Set _annIoriginalTree name_ values_
                               {-# LINE 9930 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 9935 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_Statement_Truncate :: T_Annotation  ->
                          ([Name]) ->
                          RestartIdentity ->
                          Cascade ->
                          T_Statement 
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.Truncate.ann.tpe"
                 {-# LINE 9948 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 9955 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             Truncate _annIannotatedTree tables_ restartIdentity_ cascade_
                             {-# LINE 9962 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 9967 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               Truncate _annIoriginalTree tables_ restartIdentity_ cascade_
                               {-# LINE 9972 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 9977 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_Statement_Update :: T_Annotation  ->
                        T_Name  ->
                        T_SetClauseList  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Update ann_ table_ assigns_ fromList_ whr_ returning_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 9992 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _returningOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9997 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _whrOcat ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10002 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _fromListOcat ->
            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 10007 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _assignsOcat ->
             (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                     error "missing rule: Statement.Update.table.tpe"
                     {-# LINE 10012 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _tableOtpe ->
              (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      error "missing rule: Statement.Update.ann.tpe"
                      {-# LINE 10017 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (returning_ _returningOcat ) of
                { ( _returningIannotatedTree,_returningIoriginalTree) ->
                    (case (whr_ _whrOcat ) of
                     { ( _whrIannotatedTree,_whrIoriginalTree) ->
                         (case (fromList_ _fromListOcat ) of
                          { ( _fromListIannotatedTree,_fromListIoriginalTree,_fromListIupEnv) ->
                              (case (assigns_ _assignsOcat ) of
                               { ( _assignsIannotatedTree,_assignsIoriginalTree) ->
                                   (case (table_ ) of
                                    { ( _tableIoriginalTree,table_1) ->
                                        (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _lhsIcat
                                                {-# LINE 10032 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _tableOcat ->
                                         (case (table_1 _tableOcat _tableOtpe ) of
                                          { ( _tableIannotatedTree) ->
                                              (case (ann_ ) of
                                               { ( _annIoriginalTree,ann_1) ->
                                                   (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                           _lhsIcat
                                                           {-# LINE 10041 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _annOcat ->
                                                    (case (ann_1 _annOcat _annOtpe ) of
                                                     { ( _annIannotatedTree) ->
                                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                 Update _annIannotatedTree _tableIannotatedTree _assignsIannotatedTree _fromListIannotatedTree _whrIannotatedTree _returningIannotatedTree
                                                                 {-# LINE 10048 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                 )) of
                                                          { _annotatedTree ->
                                                          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                  _annotatedTree
                                                                  {-# LINE 10053 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                  )) of
                                                           { _lhsOannotatedTree ->
                                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                   Update _annIoriginalTree _tableIoriginalTree _assignsIoriginalTree _fromListIoriginalTree _whrIoriginalTree _returningIoriginalTree
                                                                   {-# LINE 10058 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                   )) of
                                                            { _originalTree ->
                                                            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                    _originalTree
                                                                    {-# LINE 10063 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                    )) of
                                                             { _lhsOoriginalTree ->
                                                             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_WhileStatement :: T_Annotation  ->
                                (Maybe String) ->
                                T_ScalarExpr  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement ann_ lb_ expr_ sts_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 10076 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOcat ->
          (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: Statement.WhileStatement.expr.downEnv"
                  {-# LINE 10081 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOdownEnv ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10086 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.WhileStatement.ann.tpe"
                    {-# LINE 10091 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (sts_ _stsOcat ) of
              { ( _stsIannotatedTree,_stsIoriginalTree) ->
                  (case (expr_ _exprOcat _exprOdownEnv ) of
                   { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 10102 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOcat ->
                             (case (ann_1 _annOcat _annOtpe ) of
                              { ( _annIannotatedTree) ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          WhileStatement _annIannotatedTree lb_ _exprIannotatedTree _stsIannotatedTree
                                          {-# LINE 10109 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annotatedTree ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _annotatedTree
                                           {-# LINE 10114 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOannotatedTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            WhileStatement _annIoriginalTree lb_ _exprIoriginalTree _stsIoriginalTree
                                            {-# LINE 10119 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _originalTree ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _originalTree
                                             {-# LINE 10124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOoriginalTree ->
                                      ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 10172 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10177 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 10186 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 10191 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 10196 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 10201 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 10210 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10215 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10220 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 10225 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         child ann            : Annotation 
         child tb             : {NameComponent}
         child cols           : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative NoAlias:
         child ann            : Annotation 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative TableAlias:
         child ann            : Annotation 
         child tb             : {NameComponent}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data TableAlias  = FullAlias (Annotation ) (NameComponent) (([NameComponent])) 
                 | NoAlias (Annotation ) 
                 | TableAlias (Annotation ) (NameComponent) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TableAlias :: TableAlias  ->
                  T_TableAlias 
sem_TableAlias (FullAlias _ann _tb _cols )  =
    (sem_TableAlias_FullAlias (sem_Annotation _ann ) _tb _cols )
sem_TableAlias (NoAlias _ann )  =
    (sem_TableAlias_NoAlias (sem_Annotation _ann ) )
sem_TableAlias (TableAlias _ann _tb )  =
    (sem_TableAlias_TableAlias (sem_Annotation _ann ) _tb )
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
sem_TableAlias_FullAlias :: T_Annotation  ->
                            NameComponent ->
                            ([NameComponent]) ->
                            T_TableAlias 
sem_TableAlias_FullAlias ann_ tb_ cols_  =
    (\ _lhsIcat ->
         (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                 Left []
                 {-# LINE 10289 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 10296 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             FullAlias _annIannotatedTree tb_ cols_
                             {-# LINE 10303 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 10308 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               FullAlias _annIoriginalTree tb_ cols_
                               {-# LINE 10313 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 10318 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_TableAlias_NoAlias :: T_Annotation  ->
                          T_TableAlias 
sem_TableAlias_NoAlias ann_  =
    (\ _lhsIcat ->
         (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                 Left []
                 {-# LINE 10328 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 10335 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             NoAlias _annIannotatedTree
                             {-# LINE 10342 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 10347 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               NoAlias _annIoriginalTree
                               {-# LINE 10352 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 10357 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_TableAlias_TableAlias :: T_Annotation  ->
                             NameComponent ->
                             T_TableAlias 
sem_TableAlias_TableAlias ann_ tb_  =
    (\ _lhsIcat ->
         (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                 Left []
                 {-# LINE 10368 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 10375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             TableAlias _annIannotatedTree tb_
                             {-# LINE 10382 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 10387 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               TableAlias _annIoriginalTree tb_
                               {-# LINE 10392 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 10397 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
-- TableRef ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         upEnv                : Environment
   alternatives:
      alternative FunTref:
         child ann            : Annotation 
         child fn             : ScalarExpr 
         child alias          : TableAlias 
         visit 0:
            local eEnv        : {Either [TypeError] Environment}
            local annotatedTree : _
            local originalTree : _
      alternative JoinTref:
         child ann            : Annotation 
         child tbl            : TableRef 
         child nat            : {Natural}
         child joinType       : {JoinType}
         child tbl1           : TableRef 
         child onExpr         : OnExpr 
         child alias          : TableAlias 
         visit 0:
            local eEnv        : {Either [TypeError] Environment}
            local annotatedTree : _
            local originalTree : _
      alternative SubTref:
         child ann            : Annotation 
         child sel            : QueryExpr 
         child alias          : TableAlias 
         visit 0:
            local eEnv        : {Either [TypeError] Environment}
            local annotatedTree : _
            local originalTree : _
      alternative Tref:
         child ann            : Annotation 
         child tbl            : Name 
         child alias          : TableAlias 
         visit 0:
            local eEnv        : {Either [TypeError] Environment}
            local annotatedTree : _
            local originalTree : _
-}
data TableRef  = FunTref (Annotation ) (ScalarExpr ) (TableAlias ) 
               | JoinTref (Annotation ) (TableRef ) (Natural) (JoinType) (TableRef ) (OnExpr ) (TableAlias ) 
               | SubTref (Annotation ) (QueryExpr ) (TableAlias ) 
               | Tref (Annotation ) (Name ) (TableAlias ) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TableRef :: TableRef  ->
                T_TableRef 
sem_TableRef (FunTref _ann _fn _alias )  =
    (sem_TableRef_FunTref (sem_Annotation _ann ) (sem_ScalarExpr _fn ) (sem_TableAlias _alias ) )
sem_TableRef (JoinTref _ann _tbl _nat _joinType _tbl1 _onExpr _alias )  =
    (sem_TableRef_JoinTref (sem_Annotation _ann ) (sem_TableRef _tbl ) _nat _joinType (sem_TableRef _tbl1 ) (sem_OnExpr _onExpr ) (sem_TableAlias _alias ) )
sem_TableRef (SubTref _ann _sel _alias )  =
    (sem_TableRef_SubTref (sem_Annotation _ann ) (sem_QueryExpr _sel ) (sem_TableAlias _alias ) )
sem_TableRef (Tref _ann _tbl _alias )  =
    (sem_TableRef_Tref (sem_Annotation _ann ) (sem_Name _tbl ) (sem_TableAlias _alias ) )
-- semantic domain
type T_TableRef  = Catalog ->
                   ( TableRef ,TableRef ,Environment)
data Inh_TableRef  = Inh_TableRef {cat_Inh_TableRef :: Catalog}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef ,originalTree_Syn_TableRef :: TableRef ,upEnv_Syn_TableRef :: Environment}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) = sem _lhsIcat 
     in  (Syn_TableRef _lhsOannotatedTree _lhsOoriginalTree _lhsOupEnv ))
sem_TableRef_FunTref :: T_Annotation  ->
                        T_ScalarExpr  ->
                        T_TableAlias  ->
                        T_TableRef 
sem_TableRef_FunTref ann_ fn_ alias_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 error "missing rule: TableRef.FunTref.fn.downEnv"
                 {-# LINE 10483 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _fnOdownEnv ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10488 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _fnOcat ->
           (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   Left []
                   {-# LINE 10493 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _eEnv ->
            (case (({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                    either Left (const $ Left []) _eEnv
                    {-# LINE 10498 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 10503 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _aliasOcat ->
              (case (alias_ _aliasOcat ) of
               { ( _aliasIannotatedTree,_aliasIoriginalTree) ->
                   (case (fn_ _fnOcat _fnOdownEnv ) of
                    { ( _fnIannotatedTree,_fnIoriginalTree,_fnIupType) ->
                        (case (ann_ ) of
                         { ( _annIoriginalTree,ann_1) ->
                             (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIcat
                                     {-# LINE 10514 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOcat ->
                              (case (ann_1 _annOcat _annOtpe ) of
                               { ( _annIannotatedTree) ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           FunTref _annIannotatedTree _fnIannotatedTree _aliasIannotatedTree
                                           {-# LINE 10521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annotatedTree ->
                                    (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _annotatedTree
                                            {-# LINE 10526 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOannotatedTree ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             FunTref _annIoriginalTree _fnIoriginalTree _aliasIoriginalTree
                                             {-# LINE 10531 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 10536 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       (case (({-# LINE 20 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                               either (const emptyEnvironment) id _eEnv
                                               {-# LINE 10541 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOupEnv ->
                                        ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TableRef_JoinTref :: T_Annotation  ->
                         T_TableRef  ->
                         Natural ->
                         JoinType ->
                         T_TableRef  ->
                         T_OnExpr  ->
                         T_TableAlias  ->
                         T_TableRef 
sem_TableRef_JoinTref ann_ tbl_ nat_ joinType_ tbl1_ onExpr_ alias_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 10557 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _onExprOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10562 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tbl1Ocat ->
           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10567 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tblOcat ->
            (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                    Left []
                    {-# LINE 10572 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _eEnv ->
             (case (({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                     either Left (const $ Left []) _eEnv
                     {-# LINE 10577 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 10582 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _aliasOcat ->
               (case (alias_ _aliasOcat ) of
                { ( _aliasIannotatedTree,_aliasIoriginalTree) ->
                    (case (onExpr_ _onExprOcat ) of
                     { ( _onExprIannotatedTree,_onExprIoriginalTree) ->
                         (case (tbl1_ _tbl1Ocat ) of
                          { ( _tbl1IannotatedTree,_tbl1IoriginalTree,_tbl1IupEnv) ->
                              (case (tbl_ _tblOcat ) of
                               { ( _tblIannotatedTree,_tblIoriginalTree,_tblIupEnv) ->
                                   (case (ann_ ) of
                                    { ( _annIoriginalTree,ann_1) ->
                                        (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _lhsIcat
                                                {-# LINE 10597 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _annOcat ->
                                         (case (ann_1 _annOcat _annOtpe ) of
                                          { ( _annIannotatedTree) ->
                                              (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      JoinTref _annIannotatedTree _tblIannotatedTree nat_ joinType_ _tbl1IannotatedTree _onExprIannotatedTree _aliasIannotatedTree
                                                      {-# LINE 10604 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _annotatedTree ->
                                               (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                       _annotatedTree
                                                       {-# LINE 10609 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _lhsOannotatedTree ->
                                                (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                        JoinTref _annIoriginalTree _tblIoriginalTree nat_ joinType_ _tbl1IoriginalTree _onExprIoriginalTree _aliasIoriginalTree
                                                        {-# LINE 10614 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _originalTree ->
                                                 (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         _originalTree
                                                         {-# LINE 10619 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _lhsOoriginalTree ->
                                                  (case (({-# LINE 20 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                                          either (const emptyEnvironment) id _eEnv
                                                          {-# LINE 10624 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _lhsOupEnv ->
                                                   ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TableRef_SubTref :: T_Annotation  ->
                        T_QueryExpr  ->
                        T_TableAlias  ->
                        T_TableRef 
sem_TableRef_SubTref ann_ sel_ alias_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 10636 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOcat ->
          (case (({-# LINE 33 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                  Left []
                  {-# LINE 10641 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _eEnv ->
           (case (({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   either Left (const $ Left []) _eEnv
                   {-# LINE 10646 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 10651 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _aliasOcat ->
             (case (alias_ _aliasOcat ) of
              { ( _aliasIannotatedTree,_aliasIoriginalTree) ->
                  (case (sel_ _selOcat ) of
                   { ( _selIannotatedTree,_selIoriginalTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 10662 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOcat ->
                             (case (ann_1 _annOcat _annOtpe ) of
                              { ( _annIannotatedTree) ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          SubTref _annIannotatedTree _selIannotatedTree _aliasIannotatedTree
                                          {-# LINE 10669 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annotatedTree ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _annotatedTree
                                           {-# LINE 10674 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOannotatedTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            SubTref _annIoriginalTree _selIoriginalTree _aliasIoriginalTree
                                            {-# LINE 10679 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _originalTree ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _originalTree
                                             {-# LINE 10684 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOoriginalTree ->
                                      (case (({-# LINE 20 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                              either (const emptyEnvironment) id _eEnv
                                              {-# LINE 10689 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOupEnv ->
                                       ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TableRef_Tref :: T_Annotation  ->
                     T_Name  ->
                     T_TableAlias  ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_ alias_  =
    (\ _lhsIcat ->
         (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                 Left []
                 {-# LINE 10701 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tblOtpe ->
          (case (tbl_ ) of
           { ( _tblIoriginalTree,tbl_1) ->
               (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                       envCreateTrefEnvironment _lhsIcat (nameComponents _tblIoriginalTree)
                       {-# LINE 10708 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _eEnv ->
                (case (({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                        either Left (const $ Left []) _eEnv
                        {-# LINE 10713 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 10718 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _aliasOcat ->
                  (case (alias_ _aliasOcat ) of
                   { ( _aliasIannotatedTree,_aliasIoriginalTree) ->
                       (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 10725 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _tblOcat ->
                        (case (tbl_1 _tblOcat _tblOtpe ) of
                         { ( _tblIannotatedTree) ->
                             (case (ann_ ) of
                              { ( _annIoriginalTree,ann_1) ->
                                  (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 10734 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annOcat ->
                                   (case (ann_1 _annOcat _annOtpe ) of
                                    { ( _annIannotatedTree) ->
                                        (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                Tref _annIannotatedTree _tblIannotatedTree _aliasIannotatedTree
                                                {-# LINE 10741 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _annotatedTree ->
                                         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _annotatedTree
                                                 {-# LINE 10746 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _lhsOannotatedTree ->
                                          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  Tref _annIoriginalTree _tblIoriginalTree _aliasIoriginalTree
                                                  {-# LINE 10751 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _originalTree ->
                                           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _originalTree
                                                   {-# LINE 10756 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOoriginalTree ->
                                            (case (({-# LINE 20 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                                    either (const emptyEnvironment) id _eEnv
                                                    {-# LINE 10761 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOupEnv ->
                                             ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- TableRefList ------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         upEnv                : Environment
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
                       ( TableRefList ,TableRefList ,Environment)
data Inh_TableRefList  = Inh_TableRefList {cat_Inh_TableRefList :: Catalog}
data Syn_TableRefList  = Syn_TableRefList {annotatedTree_Syn_TableRefList :: TableRefList ,originalTree_Syn_TableRefList :: TableRefList ,upEnv_Syn_TableRefList :: Environment}
wrap_TableRefList :: T_TableRefList  ->
                     Inh_TableRefList  ->
                     Syn_TableRefList 
wrap_TableRefList sem (Inh_TableRefList _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) = sem _lhsIcat 
     in  (Syn_TableRefList _lhsOannotatedTree _lhsOoriginalTree _lhsOupEnv ))
sem_TableRefList_Cons :: T_TableRef  ->
                         T_TableRefList  ->
                         T_TableRefList 
sem_TableRefList_Cons hd_ tl_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 10810 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10815 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree,_tlIupEnv) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree,_hdIupEnv) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 10824 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 10829 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 10834 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 10839 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         (case (({-# LINE 12 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                 _hdIupEnv
                                 {-# LINE 10844 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOupEnv ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }) }) }) }) }))
sem_TableRefList_Nil :: T_TableRefList 
sem_TableRefList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 10853 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10858 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10863 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 10868 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             (case (({-# LINE 9 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                     emptyEnvironment
                     {-# LINE 10873 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _lhsOupEnv ->
              ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }))
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
         child ann            : Annotation 
         child name           : {NameComponent}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data TypeAttributeDef  = TypeAttDef (Annotation ) (NameComponent) (TypeName ) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TypeAttributeDef :: TypeAttributeDef  ->
                        T_TypeAttributeDef 
sem_TypeAttributeDef (TypeAttDef _ann _name _typ )  =
    (sem_TypeAttributeDef_TypeAttDef (sem_Annotation _ann ) _name (sem_TypeName _typ ) )
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
sem_TypeAttributeDef_TypeAttDef :: T_Annotation  ->
                                   NameComponent ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef ann_ name_ typ_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 10920 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: TypeAttributeDef.TypeAttDef.ann.tpe"
                  {-# LINE 10925 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (typ_ _typOcat ) of
            { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 10934 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   TypeAttDef _annIannotatedTree name_ _typIannotatedTree
                                   {-# LINE 10941 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 10946 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     TypeAttDef _annIoriginalTree name_ _typIoriginalTree
                                     {-# LINE 10951 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 10956 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 11004 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11009 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 11018 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 11023 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 11028 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 11033 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 11042 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11047 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 11052 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 11057 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- TypeName ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         annotatedTree        : SELF 
         namedType            : Maybe Type
         originalTree         : SELF 
   alternatives:
      alternative ArrayTypeName:
         child ann            : Annotation 
         child typ            : TypeName 
         visit 0:
            local tpe         : {Either [TypeError] Type}
            local annotatedTree : _
            local originalTree : _
      alternative Prec2TypeName:
         child ann            : Annotation 
         child tn             : Name 
         child prec           : {Integer}
         child prec1          : {Integer}
         visit 0:
            local tpe         : {Either [TypeError] Type}
            local annotatedTree : _
            local originalTree : _
      alternative PrecTypeName:
         child ann            : Annotation 
         child tn             : Name 
         child prec           : {Integer}
         visit 0:
            local tpe         : {Either [TypeError] Type}
            local annotatedTree : _
            local originalTree : _
      alternative SetOfTypeName:
         child ann            : Annotation 
         child typ            : TypeName 
         visit 0:
            local tpe         : {Either [TypeError] Type}
            local annotatedTree : _
            local originalTree : _
      alternative SimpleTypeName:
         child ann            : Annotation 
         child tn             : Name 
         visit 0:
            local tpe         : {Either [TypeError] Type}
            local annotatedTree : _
            local originalTree : _
-}
data TypeName  = ArrayTypeName (Annotation ) (TypeName ) 
               | Prec2TypeName (Annotation ) (Name ) (Integer) (Integer) 
               | PrecTypeName (Annotation ) (Name ) (Integer) 
               | SetOfTypeName (Annotation ) (TypeName ) 
               | SimpleTypeName (Annotation ) (Name ) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TypeName :: TypeName  ->
                T_TypeName 
sem_TypeName (ArrayTypeName _ann _typ )  =
    (sem_TypeName_ArrayTypeName (sem_Annotation _ann ) (sem_TypeName _typ ) )
sem_TypeName (Prec2TypeName _ann _tn _prec _prec1 )  =
    (sem_TypeName_Prec2TypeName (sem_Annotation _ann ) (sem_Name _tn ) _prec _prec1 )
sem_TypeName (PrecTypeName _ann _tn _prec )  =
    (sem_TypeName_PrecTypeName (sem_Annotation _ann ) (sem_Name _tn ) _prec )
sem_TypeName (SetOfTypeName _ann _typ )  =
    (sem_TypeName_SetOfTypeName (sem_Annotation _ann ) (sem_TypeName _typ ) )
sem_TypeName (SimpleTypeName _ann _tn )  =
    (sem_TypeName_SimpleTypeName (sem_Annotation _ann ) (sem_Name _tn ) )
-- semantic domain
type T_TypeName  = Catalog ->
                   ( TypeName ,(Maybe Type),TypeName )
data Inh_TypeName  = Inh_TypeName {cat_Inh_TypeName :: Catalog}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName ,namedType_Syn_TypeName :: (Maybe Type),originalTree_Syn_TypeName :: TypeName }
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIcat )  =
    (let ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) = sem _lhsIcat 
     in  (Syn_TypeName _lhsOannotatedTree _lhsOnamedType _lhsOoriginalTree ))
sem_TypeName_ArrayTypeName :: T_Annotation  ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName ann_ typ_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 11147 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (typ_ _typOcat ) of
           { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
               (case (({-# LINE 54 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                       maybe (Left []) Right _typInamedType
                       >>=  Right . ArrayType
                       {-# LINE 11155 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tpe ->
                (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        either Left (const $ Left []) _tpe
                        {-# LINE 11160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 11167 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    ArrayTypeName _annIannotatedTree _typIannotatedTree
                                    {-# LINE 11174 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 11179 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                      either (const Nothing) Just _tpe
                                      {-# LINE 11184 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOnamedType ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       ArrayTypeName _annIoriginalTree _typIoriginalTree
                                       {-# LINE 11189 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 11194 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TypeName_Prec2TypeName :: T_Annotation  ->
                              T_Name  ->
                              Integer ->
                              Integer ->
                              T_TypeName 
sem_TypeName_Prec2TypeName ann_ tn_ prec_ prec1_  =
    (\ _lhsIcat ->
         (case (tn_ ) of
          { ( _tnIoriginalTree,tn_1) ->
              (case (({-# LINE 62 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                      catLookupType _lhsIcat (nameComponents _tnIoriginalTree)
                      {-# LINE 11209 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tpe ->
               (case (({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                       Left []
                       {-# LINE 11214 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tnOtpe ->
                (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        either Left (const $ Left []) _tpe
                        {-# LINE 11219 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 11224 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tnOcat ->
                  (case (tn_1 _tnOcat _tnOtpe ) of
                   { ( _tnIannotatedTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 11233 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOcat ->
                             (case (ann_1 _annOcat _annOtpe ) of
                              { ( _annIannotatedTree) ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          Prec2TypeName _annIannotatedTree _tnIannotatedTree prec_ prec1_
                                          {-# LINE 11240 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annotatedTree ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _annotatedTree
                                           {-# LINE 11245 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOannotatedTree ->
                                    (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                            either (const Nothing) Just _tpe
                                            {-# LINE 11250 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOnamedType ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             Prec2TypeName _annIoriginalTree _tnIoriginalTree prec_ prec1_
                                             {-# LINE 11255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 11260 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TypeName_PrecTypeName :: T_Annotation  ->
                             T_Name  ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName ann_ tn_ prec_  =
    (\ _lhsIcat ->
         (case (tn_ ) of
          { ( _tnIoriginalTree,tn_1) ->
              (case (({-# LINE 60 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                      catLookupType _lhsIcat (nameComponents _tnIoriginalTree)
                      {-# LINE 11274 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tpe ->
               (case (({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                       Left []
                       {-# LINE 11279 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tnOtpe ->
                (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        either Left (const $ Left []) _tpe
                        {-# LINE 11284 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 11289 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tnOcat ->
                  (case (tn_1 _tnOcat _tnOtpe ) of
                   { ( _tnIannotatedTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 11298 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOcat ->
                             (case (ann_1 _annOcat _annOtpe ) of
                              { ( _annIannotatedTree) ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          PrecTypeName _annIannotatedTree _tnIannotatedTree prec_
                                          {-# LINE 11305 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annotatedTree ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _annotatedTree
                                           {-# LINE 11310 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOannotatedTree ->
                                    (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                            either (const Nothing) Just _tpe
                                            {-# LINE 11315 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOnamedType ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             PrecTypeName _annIoriginalTree _tnIoriginalTree prec_
                                             {-# LINE 11320 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 11325 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TypeName_SetOfTypeName :: T_Annotation  ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName ann_ typ_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 11336 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (typ_ _typOcat ) of
           { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
               (case (({-# LINE 57 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                       maybe (Left []) Right _typInamedType
                       >>=  Right . Pseudo . SetOfType
                       {-# LINE 11344 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tpe ->
                (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        either Left (const $ Left []) _tpe
                        {-# LINE 11349 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 11356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOcat ->
                       (case (ann_1 _annOcat _annOtpe ) of
                        { ( _annIannotatedTree) ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    SetOfTypeName _annIannotatedTree _typIannotatedTree
                                    {-# LINE 11363 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annotatedTree ->
                             (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _annotatedTree
                                     {-# LINE 11368 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOannotatedTree ->
                              (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                      either (const Nothing) Just _tpe
                                      {-# LINE 11373 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOnamedType ->
                               (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       SetOfTypeName _annIoriginalTree _typIoriginalTree
                                       {-# LINE 11378 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 11383 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TypeName_SimpleTypeName :: T_Annotation  ->
                               T_Name  ->
                               T_TypeName 
sem_TypeName_SimpleTypeName ann_ tn_  =
    (\ _lhsIcat ->
         (case (tn_ ) of
          { ( _tnIoriginalTree,tn_1) ->
              (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                      catLookupType _lhsIcat (nameComponents _tnIoriginalTree)
                      {-# LINE 11396 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tpe ->
               (case (({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                       Left []
                       {-# LINE 11401 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tnOtpe ->
                (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        either Left (const $ Left []) _tpe
                        {-# LINE 11406 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 11411 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tnOcat ->
                  (case (tn_1 _tnOcat _tnOtpe ) of
                   { ( _tnIannotatedTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIcat
                                    {-# LINE 11420 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOcat ->
                             (case (ann_1 _annOcat _annOtpe ) of
                              { ( _annIannotatedTree) ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          SimpleTypeName _annIannotatedTree _tnIannotatedTree
                                          {-# LINE 11427 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annotatedTree ->
                                   (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _annotatedTree
                                           {-# LINE 11432 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOannotatedTree ->
                                    (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                            either (const Nothing) Just _tpe
                                            {-# LINE 11437 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOnamedType ->
                                     (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             SimpleTypeName _annIoriginalTree _tnIoriginalTree
                                             {-# LINE 11442 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 11447 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 11495 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11500 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdInamedType,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 11509 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 11514 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 11519 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 11524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_TypeNameList_Nil :: T_TypeNameList 
sem_TypeNameList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 11533 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11538 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 11543 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 11548 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         child ann            : Annotation 
         child name           : {NameComponent}
         child i              : {Integer}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative VarAlias:
         child ann            : Annotation 
         child name           : {NameComponent}
         child aliased        : Name 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative VarDef:
         child ann            : Annotation 
         child name           : {NameComponent}
         child typ            : TypeName 
         child value          : {Maybe ScalarExpr}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data VarDef  = ParamAlias (Annotation ) (NameComponent) (Integer) 
             | VarAlias (Annotation ) (NameComponent) (Name ) 
             | VarDef (Annotation ) (NameComponent) (TypeName ) ((Maybe ScalarExpr)) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_VarDef :: VarDef  ->
              T_VarDef 
sem_VarDef (ParamAlias _ann _name _i )  =
    (sem_VarDef_ParamAlias (sem_Annotation _ann ) _name _i )
sem_VarDef (VarAlias _ann _name _aliased )  =
    (sem_VarDef_VarAlias (sem_Annotation _ann ) _name (sem_Name _aliased ) )
sem_VarDef (VarDef _ann _name _typ _value )  =
    (sem_VarDef_VarDef (sem_Annotation _ann ) _name (sem_TypeName _typ ) _value )
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
sem_VarDef_ParamAlias :: T_Annotation  ->
                         NameComponent ->
                         Integer ->
                         T_VarDef 
sem_VarDef_ParamAlias ann_ name_ i_  =
    (\ _lhsIcat ->
         (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: VarDef.ParamAlias.ann.tpe"
                 {-# LINE 11616 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 11623 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOcat ->
                (case (ann_1 _annOcat _annOtpe ) of
                 { ( _annIannotatedTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             ParamAlias _annIannotatedTree name_ i_
                             {-# LINE 11630 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 11635 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               ParamAlias _annIoriginalTree name_ i_
                               {-# LINE 11640 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 11645 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_VarDef_VarAlias :: T_Annotation  ->
                       NameComponent ->
                       T_Name  ->
                       T_VarDef 
sem_VarDef_VarAlias ann_ name_ aliased_  =
    (\ _lhsIcat ->
         (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: VarDef.VarAlias.aliased.tpe"
                 {-# LINE 11657 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _aliasedOtpe ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: VarDef.VarAlias.ann.tpe"
                  {-# LINE 11662 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (aliased_ ) of
            { ( _aliasedIoriginalTree,aliased_1) ->
                (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 11669 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _aliasedOcat ->
                 (case (aliased_1 _aliasedOcat _aliasedOtpe ) of
                  { ( _aliasedIannotatedTree) ->
                      (case (ann_ ) of
                       { ( _annIoriginalTree,ann_1) ->
                           (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _lhsIcat
                                   {-# LINE 11678 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annOcat ->
                            (case (ann_1 _annOcat _annOtpe ) of
                             { ( _annIannotatedTree) ->
                                 (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         VarAlias _annIannotatedTree name_ _aliasedIannotatedTree
                                         {-# LINE 11685 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annotatedTree ->
                                  (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _annotatedTree
                                          {-# LINE 11690 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOannotatedTree ->
                                   (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           VarAlias _annIoriginalTree name_ _aliasedIoriginalTree
                                           {-# LINE 11695 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _originalTree ->
                                    (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 11700 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_VarDef_VarDef :: T_Annotation  ->
                     NameComponent ->
                     T_TypeName  ->
                     (Maybe ScalarExpr) ->
                     T_VarDef 
sem_VarDef_VarDef ann_ name_ typ_ value_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 11713 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: VarDef.VarDef.ann.tpe"
                  {-# LINE 11718 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (typ_ _typOcat ) of
            { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 11727 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   VarDef _annIannotatedTree name_ _typIannotatedTree value_
                                   {-# LINE 11734 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 11739 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     VarDef _annIoriginalTree name_ _typIoriginalTree value_
                                     {-# LINE 11744 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 11749 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 11797 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11802 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 11811 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 11816 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 11821 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 11826 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 11835 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11840 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 11845 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 11850 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
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
         child ann            : Annotation 
         child name           : {NameComponent}
         child colAliases     : {Maybe [NameComponent]}
         child ex             : QueryExpr 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data WithQuery  = WithQuery (Annotation ) (NameComponent) ((Maybe [NameComponent])) (QueryExpr ) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_WithQuery :: WithQuery  ->
                 T_WithQuery 
sem_WithQuery (WithQuery _ann _name _colAliases _ex )  =
    (sem_WithQuery_WithQuery (sem_Annotation _ann ) _name _colAliases (sem_QueryExpr _ex ) )
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
sem_WithQuery_WithQuery :: T_Annotation  ->
                           NameComponent ->
                           (Maybe [NameComponent]) ->
                           T_QueryExpr  ->
                           T_WithQuery 
sem_WithQuery_WithQuery ann_ name_ colAliases_ ex_  =
    (\ _lhsIcat ->
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 11899 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOcat ->
          (case (({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: WithQuery.WithQuery.ann.tpe"
                  {-# LINE 11904 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ex_ _exOcat ) of
            { ( _exIannotatedTree,_exIoriginalTree) ->
                (case (ann_ ) of
                 { ( _annIoriginalTree,ann_1) ->
                     (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIcat
                             {-# LINE 11913 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOcat ->
                      (case (ann_1 _annOcat _annOtpe ) of
                       { ( _annIannotatedTree) ->
                           (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   WithQuery _annIannotatedTree name_ colAliases_ _exIannotatedTree
                                   {-# LINE 11920 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _annotatedTree ->
                            (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _annotatedTree
                                    {-# LINE 11925 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOannotatedTree ->
                             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     WithQuery _annIoriginalTree name_ colAliases_ _exIoriginalTree
                                     {-# LINE 11930 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 11935 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
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
         (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 11983 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 63 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11988 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (tl_ _tlOcat ) of
            { ( _tlIannotatedTree,_tlIoriginalTree) ->
                (case (hd_ _hdOcat ) of
                 { ( _hdIannotatedTree,_hdIoriginalTree) ->
                     (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             (:) _hdIannotatedTree _tlIannotatedTree
                             {-# LINE 11997 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annotatedTree ->
                      (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _annotatedTree
                              {-# LINE 12002 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _lhsOannotatedTree ->
                       (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIoriginalTree _tlIoriginalTree
                               {-# LINE 12007 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _originalTree ->
                        (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _originalTree
                                {-# LINE 12012 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOoriginalTree ->
                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_WithQueryList_Nil :: T_WithQueryList 
sem_WithQueryList_Nil  =
    (\ _lhsIcat ->
         (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 12021 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12026 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 12031 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 12036 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))