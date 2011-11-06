

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
   ,TypeCheckingFlags(..)
   ,defaultTypeCheckingFlags
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
   ,asrc
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

{-# LINE 350 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

nameComponents :: Name -> [NameComponent]
nameComponents (Name _ is) = is
{-# LINE 140 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 409 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data JoinType = Inner | LeftOuter| RightOuter | FullOuter | Cross
                deriving (Show,Eq,Typeable,Data)
{-# LINE 146 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 421 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data CopySource = CopyFilename String
                | Stdin
                  deriving (Show,Eq,Typeable,Data)
{-# LINE 153 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 478 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

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
{-# LINE 170 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 507 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data RaiseType = RNotice | RException | RError
                 deriving (Show,Eq,Typeable,Data)

data CombineType = Except | Union | Intersect | UnionAll
                   deriving (Show,Eq,Typeable,Data)

data Volatility = Volatile | Stable | Immutable
                  deriving (Show,Eq,Typeable,Data)

data Language = Sql | Plpgsql
                deriving (Show,Eq,Typeable,Data)
{-# LINE 185 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 526 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

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

{-# LINE 216 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 642 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

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

{-# LINE 261 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 690 "src/Database/HsSqlPpp/Internals/AstInternal.ag" #-}

data FrameClause = FrameUnboundedPreceding
                 | FrameUnboundedFull
                 | FrameRowsUnboundedPreceding
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 269 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 14 "src/Database/HsSqlPpp/Internals/Annotation.ag" #-}

-- | Represents a source file position, usually set by the parser.
type SourcePosition = (String,Int,Int)

-- | Statement type is used for getting type information for a
-- parameterized statement. The first part is the args that the
-- parameterized statement needs, and the second is the names and types
-- of the output columns. No way to signal that a statement returns
-- exactly one row at the moment
type ParameterizedStatementType = ([Type],[(String,Type)])

{-# LINE 283 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 44 "src/Database/HsSqlPpp/Internals/Annotation.ag" #-}


--some simple wrappers around uniplate for internal use. I'm not sure
--which of these are actually used

-- | An annotation value with no information.
emptyAnnotation :: Annotation
emptyAnnotation = Annotation Nothing Nothing [] Nothing Nothing []

-- | get the annotation for the root element of the tree passed
getAnnotation :: Data a => a -> Annotation
getAnnotation = head . childrenBi

atype :: Annotation -> Maybe Type
atype (Annotation _ a _ _ _ _) = a

setAtype :: Maybe Type -> Annotation -> Annotation
setAtype a (Annotation s _a e i st c) = Annotation s a e i st c

asrc :: Annotation -> Maybe SourcePosition
asrc (Annotation s _ _ _ _ _) = s

setAsrc :: Maybe SourcePosition -> Annotation -> Annotation
setAsrc s (Annotation _s a e i st c) = Annotation s a e i st c


errs :: Annotation -> [TypeError]
errs (Annotation _ _ e _ _ _) = e

setErrs :: [TypeError] -> Annotation -> Annotation
setErrs e (Annotation s a _e i st c) = Annotation s a e i st c

--don't know how to do this one with uniplate

-- | Update the first annotation in a tree using the function supplied
updateAnnotation :: Data a => (Annotation -> Annotation) -> a -> a
updateAnnotation f = gmapT (mkT f)

{-# LINE 324 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 3 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}


-- | some options when typechecking
data TypeCheckingFlags =
    TypeCheckingFlags
    { -- | add qualifiers to unqualified ids where possible
     tcfAddQualifiers :: Bool
     -- | add full aliases to every tableref and subselect
    ,tcfAddFullTablerefAliases :: Bool
     -- | add explicit aliases to all select items
    ,tcfAddSelectItemAliases :: Bool
     -- | expand stars in select lists to explicit list columns
    ,tcfExpandStars :: Bool}


-- | reasonable defaults for type checking, doesn't add anything
-- optional
defaultTypeCheckingFlags :: TypeCheckingFlags
defaultTypeCheckingFlags =
    TypeCheckingFlags
    {tcfAddQualifiers = False
    ,tcfAddFullTablerefAliases = False
    ,tcfAddSelectItemAliases = False
    ,tcfExpandStars = False}

-- | Typechecks the ast, and returns the updated catalog (which
-- includes changes from any ddl statements in the ast).
typeCheckStatements :: TypeCheckingFlags -> Catalog -> [Statement] -> (Catalog,[Statement])
typeCheckStatements f cat sts =
    let t = sem_Root (Root sts)
        ta = wrap_Root t Inh_Root {cat_Inh_Root = cat
                                  ,flags_Inh_Root = f
                                  {-,lib_Inh_Root = emptyBindings
                                  ,idenv_Inh_Root = emptyIDEnv "tcs"-}}
        tl = annotatedTree_Syn_Root ta
        cat1 = cat --producedCat_Syn_Root ta
    in case tl of
         Root r -> (cat1,r)
-- | Typecheck a query expr
typeCheckQueryExpr :: TypeCheckingFlags -> Catalog -> QueryExpr -> QueryExpr
typeCheckQueryExpr f cat qe =
   let (_,[QueryStatement _ qe']) = typeCheckStatements f cat [QueryStatement emptyAnnotation qe]
   in qe'

-- | Not working yet. Typechecks a statement possibly containing ?
-- placeholders. These are annotated with the 'inferred type', and the
-- stType annotation on the return value can be used to get this info
-- easily. Returns Left if the statement is not a query,insert,update or delete
-- statement
typeCheckParameterizedStatement :: TypeCheckingFlags -> Catalog -> Statement -> Either String Statement
typeCheckParameterizedStatement f cat st =
    case st of
      QueryStatement _ _ -> tc
      Insert _ _ _ _ _ -> tc
      Update _ _ _ _ _ _ -> tc
      Delete _ _ _ _ _ -> tc
      _ -> Left "requires select, update, insert or delete statement"
    where
      tc = let tl = typeCheckStatements f cat [st]
           in case tl of
                (_,[st1]) -> Right st1
                _ -> error "impossible happened in typeCheckPS!"


-- | type check a scalar expr
typeCheckScalarExpr :: TypeCheckingFlags -> Catalog -> ScalarExpr -> ScalarExpr
typeCheckScalarExpr f cat ex =
    let t = sem_ScalarExprRoot (ScalarExprRoot ex)
        rt = (annotatedTree_Syn_ScalarExprRoot
              (wrap_ScalarExprRoot t Inh_ScalarExprRoot {cat_Inh_ScalarExprRoot = cat
                                                        ,flags_Inh_ScalarExprRoot = f
                                                        {-,lib_Inh_ScalarExprRoot = emptyBindings
                                                        ,idenv_Inh_ScalarExprRoot = emptyIDEnv "t
cse"-}}))
    in case rt of
         ScalarExprRoot e -> e

{-# LINE 404 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 167 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}

tcAppLike :: Catalog -> Name -> [Maybe Type] -> Either [TypeError] Type
tcAppLike cat anm args = do
  -- get the types of the arguments
  -- then lookup in TypeConversion.matchApp
  tys <- mapM (maybe (Left []) Right) args
  let Name _ ns = anm
  (_,rt) <- matchApp cat ns tys
  return rt

{-# LINE 417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 149 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}


-- | thet name to choose for a column in a select list which doesn't
-- have an explicit name - these are postgresql's rules
columnName :: ScalarExpr -> String
columnName (Identifier _ i) = nm i
columnName (QIdentifier _ is) = nm $ last is
columnName (App _ (Name _ ncs) _) = nm $ last ncs
columnName (Cast _ _ (SimpleTypeName _ (Name _ ncs))) = nm $ last ncs
columnName (WindowApp _ (App _ (Name _ ncs) _) _ _ _) = nm $ last ncs
columnName (AggregateApp _ _ (App _ (Name _ ncs) _) _) = nm $ last ncs
columnName _ = "?column?"
-- quick hack together
nm :: NameComponent -> String
nm (Nmc n) = map toLower n
nm (QNmc n) = n
{-# LINE 436 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
-- AlterTableAction --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                           TypeCheckingFlags ->
                           ( AlterTableAction ,AlterTableAction )
data Inh_AlterTableAction  = Inh_AlterTableAction {cat_Inh_AlterTableAction :: Catalog,flags_Inh_AlterTableAction :: TypeCheckingFlags}
data Syn_AlterTableAction  = Syn_AlterTableAction {annotatedTree_Syn_AlterTableAction :: AlterTableAction ,originalTree_Syn_AlterTableAction :: AlterTableAction }
wrap_AlterTableAction :: T_AlterTableAction  ->
                         Inh_AlterTableAction  ->
                         Syn_AlterTableAction 
wrap_AlterTableAction sem (Inh_AlterTableAction _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_AlterTableAction _lhsOannotatedTree _lhsOoriginalTree ))
sem_AlterTableAction_AddConstraint :: T_Annotation  ->
                                      T_Constraint  ->
                                      T_AlterTableAction 
sem_AlterTableAction_AddConstraint ann_ con_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 491 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _conOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 496 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _conOcat ->
           (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: AlterTableAction.AddConstraint.ann.tpe"
                   {-# LINE 501 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (con_ _conOcat _conOflags ) of
             { ( _conIannotatedTree,_conIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 510 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 515 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     AddConstraint _annIannotatedTree _conIannotatedTree
                                     {-# LINE 522 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 527 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       AddConstraint _annIoriginalTree _conIoriginalTree
                                       {-# LINE 532 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 537 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_AlterTableAction_AlterColumnDefault :: T_Annotation  ->
                                           NameComponent ->
                                           T_ScalarExpr  ->
                                           T_AlterTableAction 
sem_AlterTableAction_AlterColumnDefault ann_ nm_ def_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 550 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _defOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: AlterTableAction.AlterColumnDefault.def.downEnv"
                  {-# LINE 555 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _defOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 560 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _defOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: AlterTableAction.AlterColumnDefault.ann.tpe"
                    {-# LINE 565 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (def_ _defOcat _defOdownEnv _defOflags ) of
              { ( _defIannotatedTree,_defIcolExprs,_defIoriginalTree,_defIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 574 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 579 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      AlterColumnDefault _annIannotatedTree nm_ _defIannotatedTree
                                      {-# LINE 586 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 591 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        AlterColumnDefault _annIoriginalTree nm_ _defIoriginalTree
                                        {-# LINE 596 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 601 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- AlterTableActionList ----------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                               TypeCheckingFlags ->
                               ( AlterTableActionList ,AlterTableActionList )
data Inh_AlterTableActionList  = Inh_AlterTableActionList {cat_Inh_AlterTableActionList :: Catalog,flags_Inh_AlterTableActionList :: TypeCheckingFlags}
data Syn_AlterTableActionList  = Syn_AlterTableActionList {annotatedTree_Syn_AlterTableActionList :: AlterTableActionList ,originalTree_Syn_AlterTableActionList :: AlterTableActionList }
wrap_AlterTableActionList :: T_AlterTableActionList  ->
                             Inh_AlterTableActionList  ->
                             Syn_AlterTableActionList 
wrap_AlterTableActionList sem (Inh_AlterTableActionList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_AlterTableActionList _lhsOannotatedTree _lhsOoriginalTree ))
sem_AlterTableActionList_Cons :: T_AlterTableAction  ->
                                 T_AlterTableActionList  ->
                                 T_AlterTableActionList 
sem_AlterTableActionList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 652 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 657 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 662 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 667 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOflags ) of
              { ( _tlIannotatedTree,_tlIoriginalTree) ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 676 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 681 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 686 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 691 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_AlterTableActionList_Nil :: T_AlterTableActionList 
sem_AlterTableActionList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 701 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 706 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 711 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 716 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
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
         flags                : TypeCheckingFlags
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
                       TypeCheckingFlags ->
                       (Either [TypeError] Type) ->
                       ( Annotation )
data Inh_Annotation  = Inh_Annotation {cat_Inh_Annotation :: Catalog,flags_Inh_Annotation :: TypeCheckingFlags,tpe_Inh_Annotation :: (Either [TypeError] Type)}
data Syn_Annotation  = Syn_Annotation {annotatedTree_Syn_Annotation :: Annotation ,originalTree_Syn_Annotation :: Annotation }
wrap_Annotation :: T_Annotation  ->
                   Inh_Annotation  ->
                   Syn_Annotation 
wrap_Annotation sem (Inh_Annotation _lhsIcat _lhsIflags _lhsItpe )  =
    (let ( _lhsOoriginalTree,sem_1) = sem 
         ( _lhsOannotatedTree) = sem_1 _lhsIcat _lhsIflags _lhsItpe 
     in  (Syn_Annotation _lhsOannotatedTree _lhsOoriginalTree ))
sem_Annotation_Annotation :: (Maybe SourcePosition) ->
                             (Maybe Type) ->
                             ([TypeError]) ->
                             (Maybe Type) ->
                             (Maybe ParameterizedStatementType) ->
                             ([CatalogUpdate]) ->
                             T_Annotation 
sem_Annotation_Annotation asrc_ atype_ errs_ implicitCast_ stType_ catUpd_  =
    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
            Annotation asrc_ atype_ errs_ implicitCast_ stType_ catUpd_
            {-# LINE 775 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
            )) of
     { _originalTree ->
     (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
             _originalTree
             {-# LINE 780 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
             )) of
      { _lhsOoriginalTree ->
      (case ((let sem_Annotation_Annotation_1 :: T_Annotation_1 
                  sem_Annotation_Annotation_1  =
                      (\ _lhsIcat
                         _lhsIflags
                         _lhsItpe ->
                           (case (({-# LINE 103 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   let t = either (const Nothing) Just _lhsItpe
                                       es = either id (const []) _lhsItpe
                                   in Annotation asrc_ t es implicitCast_ stType_ catUpd_
                                   {-# LINE 792 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOannotatedTree ->
                            ( _lhsOannotatedTree) }))
              in  sem_Annotation_Annotation_1)) of
       { ( sem_Annotation_1) ->
       ( _lhsOoriginalTree,sem_Annotation_1) }) }) })
-- AttributeDef ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                       TypeCheckingFlags ->
                       ( AttributeDef ,AttributeDef )
data Inh_AttributeDef  = Inh_AttributeDef {cat_Inh_AttributeDef :: Catalog,flags_Inh_AttributeDef :: TypeCheckingFlags}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef ,originalTree_Syn_AttributeDef :: AttributeDef }
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_AttributeDef _lhsOannotatedTree _lhsOoriginalTree ))
sem_AttributeDef_AttributeDef :: T_Annotation  ->
                                 NameComponent ->
                                 T_TypeName  ->
                                 T_MaybeScalarExpr  ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef ann_ name_ typ_ def_ cons_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 849 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _consOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 854 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _consOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 859 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _defOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 864 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _defOcat ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 869 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _typOcat ->
              (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      error "missing rule: AttributeDef.AttributeDef.ann.tpe"
                      {-# LINE 874 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (cons_ _consOcat _consOflags ) of
                { ( _consIannotatedTree,_consIoriginalTree) ->
                    (case (def_ _defOcat _defOflags ) of
                     { ( _defIannotatedTree,_defIoriginalTree) ->
                         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIflags
                                 {-# LINE 883 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _typOflags ->
                          (case (typ_ _typOcat _typOflags ) of
                           { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                               (case (ann_ ) of
                                { ( _annIoriginalTree,ann_1) ->
                                    (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _lhsIflags
                                            {-# LINE 892 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annOflags ->
                                     (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIcat
                                             {-# LINE 897 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOcat ->
                                      (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                       { ( _annIannotatedTree) ->
                                           (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   AttributeDef _annIannotatedTree name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
                                                   {-# LINE 904 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _annotatedTree ->
                                            (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _annotatedTree
                                                    {-# LINE 909 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOannotatedTree ->
                                             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     AttributeDef _annIoriginalTree name_ _typIoriginalTree _defIoriginalTree _consIoriginalTree
                                                     {-# LINE 914 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _originalTree ->
                                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _originalTree
                                                      {-# LINE 919 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOoriginalTree ->
                                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- AttributeDefList --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                           TypeCheckingFlags ->
                           ( AttributeDefList ,AttributeDefList )
data Inh_AttributeDefList  = Inh_AttributeDefList {cat_Inh_AttributeDefList :: Catalog,flags_Inh_AttributeDefList :: TypeCheckingFlags}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList ,originalTree_Syn_AttributeDefList :: AttributeDefList }
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_AttributeDefList _lhsOannotatedTree _lhsOoriginalTree ))
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 970 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 975 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 980 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 985 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOflags ) of
              { ( _tlIannotatedTree,_tlIoriginalTree) ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 994 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 999 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 1004 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 1009 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 1019 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1024 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1029 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 1034 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- CaseScalarExprListScalarExprPair ----------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
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
                                           Environment ->
                                           TypeCheckingFlags ->
                                           ( CaseScalarExprListScalarExprPair ,CaseScalarExprListScalarExprPair )
data Inh_CaseScalarExprListScalarExprPair  = Inh_CaseScalarExprListScalarExprPair {cat_Inh_CaseScalarExprListScalarExprPair :: Catalog,downEnv_Inh_CaseScalarExprListScalarExprPair :: Environment,flags_Inh_CaseScalarExprListScalarExprPair :: TypeCheckingFlags}
data Syn_CaseScalarExprListScalarExprPair  = Syn_CaseScalarExprListScalarExprPair {annotatedTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair ,originalTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair }
wrap_CaseScalarExprListScalarExprPair :: T_CaseScalarExprListScalarExprPair  ->
                                         Inh_CaseScalarExprListScalarExprPair  ->
                                         Syn_CaseScalarExprListScalarExprPair 
wrap_CaseScalarExprListScalarExprPair sem (Inh_CaseScalarExprListScalarExprPair _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_CaseScalarExprListScalarExprPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_CaseScalarExprListScalarExprPair_Tuple :: T_ScalarExprList  ->
                                              T_ScalarExpr  ->
                                              T_CaseScalarExprListScalarExprPair 
sem_CaseScalarExprListScalarExprPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 1084 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _x2Oflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 1089 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _x2OdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1094 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _x2Ocat ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 1099 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _x1Oflags ->
             (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     _lhsIdownEnv
                     {-# LINE 1104 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _x1OdownEnv ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 1109 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _x1Ocat ->
               (case (x2_ _x2Ocat _x2OdownEnv _x2Oflags ) of
                { ( _x2IannotatedTree,_x2IcolExprs,_x2IoriginalTree,_x2IupType) ->
                    (case (x1_ _x1Ocat _x1OdownEnv _x1Oflags ) of
                     { ( _x1IannotatedTree,_x1IoriginalTree,_x1IupTypes) ->
                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (_x1IannotatedTree,_x2IannotatedTree)
                                 {-# LINE 1118 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annotatedTree ->
                          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _annotatedTree
                                  {-# LINE 1123 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOannotatedTree ->
                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   (_x1IoriginalTree,_x2IoriginalTree)
                                   {-# LINE 1128 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _originalTree ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 1133 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
-- CaseScalarExprListScalarExprPairList ------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
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
                                               Environment ->
                                               TypeCheckingFlags ->
                                               ( CaseScalarExprListScalarExprPairList ,CaseScalarExprListScalarExprPairList )
data Inh_CaseScalarExprListScalarExprPairList  = Inh_CaseScalarExprListScalarExprPairList {cat_Inh_CaseScalarExprListScalarExprPairList :: Catalog,downEnv_Inh_CaseScalarExprListScalarExprPairList :: Environment,flags_Inh_CaseScalarExprListScalarExprPairList :: TypeCheckingFlags}
data Syn_CaseScalarExprListScalarExprPairList  = Syn_CaseScalarExprListScalarExprPairList {annotatedTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList ,originalTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList }
wrap_CaseScalarExprListScalarExprPairList :: T_CaseScalarExprListScalarExprPairList  ->
                                             Inh_CaseScalarExprListScalarExprPairList  ->
                                             Syn_CaseScalarExprListScalarExprPairList 
wrap_CaseScalarExprListScalarExprPairList sem (Inh_CaseScalarExprListScalarExprPairList _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_CaseScalarExprListScalarExprPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_CaseScalarExprListScalarExprPairList_Cons :: T_CaseScalarExprListScalarExprPair  ->
                                                 T_CaseScalarExprListScalarExprPairList  ->
                                                 T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 1187 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 1192 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1197 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tlOcat ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 1202 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOflags ->
             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                     _lhsIdownEnv
                     {-# LINE 1207 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _hdOdownEnv ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 1212 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _hdOcat ->
               (case (tl_ _tlOcat _tlOdownEnv _tlOflags ) of
                { ( _tlIannotatedTree,_tlIoriginalTree) ->
                    (case (hd_ _hdOcat _hdOdownEnv _hdOflags ) of
                     { ( _hdIannotatedTree,_hdIoriginalTree) ->
                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIannotatedTree _tlIannotatedTree
                                 {-# LINE 1221 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annotatedTree ->
                          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _annotatedTree
                                  {-# LINE 1226 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOannotatedTree ->
                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   (:) _hdIoriginalTree _tlIoriginalTree
                                   {-# LINE 1231 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _originalTree ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 1236 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CaseScalarExprListScalarExprPairList_Nil :: T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList_Nil  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 1247 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1252 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1257 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 1262 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- Constraint --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                     TypeCheckingFlags ->
                     ( Constraint ,Constraint )
data Inh_Constraint  = Inh_Constraint {cat_Inh_Constraint :: Catalog,flags_Inh_Constraint :: TypeCheckingFlags}
data Syn_Constraint  = Syn_Constraint {annotatedTree_Syn_Constraint :: Constraint ,originalTree_Syn_Constraint :: Constraint }
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_Constraint _lhsOannotatedTree _lhsOoriginalTree ))
sem_Constraint_CheckConstraint :: T_Annotation  ->
                                  String ->
                                  T_ScalarExpr  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 1346 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: Constraint.CheckConstraint.expr.downEnv"
                  {-# LINE 1351 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Constraint.CheckConstraint.ann.tpe"
                    {-# LINE 1361 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (expr_ _exprOcat _exprOdownEnv _exprOflags ) of
              { ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 1370 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 1375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      CheckConstraint _annIannotatedTree name_ _exprIannotatedTree
                                      {-# LINE 1382 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 1387 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        CheckConstraint _annIoriginalTree name_ _exprIoriginalTree
                                        {-# LINE 1392 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 1397 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Constraint_PrimaryKeyConstraint :: T_Annotation  ->
                                       String ->
                                       ([NameComponent]) ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint ann_ name_ x_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Constraint.PrimaryKeyConstraint.ann.tpe"
                 {-# LINE 1410 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 1417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 1422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              PrimaryKeyConstraint _annIannotatedTree name_ x_
                              {-# LINE 1429 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 1434 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                PrimaryKeyConstraint _annIoriginalTree name_ x_
                                {-# LINE 1439 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 1444 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_Constraint_ReferenceConstraint :: T_Annotation  ->
                                      String ->
                                      ([NameComponent]) ->
                                      T_Name  ->
                                      ([NameComponent]) ->
                                      Cascade ->
                                      Cascade ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: Constraint.ReferenceConstraint.table.tpe"
                 {-# LINE 1461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tableOtpe ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Constraint.ReferenceConstraint.ann.tpe"
                  {-# LINE 1466 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (table_ ) of
            { ( _tableIoriginalTree,table_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 1473 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tableOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 1478 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tableOcat ->
                  (case (table_1 _tableOcat _tableOflags _tableOtpe ) of
                   { ( _tableIannotatedTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIflags
                                    {-# LINE 1487 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOflags ->
                             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIcat
                                     {-# LINE 1492 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOcat ->
                              (case (ann_1 _annOcat _annOflags _annOtpe ) of
                               { ( _annIannotatedTree) ->
                                   (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           ReferenceConstraint _annIannotatedTree name_ atts_ _tableIannotatedTree tableAtts_ onUpdate_ onDelete_
                                           {-# LINE 1499 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annotatedTree ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _annotatedTree
                                            {-# LINE 1504 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOannotatedTree ->
                                     (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             ReferenceConstraint _annIoriginalTree name_ atts_ _tableIoriginalTree tableAtts_ onUpdate_ onDelete_
                                             {-# LINE 1509 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 1514 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Constraint_UniqueConstraint :: T_Annotation  ->
                                   String ->
                                   ([NameComponent]) ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint ann_ name_ x_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Constraint.UniqueConstraint.ann.tpe"
                 {-# LINE 1527 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 1534 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 1539 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              UniqueConstraint _annIannotatedTree name_ x_
                              {-# LINE 1546 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 1551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                UniqueConstraint _annIoriginalTree name_ x_
                                {-# LINE 1556 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 1561 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
-- ConstraintList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                         TypeCheckingFlags ->
                         ( ConstraintList ,ConstraintList )
data Inh_ConstraintList  = Inh_ConstraintList {cat_Inh_ConstraintList :: Catalog,flags_Inh_ConstraintList :: TypeCheckingFlags}
data Syn_ConstraintList  = Syn_ConstraintList {annotatedTree_Syn_ConstraintList :: ConstraintList ,originalTree_Syn_ConstraintList :: ConstraintList }
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_ConstraintList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 1612 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1617 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1622 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 1627 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOflags ) of
              { ( _tlIannotatedTree,_tlIoriginalTree) ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 1636 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 1641 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 1646 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 1651 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 1661 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1666 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1671 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 1676 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- FnBody ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                 TypeCheckingFlags ->
                 ( FnBody ,FnBody )
data Inh_FnBody  = Inh_FnBody {cat_Inh_FnBody :: Catalog,flags_Inh_FnBody :: TypeCheckingFlags}
data Syn_FnBody  = Syn_FnBody {annotatedTree_Syn_FnBody :: FnBody ,originalTree_Syn_FnBody :: FnBody }
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_FnBody _lhsOannotatedTree _lhsOoriginalTree ))
sem_FnBody_PlpgsqlFnBody :: T_Annotation  ->
                            T_Statement  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody ann_ blk_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 1733 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _blkOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1738 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _blkOcat ->
           (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: FnBody.PlpgsqlFnBody.ann.tpe"
                   {-# LINE 1743 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (blk_ _blkOcat _blkOflags ) of
             { ( _blkIannotatedTree,_blkIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 1752 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 1757 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     PlpgsqlFnBody _annIannotatedTree _blkIannotatedTree
                                     {-# LINE 1764 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 1769 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       PlpgsqlFnBody _annIoriginalTree _blkIoriginalTree
                                       {-# LINE 1774 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 1779 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_FnBody_SqlFnBody :: T_Annotation  ->
                        T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody ann_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 1791 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1796 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _stsOcat ->
           (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: FnBody.SqlFnBody.ann.tpe"
                   {-# LINE 1801 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (sts_ _stsOcat _stsOflags ) of
             { ( _stsIannotatedTree,_stsIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 1810 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 1815 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     SqlFnBody _annIannotatedTree _stsIannotatedTree
                                     {-# LINE 1822 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 1827 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       SqlFnBody _annIoriginalTree _stsIoriginalTree
                                       {-# LINE 1832 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 1837 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
-- InList ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative InList:
         child ann            : Annotation 
         child exprs          : ScalarExprList 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative InQueryExpr:
         child ann            : Annotation 
         child sel            : QueryExpr 
         visit 0:
            local tpe         : _
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
                 Environment ->
                 TypeCheckingFlags ->
                 ( InList ,InList )
data Inh_InList  = Inh_InList {cat_Inh_InList :: Catalog,downEnv_Inh_InList :: Environment,flags_Inh_InList :: TypeCheckingFlags}
data Syn_InList  = Syn_InList {annotatedTree_Syn_InList :: InList ,originalTree_Syn_InList :: InList }
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_InList _lhsOannotatedTree _lhsOoriginalTree ))
sem_InList_InList :: T_Annotation  ->
                     T_ScalarExprList  ->
                     T_InList 
sem_InList_InList ann_ exprs_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 1899 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprsOflags ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 1904 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprsOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1909 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprsOcat ->
            (case (({-# LINE 69 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                    Left []
                    {-# LINE 1914 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _tpe ->
             (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _tpe
                     {-# LINE 1919 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (exprs_ _exprsOcat _exprsOdownEnv _exprsOflags ) of
               { ( _exprsIannotatedTree,_exprsIoriginalTree,_exprsIupTypes) ->
                   (case (ann_ ) of
                    { ( _annIoriginalTree,ann_1) ->
                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIflags
                                {-# LINE 1928 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOflags ->
                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIcat
                                 {-# LINE 1933 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annOcat ->
                          (case (ann_1 _annOcat _annOflags _annOtpe ) of
                           { ( _annIannotatedTree) ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       InList _annIannotatedTree _exprsIannotatedTree
                                       {-# LINE 1940 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annotatedTree ->
                                (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _annotatedTree
                                        {-# LINE 1945 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOannotatedTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         InList _annIoriginalTree _exprsIoriginalTree
                                         {-# LINE 1950 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _originalTree
                                          {-# LINE 1955 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOoriginalTree ->
                                   ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_InList_InQueryExpr :: T_Annotation  ->
                          T_QueryExpr  ->
                          T_InList 
sem_InList_InQueryExpr ann_ sel_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 1968 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1973 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _selOcat ->
           (case (({-# LINE 69 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 1978 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tpe ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _tpe
                    {-# LINE 1983 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (({-# LINE 30 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                     Nothing
                     {-# LINE 1988 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _selOouterDownEnv ->
              (case (sel_ _selOcat _selOflags _selOouterDownEnv ) of
               { ( _selIannotatedTree,_selIoriginalTree,_selIupType) ->
                   (case (ann_ ) of
                    { ( _annIoriginalTree,ann_1) ->
                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIflags
                                {-# LINE 1997 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOflags ->
                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIcat
                                 {-# LINE 2002 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annOcat ->
                          (case (ann_1 _annOcat _annOflags _annOtpe ) of
                           { ( _annIannotatedTree) ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       InQueryExpr _annIannotatedTree _selIannotatedTree
                                       {-# LINE 2009 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annotatedTree ->
                                (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _annotatedTree
                                        {-# LINE 2014 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOannotatedTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         InQueryExpr _annIoriginalTree _selIoriginalTree
                                         {-# LINE 2019 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _originalTree
                                          {-# LINE 2024 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOoriginalTree ->
                                   ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- JoinExpr ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative JoinOn:
         child ann            : Annotation 
         child expr           : ScalarExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative JoinUsing:
         child ann            : Annotation 
         child x              : {[NameComponent]}
         visit 0:
            local tpe         : _
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
                   Environment ->
                   TypeCheckingFlags ->
                   ( JoinExpr ,JoinExpr )
data Inh_JoinExpr  = Inh_JoinExpr {cat_Inh_JoinExpr :: Catalog,downEnv_Inh_JoinExpr :: Environment,flags_Inh_JoinExpr :: TypeCheckingFlags}
data Syn_JoinExpr  = Syn_JoinExpr {annotatedTree_Syn_JoinExpr :: JoinExpr ,originalTree_Syn_JoinExpr :: JoinExpr }
wrap_JoinExpr :: T_JoinExpr  ->
                 Inh_JoinExpr  ->
                 Syn_JoinExpr 
wrap_JoinExpr sem (Inh_JoinExpr _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_JoinExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_JoinExpr_JoinOn :: T_Annotation  ->
                       T_ScalarExpr  ->
                       T_JoinExpr 
sem_JoinExpr_JoinOn ann_ expr_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 2086 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 2091 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2096 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (({-# LINE 73 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                    Left []
                    {-# LINE 2101 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _tpe ->
             (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _tpe
                     {-# LINE 2106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (expr_ _exprOcat _exprOdownEnv _exprOflags ) of
               { ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) ->
                   (case (ann_ ) of
                    { ( _annIoriginalTree,ann_1) ->
                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIflags
                                {-# LINE 2115 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOflags ->
                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIcat
                                 {-# LINE 2120 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annOcat ->
                          (case (ann_1 _annOcat _annOflags _annOtpe ) of
                           { ( _annIannotatedTree) ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       JoinOn _annIannotatedTree _exprIannotatedTree
                                       {-# LINE 2127 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annotatedTree ->
                                (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _annotatedTree
                                        {-# LINE 2132 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOannotatedTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         JoinOn _annIoriginalTree _exprIoriginalTree
                                         {-# LINE 2137 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _originalTree
                                          {-# LINE 2142 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOoriginalTree ->
                                   ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_JoinExpr_JoinUsing :: T_Annotation  ->
                          ([NameComponent]) ->
                          T_JoinExpr 
sem_JoinExpr_JoinUsing ann_ x_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 73 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                 Left []
                 {-# LINE 2155 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _tpe
                  {-# LINE 2160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 2167 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 2172 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               JoinUsing _annIannotatedTree x_
                               {-# LINE 2179 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 2184 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 JoinUsing _annIoriginalTree x_
                                 {-# LINE 2189 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 2194 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
-- MaybeBoolExpr -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
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
                        Environment ->
                        TypeCheckingFlags ->
                        ( MaybeBoolExpr ,MaybeBoolExpr )
data Inh_MaybeBoolExpr  = Inh_MaybeBoolExpr {cat_Inh_MaybeBoolExpr :: Catalog,downEnv_Inh_MaybeBoolExpr :: Environment,flags_Inh_MaybeBoolExpr :: TypeCheckingFlags}
data Syn_MaybeBoolExpr  = Syn_MaybeBoolExpr {annotatedTree_Syn_MaybeBoolExpr :: MaybeBoolExpr ,originalTree_Syn_MaybeBoolExpr :: MaybeBoolExpr }
wrap_MaybeBoolExpr :: T_MaybeBoolExpr  ->
                      Inh_MaybeBoolExpr  ->
                      Syn_MaybeBoolExpr 
wrap_MaybeBoolExpr sem (Inh_MaybeBoolExpr _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_MaybeBoolExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_MaybeBoolExpr_Just :: T_ScalarExpr  ->
                          T_MaybeBoolExpr 
sem_MaybeBoolExpr_Just just_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 2248 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _justOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 2253 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _justOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _justOcat ->
            (case (just_ _justOcat _justOdownEnv _justOflags ) of
             { ( _justIannotatedTree,_justIcolExprs,_justIoriginalTree,_justIupType) ->
                 (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         Just _justIannotatedTree
                         {-# LINE 2265 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annotatedTree ->
                  (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _annotatedTree
                          {-# LINE 2270 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _lhsOannotatedTree ->
                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           Just _justIoriginalTree
                           {-# LINE 2275 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _originalTree ->
                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                            _originalTree
                            {-# LINE 2280 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _lhsOoriginalTree ->
                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_MaybeBoolExpr_Nothing :: T_MaybeBoolExpr 
sem_MaybeBoolExpr_Nothing  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 Nothing
                 {-# LINE 2291 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2296 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2301 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 2306 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
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
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                          TypeCheckingFlags ->
                          ( MaybeScalarExpr ,MaybeScalarExpr )
data Inh_MaybeScalarExpr  = Inh_MaybeScalarExpr {cat_Inh_MaybeScalarExpr :: Catalog,flags_Inh_MaybeScalarExpr :: TypeCheckingFlags}
data Syn_MaybeScalarExpr  = Syn_MaybeScalarExpr {annotatedTree_Syn_MaybeScalarExpr :: MaybeScalarExpr ,originalTree_Syn_MaybeScalarExpr :: MaybeScalarExpr }
wrap_MaybeScalarExpr :: T_MaybeScalarExpr  ->
                        Inh_MaybeScalarExpr  ->
                        Syn_MaybeScalarExpr 
wrap_MaybeScalarExpr sem (Inh_MaybeScalarExpr _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_MaybeScalarExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_MaybeScalarExpr_Just :: T_ScalarExpr  ->
                            T_MaybeScalarExpr 
sem_MaybeScalarExpr_Just just_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 2389 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _justOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: MaybeScalarExpr.Just.just.downEnv"
                  {-# LINE 2394 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _justOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _justOcat ->
            (case (just_ _justOcat _justOdownEnv _justOflags ) of
             { ( _justIannotatedTree,_justIcolExprs,_justIoriginalTree,_justIupType) ->
                 (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         Just _justIannotatedTree
                         {-# LINE 2406 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annotatedTree ->
                  (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _annotatedTree
                          {-# LINE 2411 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _lhsOannotatedTree ->
                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           Just _justIoriginalTree
                           {-# LINE 2416 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _originalTree ->
                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                            _originalTree
                            {-# LINE 2421 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _lhsOoriginalTree ->
                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_MaybeScalarExpr_Nothing :: T_MaybeScalarExpr 
sem_MaybeScalarExpr_Nothing  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 Nothing
                 {-# LINE 2431 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2436 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2441 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 2446 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- MaybeSelectList ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                          TypeCheckingFlags ->
                          ( MaybeSelectList ,MaybeSelectList )
data Inh_MaybeSelectList  = Inh_MaybeSelectList {cat_Inh_MaybeSelectList :: Catalog,flags_Inh_MaybeSelectList :: TypeCheckingFlags}
data Syn_MaybeSelectList  = Syn_MaybeSelectList {annotatedTree_Syn_MaybeSelectList :: MaybeSelectList ,originalTree_Syn_MaybeSelectList :: MaybeSelectList }
wrap_MaybeSelectList :: T_MaybeSelectList  ->
                        Inh_MaybeSelectList  ->
                        Syn_MaybeSelectList 
wrap_MaybeSelectList sem (Inh_MaybeSelectList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_MaybeSelectList _lhsOannotatedTree _lhsOoriginalTree ))
sem_MaybeSelectList_Just :: T_SelectList  ->
                            T_MaybeSelectList 
sem_MaybeSelectList_Just just_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 2497 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _justOflags ->
          (case (({-# LINE 115 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                  error "missing rule: MaybeSelectList.Just.just.downEnv"
                  {-# LINE 2502 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _justOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2507 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _justOcat ->
            (case (just_ _justOcat _justOdownEnv _justOflags ) of
             { ( _justIannotatedTree,_justIcolExprs,_justIoriginalTree,_justIupEnv,_justIupType) ->
                 (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         Just _justIannotatedTree
                         {-# LINE 2514 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annotatedTree ->
                  (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _annotatedTree
                          {-# LINE 2519 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _lhsOannotatedTree ->
                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           Just _justIoriginalTree
                           {-# LINE 2524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _originalTree ->
                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                            _originalTree
                            {-# LINE 2529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _lhsOoriginalTree ->
                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_MaybeSelectList_Nothing :: T_MaybeSelectList 
sem_MaybeSelectList_Nothing  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 Nothing
                 {-# LINE 2539 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2544 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2549 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 2554 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
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
         flags                : TypeCheckingFlags
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
                 TypeCheckingFlags ->
                 (Either [TypeError] Type) ->
                 ( Name )
data Inh_Name  = Inh_Name {cat_Inh_Name :: Catalog,flags_Inh_Name :: TypeCheckingFlags,tpe_Inh_Name :: (Either [TypeError] Type)}
data Syn_Name  = Syn_Name {annotatedTree_Syn_Name :: Name ,originalTree_Syn_Name :: Name }
wrap_Name :: T_Name  ->
             Inh_Name  ->
             Syn_Name 
wrap_Name sem (Inh_Name _lhsIcat _lhsIflags _lhsItpe )  =
    (let ( _lhsOoriginalTree,sem_1) = sem 
         ( _lhsOannotatedTree) = sem_1 _lhsIcat _lhsIflags _lhsItpe 
     in  (Syn_Name _lhsOannotatedTree _lhsOoriginalTree ))
sem_Name_Name :: T_Annotation  ->
                 ([NameComponent]) ->
                 T_Name 
sem_Name_Name ann_ is_  =
    (case (ann_ ) of
     { ( _annIoriginalTree,ann_1) ->
         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 Name _annIoriginalTree is_
                 {-# LINE 2609 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _originalTree ->
          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2614 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOoriginalTree ->
           (case ((let sem_Name_Name_1 :: T_Name_1 
                       sem_Name_Name_1  =
                           (\ _lhsIcat
                              _lhsIflags
                              _lhsItpe ->
                                (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsItpe
                                        {-# LINE 2624 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOtpe ->
                                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIflags
                                         {-# LINE 2629 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annOflags ->
                                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 2634 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _annOcat ->
                                   (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                    { ( _annIannotatedTree) ->
                                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                Name _annIannotatedTree is_
                                                {-# LINE 2641 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _annotatedTree ->
                                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _annotatedTree
                                                 {-# LINE 2646 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _lhsOannotatedTree ->
                                          ( _lhsOannotatedTree) }) }) }) }) }) }))
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
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                               TypeCheckingFlags ->
                               ( NameTypeNameListPair ,NameTypeNameListPair )
data Inh_NameTypeNameListPair  = Inh_NameTypeNameListPair {cat_Inh_NameTypeNameListPair :: Catalog,flags_Inh_NameTypeNameListPair :: TypeCheckingFlags}
data Syn_NameTypeNameListPair  = Syn_NameTypeNameListPair {annotatedTree_Syn_NameTypeNameListPair :: NameTypeNameListPair ,originalTree_Syn_NameTypeNameListPair :: NameTypeNameListPair }
wrap_NameTypeNameListPair :: T_NameTypeNameListPair  ->
                             Inh_NameTypeNameListPair  ->
                             Syn_NameTypeNameListPair 
wrap_NameTypeNameListPair sem (Inh_NameTypeNameListPair _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_NameTypeNameListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_NameTypeNameListPair_Tuple :: T_Name  ->
                                  T_TypeNameList  ->
                                  T_NameTypeNameListPair 
sem_NameTypeNameListPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 2728 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _x2Ocat ->
          (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                  error "missing rule: NameTypeNameListPair.Tuple.x1.tpe"
                  {-# LINE 2733 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _x1Otpe ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2738 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _x2Oflags ->
            (case (x2_ _x2Ocat _x2Oflags ) of
             { ( _x2IannotatedTree,_x2IoriginalTree) ->
                 (case (x1_ ) of
                  { ( _x1IoriginalTree,x1_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 2747 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _x1Oflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 2752 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _x1Ocat ->
                        (case (x1_1 _x1Ocat _x1Oflags _x1Otpe ) of
                         { ( _x1IannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     (_x1IannotatedTree,_x2IannotatedTree)
                                     {-# LINE 2759 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 2764 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       (_x1IoriginalTree,_x2IoriginalTree)
                                       {-# LINE 2769 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 2774 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
-- NameTypeNameListPairList ------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                                   TypeCheckingFlags ->
                                   ( NameTypeNameListPairList ,NameTypeNameListPairList )
data Inh_NameTypeNameListPairList  = Inh_NameTypeNameListPairList {cat_Inh_NameTypeNameListPairList :: Catalog,flags_Inh_NameTypeNameListPairList :: TypeCheckingFlags}
data Syn_NameTypeNameListPairList  = Syn_NameTypeNameListPairList {annotatedTree_Syn_NameTypeNameListPairList :: NameTypeNameListPairList ,originalTree_Syn_NameTypeNameListPairList :: NameTypeNameListPairList }
wrap_NameTypeNameListPairList :: T_NameTypeNameListPairList  ->
                                 Inh_NameTypeNameListPairList  ->
                                 Syn_NameTypeNameListPairList 
wrap_NameTypeNameListPairList sem (Inh_NameTypeNameListPairList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_NameTypeNameListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_NameTypeNameListPairList_Cons :: T_NameTypeNameListPair  ->
                                     T_NameTypeNameListPairList  ->
                                     T_NameTypeNameListPairList 
sem_NameTypeNameListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 2825 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2830 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2835 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tlOflags ->
            (case (tl_ _tlOcat _tlOflags ) of
             { ( _tlIannotatedTree,_tlIoriginalTree) ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 2842 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _hdOflags ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 2849 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 2854 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 2859 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 2864 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_NameTypeNameListPairList_Nil :: T_NameTypeNameListPairList 
sem_NameTypeNameListPairList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 2874 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2879 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 2884 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 2889 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- OnExpr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
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
                 Environment ->
                 TypeCheckingFlags ->
                 ( OnExpr ,OnExpr )
data Inh_OnExpr  = Inh_OnExpr {cat_Inh_OnExpr :: Catalog,downEnv_Inh_OnExpr :: Environment,flags_Inh_OnExpr :: TypeCheckingFlags}
data Syn_OnExpr  = Syn_OnExpr {annotatedTree_Syn_OnExpr :: OnExpr ,originalTree_Syn_OnExpr :: OnExpr }
wrap_OnExpr :: T_OnExpr  ->
               Inh_OnExpr  ->
               Syn_OnExpr 
wrap_OnExpr sem (Inh_OnExpr _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_OnExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_OnExpr_Just :: T_JoinExpr  ->
                   T_OnExpr 
sem_OnExpr_Just just_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 2943 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _justOflags ->
          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 2948 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _justOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2953 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _justOcat ->
            (case (just_ _justOcat _justOdownEnv _justOflags ) of
             { ( _justIannotatedTree,_justIoriginalTree) ->
                 (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         Just _justIannotatedTree
                         {-# LINE 2960 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annotatedTree ->
                  (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _annotatedTree
                          {-# LINE 2965 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _lhsOannotatedTree ->
                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           Just _justIoriginalTree
                           {-# LINE 2970 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _originalTree ->
                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                            _originalTree
                            {-# LINE 2975 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _lhsOoriginalTree ->
                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 Nothing
                 {-# LINE 2986 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2991 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2996 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 3001 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- ParamDef ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                   TypeCheckingFlags ->
                   ( ParamDef ,ParamDef )
data Inh_ParamDef  = Inh_ParamDef {cat_Inh_ParamDef :: Catalog,flags_Inh_ParamDef :: TypeCheckingFlags}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef ,originalTree_Syn_ParamDef :: ParamDef }
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_ParamDef _lhsOannotatedTree _lhsOoriginalTree ))
sem_ParamDef_ParamDef :: T_Annotation  ->
                         NameComponent ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef ann_ name_ typ_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 3060 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: ParamDef.ParamDef.ann.tpe"
                  {-# LINE 3065 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3070 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _typOflags ->
            (case (typ_ _typOcat _typOflags ) of
             { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 3079 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 3084 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     ParamDef _annIannotatedTree name_ _typIannotatedTree
                                     {-# LINE 3091 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 3096 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       ParamDef _annIoriginalTree name_ _typIoriginalTree
                                       {-# LINE 3101 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 3106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ParamDef_ParamDefTp :: T_Annotation  ->
                           T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp ann_ typ_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 3118 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: ParamDef.ParamDefTp.ann.tpe"
                  {-# LINE 3123 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3128 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _typOflags ->
            (case (typ_ _typOcat _typOflags ) of
             { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 3137 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 3142 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     ParamDefTp _annIannotatedTree _typIannotatedTree
                                     {-# LINE 3149 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 3154 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       ParamDefTp _annIoriginalTree _typIoriginalTree
                                       {-# LINE 3159 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 3164 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
-- ParamDefList ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                       TypeCheckingFlags ->
                       ( ParamDefList ,ParamDefList )
data Inh_ParamDefList  = Inh_ParamDefList {cat_Inh_ParamDefList :: Catalog,flags_Inh_ParamDefList :: TypeCheckingFlags}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList ,originalTree_Syn_ParamDefList :: ParamDefList }
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_ParamDefList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 3215 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3220 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3225 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tlOflags ->
            (case (tl_ _tlOcat _tlOflags ) of
             { ( _tlIannotatedTree,_tlIoriginalTree) ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 3232 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _hdOflags ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 3239 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 3244 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 3249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 3254 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 3264 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3269 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 3274 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 3279 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- QueryExpr ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
         outerDownEnv         : Maybe Environment
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         upType               : Maybe [(String,Type)]
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
                    TypeCheckingFlags ->
                    (Maybe Environment) ->
                    ( QueryExpr ,QueryExpr ,(Maybe [(String,Type)]))
data Inh_QueryExpr  = Inh_QueryExpr {cat_Inh_QueryExpr :: Catalog,flags_Inh_QueryExpr :: TypeCheckingFlags,outerDownEnv_Inh_QueryExpr :: (Maybe Environment)}
data Syn_QueryExpr  = Syn_QueryExpr {annotatedTree_Syn_QueryExpr :: QueryExpr ,originalTree_Syn_QueryExpr :: QueryExpr ,upType_Syn_QueryExpr :: (Maybe [(String,Type)])}
wrap_QueryExpr :: T_QueryExpr  ->
                  Inh_QueryExpr  ->
                  Syn_QueryExpr 
wrap_QueryExpr sem (Inh_QueryExpr _lhsIcat _lhsIflags _lhsIouterDownEnv )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) = sem _lhsIcat _lhsIflags _lhsIouterDownEnv 
     in  (Syn_QueryExpr _lhsOannotatedTree _lhsOoriginalTree _lhsOupType ))
sem_QueryExpr_CombineQueryExpr :: T_Annotation  ->
                                  CombineType ->
                                  T_QueryExpr  ->
                                  T_QueryExpr  ->
                                  T_QueryExpr 
sem_QueryExpr_CombineQueryExpr ann_ ctype_ sel1_ sel2_  =
    (\ _lhsIcat
       _lhsIflags
       _lhsIouterDownEnv ->
         (case (({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                 _lhsIouterDownEnv
                 {-# LINE 3375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _sel2OouterDownEnv ->
          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIflags
                  {-# LINE 3380 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _sel2Oflags ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3385 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _sel2Ocat ->
            (case (({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                    _lhsIouterDownEnv
                    {-# LINE 3390 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _sel1OouterDownEnv ->
             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIflags
                     {-# LINE 3395 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _sel1Oflags ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 3400 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _sel1Ocat ->
               (case (({-# LINE 60 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                       Left []
                       {-# LINE 3405 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tpe ->
                (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _tpe
                        {-# LINE 3410 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (sel2_ _sel2Ocat _sel2Oflags _sel2OouterDownEnv ) of
                  { ( _sel2IannotatedTree,_sel2IoriginalTree,_sel2IupType) ->
                      (case (sel1_ _sel1Ocat _sel1Oflags _sel1OouterDownEnv ) of
                       { ( _sel1IannotatedTree,_sel1IoriginalTree,_sel1IupType) ->
                           (case (ann_ ) of
                            { ( _annIoriginalTree,ann_1) ->
                                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIflags
                                        {-# LINE 3421 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOflags ->
                                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIcat
                                         {-# LINE 3426 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annOcat ->
                                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                   { ( _annIannotatedTree) ->
                                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               CombineQueryExpr _annIannotatedTree ctype_ _sel1IannotatedTree _sel2IannotatedTree
                                               {-# LINE 3433 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annotatedTree ->
                                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _annotatedTree
                                                {-# LINE 3438 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOannotatedTree ->
                                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 CombineQueryExpr _annIoriginalTree ctype_ _sel1IoriginalTree _sel2IoriginalTree
                                                 {-# LINE 3443 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _originalTree ->
                                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _originalTree
                                                  {-# LINE 3448 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOoriginalTree ->
                                           (case (({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                                   _sel2IupType
                                                   {-# LINE 3453 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOupType ->
                                            ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
    (\ _lhsIcat
       _lhsIflags
       _lhsIouterDownEnv ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 3474 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOffsetOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3479 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _selOffsetOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3484 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _selLimitOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 3489 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _selLimitOcat ->
             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIflags
                     {-# LINE 3494 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _selOrderByOflags ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 3499 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _selOrderByOcat ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 3504 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _selHavingOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 3509 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _selHavingOcat ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 3514 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _selGroupByOflags ->
                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _lhsIcat
                          {-# LINE 3519 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _selGroupByOcat ->
                   (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           _lhsIflags
                           {-# LINE 3524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _selWhereOflags ->
                    (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                            _lhsIcat
                            {-# LINE 3529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _selWhereOcat ->
                     (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                             _lhsIflags
                             {-# LINE 3534 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _selTrefOflags ->
                      (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIcat
                              {-# LINE 3539 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _selTrefOcat ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 3544 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _selSelectListOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 3549 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _selSelectListOcat ->
                         (case (selTref_ _selTrefOcat _selTrefOflags ) of
                          { ( _selTrefIannotatedTree,_selTrefIoriginalTree,_selTrefIupEnv) ->
                              (case (({-# LINE 34 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                      _selTrefIupEnv
                                      {-# LINE 3556 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _selSelectListOdownEnv ->
                               (case (selSelectList_ _selSelectListOcat _selSelectListOdownEnv _selSelectListOflags ) of
                                { ( _selSelectListIannotatedTree,_selSelectListIcolExprs,_selSelectListIoriginalTree,_selSelectListIupEnv,_selSelectListIupType) ->
                                    (case (({-# LINE 40 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                            maybe (Left []) (Right . CompositeType) _selSelectListIupType
                                            {-# LINE 3563 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _tpe ->
                                     (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _tpe
                                             {-# LINE 3568 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOtpe ->
                                      (case (({-# LINE 48 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                              maybe _selTrefIupEnv
                                               (\e -> createCorrelatedSubqueryEnvironment e _selTrefIupEnv)
                                               _lhsIouterDownEnv
                                              {-# LINE 3575 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _selWhereOdownEnv ->
                                       (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                               _selSelectListIupEnv
                                               {-# LINE 3580 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _selOrderByOdownEnv ->
                                        (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                                _selTrefIupEnv
                                                {-# LINE 3585 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _selHavingOdownEnv ->
                                         (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                                 _selTrefIupEnv
                                                 {-# LINE 3590 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _selGroupByOdownEnv ->
                                          (case (selOffset_ _selOffsetOcat _selOffsetOflags ) of
                                           { ( _selOffsetIannotatedTree,_selOffsetIoriginalTree) ->
                                               (case (selLimit_ _selLimitOcat _selLimitOflags ) of
                                                { ( _selLimitIannotatedTree,_selLimitIoriginalTree) ->
                                                    (case (selOrderBy_ _selOrderByOcat _selOrderByOdownEnv _selOrderByOflags ) of
                                                     { ( _selOrderByIannotatedTree,_selOrderByIoriginalTree) ->
                                                         (case (selHaving_ _selHavingOcat _selHavingOdownEnv _selHavingOflags ) of
                                                          { ( _selHavingIannotatedTree,_selHavingIoriginalTree) ->
                                                              (case (selGroupBy_ _selGroupByOcat _selGroupByOdownEnv _selGroupByOflags ) of
                                                               { ( _selGroupByIannotatedTree,_selGroupByIoriginalTree,_selGroupByIupTypes) ->
                                                                   (case (selWhere_ _selWhereOcat _selWhereOdownEnv _selWhereOflags ) of
                                                                    { ( _selWhereIannotatedTree,_selWhereIoriginalTree) ->
                                                                        (case (ann_ ) of
                                                                         { ( _annIoriginalTree,ann_1) ->
                                                                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                                     _lhsIflags
                                                                                     {-# LINE 3609 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                     )) of
                                                                              { _annOflags ->
                                                                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                                      _lhsIcat
                                                                                      {-# LINE 3614 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                      )) of
                                                                               { _annOcat ->
                                                                               (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                                                                { ( _annIannotatedTree) ->
                                                                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                                            Select _annIannotatedTree selDistinct_ _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
                                                                                            {-# LINE 3621 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                            )) of
                                                                                     { _annotatedTree ->
                                                                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                                             _annotatedTree
                                                                                             {-# LINE 3626 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                             )) of
                                                                                      { _lhsOannotatedTree ->
                                                                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                                              Select _annIoriginalTree selDistinct_ _selSelectListIoriginalTree _selTrefIoriginalTree _selWhereIoriginalTree _selGroupByIoriginalTree _selHavingIoriginalTree _selOrderByIoriginalTree _selLimitIoriginalTree _selOffsetIoriginalTree
                                                                                              {-# LINE 3631 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                              )) of
                                                                                       { _originalTree ->
                                                                                       (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                                               _originalTree
                                                                                               {-# LINE 3636 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                               )) of
                                                                                        { _lhsOoriginalTree ->
                                                                                        (case (({-# LINE 45 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                                                                                _selSelectListIupType
                                                                                                {-# LINE 3641 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                                                )) of
                                                                                         { _lhsOupType ->
                                                                                         ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_QueryExpr_Values :: T_Annotation  ->
                        T_ScalarExprListList  ->
                        T_QueryExpr 
sem_QueryExpr_Values ann_ vll_  =
    (\ _lhsIcat
       _lhsIflags
       _lhsIouterDownEnv ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 3654 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _vllOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3659 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _vllOcat ->
           (case (({-# LINE 60 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 3664 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tpe ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _tpe
                    {-# LINE 3669 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (vll_ _vllOcat _vllOflags ) of
              { ( _vllIannotatedTree,_vllIoriginalTree) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 3678 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 3683 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      Values _annIannotatedTree _vllIannotatedTree
                                      {-# LINE 3690 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 3695 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        Values _annIoriginalTree _vllIoriginalTree
                                        {-# LINE 3700 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 3705 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  (case (({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                          error "missing rule: QueryExpr.Values.lhs.upType"
                                          {-# LINE 3710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOupType ->
                                   ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_QueryExpr_WithQueryExpr :: T_Annotation  ->
                               T_WithQueryList  ->
                               T_QueryExpr  ->
                               T_QueryExpr 
sem_QueryExpr_WithQueryExpr ann_ withs_ ex_  =
    (\ _lhsIcat
       _lhsIflags
       _lhsIouterDownEnv ->
         (case (({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                 _lhsIouterDownEnv
                 {-# LINE 3724 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOouterDownEnv ->
          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIflags
                  {-# LINE 3729 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOflags ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3734 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exOcat ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 3739 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _withsOflags ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 3744 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _withsOcat ->
              (case (({-# LINE 60 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                      Left []
                      {-# LINE 3749 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tpe ->
               (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _tpe
                       {-# LINE 3754 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOtpe ->
                (case (ex_ _exOcat _exOflags _exOouterDownEnv ) of
                 { ( _exIannotatedTree,_exIoriginalTree,_exIupType) ->
                     (case (withs_ _withsOcat _withsOflags ) of
                      { ( _withsIannotatedTree,_withsIoriginalTree) ->
                          (case (ann_ ) of
                           { ( _annIoriginalTree,ann_1) ->
                               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIflags
                                       {-# LINE 3765 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annOflags ->
                                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIcat
                                        {-# LINE 3770 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOcat ->
                                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                  { ( _annIannotatedTree) ->
                                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              WithQueryExpr _annIannotatedTree _withsIannotatedTree _exIannotatedTree
                                              {-# LINE 3777 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annotatedTree ->
                                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _annotatedTree
                                               {-# LINE 3782 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOannotatedTree ->
                                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                WithQueryExpr _annIoriginalTree _withsIoriginalTree _exIoriginalTree
                                                {-# LINE 3787 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _originalTree ->
                                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _originalTree
                                                 {-# LINE 3792 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _lhsOoriginalTree ->
                                          (case (({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                                                  _exIupType
                                                  {-# LINE 3797 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOupType ->
                                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- Root --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
               TypeCheckingFlags ->
               ( Root ,Root )
data Inh_Root  = Inh_Root {cat_Inh_Root :: Catalog,flags_Inh_Root :: TypeCheckingFlags}
data Syn_Root  = Syn_Root {annotatedTree_Syn_Root :: Root ,originalTree_Syn_Root :: Root }
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_Root _lhsOannotatedTree _lhsOoriginalTree ))
sem_Root_Root :: T_StatementList  ->
                 T_Root 
sem_Root_Root statements_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 3843 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _statementsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3848 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _statementsOcat ->
           (case (statements_ _statementsOcat _statementsOflags ) of
            { ( _statementsIannotatedTree,_statementsIoriginalTree) ->
                (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        Root _statementsIannotatedTree
                        {-# LINE 3855 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annotatedTree ->
                 (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _annotatedTree
                         {-# LINE 3860 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _lhsOannotatedTree ->
                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          Root _statementsIoriginalTree
                          {-# LINE 3865 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _originalTree ->
                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           _originalTree
                           {-# LINE 3870 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _lhsOoriginalTree ->
                    ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }))
-- RowConstraint -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                        TypeCheckingFlags ->
                        ( RowConstraint ,RowConstraint )
data Inh_RowConstraint  = Inh_RowConstraint {cat_Inh_RowConstraint :: Catalog,flags_Inh_RowConstraint :: TypeCheckingFlags}
data Syn_RowConstraint  = Syn_RowConstraint {annotatedTree_Syn_RowConstraint :: RowConstraint ,originalTree_Syn_RowConstraint :: RowConstraint }
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_RowConstraint _lhsOannotatedTree _lhsOoriginalTree ))
sem_RowConstraint_NotNullConstraint :: T_Annotation  ->
                                       String ->
                                       T_RowConstraint 
sem_RowConstraint_NotNullConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: RowConstraint.NotNullConstraint.ann.tpe"
                 {-# LINE 3968 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 3975 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 3980 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              NotNullConstraint _annIannotatedTree name_
                              {-# LINE 3987 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 3992 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                NotNullConstraint _annIoriginalTree name_
                                {-# LINE 3997 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 4002 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_RowConstraint_NullConstraint :: T_Annotation  ->
                                    String ->
                                    T_RowConstraint 
sem_RowConstraint_NullConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: RowConstraint.NullConstraint.ann.tpe"
                 {-# LINE 4014 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 4021 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 4026 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              NullConstraint _annIannotatedTree name_
                              {-# LINE 4033 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 4038 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                NullConstraint _annIoriginalTree name_
                                {-# LINE 4043 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 4048 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_RowConstraint_RowCheckConstraint :: T_Annotation  ->
                                        String ->
                                        T_ScalarExpr  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 4061 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: RowConstraint.RowCheckConstraint.expr.downEnv"
                  {-# LINE 4066 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4071 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: RowConstraint.RowCheckConstraint.ann.tpe"
                    {-# LINE 4076 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (expr_ _exprOcat _exprOdownEnv _exprOflags ) of
              { ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 4085 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 4090 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      RowCheckConstraint _annIannotatedTree name_ _exprIannotatedTree
                                      {-# LINE 4097 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 4102 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        RowCheckConstraint _annIoriginalTree name_ _exprIoriginalTree
                                        {-# LINE 4107 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 4112 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_RowConstraint_RowPrimaryKeyConstraint :: T_Annotation  ->
                                             String ->
                                             T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: RowConstraint.RowPrimaryKeyConstraint.ann.tpe"
                 {-# LINE 4124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 4131 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 4136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              RowPrimaryKeyConstraint _annIannotatedTree name_
                              {-# LINE 4143 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 4148 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                RowPrimaryKeyConstraint _annIoriginalTree name_
                                {-# LINE 4153 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 4158 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_RowConstraint_RowReferenceConstraint :: T_Annotation  ->
                                            String ->
                                            T_Name  ->
                                            (Maybe NameComponent) ->
                                            Cascade ->
                                            Cascade ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: RowConstraint.RowReferenceConstraint.table.tpe"
                 {-# LINE 4174 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tableOtpe ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: RowConstraint.RowReferenceConstraint.ann.tpe"
                  {-# LINE 4179 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (table_ ) of
            { ( _tableIoriginalTree,table_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 4186 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tableOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 4191 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tableOcat ->
                  (case (table_1 _tableOcat _tableOflags _tableOtpe ) of
                   { ( _tableIannotatedTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIflags
                                    {-# LINE 4200 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOflags ->
                             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIcat
                                     {-# LINE 4205 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOcat ->
                              (case (ann_1 _annOcat _annOflags _annOtpe ) of
                               { ( _annIannotatedTree) ->
                                   (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           RowReferenceConstraint _annIannotatedTree name_ _tableIannotatedTree att_ onUpdate_ onDelete_
                                           {-# LINE 4212 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annotatedTree ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _annotatedTree
                                            {-# LINE 4217 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOannotatedTree ->
                                     (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             RowReferenceConstraint _annIoriginalTree name_ _tableIoriginalTree att_ onUpdate_ onDelete_
                                             {-# LINE 4222 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 4227 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_RowConstraint_RowUniqueConstraint :: T_Annotation  ->
                                         String ->
                                         T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: RowConstraint.RowUniqueConstraint.ann.tpe"
                 {-# LINE 4239 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 4246 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 4251 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              RowUniqueConstraint _annIannotatedTree name_
                              {-# LINE 4258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 4263 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                RowUniqueConstraint _annIoriginalTree name_
                                {-# LINE 4268 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 4273 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
-- RowConstraintList -------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                            TypeCheckingFlags ->
                            ( RowConstraintList ,RowConstraintList )
data Inh_RowConstraintList  = Inh_RowConstraintList {cat_Inh_RowConstraintList :: Catalog,flags_Inh_RowConstraintList :: TypeCheckingFlags}
data Syn_RowConstraintList  = Syn_RowConstraintList {annotatedTree_Syn_RowConstraintList :: RowConstraintList ,originalTree_Syn_RowConstraintList :: RowConstraintList }
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_RowConstraintList _lhsOannotatedTree _lhsOoriginalTree ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 4324 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4329 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4334 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 4339 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOflags ) of
              { ( _tlIannotatedTree,_tlIoriginalTree) ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 4348 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 4353 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 4358 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 4363 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 4373 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4378 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 4383 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 4388 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- ScalarExpr --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
      synthesized attributes:
         annotatedTree        : SELF 
         colExprs             : [(String,Maybe Type,ScalarExpr)]
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
            local upType      : _
      alternative AntiScalarExpr:
         child string         : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
            local tpe         : _
      alternative App:
         child ann            : Annotation 
         child funName        : Name 
         child args           : ScalarExprList 
         visit 0:
            local originalTree : _
            local tpe         : _
            local annotatedTree : _
            local upType      : _
      alternative BinaryOp:
         child ann            : Annotation 
         child opName         : Name 
         child arg0           : ScalarExpr 
         child arg1           : ScalarExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative BooleanLit:
         child ann            : Annotation 
         child b              : {Bool}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative Case:
         child ann            : Annotation 
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative CaseSimple:
         child ann            : Annotation 
         child value          : ScalarExpr 
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative Cast:
         child ann            : Annotation 
         child expr           : ScalarExpr 
         child tn             : TypeName 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative Exists:
         child ann            : Annotation 
         child sel            : QueryExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative Extract:
         child ann            : Annotation 
         child field          : {ExtractField}
         child e              : ScalarExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative Identifier:
         child ann            : Annotation 
         child i              : {NameComponent}
         visit 0:
            local elkp        : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative InPredicate:
         child ann            : Annotation 
         child expr           : ScalarExpr 
         child i              : {Bool}
         child list           : InList 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative Interval:
         child ann            : Annotation 
         child value          : {String}
         child field          : {IntervalField}
         child prec           : {Maybe Int}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative LiftApp:
         child ann            : Annotation 
         child oper           : Name 
         child flav           : {LiftFlavour}
         child args           : ScalarExprList 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative NullLit:
         child ann            : Annotation 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative NumberLit:
         child ann            : Annotation 
         child d              : {String}
         visit 0:
            local digChars    : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative Placeholder:
         child ann            : Annotation 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative PositionalArg:
         child ann            : Annotation 
         child p              : {Integer}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative PostfixOp:
         child ann            : Annotation 
         child opName         : Name 
         child arg            : ScalarExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative PrefixOp:
         child ann            : Annotation 
         child opName         : Name 
         child arg            : ScalarExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative QIdentifier:
         child ann            : Annotation 
         child is             : {[NameComponent]}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative QStar:
         child ann            : Annotation 
         child q              : {NameComponent}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative ScalarSubQuery:
         child ann            : Annotation 
         child sel            : QueryExpr 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative SpecialOp:
         child ann            : Annotation 
         child opName         : Name 
         child args           : ScalarExprList 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative Star:
         child ann            : Annotation 
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative StringLit:
         child ann            : Annotation 
         child value          : {String}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
      alternative TypedStringLit:
         child ann            : Annotation 
         child tn             : TypeName 
         child value          : {String}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
            local upType      : _
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
            local upType      : _
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
                     TypeCheckingFlags ->
                     ( ScalarExpr ,([(String,Maybe Type,ScalarExpr)]),ScalarExpr ,(Maybe Type))
data Inh_ScalarExpr  = Inh_ScalarExpr {cat_Inh_ScalarExpr :: Catalog,downEnv_Inh_ScalarExpr :: Environment,flags_Inh_ScalarExpr :: TypeCheckingFlags}
data Syn_ScalarExpr  = Syn_ScalarExpr {annotatedTree_Syn_ScalarExpr :: ScalarExpr ,colExprs_Syn_ScalarExpr :: ([(String,Maybe Type,ScalarExpr)]),originalTree_Syn_ScalarExpr :: ScalarExpr ,upType_Syn_ScalarExpr :: (Maybe Type)}
wrap_ScalarExpr :: T_ScalarExpr  ->
                   Inh_ScalarExpr  ->
                   Syn_ScalarExpr 
wrap_ScalarExpr sem (Inh_ScalarExpr _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_ScalarExpr _lhsOannotatedTree _lhsOcolExprs _lhsOoriginalTree _lhsOupType ))
sem_ScalarExpr_AggregateApp :: T_Annotation  ->
                               Distinct ->
                               T_ScalarExpr  ->
                               T_ScalarExprDirectionPairList  ->
                               T_ScalarExpr 
sem_ScalarExpr_AggregateApp ann_ aggDistinct_ fn_ orderBy_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 4762 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _orderByOflags ->
          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 4767 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _orderByOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4772 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _orderByOcat ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 4777 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _fnOflags ->
             (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     _lhsIdownEnv
                     {-# LINE 4782 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _fnOdownEnv ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 4787 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _fnOcat ->
               (case (fn_ _fnOcat _fnOdownEnv _fnOflags ) of
                { ( _fnIannotatedTree,_fnIcolExprs,_fnIoriginalTree,_fnIupType) ->
                    (case (({-# LINE 161 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                            case _fnIoriginalTree of
                              (App _ fnm@(Name _ [Nmc "count"]) [Star _]) ->
                                  tcAppLike _lhsIcat fnm [Just UnknownType]
                              _ -> maybe (Left []) Right _fnIupType
                            {-# LINE 4797 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _tpe ->
                     (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                             _tpe
                             {-# LINE 4802 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                             )) of
                      { _annOtpe ->
                      (case (orderBy_ _orderByOcat _orderByOdownEnv _orderByOflags ) of
                       { ( _orderByIannotatedTree,_orderByIoriginalTree) ->
                           (case (ann_ ) of
                            { ( _annIoriginalTree,ann_1) ->
                                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIflags
                                        {-# LINE 4811 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOflags ->
                                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIcat
                                         {-# LINE 4816 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annOcat ->
                                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                   { ( _annIannotatedTree) ->
                                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               AggregateApp _annIannotatedTree aggDistinct_ _fnIannotatedTree _orderByIannotatedTree
                                               {-# LINE 4823 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annotatedTree ->
                                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _annotatedTree
                                                {-# LINE 4828 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOannotatedTree ->
                                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 AggregateApp _annIoriginalTree aggDistinct_ _fnIoriginalTree _orderByIoriginalTree
                                                 {-# LINE 4833 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _originalTree ->
                                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                  either (const Nothing) Just _tpe
                                                  {-# LINE 4838 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _upType ->
                                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                   let a = _annIoriginalTree
                                                       doStar is =
                                                         map (\((q,n),t) ->
                                                           let a' = setAtype (Just t) a
                                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                           ) is
                                                   in case _originalTree of
                                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                          doStar is
                                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                          doStar is
                                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                   {-# LINE 4854 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOcolExprs ->
                                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _originalTree
                                                    {-# LINE 4859 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOoriginalTree ->
                                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                     _upType
                                                     {-# LINE 4864 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOupType ->
                                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_AntiScalarExpr :: String ->
                                 T_ScalarExpr 
sem_ScalarExpr_AntiScalarExpr string_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 AntiScalarExpr string_
                 {-# LINE 4876 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4881 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 31 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   error "missing rule: ScalarExpr.AntiScalarExpr.lhs.colExprs"
                   {-# LINE 4886 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _lhsOcolExprs ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    AntiScalarExpr string_
                    {-# LINE 4891 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _originalTree ->
             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _originalTree
                     {-# LINE 4896 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _lhsOoriginalTree ->
              (case (({-# LINE 229 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                      Left []
                      {-# LINE 4901 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tpe ->
               (case (({-# LINE 31 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                       either (const Nothing) Just _tpe
                       {-# LINE 4906 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _lhsOupType ->
                ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }))
sem_ScalarExpr_App :: T_Annotation  ->
                      T_Name  ->
                      T_ScalarExprList  ->
                      T_ScalarExpr 
sem_ScalarExpr_App ann_ funName_ args_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 4920 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argsOflags ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 4925 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argsOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4930 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _argsOcat ->
            (case (args_ _argsOcat _argsOdownEnv _argsOflags ) of
             { ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) ->
                 (case (funName_ ) of
                  { ( _funNameIoriginalTree,funName_1) ->
                      (case (ann_ ) of
                       { ( _annIoriginalTree,ann_1) ->
                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   App _annIoriginalTree _funNameIoriginalTree _argsIoriginalTree
                                   {-# LINE 4941 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _originalTree ->
                            (case (({-# LINE 147 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                    case _originalTree of
                                      (App _ (Name _ [Nmc "count"]) [Star _]) ->
                                          tcAppLike _lhsIcat _funNameIoriginalTree [Just UnknownType]
                                      _ -> tcAppLike _lhsIcat _funNameIoriginalTree _argsIupTypes
                                    {-# LINE 4949 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _tpe ->
                             (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                     _tpe
                                     {-# LINE 4954 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _funNameOtpe ->
                              (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                      _tpe
                                      {-# LINE 4959 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annOtpe ->
                               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIflags
                                       {-# LINE 4964 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _funNameOflags ->
                                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIcat
                                        {-# LINE 4969 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _funNameOcat ->
                                 (case (funName_1 _funNameOcat _funNameOflags _funNameOtpe ) of
                                  { ( _funNameIannotatedTree) ->
                                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _lhsIflags
                                              {-# LINE 4976 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annOflags ->
                                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _lhsIcat
                                               {-# LINE 4981 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annOcat ->
                                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                         { ( _annIannotatedTree) ->
                                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     App _annIannotatedTree _funNameIannotatedTree _argsIannotatedTree
                                                     {-# LINE 4988 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _annotatedTree ->
                                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _annotatedTree
                                                      {-# LINE 4993 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOannotatedTree ->
                                               (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                       either (const Nothing) Just _tpe
                                                       {-# LINE 4998 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _upType ->
                                                (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                        let a = _annIoriginalTree
                                                            doStar is =
                                                              map (\((q,n),t) ->
                                                                let a' = setAtype (Just t) a
                                                                in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                                ) is
                                                        in case _originalTree of
                                                            Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                               doStar is
                                                            QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                               doStar is
                                                            _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                        {-# LINE 5014 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOcolExprs ->
                                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         _originalTree
                                                         {-# LINE 5019 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _lhsOoriginalTree ->
                                                  (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                          _upType
                                                          {-# LINE 5024 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _lhsOupType ->
                                                   ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_BinaryOp :: T_Annotation  ->
                           T_Name  ->
                           T_ScalarExpr  ->
                           T_ScalarExpr  ->
                           T_ScalarExpr 
sem_ScalarExpr_BinaryOp ann_ opName_ arg0_ arg1_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 5039 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _arg1Oflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 5044 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _arg1OdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5049 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _arg1Ocat ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 5054 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _arg0Oflags ->
             (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     _lhsIdownEnv
                     {-# LINE 5059 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _arg0OdownEnv ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 5064 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _arg0Ocat ->
               (case (arg1_ _arg1Ocat _arg1OdownEnv _arg1Oflags ) of
                { ( _arg1IannotatedTree,_arg1IcolExprs,_arg1IoriginalTree,_arg1IupType) ->
                    (case (arg0_ _arg0Ocat _arg0OdownEnv _arg0Oflags ) of
                     { ( _arg0IannotatedTree,_arg0IcolExprs,_arg0IoriginalTree,_arg0IupType) ->
                         (case (opName_ ) of
                          { ( _opNameIoriginalTree,opName_1) ->
                              (case (({-# LINE 153 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                      tcAppLike _lhsIcat _opNameIoriginalTree [_arg0IupType,_arg1IupType]
                                      {-# LINE 5075 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _tpe ->
                               (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                       _tpe
                                       {-# LINE 5080 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _opNameOtpe ->
                                (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                        _tpe
                                        {-# LINE 5085 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOtpe ->
                                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIflags
                                         {-# LINE 5090 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _opNameOflags ->
                                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 5095 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _opNameOcat ->
                                   (case (opName_1 _opNameOcat _opNameOflags _opNameOtpe ) of
                                    { ( _opNameIannotatedTree) ->
                                        (case (ann_ ) of
                                         { ( _annIoriginalTree,ann_1) ->
                                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _lhsIflags
                                                     {-# LINE 5104 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _annOflags ->
                                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _lhsIcat
                                                      {-# LINE 5109 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _annOcat ->
                                               (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                                { ( _annIannotatedTree) ->
                                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                            BinaryOp _annIannotatedTree _opNameIannotatedTree _arg0IannotatedTree _arg1IannotatedTree
                                                            {-# LINE 5116 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                            )) of
                                                     { _annotatedTree ->
                                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                             _annotatedTree
                                                             {-# LINE 5121 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                             )) of
                                                      { _lhsOannotatedTree ->
                                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                              BinaryOp _annIoriginalTree _opNameIoriginalTree _arg0IoriginalTree _arg1IoriginalTree
                                                              {-# LINE 5126 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                              )) of
                                                       { _originalTree ->
                                                       (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                               either (const Nothing) Just _tpe
                                                               {-# LINE 5131 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                               )) of
                                                        { _upType ->
                                                        (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                                let a = _annIoriginalTree
                                                                    doStar is =
                                                                      map (\((q,n),t) ->
                                                                        let a' = setAtype (Just t) a
                                                                        in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                                        ) is
                                                                in case _originalTree of
                                                                    Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                                       doStar is
                                                                    QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                                       doStar is
                                                                    _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                                {-# LINE 5147 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                )) of
                                                         { _lhsOcolExprs ->
                                                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                 _originalTree
                                                                 {-# LINE 5152 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                 )) of
                                                          { _lhsOoriginalTree ->
                                                          (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                                  _upType
                                                                  {-# LINE 5157 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                  )) of
                                                           { _lhsOupType ->
                                                           ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_BooleanLit :: T_Annotation  ->
                             Bool ->
                             T_ScalarExpr 
sem_ScalarExpr_BooleanLit ann_ b_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 74 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Right typeBool
                 {-# LINE 5170 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 5175 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 5182 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 5187 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               BooleanLit _annIannotatedTree b_
                               {-# LINE 5194 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 5199 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 BooleanLit _annIoriginalTree b_
                                 {-# LINE 5204 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 5209 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _upType ->
                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                   let a = _annIoriginalTree
                                       doStar is =
                                         map (\((q,n),t) ->
                                           let a' = setAtype (Just t) a
                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                           ) is
                                   in case _originalTree of
                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                          doStar is
                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                          doStar is
                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                   {-# LINE 5225 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOcolExprs ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 5230 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                     _upType
                                     {-# LINE 5235 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupType ->
                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Case :: T_Annotation  ->
                       T_CaseScalarExprListScalarExprPairList  ->
                       T_MaybeScalarExpr  ->
                       T_ScalarExpr 
sem_ScalarExpr_Case ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 5249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5254 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _elsOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5259 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _casesOflags ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                    _lhsIdownEnv
                    {-# LINE 5264 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _casesOdownEnv ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 5269 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _casesOcat ->
              (case (({-# LINE 229 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                      Left []
                      {-# LINE 5274 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tpe ->
               (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                       _tpe
                       {-# LINE 5279 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOtpe ->
                (case (els_ _elsOcat _elsOflags ) of
                 { ( _elsIannotatedTree,_elsIoriginalTree) ->
                     (case (cases_ _casesOcat _casesOdownEnv _casesOflags ) of
                      { ( _casesIannotatedTree,_casesIoriginalTree) ->
                          (case (ann_ ) of
                           { ( _annIoriginalTree,ann_1) ->
                               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIflags
                                       {-# LINE 5290 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annOflags ->
                                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIcat
                                        {-# LINE 5295 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOcat ->
                                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                  { ( _annIannotatedTree) ->
                                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              Case _annIannotatedTree _casesIannotatedTree _elsIannotatedTree
                                              {-# LINE 5302 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annotatedTree ->
                                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _annotatedTree
                                               {-# LINE 5307 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOannotatedTree ->
                                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                Case _annIoriginalTree _casesIoriginalTree _elsIoriginalTree
                                                {-# LINE 5312 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _originalTree ->
                                         (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                 either (const Nothing) Just _tpe
                                                 {-# LINE 5317 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _upType ->
                                          (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                  let a = _annIoriginalTree
                                                      doStar is =
                                                        map (\((q,n),t) ->
                                                          let a' = setAtype (Just t) a
                                                          in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                          ) is
                                                  in case _originalTree of
                                                      Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                         doStar is
                                                      QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                         doStar is
                                                      _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                  {-# LINE 5333 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOcolExprs ->
                                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _originalTree
                                                   {-# LINE 5338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOoriginalTree ->
                                            (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                    _upType
                                                    {-# LINE 5343 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOupType ->
                                             ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_CaseSimple :: T_Annotation  ->
                             T_ScalarExpr  ->
                             T_CaseScalarExprListScalarExprPairList  ->
                             T_MaybeScalarExpr  ->
                             T_ScalarExpr 
sem_ScalarExpr_CaseSimple ann_ value_ cases_ els_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 5358 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5363 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _elsOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5368 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _casesOflags ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                    _lhsIdownEnv
                    {-# LINE 5373 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _casesOdownEnv ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 5378 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _casesOcat ->
              (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIflags
                      {-# LINE 5383 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _valueOflags ->
               (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                       _lhsIdownEnv
                       {-# LINE 5388 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _valueOdownEnv ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 5393 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _valueOcat ->
                 (case (({-# LINE 229 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                         Left []
                         {-# LINE 5398 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tpe ->
                  (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                          _tpe
                          {-# LINE 5403 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _annOtpe ->
                   (case (els_ _elsOcat _elsOflags ) of
                    { ( _elsIannotatedTree,_elsIoriginalTree) ->
                        (case (cases_ _casesOcat _casesOdownEnv _casesOflags ) of
                         { ( _casesIannotatedTree,_casesIoriginalTree) ->
                             (case (value_ _valueOcat _valueOdownEnv _valueOflags ) of
                              { ( _valueIannotatedTree,_valueIcolExprs,_valueIoriginalTree,_valueIupType) ->
                                  (case (ann_ ) of
                                   { ( _annIoriginalTree,ann_1) ->
                                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _lhsIflags
                                               {-# LINE 5416 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annOflags ->
                                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _lhsIcat
                                                {-# LINE 5421 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _annOcat ->
                                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                          { ( _annIannotatedTree) ->
                                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      CaseSimple _annIannotatedTree _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                                                      {-# LINE 5428 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _annotatedTree ->
                                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                       _annotatedTree
                                                       {-# LINE 5433 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _lhsOannotatedTree ->
                                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                        CaseSimple _annIoriginalTree _valueIoriginalTree _casesIoriginalTree _elsIoriginalTree
                                                        {-# LINE 5438 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _originalTree ->
                                                 (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                         either (const Nothing) Just _tpe
                                                         {-# LINE 5443 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _upType ->
                                                  (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                          let a = _annIoriginalTree
                                                              doStar is =
                                                                map (\((q,n),t) ->
                                                                  let a' = setAtype (Just t) a
                                                                  in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                                  ) is
                                                          in case _originalTree of
                                                              Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                                 doStar is
                                                              QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                                 doStar is
                                                              _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                          {-# LINE 5459 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _lhsOcolExprs ->
                                                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                           _originalTree
                                                           {-# LINE 5464 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _lhsOoriginalTree ->
                                                    (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                            _upType
                                                            {-# LINE 5469 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                            )) of
                                                     { _lhsOupType ->
                                                     ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Cast :: T_Annotation  ->
                       T_ScalarExpr  ->
                       T_TypeName  ->
                       T_ScalarExpr 
sem_ScalarExpr_Cast ann_ expr_ tn_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 5483 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tnOcat ->
          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIflags
                  {-# LINE 5488 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOflags ->
           (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 5493 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOdownEnv ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 5498 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _exprOcat ->
             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIflags
                     {-# LINE 5503 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _tnOflags ->
              (case (tn_ _tnOcat _tnOflags ) of
               { ( _tnIannotatedTree,_tnInamedType,_tnIoriginalTree) ->
                   (case (({-# LINE 103 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                           maybe (Left []) Right _tnInamedType
                           {-# LINE 5510 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _tpe ->
                    (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                            _tpe
                            {-# LINE 5515 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _annOtpe ->
                     (case (expr_ _exprOcat _exprOdownEnv _exprOflags ) of
                      { ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) ->
                          (case (ann_ ) of
                           { ( _annIoriginalTree,ann_1) ->
                               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIflags
                                       {-# LINE 5524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annOflags ->
                                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIcat
                                        {-# LINE 5529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOcat ->
                                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                  { ( _annIannotatedTree) ->
                                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              Cast _annIannotatedTree _exprIannotatedTree _tnIannotatedTree
                                              {-# LINE 5536 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annotatedTree ->
                                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _annotatedTree
                                               {-# LINE 5541 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOannotatedTree ->
                                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                Cast _annIoriginalTree _exprIoriginalTree _tnIoriginalTree
                                                {-# LINE 5546 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _originalTree ->
                                         (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                 either (const Nothing) Just _tpe
                                                 {-# LINE 5551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _upType ->
                                          (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                  let a = _annIoriginalTree
                                                      doStar is =
                                                        map (\((q,n),t) ->
                                                          let a' = setAtype (Just t) a
                                                          in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                          ) is
                                                  in case _originalTree of
                                                      Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                         doStar is
                                                      QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                         doStar is
                                                      _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                  {-# LINE 5567 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOcolExprs ->
                                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _originalTree
                                                   {-# LINE 5572 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOoriginalTree ->
                                            (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                    _upType
                                                    {-# LINE 5577 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOupType ->
                                             ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Exists :: T_Annotation  ->
                         T_QueryExpr  ->
                         T_ScalarExpr 
sem_ScalarExpr_Exists ann_ sel_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 5590 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5595 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _selOcat ->
           (case (({-# LINE 216 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Just _lhsIdownEnv
                   {-# LINE 5600 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _selOouterDownEnv ->
            (case (({-# LINE 195 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    Right typeBool
                    {-# LINE 5605 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _tpe ->
             (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     _tpe
                     {-# LINE 5610 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (sel_ _selOcat _selOflags _selOouterDownEnv ) of
               { ( _selIannotatedTree,_selIoriginalTree,_selIupType) ->
                   (case (ann_ ) of
                    { ( _annIoriginalTree,ann_1) ->
                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIflags
                                {-# LINE 5619 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOflags ->
                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIcat
                                 {-# LINE 5624 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annOcat ->
                          (case (ann_1 _annOcat _annOflags _annOtpe ) of
                           { ( _annIannotatedTree) ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       Exists _annIannotatedTree _selIannotatedTree
                                       {-# LINE 5631 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annotatedTree ->
                                (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _annotatedTree
                                        {-# LINE 5636 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOannotatedTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         Exists _annIoriginalTree _selIoriginalTree
                                         {-# LINE 5641 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                          either (const Nothing) Just _tpe
                                          {-# LINE 5646 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _upType ->
                                   (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                           let a = _annIoriginalTree
                                               doStar is =
                                                 map (\((q,n),t) ->
                                                   let a' = setAtype (Just t) a
                                                   in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                   ) is
                                           in case _originalTree of
                                               Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                  doStar is
                                               QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                  doStar is
                                               _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                           {-# LINE 5662 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOcolExprs ->
                                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 5667 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                             _upType
                                             {-# LINE 5672 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOupType ->
                                      ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Extract :: T_Annotation  ->
                          ExtractField ->
                          T_ScalarExpr  ->
                          T_ScalarExpr 
sem_ScalarExpr_Extract ann_ field_ e_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 5686 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _eOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 5691 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _eOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5696 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _eOcat ->
            (case (e_ _eOcat _eOdownEnv _eOflags ) of
             { ( _eIannotatedTree,_eIcolExprs,_eIoriginalTree,_eIupType) ->
                 (case (({-# LINE 107 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                         do
                         x <- maybe (Left []) Right _eIupType
                         if x == typeDate
                           then Right typeFloat8
                           else Left [NoMatchingOperator "extract" [x]]
                         {-# LINE 5707 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tpe ->
                  (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                          _tpe
                          {-# LINE 5712 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _annOtpe ->
                   (case (ann_ ) of
                    { ( _annIoriginalTree,ann_1) ->
                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIflags
                                {-# LINE 5719 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOflags ->
                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIcat
                                 {-# LINE 5724 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annOcat ->
                          (case (ann_1 _annOcat _annOflags _annOtpe ) of
                           { ( _annIannotatedTree) ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       Extract _annIannotatedTree field_ _eIannotatedTree
                                       {-# LINE 5731 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annotatedTree ->
                                (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _annotatedTree
                                        {-# LINE 5736 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOannotatedTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         Extract _annIoriginalTree field_ _eIoriginalTree
                                         {-# LINE 5741 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                          either (const Nothing) Just _tpe
                                          {-# LINE 5746 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _upType ->
                                   (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                           let a = _annIoriginalTree
                                               doStar is =
                                                 map (\((q,n),t) ->
                                                   let a' = setAtype (Just t) a
                                                   in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                   ) is
                                           in case _originalTree of
                                               Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                  doStar is
                                               QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                  doStar is
                                               _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                           {-# LINE 5762 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOcolExprs ->
                                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 5767 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                             _upType
                                             {-# LINE 5772 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOupType ->
                                      ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Identifier :: T_Annotation  ->
                             NameComponent ->
                             T_ScalarExpr 
sem_ScalarExpr_Identifier ann_ i_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 122 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 envLookupIdentifier [i_] _lhsIdownEnv
                 {-# LINE 5785 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elkp ->
          (case (({-# LINE 123 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  fmap snd _elkp
                  {-# LINE 5790 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tpe ->
           (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 5795 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (ann_ ) of
             { ( _annIoriginalTree,ann_1) ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 5802 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOflags ->
                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _lhsIcat
                          {-# LINE 5807 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _annOcat ->
                   (case (ann_1 _annOcat _annOflags _annOtpe ) of
                    { ( _annIannotatedTree) ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                Identifier _annIannotatedTree i_
                                {-# LINE 5814 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annotatedTree ->
                         (case (({-# LINE 124 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                 fromMaybe _annotatedTree $ do
                                     case tcfAddQualifiers _lhsIflags of
                                       False -> Nothing
                                       True -> do
                                              ((q,i),_) <- either (const Nothing) Just _elkp
                                              return $ QIdentifier _annIannotatedTree [Nmc q, Nmc i]
                                 {-# LINE 5824 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOannotatedTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  Identifier _annIoriginalTree i_
                                  {-# LINE 5829 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _originalTree ->
                           (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                   either (const Nothing) Just _tpe
                                   {-# LINE 5834 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _upType ->
                            (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                    let a = _annIoriginalTree
                                        doStar is =
                                          map (\((q,n),t) ->
                                            let a' = setAtype (Just t) a
                                            in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                            ) is
                                    in case _originalTree of
                                        Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                           doStar is
                                        QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                           doStar is
                                        _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                    {-# LINE 5850 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOcolExprs ->
                             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _originalTree
                                     {-# LINE 5855 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOoriginalTree ->
                              (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                      _upType
                                      {-# LINE 5860 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOupType ->
                               ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_InPredicate :: T_Annotation  ->
                              T_ScalarExpr  ->
                              Bool ->
                              T_InList  ->
                              T_ScalarExpr 
sem_ScalarExpr_InPredicate ann_ expr_ i_ list_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 5875 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _listOflags ->
          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 5880 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _listOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5885 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _listOcat ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 5890 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _exprOflags ->
             (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     _lhsIdownEnv
                     {-# LINE 5895 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _exprOdownEnv ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 5900 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _exprOcat ->
               (case (({-# LINE 229 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                       Left []
                       {-# LINE 5905 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tpe ->
                (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                        _tpe
                        {-# LINE 5910 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (list_ _listOcat _listOdownEnv _listOflags ) of
                  { ( _listIannotatedTree,_listIoriginalTree) ->
                      (case (expr_ _exprOcat _exprOdownEnv _exprOflags ) of
                       { ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) ->
                           (case (ann_ ) of
                            { ( _annIoriginalTree,ann_1) ->
                                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIflags
                                        {-# LINE 5921 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOflags ->
                                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIcat
                                         {-# LINE 5926 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _annOcat ->
                                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                   { ( _annIannotatedTree) ->
                                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               InPredicate _annIannotatedTree _exprIannotatedTree i_ _listIannotatedTree
                                               {-# LINE 5933 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annotatedTree ->
                                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _annotatedTree
                                                {-# LINE 5938 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOannotatedTree ->
                                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 InPredicate _annIoriginalTree _exprIoriginalTree i_ _listIoriginalTree
                                                 {-# LINE 5943 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _originalTree ->
                                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                  either (const Nothing) Just _tpe
                                                  {-# LINE 5948 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _upType ->
                                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                   let a = _annIoriginalTree
                                                       doStar is =
                                                         map (\((q,n),t) ->
                                                           let a' = setAtype (Just t) a
                                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                           ) is
                                                   in case _originalTree of
                                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                          doStar is
                                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                          doStar is
                                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                   {-# LINE 5964 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOcolExprs ->
                                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _originalTree
                                                    {-# LINE 5969 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOoriginalTree ->
                                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                     _upType
                                                     {-# LINE 5974 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOupType ->
                                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Interval :: T_Annotation  ->
                           String ->
                           IntervalField ->
                           (Maybe Int) ->
                           T_ScalarExpr 
sem_ScalarExpr_Interval ann_ value_ field_ prec_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 105 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Right $ ScalarType "interval"
                 {-# LINE 5989 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 5994 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 6001 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 6006 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               Interval _annIannotatedTree value_ field_ prec_
                               {-# LINE 6013 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 6018 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 Interval _annIoriginalTree value_ field_ prec_
                                 {-# LINE 6023 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 6028 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _upType ->
                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                   let a = _annIoriginalTree
                                       doStar is =
                                         map (\((q,n),t) ->
                                           let a' = setAtype (Just t) a
                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                           ) is
                                   in case _originalTree of
                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                          doStar is
                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                          doStar is
                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                   {-# LINE 6044 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOcolExprs ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 6049 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                     _upType
                                     {-# LINE 6054 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupType ->
                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_LiftApp :: T_Annotation  ->
                          T_Name  ->
                          LiftFlavour ->
                          T_ScalarExprList  ->
                          T_ScalarExpr 
sem_ScalarExpr_LiftApp ann_ oper_ flav_ args_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 6069 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argsOflags ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 6074 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argsOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6079 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _argsOcat ->
            (case (({-# LINE 229 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    Left []
                    {-# LINE 6084 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _tpe ->
             (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                     _tpe
                     {-# LINE 6089 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _operOtpe ->
              (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                      _tpe
                      {-# LINE 6094 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (args_ _argsOcat _argsOdownEnv _argsOflags ) of
                { ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) ->
                    (case (oper_ ) of
                     { ( _operIoriginalTree,oper_1) ->
                         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIflags
                                 {-# LINE 6103 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _operOflags ->
                          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _lhsIcat
                                  {-# LINE 6108 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _operOcat ->
                           (case (oper_1 _operOcat _operOflags _operOtpe ) of
                            { ( _operIannotatedTree) ->
                                (case (ann_ ) of
                                 { ( _annIoriginalTree,ann_1) ->
                                     (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIflags
                                             {-# LINE 6117 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOflags ->
                                      (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _lhsIcat
                                              {-# LINE 6122 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annOcat ->
                                       (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                        { ( _annIannotatedTree) ->
                                            (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    LiftApp _annIannotatedTree _operIannotatedTree flav_ _argsIannotatedTree
                                                    {-# LINE 6129 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _annotatedTree ->
                                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _annotatedTree
                                                     {-# LINE 6134 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOannotatedTree ->
                                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      LiftApp _annIoriginalTree _operIoriginalTree flav_ _argsIoriginalTree
                                                      {-# LINE 6139 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _originalTree ->
                                               (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                       either (const Nothing) Just _tpe
                                                       {-# LINE 6144 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _upType ->
                                                (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                        let a = _annIoriginalTree
                                                            doStar is =
                                                              map (\((q,n),t) ->
                                                                let a' = setAtype (Just t) a
                                                                in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                                ) is
                                                        in case _originalTree of
                                                            Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                               doStar is
                                                            QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                               doStar is
                                                            _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                        {-# LINE 6160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOcolExprs ->
                                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         _originalTree
                                                         {-# LINE 6165 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _lhsOoriginalTree ->
                                                  (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                          _upType
                                                          {-# LINE 6170 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _lhsOupType ->
                                                   ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_NullLit :: T_Annotation  ->
                          T_ScalarExpr 
sem_ScalarExpr_NullLit ann_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 93 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Right UnknownType
                 {-# LINE 6182 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 6187 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 6194 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 6199 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               NullLit _annIannotatedTree
                               {-# LINE 6206 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 6211 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 NullLit _annIoriginalTree
                                 {-# LINE 6216 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 6221 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _upType ->
                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                   let a = _annIoriginalTree
                                       doStar is =
                                         map (\((q,n),t) ->
                                           let a' = setAtype (Just t) a
                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                           ) is
                                   in case _originalTree of
                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                          doStar is
                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                          doStar is
                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                   {-# LINE 6237 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOcolExprs ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 6242 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                     _upType
                                     {-# LINE 6247 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupType ->
                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_NumberLit :: T_Annotation  ->
                            String ->
                            T_ScalarExpr 
sem_ScalarExpr_NumberLit ann_ d_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 86 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 concatMap show [(0::Int)..9]
                 {-# LINE 6260 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _digChars ->
          (case (({-# LINE 83 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  Right $ if all (`elem` _digChars    ) d_
                          then typeInt
                          else typeNumeric
                  {-# LINE 6267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tpe ->
           (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 6272 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (ann_ ) of
             { ( _annIoriginalTree,ann_1) ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 6279 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOflags ->
                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _lhsIcat
                          {-# LINE 6284 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _annOcat ->
                   (case (ann_1 _annOcat _annOflags _annOtpe ) of
                    { ( _annIannotatedTree) ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                NumberLit _annIannotatedTree d_
                                {-# LINE 6291 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annotatedTree ->
                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _annotatedTree
                                 {-# LINE 6296 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOannotatedTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  NumberLit _annIoriginalTree d_
                                  {-# LINE 6301 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _originalTree ->
                           (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                   either (const Nothing) Just _tpe
                                   {-# LINE 6306 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _upType ->
                            (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                    let a = _annIoriginalTree
                                        doStar is =
                                          map (\((q,n),t) ->
                                            let a' = setAtype (Just t) a
                                            in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                            ) is
                                    in case _originalTree of
                                        Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                           doStar is
                                        QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                           doStar is
                                        _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                    {-# LINE 6322 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOcolExprs ->
                             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _originalTree
                                     {-# LINE 6327 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOoriginalTree ->
                              (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                      _upType
                                      {-# LINE 6332 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOupType ->
                               ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Placeholder :: T_Annotation  ->
                              T_ScalarExpr 
sem_ScalarExpr_Placeholder ann_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 114 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Right UnknownType
                 {-# LINE 6344 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 6349 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 6356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 6361 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               Placeholder _annIannotatedTree
                               {-# LINE 6368 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 6373 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 Placeholder _annIoriginalTree
                                 {-# LINE 6378 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 6383 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _upType ->
                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                   let a = _annIoriginalTree
                                       doStar is =
                                         map (\((q,n),t) ->
                                           let a' = setAtype (Just t) a
                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                           ) is
                                   in case _originalTree of
                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                          doStar is
                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                          doStar is
                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                   {-# LINE 6399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOcolExprs ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 6404 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                     _upType
                                     {-# LINE 6409 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupType ->
                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_PositionalArg :: T_Annotation  ->
                                Integer ->
                                T_ScalarExpr 
sem_ScalarExpr_PositionalArg ann_ p_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 229 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Left []
                 {-# LINE 6422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 6427 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 6434 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 6439 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               PositionalArg _annIannotatedTree p_
                               {-# LINE 6446 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 6451 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 PositionalArg _annIoriginalTree p_
                                 {-# LINE 6456 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 6461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _upType ->
                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                   let a = _annIoriginalTree
                                       doStar is =
                                         map (\((q,n),t) ->
                                           let a' = setAtype (Just t) a
                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                           ) is
                                   in case _originalTree of
                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                          doStar is
                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                          doStar is
                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                   {-# LINE 6477 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOcolExprs ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 6482 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                     _upType
                                     {-# LINE 6487 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupType ->
                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_PostfixOp :: T_Annotation  ->
                            T_Name  ->
                            T_ScalarExpr  ->
                            T_ScalarExpr 
sem_ScalarExpr_PostfixOp ann_ opName_ arg_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 6501 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 6506 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6511 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _argOcat ->
            (case (arg_ _argOcat _argOdownEnv _argOflags ) of
             { ( _argIannotatedTree,_argIcolExprs,_argIoriginalTree,_argIupType) ->
                 (case (opName_ ) of
                  { ( _opNameIoriginalTree,opName_1) ->
                      (case (({-# LINE 157 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                              tcAppLike _lhsIcat _opNameIoriginalTree [_argIupType]
                              {-# LINE 6520 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _tpe ->
                       (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                               _tpe
                               {-# LINE 6525 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _opNameOtpe ->
                        (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                _tpe
                                {-# LINE 6530 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOtpe ->
                         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIflags
                                 {-# LINE 6535 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _opNameOflags ->
                          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _lhsIcat
                                  {-# LINE 6540 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _opNameOcat ->
                           (case (opName_1 _opNameOcat _opNameOflags _opNameOtpe ) of
                            { ( _opNameIannotatedTree) ->
                                (case (ann_ ) of
                                 { ( _annIoriginalTree,ann_1) ->
                                     (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIflags
                                             {-# LINE 6549 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOflags ->
                                      (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _lhsIcat
                                              {-# LINE 6554 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annOcat ->
                                       (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                        { ( _annIannotatedTree) ->
                                            (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    PostfixOp _annIannotatedTree _opNameIannotatedTree _argIannotatedTree
                                                    {-# LINE 6561 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _annotatedTree ->
                                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _annotatedTree
                                                     {-# LINE 6566 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOannotatedTree ->
                                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      PostfixOp _annIoriginalTree _opNameIoriginalTree _argIoriginalTree
                                                      {-# LINE 6571 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _originalTree ->
                                               (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                       either (const Nothing) Just _tpe
                                                       {-# LINE 6576 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _upType ->
                                                (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                        let a = _annIoriginalTree
                                                            doStar is =
                                                              map (\((q,n),t) ->
                                                                let a' = setAtype (Just t) a
                                                                in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                                ) is
                                                        in case _originalTree of
                                                            Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                               doStar is
                                                            QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                               doStar is
                                                            _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                        {-# LINE 6592 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOcolExprs ->
                                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         _originalTree
                                                         {-# LINE 6597 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _lhsOoriginalTree ->
                                                  (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                          _upType
                                                          {-# LINE 6602 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _lhsOupType ->
                                                   ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_PrefixOp :: T_Annotation  ->
                           T_Name  ->
                           T_ScalarExpr  ->
                           T_ScalarExpr 
sem_ScalarExpr_PrefixOp ann_ opName_ arg_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 6616 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 6621 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6626 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _argOcat ->
            (case (arg_ _argOcat _argOdownEnv _argOflags ) of
             { ( _argIannotatedTree,_argIcolExprs,_argIoriginalTree,_argIupType) ->
                 (case (opName_ ) of
                  { ( _opNameIoriginalTree,opName_1) ->
                      (case (({-# LINE 155 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                              tcAppLike _lhsIcat _opNameIoriginalTree [_argIupType]
                              {-# LINE 6635 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _tpe ->
                       (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                               _tpe
                               {-# LINE 6640 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _opNameOtpe ->
                        (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                _tpe
                                {-# LINE 6645 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOtpe ->
                         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIflags
                                 {-# LINE 6650 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _opNameOflags ->
                          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _lhsIcat
                                  {-# LINE 6655 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _opNameOcat ->
                           (case (opName_1 _opNameOcat _opNameOflags _opNameOtpe ) of
                            { ( _opNameIannotatedTree) ->
                                (case (ann_ ) of
                                 { ( _annIoriginalTree,ann_1) ->
                                     (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIflags
                                             {-# LINE 6664 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOflags ->
                                      (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _lhsIcat
                                              {-# LINE 6669 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annOcat ->
                                       (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                        { ( _annIannotatedTree) ->
                                            (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    PrefixOp _annIannotatedTree _opNameIannotatedTree _argIannotatedTree
                                                    {-# LINE 6676 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _annotatedTree ->
                                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _annotatedTree
                                                     {-# LINE 6681 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOannotatedTree ->
                                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      PrefixOp _annIoriginalTree _opNameIoriginalTree _argIoriginalTree
                                                      {-# LINE 6686 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _originalTree ->
                                               (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                       either (const Nothing) Just _tpe
                                                       {-# LINE 6691 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _upType ->
                                                (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                        let a = _annIoriginalTree
                                                            doStar is =
                                                              map (\((q,n),t) ->
                                                                let a' = setAtype (Just t) a
                                                                in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                                ) is
                                                        in case _originalTree of
                                                            Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                               doStar is
                                                            QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                               doStar is
                                                            _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                        {-# LINE 6707 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOcolExprs ->
                                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         _originalTree
                                                         {-# LINE 6712 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _lhsOoriginalTree ->
                                                  (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                          _upType
                                                          {-# LINE 6717 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _lhsOupType ->
                                                   ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_QIdentifier :: T_Annotation  ->
                              ([NameComponent]) ->
                              T_ScalarExpr 
sem_ScalarExpr_QIdentifier ann_ is_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 134 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 fmap snd $ envLookupIdentifier is_ _lhsIdownEnv
                 {-# LINE 6730 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 6735 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 6742 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 6747 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               QIdentifier _annIannotatedTree is_
                               {-# LINE 6754 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 6759 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 QIdentifier _annIoriginalTree is_
                                 {-# LINE 6764 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 6769 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _upType ->
                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                   let a = _annIoriginalTree
                                       doStar is =
                                         map (\((q,n),t) ->
                                           let a' = setAtype (Just t) a
                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                           ) is
                                   in case _originalTree of
                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                          doStar is
                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                          doStar is
                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                   {-# LINE 6785 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOcolExprs ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 6790 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                     _upType
                                     {-# LINE 6795 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupType ->
                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_QStar :: T_Annotation  ->
                        NameComponent ->
                        T_ScalarExpr 
sem_ScalarExpr_QStar ann_ q_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 229 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Left []
                 {-# LINE 6808 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 6813 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 6820 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 6825 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               QStar _annIannotatedTree q_
                               {-# LINE 6832 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 6837 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 QStar _annIoriginalTree q_
                                 {-# LINE 6842 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 6847 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _upType ->
                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                   let a = _annIoriginalTree
                                       doStar is =
                                         map (\((q,n),t) ->
                                           let a' = setAtype (Just t) a
                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                           ) is
                                   in case _originalTree of
                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                          doStar is
                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                          doStar is
                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                   {-# LINE 6863 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOcolExprs ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 6868 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                     _upType
                                     {-# LINE 6873 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupType ->
                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_ScalarSubQuery :: T_Annotation  ->
                                 T_QueryExpr  ->
                                 T_ScalarExpr 
sem_ScalarExpr_ScalarSubQuery ann_ sel_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 6886 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6891 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _selOcat ->
           (case (({-# LINE 214 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Just _lhsIdownEnv
                   {-# LINE 6896 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _selOouterDownEnv ->
            (case (sel_ _selOcat _selOflags _selOouterDownEnv ) of
             { ( _selIannotatedTree,_selIoriginalTree,_selIupType) ->
                 (case (({-# LINE 205 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                         do
                         selType <- maybe (Left []) Right _selIupType
                         case length selType of
                           0 -> Left [InternalError "no columns in scalar subquery?"]
                           1 -> Right $ snd $ head selType
                           _ -> Right $ AnonymousCompositeType $ map snd selType
                         {-# LINE 6908 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tpe ->
                  (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                          _tpe
                          {-# LINE 6913 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _annOtpe ->
                   (case (ann_ ) of
                    { ( _annIoriginalTree,ann_1) ->
                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIflags
                                {-# LINE 6920 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOflags ->
                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIcat
                                 {-# LINE 6925 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annOcat ->
                          (case (ann_1 _annOcat _annOflags _annOtpe ) of
                           { ( _annIannotatedTree) ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       ScalarSubQuery _annIannotatedTree _selIannotatedTree
                                       {-# LINE 6932 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annotatedTree ->
                                (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _annotatedTree
                                        {-# LINE 6937 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOannotatedTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         ScalarSubQuery _annIoriginalTree _selIoriginalTree
                                         {-# LINE 6942 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                          either (const Nothing) Just _tpe
                                          {-# LINE 6947 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _upType ->
                                   (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                           let a = _annIoriginalTree
                                               doStar is =
                                                 map (\((q,n),t) ->
                                                   let a' = setAtype (Just t) a
                                                   in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                   ) is
                                           in case _originalTree of
                                               Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                  doStar is
                                               QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                  doStar is
                                               _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                           {-# LINE 6963 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOcolExprs ->
                                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _originalTree
                                            {-# LINE 6968 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOoriginalTree ->
                                     (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                             _upType
                                             {-# LINE 6973 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOupType ->
                                      ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_SpecialOp :: T_Annotation  ->
                            T_Name  ->
                            T_ScalarExprList  ->
                            T_ScalarExpr 
sem_ScalarExpr_SpecialOp ann_ opName_ args_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 6987 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argsOflags ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 6992 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argsOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6997 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _argsOcat ->
            (case (args_ _argsOcat _argsOdownEnv _argsOflags ) of
             { ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) ->
                 (case (opName_ ) of
                  { ( _opNameIoriginalTree,opName_1) ->
                      (case (({-# LINE 159 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                              tcAppLike _lhsIcat _opNameIoriginalTree _argsIupTypes
                              {-# LINE 7006 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _tpe ->
                       (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                               _tpe
                               {-# LINE 7011 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _opNameOtpe ->
                        (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                _tpe
                                {-# LINE 7016 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOtpe ->
                         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIflags
                                 {-# LINE 7021 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _opNameOflags ->
                          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _lhsIcat
                                  {-# LINE 7026 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _opNameOcat ->
                           (case (opName_1 _opNameOcat _opNameOflags _opNameOtpe ) of
                            { ( _opNameIannotatedTree) ->
                                (case (ann_ ) of
                                 { ( _annIoriginalTree,ann_1) ->
                                     (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIflags
                                             {-# LINE 7035 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOflags ->
                                      (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _lhsIcat
                                              {-# LINE 7040 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annOcat ->
                                       (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                        { ( _annIannotatedTree) ->
                                            (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    SpecialOp _annIannotatedTree _opNameIannotatedTree _argsIannotatedTree
                                                    {-# LINE 7047 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _annotatedTree ->
                                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _annotatedTree
                                                     {-# LINE 7052 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOannotatedTree ->
                                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      SpecialOp _annIoriginalTree _opNameIoriginalTree _argsIoriginalTree
                                                      {-# LINE 7057 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _originalTree ->
                                               (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                       either (const Nothing) Just _tpe
                                                       {-# LINE 7062 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _upType ->
                                                (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                        let a = _annIoriginalTree
                                                            doStar is =
                                                              map (\((q,n),t) ->
                                                                let a' = setAtype (Just t) a
                                                                in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                                ) is
                                                        in case _originalTree of
                                                            Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                               doStar is
                                                            QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                               doStar is
                                                            _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                        {-# LINE 7078 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOcolExprs ->
                                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         _originalTree
                                                         {-# LINE 7083 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _lhsOoriginalTree ->
                                                  (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                          _upType
                                                          {-# LINE 7088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _lhsOupType ->
                                                   ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_Star :: T_Annotation  ->
                       T_ScalarExpr 
sem_ScalarExpr_Star ann_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 229 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Left []
                 {-# LINE 7100 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 7105 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 7112 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 7117 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               Star _annIannotatedTree
                               {-# LINE 7124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 7129 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 Star _annIoriginalTree
                                 {-# LINE 7134 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 7139 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _upType ->
                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                   let a = _annIoriginalTree
                                       doStar is =
                                         map (\((q,n),t) ->
                                           let a' = setAtype (Just t) a
                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                           ) is
                                   in case _originalTree of
                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                          doStar is
                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                          doStar is
                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                   {-# LINE 7155 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOcolExprs ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 7160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                     _upType
                                     {-# LINE 7165 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupType ->
                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_StringLit :: T_Annotation  ->
                            String ->
                            T_ScalarExpr 
sem_ScalarExpr_StringLit ann_ value_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                 Right UnknownType
                 {-# LINE 7178 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tpe ->
          (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _tpe
                  {-# LINE 7183 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (ann_ ) of
            { ( _annIoriginalTree,ann_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 7190 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 7195 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOcat ->
                  (case (ann_1 _annOcat _annOflags _annOtpe ) of
                   { ( _annIannotatedTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               StringLit _annIannotatedTree value_
                               {-# LINE 7202 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 7207 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 StringLit _annIoriginalTree value_
                                 {-# LINE 7212 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                  either (const Nothing) Just _tpe
                                  {-# LINE 7217 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _upType ->
                           (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                   let a = _annIoriginalTree
                                       doStar is =
                                         map (\((q,n),t) ->
                                           let a' = setAtype (Just t) a
                                           in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                           ) is
                                   in case _originalTree of
                                       Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                          doStar is
                                       QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                          doStar is
                                       _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                   {-# LINE 7233 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOcolExprs ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 7238 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                     _upType
                                     {-# LINE 7243 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupType ->
                              ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_TypedStringLit :: T_Annotation  ->
                                 T_TypeName  ->
                                 String ->
                                 T_ScalarExpr 
sem_ScalarExpr_TypedStringLit ann_ tn_ value_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 7257 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tnOcat ->
          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIflags
                  {-# LINE 7262 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tnOflags ->
           (case (tn_ _tnOcat _tnOflags ) of
            { ( _tnIannotatedTree,_tnInamedType,_tnIoriginalTree) ->
                (case (({-# LINE 103 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                        maybe (Left []) Right _tnInamedType
                        {-# LINE 7269 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tpe ->
                 (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                         _tpe
                         {-# LINE 7274 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOtpe ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 7281 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 7286 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      TypedStringLit _annIannotatedTree _tnIannotatedTree value_
                                      {-# LINE 7293 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 7298 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        TypedStringLit _annIoriginalTree _tnIoriginalTree value_
                                        {-# LINE 7303 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                         either (const Nothing) Just _tpe
                                         {-# LINE 7308 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _upType ->
                                  (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                          let a = _annIoriginalTree
                                              doStar is =
                                                map (\((q,n),t) ->
                                                  let a' = setAtype (Just t) a
                                                  in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                  ) is
                                          in case _originalTree of
                                              Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                 doStar is
                                              QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                 doStar is
                                              _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                          {-# LINE 7324 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOcolExprs ->
                                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _originalTree
                                           {-# LINE 7329 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOoriginalTree ->
                                    (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                            _upType
                                            {-# LINE 7334 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOupType ->
                                     ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExpr_WindowApp :: T_Annotation  ->
                            T_ScalarExpr  ->
                            T_ScalarExprList  ->
                            T_ScalarExprDirectionPairList  ->
                            FrameClause ->
                            T_ScalarExpr 
sem_ScalarExpr_WindowApp ann_ fn_ partitionBy_ orderBy_ frm_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 7350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _orderByOflags ->
          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 7355 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _orderByOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7360 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _orderByOcat ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 7365 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _partitionByOflags ->
             (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     _lhsIdownEnv
                     {-# LINE 7370 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _partitionByOdownEnv ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 7375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _partitionByOcat ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 7380 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _fnOflags ->
                (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                        _lhsIdownEnv
                        {-# LINE 7385 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _fnOdownEnv ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 7390 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _fnOcat ->
                  (case (({-# LINE 229 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                          Left []
                          {-# LINE 7395 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _tpe ->
                   (case (({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                           _tpe
                           {-# LINE 7400 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _annOtpe ->
                    (case (orderBy_ _orderByOcat _orderByOdownEnv _orderByOflags ) of
                     { ( _orderByIannotatedTree,_orderByIoriginalTree) ->
                         (case (partitionBy_ _partitionByOcat _partitionByOdownEnv _partitionByOflags ) of
                          { ( _partitionByIannotatedTree,_partitionByIoriginalTree,_partitionByIupTypes) ->
                              (case (fn_ _fnOcat _fnOdownEnv _fnOflags ) of
                               { ( _fnIannotatedTree,_fnIcolExprs,_fnIoriginalTree,_fnIupType) ->
                                   (case (ann_ ) of
                                    { ( _annIoriginalTree,ann_1) ->
                                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _lhsIflags
                                                {-# LINE 7413 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _annOflags ->
                                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _lhsIcat
                                                 {-# LINE 7418 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _annOcat ->
                                          (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                           { ( _annIannotatedTree) ->
                                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                       WindowApp _annIannotatedTree _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree frm_
                                                       {-# LINE 7425 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _annotatedTree ->
                                                (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                        _annotatedTree
                                                        {-# LINE 7430 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOannotatedTree ->
                                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         WindowApp _annIoriginalTree _fnIoriginalTree _partitionByIoriginalTree _orderByIoriginalTree frm_
                                                         {-# LINE 7435 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _originalTree ->
                                                  (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                          either (const Nothing) Just _tpe
                                                          {-# LINE 7440 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _upType ->
                                                   (case (({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                                           let a = _annIoriginalTree
                                                               doStar is =
                                                                 map (\((q,n),t) ->
                                                                   let a' = setAtype (Just t) a
                                                                   in (n, Just t, QIdentifier a' [Nmc q,Nmc n])
                                                                   ) is
                                                           in case _originalTree of
                                                               Star _ | Right is <- envExpandStar Nothing _lhsIdownEnv ->
                                                                  doStar is
                                                               QStar _ q | Right is <- envExpandStar (Just q) _lhsIdownEnv ->
                                                                  doStar is
                                                               _ -> [(columnName _originalTree,_upType,_annotatedTree)]
                                                           {-# LINE 7456 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _lhsOcolExprs ->
                                                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                            _originalTree
                                                            {-# LINE 7461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                            )) of
                                                     { _lhsOoriginalTree ->
                                                     (case (({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                                             _upType
                                                             {-# LINE 7466 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                             )) of
                                                      { _lhsOupType ->
                                                      ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- ScalarExprDirectionPair -------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
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
                                  Environment ->
                                  TypeCheckingFlags ->
                                  ( ScalarExprDirectionPair ,ScalarExprDirectionPair )
data Inh_ScalarExprDirectionPair  = Inh_ScalarExprDirectionPair {cat_Inh_ScalarExprDirectionPair :: Catalog,downEnv_Inh_ScalarExprDirectionPair :: Environment,flags_Inh_ScalarExprDirectionPair :: TypeCheckingFlags}
data Syn_ScalarExprDirectionPair  = Syn_ScalarExprDirectionPair {annotatedTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair ,originalTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair }
wrap_ScalarExprDirectionPair :: T_ScalarExprDirectionPair  ->
                                Inh_ScalarExprDirectionPair  ->
                                Syn_ScalarExprDirectionPair 
wrap_ScalarExprDirectionPair sem (Inh_ScalarExprDirectionPair _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_ScalarExprDirectionPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprDirectionPair_Tuple :: T_ScalarExpr  ->
                                     Direction ->
                                     T_ScalarExprDirectionPair 
sem_ScalarExprDirectionPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 7516 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _x1Oflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 7521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _x1OdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7526 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _x1Ocat ->
            (case (x1_ _x1Ocat _x1OdownEnv _x1Oflags ) of
             { ( _x1IannotatedTree,_x1IcolExprs,_x1IoriginalTree,_x1IupType) ->
                 (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         (_x1IannotatedTree,x2_)
                         {-# LINE 7533 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annotatedTree ->
                  (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _annotatedTree
                          {-# LINE 7538 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _lhsOannotatedTree ->
                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           (_x1IoriginalTree,x2_)
                           {-# LINE 7543 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _originalTree ->
                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                            _originalTree
                            {-# LINE 7548 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _lhsOoriginalTree ->
                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
-- ScalarExprDirectionPairList ---------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
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
                                      Environment ->
                                      TypeCheckingFlags ->
                                      ( ScalarExprDirectionPairList ,ScalarExprDirectionPairList )
data Inh_ScalarExprDirectionPairList  = Inh_ScalarExprDirectionPairList {cat_Inh_ScalarExprDirectionPairList :: Catalog,downEnv_Inh_ScalarExprDirectionPairList :: Environment,flags_Inh_ScalarExprDirectionPairList :: TypeCheckingFlags}
data Syn_ScalarExprDirectionPairList  = Syn_ScalarExprDirectionPairList {annotatedTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList ,originalTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList }
wrap_ScalarExprDirectionPairList :: T_ScalarExprDirectionPairList  ->
                                    Inh_ScalarExprDirectionPairList  ->
                                    Syn_ScalarExprDirectionPairList 
wrap_ScalarExprDirectionPairList sem (Inh_ScalarExprDirectionPairList _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_ScalarExprDirectionPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprDirectionPairList_Cons :: T_ScalarExprDirectionPair  ->
                                        T_ScalarExprDirectionPairList  ->
                                        T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 7602 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 7607 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7612 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tlOcat ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 7617 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOflags ->
             (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                     _lhsIdownEnv
                     {-# LINE 7622 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _hdOdownEnv ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 7627 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _hdOcat ->
               (case (tl_ _tlOcat _tlOdownEnv _tlOflags ) of
                { ( _tlIannotatedTree,_tlIoriginalTree) ->
                    (case (hd_ _hdOcat _hdOdownEnv _hdOflags ) of
                     { ( _hdIannotatedTree,_hdIoriginalTree) ->
                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIannotatedTree _tlIannotatedTree
                                 {-# LINE 7636 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annotatedTree ->
                          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _annotatedTree
                                  {-# LINE 7641 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOannotatedTree ->
                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   (:) _hdIoriginalTree _tlIoriginalTree
                                   {-# LINE 7646 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _originalTree ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 7651 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExprDirectionPairList_Nil :: T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList_Nil  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 7662 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7667 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 7672 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 7677 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- ScalarExprList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
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
                         TypeCheckingFlags ->
                         ( ScalarExprList ,ScalarExprList ,([Maybe Type]))
data Inh_ScalarExprList  = Inh_ScalarExprList {cat_Inh_ScalarExprList :: Catalog,downEnv_Inh_ScalarExprList :: Environment,flags_Inh_ScalarExprList :: TypeCheckingFlags}
data Syn_ScalarExprList  = Syn_ScalarExprList {annotatedTree_Syn_ScalarExprList :: ScalarExprList ,originalTree_Syn_ScalarExprList :: ScalarExprList ,upTypes_Syn_ScalarExprList :: ([Maybe Type])}
wrap_ScalarExprList :: T_ScalarExprList  ->
                       Inh_ScalarExprList  ->
                       Syn_ScalarExprList 
wrap_ScalarExprList sem (Inh_ScalarExprList _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupTypes) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_ScalarExprList _lhsOannotatedTree _lhsOoriginalTree _lhsOupTypes ))
sem_ScalarExprList_Cons :: T_ScalarExpr  ->
                           T_ScalarExprList  ->
                           T_ScalarExprList 
sem_ScalarExprList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 7732 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 7737 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7742 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tlOcat ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 7747 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOflags ->
             (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     _lhsIdownEnv
                     {-# LINE 7752 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _hdOdownEnv ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 7757 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _hdOcat ->
               (case (tl_ _tlOcat _tlOdownEnv _tlOflags ) of
                { ( _tlIannotatedTree,_tlIoriginalTree,_tlIupTypes) ->
                    (case (hd_ _hdOcat _hdOdownEnv _hdOflags ) of
                     { ( _hdIannotatedTree,_hdIcolExprs,_hdIoriginalTree,_hdIupType) ->
                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIannotatedTree _tlIannotatedTree
                                 {-# LINE 7766 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annotatedTree ->
                          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _annotatedTree
                                  {-# LINE 7771 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOannotatedTree ->
                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   (:) _hdIoriginalTree _tlIoriginalTree
                                   {-# LINE 7776 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _originalTree ->
                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _originalTree
                                    {-# LINE 7781 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOoriginalTree ->
                             (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                                     _hdIupType : _tlIupTypes
                                     {-# LINE 7786 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _lhsOupTypes ->
                              ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupTypes) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExprList_Nil :: T_ScalarExprList 
sem_ScalarExprList_Nil  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 7797 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7802 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 7807 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 7812 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             (case (({-# LINE 47 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                     []
                     {-# LINE 7817 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _lhsOupTypes ->
              ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupTypes) }) }) }) }) }))
-- ScalarExprListList ------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                             TypeCheckingFlags ->
                             ( ScalarExprListList ,ScalarExprListList )
data Inh_ScalarExprListList  = Inh_ScalarExprListList {cat_Inh_ScalarExprListList :: Catalog,flags_Inh_ScalarExprListList :: TypeCheckingFlags}
data Syn_ScalarExprListList  = Syn_ScalarExprListList {annotatedTree_Syn_ScalarExprListList :: ScalarExprListList ,originalTree_Syn_ScalarExprListList :: ScalarExprListList }
wrap_ScalarExprListList :: T_ScalarExprListList  ->
                           Inh_ScalarExprListList  ->
                           Syn_ScalarExprListList 
wrap_ScalarExprListList sem (Inh_ScalarExprListList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_ScalarExprListList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprListList_Cons :: T_ScalarExprList  ->
                               T_ScalarExprListList  ->
                               T_ScalarExprListList 
sem_ScalarExprListList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 7868 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7873 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7878 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    error "missing rule: ScalarExprListList.Cons.hd.downEnv"
                    {-# LINE 7883 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOdownEnv ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 7888 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _hdOcat ->
              (case (tl_ _tlOcat _tlOflags ) of
               { ( _tlIannotatedTree,_tlIoriginalTree) ->
                   (case (hd_ _hdOcat _hdOdownEnv _hdOflags ) of
                    { ( _hdIannotatedTree,_hdIoriginalTree,_hdIupTypes) ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                (:) _hdIannotatedTree _tlIannotatedTree
                                {-# LINE 7897 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annotatedTree ->
                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _annotatedTree
                                 {-# LINE 7902 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOannotatedTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  (:) _hdIoriginalTree _tlIoriginalTree
                                  {-# LINE 7907 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _originalTree ->
                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _originalTree
                                   {-# LINE 7912 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOoriginalTree ->
                            ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExprListList_Nil :: T_ScalarExprListList 
sem_ScalarExprListList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 7922 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7927 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 7932 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 7937 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- ScalarExprListStatementListPair -----------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                                          TypeCheckingFlags ->
                                          ( ScalarExprListStatementListPair ,ScalarExprListStatementListPair )
data Inh_ScalarExprListStatementListPair  = Inh_ScalarExprListStatementListPair {cat_Inh_ScalarExprListStatementListPair :: Catalog,flags_Inh_ScalarExprListStatementListPair :: TypeCheckingFlags}
data Syn_ScalarExprListStatementListPair  = Syn_ScalarExprListStatementListPair {annotatedTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair ,originalTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair }
wrap_ScalarExprListStatementListPair :: T_ScalarExprListStatementListPair  ->
                                        Inh_ScalarExprListStatementListPair  ->
                                        Syn_ScalarExprListStatementListPair 
wrap_ScalarExprListStatementListPair sem (Inh_ScalarExprListStatementListPair _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_ScalarExprListStatementListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprListStatementListPair_Tuple :: T_ScalarExprList  ->
                                             T_StatementList  ->
                                             T_ScalarExprListStatementListPair 
sem_ScalarExprListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 7984 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _x2Oflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7989 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _x2Ocat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7994 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _x1Oflags ->
            (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    error "missing rule: ScalarExprListStatementListPair.Tuple.x1.downEnv"
                    {-# LINE 7999 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _x1OdownEnv ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 8004 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _x1Ocat ->
              (case (x2_ _x2Ocat _x2Oflags ) of
               { ( _x2IannotatedTree,_x2IoriginalTree) ->
                   (case (x1_ _x1Ocat _x1OdownEnv _x1Oflags ) of
                    { ( _x1IannotatedTree,_x1IoriginalTree,_x1IupTypes) ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                (_x1IannotatedTree,_x2IannotatedTree)
                                {-# LINE 8013 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annotatedTree ->
                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _annotatedTree
                                 {-# LINE 8018 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOannotatedTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  (_x1IoriginalTree,_x2IoriginalTree)
                                  {-# LINE 8023 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _originalTree ->
                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _originalTree
                                   {-# LINE 8028 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOoriginalTree ->
                            ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
-- ScalarExprListStatementListPairList -------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                                              TypeCheckingFlags ->
                                              ( ScalarExprListStatementListPairList ,ScalarExprListStatementListPairList )
data Inh_ScalarExprListStatementListPairList  = Inh_ScalarExprListStatementListPairList {cat_Inh_ScalarExprListStatementListPairList :: Catalog,flags_Inh_ScalarExprListStatementListPairList :: TypeCheckingFlags}
data Syn_ScalarExprListStatementListPairList  = Syn_ScalarExprListStatementListPairList {annotatedTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList ,originalTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList }
wrap_ScalarExprListStatementListPairList :: T_ScalarExprListStatementListPairList  ->
                                            Inh_ScalarExprListStatementListPairList  ->
                                            Syn_ScalarExprListStatementListPairList 
wrap_ScalarExprListStatementListPairList sem (Inh_ScalarExprListStatementListPairList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_ScalarExprListStatementListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprListStatementListPairList_Cons :: T_ScalarExprListStatementListPair  ->
                                                T_ScalarExprListStatementListPairList  ->
                                                T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 8079 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8084 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8089 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 8094 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOflags ) of
              { ( _tlIannotatedTree,_tlIoriginalTree) ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 8103 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 8108 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 8113 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 8118 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExprListStatementListPairList_Nil :: T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 8128 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8133 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 8138 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 8143 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- ScalarExprRoot ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                         TypeCheckingFlags ->
                         ( ScalarExprRoot ,ScalarExprRoot )
data Inh_ScalarExprRoot  = Inh_ScalarExprRoot {cat_Inh_ScalarExprRoot :: Catalog,flags_Inh_ScalarExprRoot :: TypeCheckingFlags}
data Syn_ScalarExprRoot  = Syn_ScalarExprRoot {annotatedTree_Syn_ScalarExprRoot :: ScalarExprRoot ,originalTree_Syn_ScalarExprRoot :: ScalarExprRoot }
wrap_ScalarExprRoot :: T_ScalarExprRoot  ->
                       Inh_ScalarExprRoot  ->
                       Syn_ScalarExprRoot 
wrap_ScalarExprRoot sem (Inh_ScalarExprRoot _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_ScalarExprRoot _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprRoot_ScalarExprRoot :: T_ScalarExpr  ->
                                     T_ScalarExprRoot 
sem_ScalarExprRoot_ScalarExprRoot expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 8189 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: ScalarExprRoot.ScalarExprRoot.expr.downEnv"
                  {-# LINE 8194 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8199 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (expr_ _exprOcat _exprOdownEnv _exprOflags ) of
             { ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) ->
                 (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         ScalarExprRoot _exprIannotatedTree
                         {-# LINE 8206 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annotatedTree ->
                  (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _annotatedTree
                          {-# LINE 8211 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _lhsOannotatedTree ->
                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           ScalarExprRoot _exprIoriginalTree
                           {-# LINE 8216 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _originalTree ->
                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                            _originalTree
                            {-# LINE 8221 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _lhsOoriginalTree ->
                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }))
-- ScalarExprStatementListPair ---------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                                      TypeCheckingFlags ->
                                      ( ScalarExprStatementListPair ,ScalarExprStatementListPair )
data Inh_ScalarExprStatementListPair  = Inh_ScalarExprStatementListPair {cat_Inh_ScalarExprStatementListPair :: Catalog,flags_Inh_ScalarExprStatementListPair :: TypeCheckingFlags}
data Syn_ScalarExprStatementListPair  = Syn_ScalarExprStatementListPair {annotatedTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair ,originalTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair }
wrap_ScalarExprStatementListPair :: T_ScalarExprStatementListPair  ->
                                    Inh_ScalarExprStatementListPair  ->
                                    Syn_ScalarExprStatementListPair 
wrap_ScalarExprStatementListPair sem (Inh_ScalarExprStatementListPair _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_ScalarExprStatementListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprStatementListPair_Tuple :: T_ScalarExpr  ->
                                         T_StatementList  ->
                                         T_ScalarExprStatementListPair 
sem_ScalarExprStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 8268 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _x2Oflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8273 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _x2Ocat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8278 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _x1Oflags ->
            (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    error "missing rule: ScalarExprStatementListPair.Tuple.x1.downEnv"
                    {-# LINE 8283 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _x1OdownEnv ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 8288 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _x1Ocat ->
              (case (x2_ _x2Ocat _x2Oflags ) of
               { ( _x2IannotatedTree,_x2IoriginalTree) ->
                   (case (x1_ _x1Ocat _x1OdownEnv _x1Oflags ) of
                    { ( _x1IannotatedTree,_x1IcolExprs,_x1IoriginalTree,_x1IupType) ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                (_x1IannotatedTree,_x2IannotatedTree)
                                {-# LINE 8297 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annotatedTree ->
                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _annotatedTree
                                 {-# LINE 8302 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOannotatedTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  (_x1IoriginalTree,_x2IoriginalTree)
                                  {-# LINE 8307 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _originalTree ->
                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                   _originalTree
                                   {-# LINE 8312 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOoriginalTree ->
                            ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }))
-- ScalarExprStatementListPairList -----------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                                          TypeCheckingFlags ->
                                          ( ScalarExprStatementListPairList ,ScalarExprStatementListPairList )
data Inh_ScalarExprStatementListPairList  = Inh_ScalarExprStatementListPairList {cat_Inh_ScalarExprStatementListPairList :: Catalog,flags_Inh_ScalarExprStatementListPairList :: TypeCheckingFlags}
data Syn_ScalarExprStatementListPairList  = Syn_ScalarExprStatementListPairList {annotatedTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList ,originalTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList }
wrap_ScalarExprStatementListPairList :: T_ScalarExprStatementListPairList  ->
                                        Inh_ScalarExprStatementListPairList  ->
                                        Syn_ScalarExprStatementListPairList 
wrap_ScalarExprStatementListPairList sem (Inh_ScalarExprStatementListPairList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_ScalarExprStatementListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprStatementListPairList_Cons :: T_ScalarExprStatementListPair  ->
                                            T_ScalarExprStatementListPairList  ->
                                            T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 8363 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8368 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8373 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 8378 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOflags ) of
              { ( _tlIannotatedTree,_tlIoriginalTree) ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 8387 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 8392 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 8397 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 8402 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_ScalarExprStatementListPairList_Nil :: T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 8412 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 8422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 8427 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- SelectItem --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
      synthesized attributes:
         annotatedTree        : SELF 
         colExprs             : [(String,Maybe Type,ScalarExpr)]
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
                     TypeCheckingFlags ->
                     ( SelectItem ,([(String,Maybe Type,ScalarExpr)]),SelectItem )
data Inh_SelectItem  = Inh_SelectItem {cat_Inh_SelectItem :: Catalog,downEnv_Inh_SelectItem :: Environment,flags_Inh_SelectItem :: TypeCheckingFlags}
data Syn_SelectItem  = Syn_SelectItem {annotatedTree_Syn_SelectItem :: SelectItem ,colExprs_Syn_SelectItem :: ([(String,Maybe Type,ScalarExpr)]),originalTree_Syn_SelectItem :: SelectItem }
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_SelectItem _lhsOannotatedTree _lhsOcolExprs _lhsOoriginalTree ))
sem_SelectItem_SelExp :: T_Annotation  ->
                         T_ScalarExpr  ->
                         T_SelectItem 
sem_SelectItem_SelExp ann_ ex_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 8489 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 8494 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8499 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exOcat ->
            (case (({-# LINE 147 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                    Left []
                    {-# LINE 8504 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (ex_ _exOcat _exOdownEnv _exOflags ) of
              { ( _exIannotatedTree,_exIcolExprs,_exIoriginalTree,_exIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 8513 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 8518 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      SelExp _annIannotatedTree _exIannotatedTree
                                      {-# LINE 8525 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 78 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                       if tcfAddSelectItemAliases _lhsIflags
                                       then case _exIcolExprs of
                                              [(n,_,_)] -> SelectItem _annIannotatedTree _exIannotatedTree (Nmc n)
                                              _ ->
                                                   _annotatedTree
                                       else _annotatedTree
                                       {-# LINE 8535 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 111 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                        _exIcolExprs
                                        {-# LINE 8540 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOcolExprs ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         SelExp _annIoriginalTree _exIoriginalTree
                                         {-# LINE 8545 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _originalTree
                                          {-# LINE 8550 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOoriginalTree ->
                                   ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_SelectItem_SelectItem :: T_Annotation  ->
                             T_ScalarExpr  ->
                             NameComponent ->
                             T_SelectItem 
sem_SelectItem_SelectItem ann_ ex_ name_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 8564 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 8569 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8574 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exOcat ->
            (case (({-# LINE 147 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                    Left []
                    {-# LINE 8579 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (ex_ _exOcat _exOdownEnv _exOflags ) of
              { ( _exIannotatedTree,_exIcolExprs,_exIoriginalTree,_exIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 8588 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 8593 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      SelectItem _annIannotatedTree _exIannotatedTree name_
                                      {-# LINE 8600 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 8605 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 73 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                        case _exIcolExprs of
                                          [(_,t,e)] -> [(ncStr name_, t,e)]
                                          x -> x
                                        {-# LINE 8612 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOcolExprs ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         SelectItem _annIoriginalTree _exIoriginalTree name_
                                         {-# LINE 8617 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _originalTree
                                          {-# LINE 8622 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOoriginalTree ->
                                   ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- SelectItemList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
      synthesized attributes:
         annotatedTree        : SELF 
         colExprs             : [(String,Maybe Type,ScalarExpr)]
         originalTree         : SELF 
         upEnv                : Environment
         upType               : Maybe [(String,Type)]
   alternatives:
      alternative Cons:
         child hd             : SelectItem 
         child tl             : SelectItemList 
         visit 0:
            local annotatedTree : _
            local colExprs    : _
            local originalTree : _
      alternative Nil:
         visit 0:
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
                         TypeCheckingFlags ->
                         ( SelectItemList ,([(String,Maybe Type,ScalarExpr)]),SelectItemList ,Environment,(Maybe [(String,Type)]))
data Inh_SelectItemList  = Inh_SelectItemList {cat_Inh_SelectItemList :: Catalog,downEnv_Inh_SelectItemList :: Environment,flags_Inh_SelectItemList :: TypeCheckingFlags}
data Syn_SelectItemList  = Syn_SelectItemList {annotatedTree_Syn_SelectItemList :: SelectItemList ,colExprs_Syn_SelectItemList :: ([(String,Maybe Type,ScalarExpr)]),originalTree_Syn_SelectItemList :: SelectItemList ,upEnv_Syn_SelectItemList :: Environment,upType_Syn_SelectItemList :: (Maybe [(String,Type)])}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupEnv,_lhsOupType) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_SelectItemList _lhsOannotatedTree _lhsOcolExprs _lhsOoriginalTree _lhsOupEnv _lhsOupType ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 8679 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 115 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 8684 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8689 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tlOcat ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 8694 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOflags ->
             (case (({-# LINE 112 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                     _lhsIdownEnv
                     {-# LINE 8699 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _hdOdownEnv ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 8704 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _hdOcat ->
               (case (tl_ _tlOcat _tlOdownEnv _tlOflags ) of
                { ( _tlIannotatedTree,_tlIcolExprs,_tlIoriginalTree,_tlIupEnv,_tlIupType) ->
                    (case (hd_ _hdOcat _hdOdownEnv _hdOflags ) of
                     { ( _hdIannotatedTree,_hdIcolExprs,_hdIoriginalTree) ->
                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIannotatedTree _tlIannotatedTree
                                 {-# LINE 8713 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _annotatedTree ->
                          (case (({-# LINE 134 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                  _hdIcolExprs ++ _tlIcolExprs
                                  {-# LINE 8718 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _colExprs ->
                           (case (({-# LINE 93 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                   if tcfExpandStars _lhsIflags
                                   then flip map _colExprs $ \(n,_t,e) ->
                                         if n == columnName e
                                         then SelExp emptyAnnotation e
                                         else SelectItem emptyAnnotation e (Nmc n)
                                   else _annotatedTree
                                   {-# LINE 8728 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOannotatedTree ->
                            (case (({-# LINE 135 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                    _colExprs
                                    {-# LINE 8733 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _lhsOcolExprs ->
                             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     (:) _hdIoriginalTree _tlIoriginalTree
                                     {-# LINE 8738 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _originalTree ->
                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _originalTree
                                      {-# LINE 8743 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOoriginalTree ->
                               (case (({-# LINE 118 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                       _tlIupEnv
                                       {-# LINE 8748 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOupEnv ->
                                (case (({-# LINE 136 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                        sequence $ flip map _colExprs
                                        $ \(n,t,_) -> fmap (n,) t
                                        {-# LINE 8754 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOupType ->
                                 ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupEnv,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                 []
                 {-# LINE 8765 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _lhsOannotatedTree ->
          (case (({-# LINE 131 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                  []
                  {-# LINE 8770 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOcolExprs ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 8775 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 8780 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             (case (({-# LINE 118 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                     error "missing rule: SelectItemList.Nil.lhs.upEnv"
                     {-# LINE 8785 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _lhsOupEnv ->
              (case (({-# LINE 132 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                      Nothing
                      {-# LINE 8790 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _lhsOupType ->
               ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupEnv,_lhsOupType) }) }) }) }) }) }))
-- SelectList --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
      synthesized attributes:
         annotatedTree        : SELF 
         colExprs             : [(String,Maybe Type,ScalarExpr)]
         originalTree         : SELF 
         upEnv                : Environment
         upType               : Maybe [(String,Type)]
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
                     TypeCheckingFlags ->
                     ( SelectList ,([(String,Maybe Type,ScalarExpr)]),SelectList ,Environment,(Maybe [(String,Type)]))
data Inh_SelectList  = Inh_SelectList {cat_Inh_SelectList :: Catalog,downEnv_Inh_SelectList :: Environment,flags_Inh_SelectList :: TypeCheckingFlags}
data Syn_SelectList  = Syn_SelectList {annotatedTree_Syn_SelectList :: SelectList ,colExprs_Syn_SelectList :: ([(String,Maybe Type,ScalarExpr)]),originalTree_Syn_SelectList :: SelectList ,upEnv_Syn_SelectList :: Environment,upType_Syn_SelectList :: (Maybe [(String,Type)])}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupEnv,_lhsOupType) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_SelectList _lhsOannotatedTree _lhsOcolExprs _lhsOoriginalTree _lhsOupEnv _lhsOupType ))
sem_SelectList_SelectList :: T_Annotation  ->
                             T_SelectItemList  ->
                             T_SelectList 
sem_SelectList_SelectList ann_ items_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 8844 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _itemsOflags ->
          (case (({-# LINE 115 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                  _lhsIdownEnv
                  {-# LINE 8849 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _itemsOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8854 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _itemsOcat ->
            (case (({-# LINE 144 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                    Left []
                    {-# LINE 8859 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (items_ _itemsOcat _itemsOdownEnv _itemsOflags ) of
              { ( _itemsIannotatedTree,_itemsIcolExprs,_itemsIoriginalTree,_itemsIupEnv,_itemsIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 8868 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 8873 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      SelectList _annIannotatedTree _itemsIannotatedTree
                                      {-# LINE 8880 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 8885 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 116 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                        _itemsIcolExprs
                                        {-# LINE 8890 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOcolExprs ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         SelectList _annIoriginalTree _itemsIoriginalTree
                                         {-# LINE 8895 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _originalTree
                                          {-# LINE 8900 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOoriginalTree ->
                                   (case (({-# LINE 118 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                           _itemsIupEnv
                                           {-# LINE 8905 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _lhsOupEnv ->
                                    (case (({-# LINE 117 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                                            _itemsIupType
                                            {-# LINE 8910 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOupType ->
                                     ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupEnv,_lhsOupType) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- SetClause ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                    TypeCheckingFlags ->
                    ( SetClause ,SetClause )
data Inh_SetClause  = Inh_SetClause {cat_Inh_SetClause :: Catalog,flags_Inh_SetClause :: TypeCheckingFlags}
data Syn_SetClause  = Syn_SetClause {annotatedTree_Syn_SetClause :: SetClause ,originalTree_Syn_SetClause :: SetClause }
wrap_SetClause :: T_SetClause  ->
                  Inh_SetClause  ->
                  Syn_SetClause 
wrap_SetClause sem (Inh_SetClause _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_SetClause _lhsOannotatedTree _lhsOoriginalTree ))
sem_SetClause_MultiSetClause :: T_Annotation  ->
                                ([NameComponent]) ->
                                T_ScalarExpr  ->
                                T_SetClause 
sem_SetClause_MultiSetClause ann_ setTargets_ ex_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 8970 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: SetClause.MultiSetClause.ex.downEnv"
                  {-# LINE 8975 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8980 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: SetClause.MultiSetClause.ann.tpe"
                    {-# LINE 8985 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (ex_ _exOcat _exOdownEnv _exOflags ) of
              { ( _exIannotatedTree,_exIcolExprs,_exIoriginalTree,_exIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 8994 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 8999 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      MultiSetClause _annIannotatedTree setTargets_ _exIannotatedTree
                                      {-# LINE 9006 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 9011 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        MultiSetClause _annIoriginalTree setTargets_ _exIoriginalTree
                                        {-# LINE 9016 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 9021 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_SetClause_SetClause :: T_Annotation  ->
                           NameComponent ->
                           T_ScalarExpr  ->
                           T_SetClause 
sem_SetClause_SetClause ann_ setTarget_ ex_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 9034 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: SetClause.SetClause.ex.downEnv"
                  {-# LINE 9039 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9044 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: SetClause.SetClause.ann.tpe"
                    {-# LINE 9049 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (ex_ _exOcat _exOdownEnv _exOflags ) of
              { ( _exIannotatedTree,_exIcolExprs,_exIoriginalTree,_exIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 9058 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 9063 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      SetClause _annIannotatedTree setTarget_ _exIannotatedTree
                                      {-# LINE 9070 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 9075 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        SetClause _annIoriginalTree setTarget_ _exIoriginalTree
                                        {-# LINE 9080 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 9085 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- SetClauseList -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                        TypeCheckingFlags ->
                        ( SetClauseList ,SetClauseList )
data Inh_SetClauseList  = Inh_SetClauseList {cat_Inh_SetClauseList :: Catalog,flags_Inh_SetClauseList :: TypeCheckingFlags}
data Syn_SetClauseList  = Syn_SetClauseList {annotatedTree_Syn_SetClauseList :: SetClauseList ,originalTree_Syn_SetClauseList :: SetClauseList }
wrap_SetClauseList :: T_SetClauseList  ->
                      Inh_SetClauseList  ->
                      Syn_SetClauseList 
wrap_SetClauseList sem (Inh_SetClauseList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_SetClauseList _lhsOannotatedTree _lhsOoriginalTree ))
sem_SetClauseList_Cons :: T_SetClause  ->
                          T_SetClauseList  ->
                          T_SetClauseList 
sem_SetClauseList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 9136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9141 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9146 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 9151 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOflags ) of
              { ( _tlIannotatedTree,_tlIoriginalTree) ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 9160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 9165 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 9170 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 9175 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 9185 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9190 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9195 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 9200 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- Statement ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                    TypeCheckingFlags ->
                    ( Statement ,Statement )
data Inh_Statement  = Inh_Statement {cat_Inh_Statement :: Catalog,flags_Inh_Statement :: TypeCheckingFlags}
data Syn_Statement  = Syn_Statement {annotatedTree_Syn_Statement :: Statement ,originalTree_Syn_Statement :: Statement }
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_Statement _lhsOannotatedTree _lhsOoriginalTree ))
sem_Statement_AlterSequence :: T_Annotation  ->
                               T_Name  ->
                               T_Name  ->
                               T_Statement 
sem_Statement_AlterSequence ann_ name_ ownedBy_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: Statement.AlterSequence.ownedBy.tpe"
                 {-# LINE 9685 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _ownedByOtpe ->
          (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                  error "missing rule: Statement.AlterSequence.name.tpe"
                  {-# LINE 9690 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _nameOtpe ->
           (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.AlterSequence.ann.tpe"
                   {-# LINE 9695 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (ownedBy_ ) of
             { ( _ownedByIoriginalTree,ownedBy_1) ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 9702 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _ownedByOflags ->
                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _lhsIcat
                          {-# LINE 9707 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _ownedByOcat ->
                   (case (ownedBy_1 _ownedByOcat _ownedByOflags _ownedByOtpe ) of
                    { ( _ownedByIannotatedTree) ->
                        (case (name_ ) of
                         { ( _nameIoriginalTree,name_1) ->
                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIflags
                                     {-# LINE 9716 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _nameOflags ->
                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIcat
                                      {-# LINE 9721 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _nameOcat ->
                               (case (name_1 _nameOcat _nameOflags _nameOtpe ) of
                                { ( _nameIannotatedTree) ->
                                    (case (ann_ ) of
                                     { ( _annIoriginalTree,ann_1) ->
                                         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _lhsIflags
                                                 {-# LINE 9730 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _annOflags ->
                                          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _lhsIcat
                                                  {-# LINE 9735 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _annOcat ->
                                           (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                            { ( _annIannotatedTree) ->
                                                (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                        AlterSequence _annIannotatedTree _nameIannotatedTree _ownedByIannotatedTree
                                                        {-# LINE 9742 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _annotatedTree ->
                                                 (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         _annotatedTree
                                                         {-# LINE 9747 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _lhsOannotatedTree ->
                                                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                          AlterSequence _annIoriginalTree _nameIoriginalTree _ownedByIoriginalTree
                                                          {-# LINE 9752 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _originalTree ->
                                                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                           _originalTree
                                                           {-# LINE 9757 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _lhsOoriginalTree ->
                                                    ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_AlterTable :: T_Annotation  ->
                            T_Name  ->
                            T_AlterTableActionList  ->
                            T_Statement 
sem_Statement_AlterTable ann_ name_ actions_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 9770 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _actionsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9775 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _actionsOcat ->
           (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.AlterTable.name.tpe"
                   {-# LINE 9780 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _nameOtpe ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.AlterTable.ann.tpe"
                    {-# LINE 9785 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (actions_ _actionsOcat _actionsOflags ) of
              { ( _actionsIannotatedTree,_actionsIoriginalTree) ->
                  (case (name_ ) of
                   { ( _nameIoriginalTree,name_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 9794 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _nameOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 9799 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _nameOcat ->
                         (case (name_1 _nameOcat _nameOflags _nameOtpe ) of
                          { ( _nameIannotatedTree) ->
                              (case (ann_ ) of
                               { ( _annIoriginalTree,ann_1) ->
                                   (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _lhsIflags
                                           {-# LINE 9808 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annOflags ->
                                    (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _lhsIcat
                                            {-# LINE 9813 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annOcat ->
                                     (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                      { ( _annIannotatedTree) ->
                                          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  AlterTable _annIannotatedTree _nameIannotatedTree _actionsIannotatedTree
                                                  {-# LINE 9820 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _annotatedTree ->
                                           (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _annotatedTree
                                                   {-# LINE 9825 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOannotatedTree ->
                                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    AlterTable _annIoriginalTree _nameIoriginalTree _actionsIoriginalTree
                                                    {-# LINE 9830 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _originalTree ->
                                             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _originalTree
                                                     {-# LINE 9835 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOoriginalTree ->
                                              ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_AntiStatement :: String ->
                               T_Statement 
sem_Statement_AntiStatement string_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 AntiStatement string_
                 {-# LINE 9846 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9851 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiStatement string_
                   {-# LINE 9856 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 9861 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
sem_Statement_Assignment :: T_Annotation  ->
                            T_Name  ->
                            T_ScalarExpr  ->
                            T_Statement 
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 9874 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _valueOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: Statement.Assignment.value.downEnv"
                  {-# LINE 9879 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _valueOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9884 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _valueOcat ->
            (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                    error "missing rule: Statement.Assignment.target.tpe"
                    {-# LINE 9889 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _targetOtpe ->
             (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     error "missing rule: Statement.Assignment.ann.tpe"
                     {-# LINE 9894 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (value_ _valueOcat _valueOdownEnv _valueOflags ) of
               { ( _valueIannotatedTree,_valueIcolExprs,_valueIoriginalTree,_valueIupType) ->
                   (case (target_ ) of
                    { ( _targetIoriginalTree,target_1) ->
                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIflags
                                {-# LINE 9903 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _targetOflags ->
                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIcat
                                 {-# LINE 9908 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _targetOcat ->
                          (case (target_1 _targetOcat _targetOflags _targetOtpe ) of
                           { ( _targetIannotatedTree) ->
                               (case (ann_ ) of
                                { ( _annIoriginalTree,ann_1) ->
                                    (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _lhsIflags
                                            {-# LINE 9917 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annOflags ->
                                     (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIcat
                                             {-# LINE 9922 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOcat ->
                                      (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                       { ( _annIannotatedTree) ->
                                           (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   Assignment _annIannotatedTree _targetIannotatedTree _valueIannotatedTree
                                                   {-# LINE 9929 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _annotatedTree ->
                                            (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _annotatedTree
                                                    {-# LINE 9934 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOannotatedTree ->
                                             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     Assignment _annIoriginalTree _targetIoriginalTree _valueIoriginalTree
                                                     {-# LINE 9939 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _originalTree ->
                                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _originalTree
                                                      {-# LINE 9944 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOoriginalTree ->
                                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Block :: T_Annotation  ->
                       (Maybe String) ->
                       T_VarDefList  ->
                       T_StatementList  ->
                       T_Statement 
sem_Statement_Block ann_ lb_ vars_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 9958 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9963 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _stsOcat ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9968 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _varsOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.Block.ann.tpe"
                    {-# LINE 9973 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (sts_ _stsOcat _stsOflags ) of
              { ( _stsIannotatedTree,_stsIoriginalTree) ->
                  (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _lhsIflags
                          {-# LINE 9980 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _varsOflags ->
                   (case (vars_ _varsOcat _varsOflags ) of
                    { ( _varsIannotatedTree,_varsIoriginalTree) ->
                        (case (ann_ ) of
                         { ( _annIoriginalTree,ann_1) ->
                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIflags
                                     {-# LINE 9989 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOflags ->
                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIcat
                                      {-# LINE 9994 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annOcat ->
                               (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                { ( _annIannotatedTree) ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            Block _annIannotatedTree lb_ _varsIannotatedTree _stsIannotatedTree
                                            {-# LINE 10001 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annotatedTree ->
                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _annotatedTree
                                             {-# LINE 10006 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOannotatedTree ->
                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              Block _annIoriginalTree lb_ _varsIoriginalTree _stsIoriginalTree
                                              {-# LINE 10011 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _originalTree ->
                                       (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _originalTree
                                               {-# LINE 10016 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOoriginalTree ->
                                        ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CaseStatement :: T_Annotation  ->
                               T_ScalarExprListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 10029 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10034 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _elsOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10039 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _casesOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 10044 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _casesOcat ->
             (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     error "missing rule: Statement.CaseStatement.ann.tpe"
                     {-# LINE 10049 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (els_ _elsOcat _elsOflags ) of
               { ( _elsIannotatedTree,_elsIoriginalTree) ->
                   (case (cases_ _casesOcat _casesOflags ) of
                    { ( _casesIannotatedTree,_casesIoriginalTree) ->
                        (case (ann_ ) of
                         { ( _annIoriginalTree,ann_1) ->
                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIflags
                                     {-# LINE 10060 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOflags ->
                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIcat
                                      {-# LINE 10065 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annOcat ->
                               (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                { ( _annIannotatedTree) ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            CaseStatement _annIannotatedTree _casesIannotatedTree _elsIannotatedTree
                                            {-# LINE 10072 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annotatedTree ->
                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _annotatedTree
                                             {-# LINE 10077 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOannotatedTree ->
                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              CaseStatement _annIoriginalTree _casesIoriginalTree _elsIoriginalTree
                                              {-# LINE 10082 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _originalTree ->
                                       (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _originalTree
                                               {-# LINE 10087 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOoriginalTree ->
                                        ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CaseStatementSimple :: T_Annotation  ->
                                     T_ScalarExpr  ->
                                     T_ScalarExprListStatementListPairList  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_CaseStatementSimple ann_ val_ cases_ els_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 10101 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _elsOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10111 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _casesOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 10116 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _casesOcat ->
             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIflags
                     {-# LINE 10121 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _valOflags ->
              (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                      error "missing rule: Statement.CaseStatementSimple.val.downEnv"
                      {-# LINE 10126 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _valOdownEnv ->
               (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 10131 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _valOcat ->
                (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        error "missing rule: Statement.CaseStatementSimple.ann.tpe"
                        {-# LINE 10136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (els_ _elsOcat _elsOflags ) of
                  { ( _elsIannotatedTree,_elsIoriginalTree) ->
                      (case (cases_ _casesOcat _casesOflags ) of
                       { ( _casesIannotatedTree,_casesIoriginalTree) ->
                           (case (val_ _valOcat _valOdownEnv _valOflags ) of
                            { ( _valIannotatedTree,_valIcolExprs,_valIoriginalTree,_valIupType) ->
                                (case (ann_ ) of
                                 { ( _annIoriginalTree,ann_1) ->
                                     (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIflags
                                             {-# LINE 10149 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOflags ->
                                      (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _lhsIcat
                                              {-# LINE 10154 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annOcat ->
                                       (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                        { ( _annIannotatedTree) ->
                                            (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    CaseStatementSimple _annIannotatedTree _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                                                    {-# LINE 10161 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _annotatedTree ->
                                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _annotatedTree
                                                     {-# LINE 10166 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOannotatedTree ->
                                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      CaseStatementSimple _annIoriginalTree _valIoriginalTree _casesIoriginalTree _elsIoriginalTree
                                                      {-# LINE 10171 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _originalTree ->
                                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                       _originalTree
                                                       {-# LINE 10176 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _lhsOoriginalTree ->
                                                ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_ContinueStatement :: T_Annotation  ->
                                   (Maybe String) ->
                                   T_Statement 
sem_Statement_ContinueStatement ann_ lb_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.ContinueStatement.ann.tpe"
                 {-# LINE 10188 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 10195 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 10200 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              ContinueStatement _annIannotatedTree lb_
                              {-# LINE 10207 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 10212 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                ContinueStatement _annIoriginalTree lb_
                                {-# LINE 10217 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 10222 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_Statement_Copy :: T_Annotation  ->
                      T_Name  ->
                      ([NameComponent]) ->
                      CopySource ->
                      T_Statement 
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: Statement.Copy.table.tpe"
                 {-# LINE 10236 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tableOtpe ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.Copy.ann.tpe"
                  {-# LINE 10241 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (table_ ) of
            { ( _tableIoriginalTree,table_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 10248 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tableOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 10253 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tableOcat ->
                  (case (table_1 _tableOcat _tableOflags _tableOtpe ) of
                   { ( _tableIannotatedTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIflags
                                    {-# LINE 10262 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOflags ->
                             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIcat
                                     {-# LINE 10267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOcat ->
                              (case (ann_1 _annOcat _annOflags _annOtpe ) of
                               { ( _annIannotatedTree) ->
                                   (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           Copy _annIannotatedTree _tableIannotatedTree targetCols_ source_
                                           {-# LINE 10274 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annotatedTree ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _annotatedTree
                                            {-# LINE 10279 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOannotatedTree ->
                                     (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             Copy _annIoriginalTree _tableIoriginalTree targetCols_ source_
                                             {-# LINE 10284 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 10289 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CopyData :: T_Annotation  ->
                          String ->
                          T_Statement 
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.CopyData.ann.tpe"
                 {-# LINE 10301 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 10308 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 10313 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              CopyData _annIannotatedTree insData_
                              {-# LINE 10320 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 10325 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                CopyData _annIoriginalTree insData_
                                {-# LINE 10330 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 10335 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateDomain :: T_Annotation  ->
                              T_Name  ->
                              T_TypeName  ->
                              String ->
                              T_MaybeBoolExpr  ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ constraintName_ check_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 10350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _checkOflags ->
          (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                  error "missing rule: Statement.CreateDomain.check.downEnv"
                  {-# LINE 10355 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _checkOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10360 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _checkOcat ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 10365 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _typOcat ->
             (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                     error "missing rule: Statement.CreateDomain.name.tpe"
                     {-# LINE 10370 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _nameOtpe ->
              (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      error "missing rule: Statement.CreateDomain.ann.tpe"
                      {-# LINE 10375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (check_ _checkOcat _checkOdownEnv _checkOflags ) of
                { ( _checkIannotatedTree,_checkIoriginalTree) ->
                    (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                            _lhsIflags
                            {-# LINE 10382 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _typOflags ->
                     (case (typ_ _typOcat _typOflags ) of
                      { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                          (case (name_ ) of
                           { ( _nameIoriginalTree,name_1) ->
                               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIflags
                                       {-# LINE 10391 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _nameOflags ->
                                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIcat
                                        {-# LINE 10396 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _nameOcat ->
                                 (case (name_1 _nameOcat _nameOflags _nameOtpe ) of
                                  { ( _nameIannotatedTree) ->
                                      (case (ann_ ) of
                                       { ( _annIoriginalTree,ann_1) ->
                                           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _lhsIflags
                                                   {-# LINE 10405 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _annOflags ->
                                            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _lhsIcat
                                                    {-# LINE 10410 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _annOcat ->
                                             (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                              { ( _annIannotatedTree) ->
                                                  (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                          CreateDomain _annIannotatedTree _nameIannotatedTree _typIannotatedTree constraintName_ _checkIannotatedTree
                                                          {-# LINE 10417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _annotatedTree ->
                                                   (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                           _annotatedTree
                                                           {-# LINE 10422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _lhsOannotatedTree ->
                                                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                            CreateDomain _annIoriginalTree _nameIoriginalTree _typIoriginalTree constraintName_ _checkIoriginalTree
                                                            {-# LINE 10427 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                            )) of
                                                     { _originalTree ->
                                                     (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                             _originalTree
                                                             {-# LINE 10432 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                             )) of
                                                      { _lhsOoriginalTree ->
                                                      ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 10450 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _bodyOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10455 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _bodyOcat ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10460 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _rettypeOcat ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 10465 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _paramsOcat ->
             (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                     error "missing rule: Statement.CreateFunction.name.tpe"
                     {-# LINE 10470 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _nameOtpe ->
              (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      error "missing rule: Statement.CreateFunction.ann.tpe"
                      {-# LINE 10475 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (body_ _bodyOcat _bodyOflags ) of
                { ( _bodyIannotatedTree,_bodyIoriginalTree) ->
                    (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                            _lhsIflags
                            {-# LINE 10482 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _rettypeOflags ->
                     (case (rettype_ _rettypeOcat _rettypeOflags ) of
                      { ( _rettypeIannotatedTree,_rettypeInamedType,_rettypeIoriginalTree) ->
                          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _lhsIflags
                                  {-# LINE 10489 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _paramsOflags ->
                           (case (params_ _paramsOcat _paramsOflags ) of
                            { ( _paramsIannotatedTree,_paramsIoriginalTree) ->
                                (case (name_ ) of
                                 { ( _nameIoriginalTree,name_1) ->
                                     (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIflags
                                             {-# LINE 10498 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _nameOflags ->
                                      (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _lhsIcat
                                              {-# LINE 10503 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _nameOcat ->
                                       (case (name_1 _nameOcat _nameOflags _nameOtpe ) of
                                        { ( _nameIannotatedTree) ->
                                            (case (ann_ ) of
                                             { ( _annIoriginalTree,ann_1) ->
                                                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         _lhsIflags
                                                         {-# LINE 10512 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _annOflags ->
                                                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                          _lhsIcat
                                                          {-# LINE 10517 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _annOcat ->
                                                   (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                                    { ( _annIannotatedTree) ->
                                                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                CreateFunction _annIannotatedTree _nameIannotatedTree _paramsIannotatedTree _rettypeIannotatedTree rep_ lang_ _bodyIannotatedTree vol_
                                                                {-# LINE 10524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                )) of
                                                         { _annotatedTree ->
                                                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                 _annotatedTree
                                                                 {-# LINE 10529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                 )) of
                                                          { _lhsOannotatedTree ->
                                                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                  CreateFunction _annIoriginalTree _nameIoriginalTree _paramsIoriginalTree _rettypeIoriginalTree rep_ lang_ _bodyIoriginalTree vol_
                                                                  {-# LINE 10534 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                  )) of
                                                           { _originalTree ->
                                                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                   _originalTree
                                                                   {-# LINE 10539 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                   )) of
                                                            { _lhsOoriginalTree ->
                                                            ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateLanguage :: T_Annotation  ->
                                String ->
                                T_Statement 
sem_Statement_CreateLanguage ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.CreateLanguage.ann.tpe"
                 {-# LINE 10551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 10558 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 10563 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              CreateLanguage _annIannotatedTree name_
                              {-# LINE 10570 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 10575 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                CreateLanguage _annIoriginalTree name_
                                {-# LINE 10580 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 10585 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateSequence :: T_Annotation  ->
                                T_Name  ->
                                Integer ->
                                Integer ->
                                Integer ->
                                Integer ->
                                Integer ->
                                T_Statement 
sem_Statement_CreateSequence ann_ name_ incr_ min_ max_ start_ cache_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: Statement.CreateSequence.name.tpe"
                 {-# LINE 10602 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _nameOtpe ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.CreateSequence.ann.tpe"
                  {-# LINE 10607 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (name_ ) of
            { ( _nameIoriginalTree,name_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 10614 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _nameOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 10619 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _nameOcat ->
                  (case (name_1 _nameOcat _nameOflags _nameOtpe ) of
                   { ( _nameIannotatedTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIflags
                                    {-# LINE 10628 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOflags ->
                             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIcat
                                     {-# LINE 10633 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOcat ->
                              (case (ann_1 _annOcat _annOflags _annOtpe ) of
                               { ( _annIannotatedTree) ->
                                   (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           CreateSequence _annIannotatedTree _nameIannotatedTree incr_ min_ max_ start_ cache_
                                           {-# LINE 10640 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annotatedTree ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _annotatedTree
                                            {-# LINE 10645 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOannotatedTree ->
                                     (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             CreateSequence _annIoriginalTree _nameIoriginalTree incr_ min_ max_ start_ cache_
                                             {-# LINE 10650 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 10655 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateTable :: T_Annotation  ->
                             T_Name  ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 10669 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _consOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10674 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _consOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10679 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _attsOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 10684 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _attsOcat ->
             (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                     error "missing rule: Statement.CreateTable.name.tpe"
                     {-# LINE 10689 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _nameOtpe ->
              (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      error "missing rule: Statement.CreateTable.ann.tpe"
                      {-# LINE 10694 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (cons_ _consOcat _consOflags ) of
                { ( _consIannotatedTree,_consIoriginalTree) ->
                    (case (atts_ _attsOcat _attsOflags ) of
                     { ( _attsIannotatedTree,_attsIoriginalTree) ->
                         (case (name_ ) of
                          { ( _nameIoriginalTree,name_1) ->
                              (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIflags
                                      {-# LINE 10705 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _nameOflags ->
                               (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIcat
                                       {-# LINE 10710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _nameOcat ->
                                (case (name_1 _nameOcat _nameOflags _nameOtpe ) of
                                 { ( _nameIannotatedTree) ->
                                     (case (ann_ ) of
                                      { ( _annIoriginalTree,ann_1) ->
                                          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  _lhsIflags
                                                  {-# LINE 10719 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _annOflags ->
                                           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _lhsIcat
                                                   {-# LINE 10724 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _annOcat ->
                                            (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                             { ( _annIannotatedTree) ->
                                                 (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         CreateTable _annIannotatedTree _nameIannotatedTree _attsIannotatedTree _consIannotatedTree
                                                         {-# LINE 10731 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _annotatedTree ->
                                                  (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                          _annotatedTree
                                                          {-# LINE 10736 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _lhsOannotatedTree ->
                                                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                           CreateTable _annIoriginalTree _nameIoriginalTree _attsIoriginalTree _consIoriginalTree
                                                           {-# LINE 10741 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _originalTree ->
                                                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                            _originalTree
                                                            {-# LINE 10746 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                            )) of
                                                     { _lhsOoriginalTree ->
                                                     ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateTableAs :: T_Annotation  ->
                               T_Name  ->
                               T_QueryExpr  ->
                               T_Statement 
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                 error "missing rule: Statement.CreateTableAs.expr.outerDownEnv"
                 {-# LINE 10759 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOouterDownEnv ->
          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIflags
                  {-# LINE 10764 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOflags ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10769 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                    error "missing rule: Statement.CreateTableAs.name.tpe"
                    {-# LINE 10774 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _nameOtpe ->
             (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     error "missing rule: Statement.CreateTableAs.ann.tpe"
                     {-# LINE 10779 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (expr_ _exprOcat _exprOflags _exprOouterDownEnv ) of
               { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                   (case (name_ ) of
                    { ( _nameIoriginalTree,name_1) ->
                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIflags
                                {-# LINE 10788 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _nameOflags ->
                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIcat
                                 {-# LINE 10793 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _nameOcat ->
                          (case (name_1 _nameOcat _nameOflags _nameOtpe ) of
                           { ( _nameIannotatedTree) ->
                               (case (ann_ ) of
                                { ( _annIoriginalTree,ann_1) ->
                                    (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _lhsIflags
                                            {-# LINE 10802 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annOflags ->
                                     (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIcat
                                             {-# LINE 10807 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOcat ->
                                      (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                       { ( _annIannotatedTree) ->
                                           (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   CreateTableAs _annIannotatedTree _nameIannotatedTree _exprIannotatedTree
                                                   {-# LINE 10814 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _annotatedTree ->
                                            (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _annotatedTree
                                                    {-# LINE 10819 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOannotatedTree ->
                                             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     CreateTableAs _annIoriginalTree _nameIoriginalTree _exprIoriginalTree
                                                     {-# LINE 10824 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _originalTree ->
                                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _originalTree
                                                      {-# LINE 10829 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOoriginalTree ->
                                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 10847 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _fnArgsOflags ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: Statement.CreateTrigger.fnArgs.downEnv"
                  {-# LINE 10852 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _fnArgsOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10857 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _fnArgsOcat ->
            (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                    error "missing rule: Statement.CreateTrigger.fnName.tpe"
                    {-# LINE 10862 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _fnNameOtpe ->
             (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                     error "missing rule: Statement.CreateTrigger.tbl.tpe"
                     {-# LINE 10867 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _tblOtpe ->
              (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      error "missing rule: Statement.CreateTrigger.ann.tpe"
                      {-# LINE 10872 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (fnArgs_ _fnArgsOcat _fnArgsOdownEnv _fnArgsOflags ) of
                { ( _fnArgsIannotatedTree,_fnArgsIoriginalTree,_fnArgsIupTypes) ->
                    (case (fnName_ ) of
                     { ( _fnNameIoriginalTree,fnName_1) ->
                         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIflags
                                 {-# LINE 10881 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _fnNameOflags ->
                          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _lhsIcat
                                  {-# LINE 10886 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _fnNameOcat ->
                           (case (fnName_1 _fnNameOcat _fnNameOflags _fnNameOtpe ) of
                            { ( _fnNameIannotatedTree) ->
                                (case (tbl_ ) of
                                 { ( _tblIoriginalTree,tbl_1) ->
                                     (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIflags
                                             {-# LINE 10895 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _tblOflags ->
                                      (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _lhsIcat
                                              {-# LINE 10900 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _tblOcat ->
                                       (case (tbl_1 _tblOcat _tblOflags _tblOtpe ) of
                                        { ( _tblIannotatedTree) ->
                                            (case (ann_ ) of
                                             { ( _annIoriginalTree,ann_1) ->
                                                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                         _lhsIflags
                                                         {-# LINE 10909 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                         )) of
                                                  { _annOflags ->
                                                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                          _lhsIcat
                                                          {-# LINE 10914 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _annOcat ->
                                                   (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                                    { ( _annIannotatedTree) ->
                                                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                CreateTrigger _annIannotatedTree name_ wh_ events_ _tblIannotatedTree firing_ _fnNameIannotatedTree _fnArgsIannotatedTree
                                                                {-# LINE 10921 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                )) of
                                                         { _annotatedTree ->
                                                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                 _annotatedTree
                                                                 {-# LINE 10926 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                 )) of
                                                          { _lhsOannotatedTree ->
                                                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                  CreateTrigger _annIoriginalTree name_ wh_ events_ _tblIoriginalTree firing_ _fnNameIoriginalTree _fnArgsIoriginalTree
                                                                  {-# LINE 10931 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                  )) of
                                                           { _originalTree ->
                                                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                   _originalTree
                                                                   {-# LINE 10936 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                   )) of
                                                            { _lhsOoriginalTree ->
                                                            ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateType :: T_Annotation  ->
                            T_Name  ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 10949 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _attsOcat ->
          (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                  error "missing rule: Statement.CreateType.name.tpe"
                  {-# LINE 10954 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _nameOtpe ->
           (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateType.ann.tpe"
                   {-# LINE 10959 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 10964 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _attsOflags ->
             (case (atts_ _attsOcat _attsOflags ) of
              { ( _attsIannotatedTree,_attsIoriginalTree) ->
                  (case (name_ ) of
                   { ( _nameIoriginalTree,name_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 10973 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _nameOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 10978 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _nameOcat ->
                         (case (name_1 _nameOcat _nameOflags _nameOtpe ) of
                          { ( _nameIannotatedTree) ->
                              (case (ann_ ) of
                               { ( _annIoriginalTree,ann_1) ->
                                   (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           _lhsIflags
                                           {-# LINE 10987 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annOflags ->
                                    (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _lhsIcat
                                            {-# LINE 10992 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annOcat ->
                                     (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                      { ( _annIannotatedTree) ->
                                          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                  CreateType _annIannotatedTree _nameIannotatedTree _attsIannotatedTree
                                                  {-# LINE 10999 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _annotatedTree ->
                                           (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _annotatedTree
                                                   {-# LINE 11004 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _lhsOannotatedTree ->
                                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    CreateType _annIoriginalTree _nameIoriginalTree _attsIoriginalTree
                                                    {-# LINE 11009 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _originalTree ->
                                             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _originalTree
                                                     {-# LINE 11014 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _lhsOoriginalTree ->
                                              ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_CreateView :: T_Annotation  ->
                            T_Name  ->
                            MaybeNameComponentList ->
                            T_QueryExpr  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ colNames_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                 error "missing rule: Statement.CreateView.expr.outerDownEnv"
                 {-# LINE 11028 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOouterDownEnv ->
          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIflags
                  {-# LINE 11033 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOflags ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11038 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                    error "missing rule: Statement.CreateView.name.tpe"
                    {-# LINE 11043 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _nameOtpe ->
             (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     error "missing rule: Statement.CreateView.ann.tpe"
                     {-# LINE 11048 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (expr_ _exprOcat _exprOflags _exprOouterDownEnv ) of
               { ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) ->
                   (case (name_ ) of
                    { ( _nameIoriginalTree,name_1) ->
                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIflags
                                {-# LINE 11057 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _nameOflags ->
                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIcat
                                 {-# LINE 11062 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _nameOcat ->
                          (case (name_1 _nameOcat _nameOflags _nameOtpe ) of
                           { ( _nameIannotatedTree) ->
                               (case (ann_ ) of
                                { ( _annIoriginalTree,ann_1) ->
                                    (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _lhsIflags
                                            {-# LINE 11071 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annOflags ->
                                     (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIcat
                                             {-# LINE 11076 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOcat ->
                                      (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                       { ( _annIannotatedTree) ->
                                           (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   CreateView _annIannotatedTree _nameIannotatedTree colNames_ _exprIannotatedTree
                                                   {-# LINE 11083 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _annotatedTree ->
                                            (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _annotatedTree
                                                    {-# LINE 11088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOannotatedTree ->
                                             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     CreateView _annIoriginalTree _nameIoriginalTree colNames_ _exprIoriginalTree
                                                     {-# LINE 11093 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _originalTree ->
                                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _originalTree
                                                      {-# LINE 11098 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOoriginalTree ->
                                               ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Delete :: T_Annotation  ->
                        T_Name  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Delete ann_ table_ using_ whr_ returning_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 11113 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _returningOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11118 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _returningOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11123 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _whrOflags ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                    error "missing rule: Statement.Delete.whr.downEnv"
                    {-# LINE 11128 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _whrOdownEnv ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 11133 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _whrOcat ->
              (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIflags
                      {-# LINE 11138 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _usingOflags ->
               (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 11143 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _usingOcat ->
                (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        error "missing rule: Statement.Delete.table.tpe"
                        {-# LINE 11148 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tableOtpe ->
                 (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         error "missing rule: Statement.Delete.ann.tpe"
                         {-# LINE 11153 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOtpe ->
                  (case (returning_ _returningOcat _returningOflags ) of
                   { ( _returningIannotatedTree,_returningIoriginalTree) ->
                       (case (whr_ _whrOcat _whrOdownEnv _whrOflags ) of
                        { ( _whrIannotatedTree,_whrIoriginalTree) ->
                            (case (using_ _usingOcat _usingOflags ) of
                             { ( _usingIannotatedTree,_usingIoriginalTree,_usingIupEnv) ->
                                 (case (table_ ) of
                                  { ( _tableIoriginalTree,table_1) ->
                                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _lhsIflags
                                              {-# LINE 11166 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _tableOflags ->
                                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _lhsIcat
                                               {-# LINE 11171 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _tableOcat ->
                                        (case (table_1 _tableOcat _tableOflags _tableOtpe ) of
                                         { ( _tableIannotatedTree) ->
                                             (case (ann_ ) of
                                              { ( _annIoriginalTree,ann_1) ->
                                                  (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                          _lhsIflags
                                                          {-# LINE 11180 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _annOflags ->
                                                   (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                           _lhsIcat
                                                           {-# LINE 11185 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _annOcat ->
                                                    (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                                     { ( _annIannotatedTree) ->
                                                         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                 Delete _annIannotatedTree _tableIannotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
                                                                 {-# LINE 11192 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                 )) of
                                                          { _annotatedTree ->
                                                          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                  _annotatedTree
                                                                  {-# LINE 11197 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                  )) of
                                                           { _lhsOannotatedTree ->
                                                           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                   Delete _annIoriginalTree _tableIoriginalTree _usingIoriginalTree _whrIoriginalTree _returningIoriginalTree
                                                                   {-# LINE 11202 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                   )) of
                                                            { _originalTree ->
                                                            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                    _originalTree
                                                                    {-# LINE 11207 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                    )) of
                                                             { _lhsOoriginalTree ->
                                                             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_DropFunction :: T_Annotation  ->
                              IfExists ->
                              T_NameTypeNameListPairList  ->
                              Cascade ->
                              T_Statement 
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 11221 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _sigsOcat ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: Statement.DropFunction.ann.tpe"
                  {-# LINE 11226 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11231 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _sigsOflags ->
            (case (sigs_ _sigsOcat _sigsOflags ) of
             { ( _sigsIannotatedTree,_sigsIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 11240 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 11245 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     DropFunction _annIannotatedTree ifE_ _sigsIannotatedTree cascade_
                                     {-# LINE 11252 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 11257 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       DropFunction _annIoriginalTree ifE_ _sigsIoriginalTree cascade_
                                       {-# LINE 11262 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 11267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_DropSomething :: T_Annotation  ->
                               DropType ->
                               IfExists ->
                               ([Name]) ->
                               Cascade ->
                               T_Statement 
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.DropSomething.ann.tpe"
                 {-# LINE 11282 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 11289 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 11294 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              DropSomething _annIannotatedTree dropType_ ifE_ names_ cascade_
                              {-# LINE 11301 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 11306 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                DropSomething _annIoriginalTree dropType_ ifE_ names_ cascade_
                                {-# LINE 11311 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 11316 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_Statement_Execute :: T_Annotation  ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 11328 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: Statement.Execute.expr.downEnv"
                  {-# LINE 11333 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.Execute.ann.tpe"
                    {-# LINE 11343 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (expr_ _exprOcat _exprOdownEnv _exprOflags ) of
              { ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 11352 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 11357 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      Execute _annIannotatedTree _exprIannotatedTree
                                      {-# LINE 11364 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 11369 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        Execute _annIoriginalTree _exprIoriginalTree
                                        {-# LINE 11374 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 11379 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_ExitStatement :: T_Annotation  ->
                               (Maybe String) ->
                               T_Statement 
sem_Statement_ExitStatement ann_ lb_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.ExitStatement.ann.tpe"
                 {-# LINE 11391 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 11398 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 11403 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              ExitStatement _annIannotatedTree lb_
                              {-# LINE 11410 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 11415 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                ExitStatement _annIoriginalTree lb_
                                {-# LINE 11420 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 11425 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_Statement_ForIntegerStatement :: T_Annotation  ->
                                     (Maybe String) ->
                                     NameComponent ->
                                     T_ScalarExpr  ->
                                     T_ScalarExpr  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement ann_ lb_ var_ from_ to_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 11441 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11446 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _stsOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11451 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _toOflags ->
            (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    error "missing rule: Statement.ForIntegerStatement.to.downEnv"
                    {-# LINE 11456 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _toOdownEnv ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 11461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _toOcat ->
              (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIflags
                      {-# LINE 11466 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _fromOflags ->
               (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                       error "missing rule: Statement.ForIntegerStatement.from.downEnv"
                       {-# LINE 11471 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _fromOdownEnv ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 11476 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _fromOcat ->
                 (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         error "missing rule: Statement.ForIntegerStatement.ann.tpe"
                         {-# LINE 11481 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOtpe ->
                  (case (sts_ _stsOcat _stsOflags ) of
                   { ( _stsIannotatedTree,_stsIoriginalTree) ->
                       (case (to_ _toOcat _toOdownEnv _toOflags ) of
                        { ( _toIannotatedTree,_toIcolExprs,_toIoriginalTree,_toIupType) ->
                            (case (from_ _fromOcat _fromOdownEnv _fromOflags ) of
                             { ( _fromIannotatedTree,_fromIcolExprs,_fromIoriginalTree,_fromIupType) ->
                                 (case (ann_ ) of
                                  { ( _annIoriginalTree,ann_1) ->
                                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _lhsIflags
                                              {-# LINE 11494 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annOflags ->
                                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _lhsIcat
                                               {-# LINE 11499 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _annOcat ->
                                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                         { ( _annIannotatedTree) ->
                                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     ForIntegerStatement _annIannotatedTree lb_ var_ _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                                                     {-# LINE 11506 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _annotatedTree ->
                                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _annotatedTree
                                                      {-# LINE 11511 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOannotatedTree ->
                                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                       ForIntegerStatement _annIoriginalTree lb_ var_ _fromIoriginalTree _toIoriginalTree _stsIoriginalTree
                                                       {-# LINE 11516 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _originalTree ->
                                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                        _originalTree
                                                        {-# LINE 11521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                        )) of
                                                 { _lhsOoriginalTree ->
                                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_ForQueryStatement :: T_Annotation  ->
                                   (Maybe String) ->
                                   NameComponent ->
                                   T_QueryExpr  ->
                                   T_StatementList  ->
                                   T_Statement 
sem_Statement_ForQueryStatement ann_ lb_ var_ sel_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 11536 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11541 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _stsOcat ->
           (case (({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: Statement.ForQueryStatement.sel.outerDownEnv"
                   {-# LINE 11546 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _selOouterDownEnv ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 11551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _selOflags ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 11556 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _selOcat ->
              (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      error "missing rule: Statement.ForQueryStatement.ann.tpe"
                      {-# LINE 11561 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (sts_ _stsOcat _stsOflags ) of
                { ( _stsIannotatedTree,_stsIoriginalTree) ->
                    (case (sel_ _selOcat _selOflags _selOouterDownEnv ) of
                     { ( _selIannotatedTree,_selIoriginalTree,_selIupType) ->
                         (case (ann_ ) of
                          { ( _annIoriginalTree,ann_1) ->
                              (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIflags
                                      {-# LINE 11572 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annOflags ->
                               (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIcat
                                       {-# LINE 11577 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annOcat ->
                                (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                 { ( _annIannotatedTree) ->
                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             ForQueryStatement _annIannotatedTree lb_ var_ _selIannotatedTree _stsIannotatedTree
                                             {-# LINE 11584 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annotatedTree ->
                                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _annotatedTree
                                              {-# LINE 11589 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOannotatedTree ->
                                       (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               ForQueryStatement _annIoriginalTree lb_ var_ _selIoriginalTree _stsIoriginalTree
                                               {-# LINE 11594 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _originalTree ->
                                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _originalTree
                                                {-# LINE 11599 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOoriginalTree ->
                                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_If :: T_Annotation  ->
                    T_ScalarExprStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 11612 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _elsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11617 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _elsOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11622 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _casesOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 11627 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _casesOcat ->
             (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     error "missing rule: Statement.If.ann.tpe"
                     {-# LINE 11632 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (els_ _elsOcat _elsOflags ) of
               { ( _elsIannotatedTree,_elsIoriginalTree) ->
                   (case (cases_ _casesOcat _casesOflags ) of
                    { ( _casesIannotatedTree,_casesIoriginalTree) ->
                        (case (ann_ ) of
                         { ( _annIoriginalTree,ann_1) ->
                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIflags
                                     {-# LINE 11643 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOflags ->
                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIcat
                                      {-# LINE 11648 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annOcat ->
                               (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                { ( _annIannotatedTree) ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            If _annIannotatedTree _casesIannotatedTree _elsIannotatedTree
                                            {-# LINE 11655 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annotatedTree ->
                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _annotatedTree
                                             {-# LINE 11660 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOannotatedTree ->
                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              If _annIoriginalTree _casesIoriginalTree _elsIoriginalTree
                                              {-# LINE 11665 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _originalTree ->
                                       (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _originalTree
                                               {-# LINE 11670 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOoriginalTree ->
                                        ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Insert :: T_Annotation  ->
                        T_Name  ->
                        ([NameComponent]) ->
                        T_QueryExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 11685 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _returningOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11690 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _returningOcat ->
           (case (({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: Statement.Insert.insData.outerDownEnv"
                   {-# LINE 11695 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _insDataOouterDownEnv ->
            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIflags
                    {-# LINE 11700 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _insDataOflags ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 11705 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _insDataOcat ->
              (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                      error "missing rule: Statement.Insert.table.tpe"
                      {-# LINE 11710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tableOtpe ->
               (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       error "missing rule: Statement.Insert.ann.tpe"
                       {-# LINE 11715 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOtpe ->
                (case (returning_ _returningOcat _returningOflags ) of
                 { ( _returningIannotatedTree,_returningIoriginalTree) ->
                     (case (insData_ _insDataOcat _insDataOflags _insDataOouterDownEnv ) of
                      { ( _insDataIannotatedTree,_insDataIoriginalTree,_insDataIupType) ->
                          (case (table_ ) of
                           { ( _tableIoriginalTree,table_1) ->
                               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIflags
                                       {-# LINE 11726 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _tableOflags ->
                                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIcat
                                        {-# LINE 11731 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _tableOcat ->
                                 (case (table_1 _tableOcat _tableOflags _tableOtpe ) of
                                  { ( _tableIannotatedTree) ->
                                      (case (ann_ ) of
                                       { ( _annIoriginalTree,ann_1) ->
                                           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   _lhsIflags
                                                   {-# LINE 11740 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _annOflags ->
                                            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _lhsIcat
                                                    {-# LINE 11745 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _annOcat ->
                                             (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                              { ( _annIannotatedTree) ->
                                                  (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                          Insert _annIannotatedTree _tableIannotatedTree targetCols_ _insDataIannotatedTree _returningIannotatedTree
                                                          {-# LINE 11752 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                          )) of
                                                   { _annotatedTree ->
                                                   (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                           _annotatedTree
                                                           {-# LINE 11757 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                           )) of
                                                    { _lhsOannotatedTree ->
                                                    (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                            Insert _annIoriginalTree _tableIoriginalTree targetCols_ _insDataIoriginalTree _returningIoriginalTree
                                                            {-# LINE 11762 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                            )) of
                                                     { _originalTree ->
                                                     (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                             _originalTree
                                                             {-# LINE 11767 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                             )) of
                                                      { _lhsOoriginalTree ->
                                                      ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Into :: T_Annotation  ->
                      Bool ->
                      ([Name]) ->
                      T_Statement  ->
                      T_Statement 
sem_Statement_Into ann_ strict_ into_ stmt_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 11781 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stmtOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11786 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _stmtOcat ->
           (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Into.ann.tpe"
                   {-# LINE 11791 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (stmt_ _stmtOcat _stmtOflags ) of
             { ( _stmtIannotatedTree,_stmtIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 11800 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 11805 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     Into _annIannotatedTree strict_ into_ _stmtIannotatedTree
                                     {-# LINE 11812 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 11817 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       Into _annIoriginalTree strict_ into_ _stmtIoriginalTree
                                       {-# LINE 11822 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 11827 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_LoopStatement :: T_Annotation  ->
                               (Maybe String) ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_LoopStatement ann_ lb_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 11840 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11845 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _stsOcat ->
           (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.LoopStatement.ann.tpe"
                   {-# LINE 11850 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (sts_ _stsOcat _stsOflags ) of
             { ( _stsIannotatedTree,_stsIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 11859 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 11864 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     LoopStatement _annIannotatedTree lb_ _stsIannotatedTree
                                     {-# LINE 11871 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 11876 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       LoopStatement _annIoriginalTree lb_ _stsIoriginalTree
                                       {-# LINE 11881 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 11886 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Notify :: T_Annotation  ->
                        String ->
                        T_Statement 
sem_Statement_Notify ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.Notify.ann.tpe"
                 {-# LINE 11898 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 11905 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 11910 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              Notify _annIannotatedTree name_
                              {-# LINE 11917 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 11922 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                Notify _annIoriginalTree name_
                                {-# LINE 11927 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 11932 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_Statement_NullStatement :: T_Annotation  ->
                               T_Statement 
sem_Statement_NullStatement ann_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.NullStatement.ann.tpe"
                 {-# LINE 11943 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 11950 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 11955 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              NullStatement _annIannotatedTree
                              {-# LINE 11962 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 11967 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                NullStatement _annIoriginalTree
                                {-# LINE 11972 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 11977 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_Statement_Perform :: T_Annotation  ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 11989 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: Statement.Perform.expr.downEnv"
                  {-# LINE 11994 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11999 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.Perform.ann.tpe"
                    {-# LINE 12004 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (expr_ _exprOcat _exprOdownEnv _exprOflags ) of
              { ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 12013 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 12018 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      Perform _annIannotatedTree _exprIannotatedTree
                                      {-# LINE 12025 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 12030 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        Perform _annIoriginalTree _exprIoriginalTree
                                        {-# LINE 12035 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 12040 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_QueryStatement :: T_Annotation  ->
                                T_QueryExpr  ->
                                T_Statement 
sem_Statement_QueryStatement ann_ ex_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 12052 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12057 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOcat ->
           (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.QueryStatement.ann.tpe"
                   {-# LINE 12062 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                    Nothing
                    {-# LINE 12067 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _exOouterDownEnv ->
             (case (ex_ _exOcat _exOflags _exOouterDownEnv ) of
              { ( _exIannotatedTree,_exIoriginalTree,_exIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 12076 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 12081 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      QueryStatement _annIannotatedTree _exIannotatedTree
                                      {-# LINE 12088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 12093 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        QueryStatement _annIoriginalTree _exIoriginalTree
                                        {-# LINE 12098 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 12103 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Raise :: T_Annotation  ->
                       RaiseType ->
                       String ->
                       T_ScalarExprList  ->
                       T_Statement 
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 12117 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _argsOflags ->
          (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: Statement.Raise.args.downEnv"
                  {-# LINE 12122 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _argsOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12127 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _argsOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.Raise.ann.tpe"
                    {-# LINE 12132 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (args_ _argsOcat _argsOdownEnv _argsOflags ) of
              { ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 12141 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 12146 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      Raise _annIannotatedTree level_ message_ _argsIannotatedTree
                                      {-# LINE 12153 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 12158 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        Raise _annIoriginalTree level_ message_ _argsIoriginalTree
                                        {-# LINE 12163 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 12168 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Return :: T_Annotation  ->
                        T_MaybeScalarExpr  ->
                        T_Statement 
sem_Statement_Return ann_ value_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 12180 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _valueOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12185 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _valueOcat ->
           (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Return.ann.tpe"
                   {-# LINE 12190 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (value_ _valueOcat _valueOflags ) of
             { ( _valueIannotatedTree,_valueIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 12199 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 12204 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     Return _annIannotatedTree _valueIannotatedTree
                                     {-# LINE 12211 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 12216 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       Return _annIoriginalTree _valueIoriginalTree
                                       {-# LINE 12221 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 12226 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_ReturnNext :: T_Annotation  ->
                            T_ScalarExpr  ->
                            T_Statement 
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 12238 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exprOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: Statement.ReturnNext.expr.downEnv"
                  {-# LINE 12243 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exprOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12248 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.ReturnNext.ann.tpe"
                    {-# LINE 12253 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (expr_ _exprOcat _exprOdownEnv _exprOflags ) of
              { ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 12262 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 12267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      ReturnNext _annIannotatedTree _exprIannotatedTree
                                      {-# LINE 12274 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 12279 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        ReturnNext _annIoriginalTree _exprIoriginalTree
                                        {-# LINE 12284 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 12289 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_ReturnQuery :: T_Annotation  ->
                             T_QueryExpr  ->
                             T_Statement 
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                 error "missing rule: Statement.ReturnQuery.sel.outerDownEnv"
                 {-# LINE 12301 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOouterDownEnv ->
          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIflags
                  {-# LINE 12306 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _selOflags ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12311 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _selOcat ->
            (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    error "missing rule: Statement.ReturnQuery.ann.tpe"
                    {-# LINE 12316 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _annOtpe ->
             (case (sel_ _selOcat _selOflags _selOouterDownEnv ) of
              { ( _selIannotatedTree,_selIoriginalTree,_selIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 12325 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 12330 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      ReturnQuery _annIannotatedTree _selIannotatedTree
                                      {-# LINE 12337 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 12342 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        ReturnQuery _annIoriginalTree _selIoriginalTree
                                        {-# LINE 12347 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 12352 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_Set :: T_Annotation  ->
                     String ->
                     ([SetValue]) ->
                     T_Statement 
sem_Statement_Set ann_ name_ values_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.Set.ann.tpe"
                 {-# LINE 12365 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 12372 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 12377 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              Set _annIannotatedTree name_ values_
                              {-# LINE 12384 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 12389 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                Set _annIoriginalTree name_ values_
                                {-# LINE 12394 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 12399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_Statement_Truncate :: T_Annotation  ->
                          ([Name]) ->
                          RestartIdentity ->
                          Cascade ->
                          T_Statement 
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: Statement.Truncate.ann.tpe"
                 {-# LINE 12413 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 12420 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 12425 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              Truncate _annIannotatedTree tables_ restartIdentity_ cascade_
                              {-# LINE 12432 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 12437 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                Truncate _annIoriginalTree tables_ restartIdentity_ cascade_
                                {-# LINE 12442 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 12447 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_Statement_Update :: T_Annotation  ->
                        T_Name  ->
                        T_SetClauseList  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Update ann_ table_ assigns_ fromList_ whr_ returning_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 12463 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _returningOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12468 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _returningOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12473 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _whrOflags ->
            (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                    error "missing rule: Statement.Update.whr.downEnv"
                    {-# LINE 12478 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _whrOdownEnv ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 12483 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _whrOcat ->
              (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIflags
                      {-# LINE 12488 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _fromListOflags ->
               (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 12493 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _fromListOcat ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 12498 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _assignsOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 12503 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _assignsOcat ->
                  (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                          error "missing rule: Statement.Update.table.tpe"
                          {-# LINE 12508 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _tableOtpe ->
                   (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           error "missing rule: Statement.Update.ann.tpe"
                           {-# LINE 12513 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _annOtpe ->
                    (case (returning_ _returningOcat _returningOflags ) of
                     { ( _returningIannotatedTree,_returningIoriginalTree) ->
                         (case (whr_ _whrOcat _whrOdownEnv _whrOflags ) of
                          { ( _whrIannotatedTree,_whrIoriginalTree) ->
                              (case (fromList_ _fromListOcat _fromListOflags ) of
                               { ( _fromListIannotatedTree,_fromListIoriginalTree,_fromListIupEnv) ->
                                   (case (assigns_ _assignsOcat _assignsOflags ) of
                                    { ( _assignsIannotatedTree,_assignsIoriginalTree) ->
                                        (case (table_ ) of
                                         { ( _tableIoriginalTree,table_1) ->
                                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _lhsIflags
                                                     {-# LINE 12528 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _tableOflags ->
                                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _lhsIcat
                                                      {-# LINE 12533 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _tableOcat ->
                                               (case (table_1 _tableOcat _tableOflags _tableOtpe ) of
                                                { ( _tableIannotatedTree) ->
                                                    (case (ann_ ) of
                                                     { ( _annIoriginalTree,ann_1) ->
                                                         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                 _lhsIflags
                                                                 {-# LINE 12542 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                 )) of
                                                          { _annOflags ->
                                                          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                  _lhsIcat
                                                                  {-# LINE 12547 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                  )) of
                                                           { _annOcat ->
                                                           (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                                            { ( _annIannotatedTree) ->
                                                                (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                        Update _annIannotatedTree _tableIannotatedTree _assignsIannotatedTree _fromListIannotatedTree _whrIannotatedTree _returningIannotatedTree
                                                                        {-# LINE 12554 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                        )) of
                                                                 { _annotatedTree ->
                                                                 (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                         _annotatedTree
                                                                         {-# LINE 12559 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                         )) of
                                                                  { _lhsOannotatedTree ->
                                                                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                          Update _annIoriginalTree _tableIoriginalTree _assignsIoriginalTree _fromListIoriginalTree _whrIoriginalTree _returningIoriginalTree
                                                                          {-# LINE 12564 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                          )) of
                                                                   { _originalTree ->
                                                                   (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                                           _originalTree
                                                                           {-# LINE 12569 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                           )) of
                                                                    { _lhsOoriginalTree ->
                                                                    ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Statement_WhileStatement :: T_Annotation  ->
                                (Maybe String) ->
                                T_ScalarExpr  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement ann_ lb_ expr_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 12583 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _stsOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12588 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _stsOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12593 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _exprOflags ->
            (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                    error "missing rule: Statement.WhileStatement.expr.downEnv"
                    {-# LINE 12598 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _exprOdownEnv ->
             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIcat
                     {-# LINE 12603 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _exprOcat ->
              (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      error "missing rule: Statement.WhileStatement.ann.tpe"
                      {-# LINE 12608 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _annOtpe ->
               (case (sts_ _stsOcat _stsOflags ) of
                { ( _stsIannotatedTree,_stsIoriginalTree) ->
                    (case (expr_ _exprOcat _exprOdownEnv _exprOflags ) of
                     { ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) ->
                         (case (ann_ ) of
                          { ( _annIoriginalTree,ann_1) ->
                              (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIflags
                                      {-# LINE 12619 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annOflags ->
                               (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIcat
                                       {-# LINE 12624 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annOcat ->
                                (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                 { ( _annIannotatedTree) ->
                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             WhileStatement _annIannotatedTree lb_ _exprIannotatedTree _stsIannotatedTree
                                             {-# LINE 12631 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annotatedTree ->
                                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _annotatedTree
                                              {-# LINE 12636 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOannotatedTree ->
                                       (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               WhileStatement _annIoriginalTree lb_ _exprIoriginalTree _stsIoriginalTree
                                               {-# LINE 12641 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _originalTree ->
                                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _originalTree
                                                {-# LINE 12646 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOoriginalTree ->
                                         ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- StatementList -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                        TypeCheckingFlags ->
                        ( StatementList ,StatementList )
data Inh_StatementList  = Inh_StatementList {cat_Inh_StatementList :: Catalog,flags_Inh_StatementList :: TypeCheckingFlags}
data Syn_StatementList  = Syn_StatementList {annotatedTree_Syn_StatementList :: StatementList ,originalTree_Syn_StatementList :: StatementList }
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_StatementList _lhsOannotatedTree _lhsOoriginalTree ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 12697 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12702 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12707 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 12712 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOflags ) of
              { ( _tlIannotatedTree,_tlIoriginalTree) ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 12721 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 12726 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 12731 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 12736 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 12746 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12751 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 12756 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 12761 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- TableAlias --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                     TypeCheckingFlags ->
                     ( TableAlias ,TableAlias )
data Inh_TableAlias  = Inh_TableAlias {cat_Inh_TableAlias :: Catalog,flags_Inh_TableAlias :: TypeCheckingFlags}
data Syn_TableAlias  = Syn_TableAlias {annotatedTree_Syn_TableAlias :: TableAlias ,originalTree_Syn_TableAlias :: TableAlias }
wrap_TableAlias :: T_TableAlias  ->
                   Inh_TableAlias  ->
                   Syn_TableAlias 
wrap_TableAlias sem (Inh_TableAlias _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_TableAlias _lhsOannotatedTree _lhsOoriginalTree ))
sem_TableAlias_FullAlias :: T_Annotation  ->
                            NameComponent ->
                            ([NameComponent]) ->
                            T_TableAlias 
sem_TableAlias_FullAlias ann_ tb_ cols_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                 Left []
                 {-# LINE 12828 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 12835 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 12840 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              FullAlias _annIannotatedTree tb_ cols_
                              {-# LINE 12847 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 12852 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                FullAlias _annIoriginalTree tb_ cols_
                                {-# LINE 12857 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 12862 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_TableAlias_NoAlias :: T_Annotation  ->
                          T_TableAlias 
sem_TableAlias_NoAlias ann_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                 Left []
                 {-# LINE 12873 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 12880 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 12885 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              NoAlias _annIannotatedTree
                              {-# LINE 12892 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 12897 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                NoAlias _annIoriginalTree
                                {-# LINE 12902 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 12907 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_TableAlias_TableAlias :: T_Annotation  ->
                             NameComponent ->
                             T_TableAlias 
sem_TableAlias_TableAlias ann_ tb_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 65 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                 Left []
                 {-# LINE 12919 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 12926 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 12931 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              TableAlias _annIannotatedTree tb_
                              {-# LINE 12938 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 12943 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                TableAlias _annIoriginalTree tb_
                                {-# LINE 12948 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 12953 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
-- TableRef ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
         child tbl0           : TableRef 
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
sem_TableRef (JoinTref _ann _tbl0 _nat _joinType _tbl1 _onExpr _alias )  =
    (sem_TableRef_JoinTref (sem_Annotation _ann ) (sem_TableRef _tbl0 ) _nat _joinType (sem_TableRef _tbl1 ) (sem_OnExpr _onExpr ) (sem_TableAlias _alias ) )
sem_TableRef (SubTref _ann _sel _alias )  =
    (sem_TableRef_SubTref (sem_Annotation _ann ) (sem_QueryExpr _sel ) (sem_TableAlias _alias ) )
sem_TableRef (Tref _ann _tbl _alias )  =
    (sem_TableRef_Tref (sem_Annotation _ann ) (sem_Name _tbl ) (sem_TableAlias _alias ) )
-- semantic domain
type T_TableRef  = Catalog ->
                   TypeCheckingFlags ->
                   ( TableRef ,TableRef ,Environment)
data Inh_TableRef  = Inh_TableRef {cat_Inh_TableRef :: Catalog,flags_Inh_TableRef :: TypeCheckingFlags}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef ,originalTree_Syn_TableRef :: TableRef ,upEnv_Syn_TableRef :: Environment}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) = sem _lhsIcat _lhsIflags 
     in  (Syn_TableRef _lhsOannotatedTree _lhsOoriginalTree _lhsOupEnv ))
sem_TableRef_FunTref :: T_Annotation  ->
                        T_ScalarExpr  ->
                        T_TableAlias  ->
                        T_TableRef 
sem_TableRef_FunTref ann_ fn_ alias_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 13042 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _fnOflags ->
          (case (({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                  error "missing rule: TableRef.FunTref.fn.downEnv"
                  {-# LINE 13047 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _fnOdownEnv ->
           (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13052 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _fnOcat ->
            (case (({-# LINE 61 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                    Left []
                    {-# LINE 13057 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _eEnv ->
             (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                     either Left (const $ Left []) _eEnv
                     {-# LINE 13062 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _annOtpe ->
              (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIflags
                      {-# LINE 13067 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _aliasOflags ->
               (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIcat
                       {-# LINE 13072 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _aliasOcat ->
                (case (alias_ _aliasOcat _aliasOflags ) of
                 { ( _aliasIannotatedTree,_aliasIoriginalTree) ->
                     (case (fn_ _fnOcat _fnOdownEnv _fnOflags ) of
                      { ( _fnIannotatedTree,_fnIcolExprs,_fnIoriginalTree,_fnIupType) ->
                          (case (ann_ ) of
                           { ( _annIoriginalTree,ann_1) ->
                               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIflags
                                       {-# LINE 13083 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annOflags ->
                                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIcat
                                        {-# LINE 13088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOcat ->
                                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                  { ( _annIannotatedTree) ->
                                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              FunTref _annIannotatedTree _fnIannotatedTree _aliasIannotatedTree
                                              {-# LINE 13095 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annotatedTree ->
                                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _annotatedTree
                                               {-# LINE 13100 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOannotatedTree ->
                                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                FunTref _annIoriginalTree _fnIoriginalTree _aliasIoriginalTree
                                                {-# LINE 13105 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _originalTree ->
                                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _originalTree
                                                 {-# LINE 13110 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _lhsOoriginalTree ->
                                          (case (({-# LINE 30 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                                  either (const emptyEnvironment) id _eEnv
                                                  {-# LINE 13115 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOupEnv ->
                                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TableRef_JoinTref :: T_Annotation  ->
                         T_TableRef  ->
                         Natural ->
                         JoinType ->
                         T_TableRef  ->
                         T_OnExpr  ->
                         T_TableAlias  ->
                         T_TableRef 
sem_TableRef_JoinTref ann_ tbl0_ nat_ joinType_ tbl1_ onExpr_ alias_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 13132 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _onExprOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13137 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _onExprOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13142 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tbl1Oflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 13147 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _tbl1Ocat ->
             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                     _lhsIflags
                     {-# LINE 13152 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _tbl0Oflags ->
              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                      _lhsIcat
                      {-# LINE 13157 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tbl0Ocat ->
               (case (({-# LINE 52 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                       emptyEnvironment
                       {-# LINE 13162 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _onExprOdownEnv ->
                (case (onExpr_ _onExprOcat _onExprOdownEnv _onExprOflags ) of
                 { ( _onExprIannotatedTree,_onExprIoriginalTree) ->
                     (case (tbl1_ _tbl1Ocat _tbl1Oflags ) of
                      { ( _tbl1IannotatedTree,_tbl1IoriginalTree,_tbl1IupEnv) ->
                          (case (tbl0_ _tbl0Ocat _tbl0Oflags ) of
                           { ( _tbl0IannotatedTree,_tbl0IoriginalTree,_tbl0IupEnv) ->
                               (case (({-# LINE 42 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                       createJoinTrefEnvironment _lhsIcat
                                         _tbl0IupEnv _tbl1IupEnv
                                         $ case (joinType_,_onExprIoriginalTree) of
                                            (x,Nothing) | x /= Cross -> Nothing
                                            (_,Just (JoinUsing _ nms)) -> Just nms
                                            _ -> Just []
                                       {-# LINE 13178 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _eEnv ->
                                (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                        either Left (const $ Left []) _eEnv
                                        {-# LINE 13183 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOtpe ->
                                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _lhsIflags
                                         {-# LINE 13188 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _aliasOflags ->
                                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _lhsIcat
                                          {-# LINE 13193 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _aliasOcat ->
                                   (case (alias_ _aliasOcat _aliasOflags ) of
                                    { ( _aliasIannotatedTree,_aliasIoriginalTree) ->
                                        (case (ann_ ) of
                                         { ( _annIoriginalTree,ann_1) ->
                                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     _lhsIflags
                                                     {-# LINE 13202 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _annOflags ->
                                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _lhsIcat
                                                      {-# LINE 13207 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _annOcat ->
                                               (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                                { ( _annIannotatedTree) ->
                                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                            JoinTref _annIannotatedTree _tbl0IannotatedTree nat_ joinType_ _tbl1IannotatedTree _onExprIannotatedTree _aliasIannotatedTree
                                                            {-# LINE 13214 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                            )) of
                                                     { _annotatedTree ->
                                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                             _annotatedTree
                                                             {-# LINE 13219 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                             )) of
                                                      { _lhsOannotatedTree ->
                                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                              JoinTref _annIoriginalTree _tbl0IoriginalTree nat_ joinType_ _tbl1IoriginalTree _onExprIoriginalTree _aliasIoriginalTree
                                                              {-# LINE 13224 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                              )) of
                                                       { _originalTree ->
                                                       (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                               _originalTree
                                                               {-# LINE 13229 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                               )) of
                                                        { _lhsOoriginalTree ->
                                                        (case (({-# LINE 30 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                                                either (const emptyEnvironment) id _eEnv
                                                                {-# LINE 13234 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                                )) of
                                                         { _lhsOupEnv ->
                                                         ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TableRef_SubTref :: T_Annotation  ->
                        T_QueryExpr  ->
                        T_TableAlias  ->
                        T_TableRef 
sem_TableRef_SubTref ann_ sel_ alias_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 13247 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _selOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13252 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _selOcat ->
           (case (({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Nothing
                   {-# LINE 13257 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _selOouterDownEnv ->
            (case (sel_ _selOcat _selOflags _selOouterDownEnv ) of
             { ( _selIannotatedTree,_selIoriginalTree,_selIupType) ->
                 (case (({-# LINE 54 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                         maybe (Left []) id
                         $ fmap envSelectListEnvironment _selIupType
                         {-# LINE 13265 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _eEnv ->
                  (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                          either Left (const $ Left []) _eEnv
                          {-# LINE 13270 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _annOtpe ->
                   (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                           _lhsIflags
                           {-# LINE 13275 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                           )) of
                    { _aliasOflags ->
                    (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                            _lhsIcat
                            {-# LINE 13280 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                            )) of
                     { _aliasOcat ->
                     (case (alias_ _aliasOcat _aliasOflags ) of
                      { ( _aliasIannotatedTree,_aliasIoriginalTree) ->
                          (case (ann_ ) of
                           { ( _annIoriginalTree,ann_1) ->
                               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _lhsIflags
                                       {-# LINE 13289 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _annOflags ->
                                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _lhsIcat
                                        {-# LINE 13294 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _annOcat ->
                                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                  { ( _annIannotatedTree) ->
                                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              SubTref _annIannotatedTree _selIannotatedTree _aliasIannotatedTree
                                              {-# LINE 13301 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _annotatedTree ->
                                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               _annotatedTree
                                               {-# LINE 13306 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _lhsOannotatedTree ->
                                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                SubTref _annIoriginalTree _selIoriginalTree _aliasIoriginalTree
                                                {-# LINE 13311 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _originalTree ->
                                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                 _originalTree
                                                 {-# LINE 13316 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                 )) of
                                          { _lhsOoriginalTree ->
                                          (case (({-# LINE 30 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                                  either (const emptyEnvironment) id _eEnv
                                                  {-# LINE 13321 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                  )) of
                                           { _lhsOupEnv ->
                                           ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TableRef_Tref :: T_Annotation  ->
                     T_Name  ->
                     T_TableAlias  ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_ alias_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                 Left []
                 {-# LINE 13334 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tblOtpe ->
          (case (tbl_ ) of
           { ( _tblIoriginalTree,tbl_1) ->
               (case (({-# LINE 38 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                       envCreateTrefEnvironment _lhsIcat (nameComponents _tblIoriginalTree)
                       {-# LINE 13341 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _eEnv ->
                (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                        either Left (const $ Left []) _eEnv
                        {-# LINE 13346 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 13351 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _aliasOflags ->
                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _lhsIcat
                          {-# LINE 13356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _aliasOcat ->
                   (case (alias_ _aliasOcat _aliasOflags ) of
                    { ( _aliasIannotatedTree,_aliasIoriginalTree) ->
                        (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIflags
                                {-# LINE 13363 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _tblOflags ->
                         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _lhsIcat
                                 {-# LINE 13368 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _tblOcat ->
                          (case (tbl_1 _tblOcat _tblOflags _tblOtpe ) of
                           { ( _tblIannotatedTree) ->
                               (case (ann_ ) of
                                { ( _annIoriginalTree,ann_1) ->
                                    (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _lhsIflags
                                            {-# LINE 13377 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annOflags ->
                                     (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _lhsIcat
                                             {-# LINE 13382 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _annOcat ->
                                      (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                       { ( _annIannotatedTree) ->
                                           (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                   Tref _annIannotatedTree _tblIannotatedTree _aliasIannotatedTree
                                                   {-# LINE 13389 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                   )) of
                                            { _annotatedTree ->
                                            (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                    _annotatedTree
                                                    {-# LINE 13394 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                    )) of
                                             { _lhsOannotatedTree ->
                                             (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                     Tref _annIoriginalTree _tblIoriginalTree _aliasIoriginalTree
                                                     {-# LINE 13399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                     )) of
                                              { _originalTree ->
                                              (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                      _originalTree
                                                      {-# LINE 13404 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                      )) of
                                               { _lhsOoriginalTree ->
                                               (case (({-# LINE 30 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                                       either (const emptyEnvironment) id _eEnv
                                                       {-# LINE 13409 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                       )) of
                                                { _lhsOupEnv ->
                                                ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- TableRefList ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                       TypeCheckingFlags ->
                       ( TableRefList ,TableRefList ,Environment)
data Inh_TableRefList  = Inh_TableRefList {cat_Inh_TableRefList :: Catalog,flags_Inh_TableRefList :: TypeCheckingFlags}
data Syn_TableRefList  = Syn_TableRefList {annotatedTree_Syn_TableRefList :: TableRefList ,originalTree_Syn_TableRefList :: TableRefList ,upEnv_Syn_TableRefList :: Environment}
wrap_TableRefList :: T_TableRefList  ->
                     Inh_TableRefList  ->
                     Syn_TableRefList 
wrap_TableRefList sem (Inh_TableRefList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) = sem _lhsIcat _lhsIflags 
     in  (Syn_TableRefList _lhsOannotatedTree _lhsOoriginalTree _lhsOupEnv ))
sem_TableRefList_Cons :: T_TableRef  ->
                         T_TableRefList  ->
                         T_TableRefList 
sem_TableRefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 13461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13466 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13471 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 13476 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOflags ) of
              { ( _tlIannotatedTree,_tlIoriginalTree,_tlIupEnv) ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree,_hdIupEnv) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 13485 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 13490 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 13495 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 13500 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           (case (({-# LINE 15 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                                   if isEmptyEnv _tlIupEnv
                                   then _hdIupEnv
                                   else
                                          either (error . show) id $
                                          createJoinTrefEnvironment _lhsIcat _hdIupEnv _tlIupEnv $ Just []
                                   {-# LINE 13509 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                   )) of
                            { _lhsOupEnv ->
                            ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }) }) }) }) }) }) }))
sem_TableRefList_Nil :: T_TableRefList 
sem_TableRefList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 13519 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 13529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 13534 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             (case (({-# LINE 13 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                     emptyEnvironment
                     {-# LINE 13539 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                     )) of
              { _lhsOupEnv ->
              ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv) }) }) }) }) }))
-- TypeAttributeDef --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                           TypeCheckingFlags ->
                           ( TypeAttributeDef ,TypeAttributeDef )
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {cat_Inh_TypeAttributeDef :: Catalog,flags_Inh_TypeAttributeDef :: TypeCheckingFlags}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef ,originalTree_Syn_TypeAttributeDef :: TypeAttributeDef }
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_TypeAttributeDef _lhsOannotatedTree _lhsOoriginalTree ))
sem_TypeAttributeDef_TypeAttDef :: T_Annotation  ->
                                   NameComponent ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef ann_ name_ typ_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 13589 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: TypeAttributeDef.TypeAttDef.ann.tpe"
                  {-# LINE 13594 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13599 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _typOflags ->
            (case (typ_ _typOcat _typOflags ) of
             { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 13608 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 13613 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     TypeAttDef _annIannotatedTree name_ _typIannotatedTree
                                     {-# LINE 13620 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 13625 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       TypeAttDef _annIoriginalTree name_ _typIoriginalTree
                                       {-# LINE 13630 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 13635 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
-- TypeAttributeDefList ----------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                               TypeCheckingFlags ->
                               ( TypeAttributeDefList ,TypeAttributeDefList )
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {cat_Inh_TypeAttributeDefList :: Catalog,flags_Inh_TypeAttributeDefList :: TypeCheckingFlags}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList ,originalTree_Syn_TypeAttributeDefList :: TypeAttributeDefList }
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_TypeAttributeDefList _lhsOannotatedTree _lhsOoriginalTree ))
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 13686 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13691 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13696 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tlOflags ->
            (case (tl_ _tlOcat _tlOflags ) of
             { ( _tlIannotatedTree,_tlIoriginalTree) ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 13703 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _hdOflags ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 13710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 13715 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 13720 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 13725 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 13735 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13740 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 13745 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 13750 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- TypeName ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                   TypeCheckingFlags ->
                   ( TypeName ,(Maybe Type),TypeName )
data Inh_TypeName  = Inh_TypeName {cat_Inh_TypeName :: Catalog,flags_Inh_TypeName :: TypeCheckingFlags}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName ,namedType_Syn_TypeName :: (Maybe Type),originalTree_Syn_TypeName :: TypeName }
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_TypeName _lhsOannotatedTree _lhsOnamedType _lhsOoriginalTree ))
sem_TypeName_ArrayTypeName :: T_Annotation  ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName ann_ typ_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 13843 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIflags
                  {-# LINE 13848 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _typOflags ->
           (case (typ_ _typOcat _typOflags ) of
            { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                (case (({-# LINE 58 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        maybe (Left []) Right _typInamedType
                        >>=  Right . ArrayType
                        {-# LINE 13856 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tpe ->
                 (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                         either Left (const $ Left []) _tpe
                         {-# LINE 13861 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOtpe ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 13868 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 13873 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      ArrayTypeName _annIannotatedTree _typIannotatedTree
                                      {-# LINE 13880 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 13885 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                        either (const Nothing) Just _tpe
                                        {-# LINE 13890 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOnamedType ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         ArrayTypeName _annIoriginalTree _typIoriginalTree
                                         {-# LINE 13895 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _originalTree
                                          {-# LINE 13900 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOoriginalTree ->
                                   ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TypeName_Prec2TypeName :: T_Annotation  ->
                              T_Name  ->
                              Integer ->
                              Integer ->
                              T_TypeName 
sem_TypeName_Prec2TypeName ann_ tn_ prec_ prec1_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (tn_ ) of
          { ( _tnIoriginalTree,tn_1) ->
              (case (({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                      catLookupType _lhsIcat (nameComponents _tnIoriginalTree)
                      {-# LINE 13916 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tpe ->
               (case (({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                       Left []
                       {-# LINE 13921 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tnOtpe ->
                (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        either Left (const $ Left []) _tpe
                        {-# LINE 13926 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 13931 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tnOflags ->
                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _lhsIcat
                          {-# LINE 13936 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _tnOcat ->
                   (case (tn_1 _tnOcat _tnOflags _tnOtpe ) of
                    { ( _tnIannotatedTree) ->
                        (case (ann_ ) of
                         { ( _annIoriginalTree,ann_1) ->
                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIflags
                                     {-# LINE 13945 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOflags ->
                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIcat
                                      {-# LINE 13950 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annOcat ->
                               (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                { ( _annIannotatedTree) ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            Prec2TypeName _annIannotatedTree _tnIannotatedTree prec_ prec1_
                                            {-# LINE 13957 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annotatedTree ->
                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _annotatedTree
                                             {-# LINE 13962 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOannotatedTree ->
                                      (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                              either (const Nothing) Just _tpe
                                              {-# LINE 13967 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOnamedType ->
                                       (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               Prec2TypeName _annIoriginalTree _tnIoriginalTree prec_ prec1_
                                               {-# LINE 13972 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _originalTree ->
                                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _originalTree
                                                {-# LINE 13977 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOoriginalTree ->
                                         ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TypeName_PrecTypeName :: T_Annotation  ->
                             T_Name  ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName ann_ tn_ prec_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (tn_ ) of
          { ( _tnIoriginalTree,tn_1) ->
              (case (({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                      catLookupType _lhsIcat (nameComponents _tnIoriginalTree)
                      {-# LINE 13992 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tpe ->
               (case (({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                       Left []
                       {-# LINE 13997 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tnOtpe ->
                (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        either Left (const $ Left []) _tpe
                        {-# LINE 14002 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 14007 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tnOflags ->
                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _lhsIcat
                          {-# LINE 14012 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _tnOcat ->
                   (case (tn_1 _tnOcat _tnOflags _tnOtpe ) of
                    { ( _tnIannotatedTree) ->
                        (case (ann_ ) of
                         { ( _annIoriginalTree,ann_1) ->
                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIflags
                                     {-# LINE 14021 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOflags ->
                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIcat
                                      {-# LINE 14026 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annOcat ->
                               (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                { ( _annIannotatedTree) ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            PrecTypeName _annIannotatedTree _tnIannotatedTree prec_
                                            {-# LINE 14033 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annotatedTree ->
                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _annotatedTree
                                             {-# LINE 14038 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOannotatedTree ->
                                      (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                              either (const Nothing) Just _tpe
                                              {-# LINE 14043 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOnamedType ->
                                       (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               PrecTypeName _annIoriginalTree _tnIoriginalTree prec_
                                               {-# LINE 14048 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _originalTree ->
                                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _originalTree
                                                {-# LINE 14053 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOoriginalTree ->
                                         ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TypeName_SetOfTypeName :: T_Annotation  ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName ann_ typ_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 14065 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIflags
                  {-# LINE 14070 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _typOflags ->
           (case (typ_ _typOcat _typOflags ) of
            { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                (case (({-# LINE 61 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        maybe (Left []) Right _typInamedType
                        >>=  Right . Pseudo . SetOfType
                        {-# LINE 14078 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _tpe ->
                 (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                         either Left (const $ Left []) _tpe
                         {-# LINE 14083 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _annOtpe ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 14090 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 14095 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      SetOfTypeName _annIannotatedTree _typIannotatedTree
                                      {-# LINE 14102 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 14107 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                        either (const Nothing) Just _tpe
                                        {-# LINE 14112 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOnamedType ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         SetOfTypeName _annIoriginalTree _typIoriginalTree
                                         {-# LINE 14117 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _originalTree ->
                                  (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                          _originalTree
                                          {-# LINE 14122 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                          )) of
                                   { _lhsOoriginalTree ->
                                   ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_TypeName_SimpleTypeName :: T_Annotation  ->
                               T_Name  ->
                               T_TypeName 
sem_TypeName_SimpleTypeName ann_ tn_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (tn_ ) of
          { ( _tnIoriginalTree,tn_1) ->
              (case (({-# LINE 50 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                      catLookupType _lhsIcat (nameComponents _tnIoriginalTree)
                      {-# LINE 14136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                      )) of
               { _tpe ->
               (case (({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                       Left []
                       {-# LINE 14141 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _tnOtpe ->
                (case (({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                        either Left (const $ Left []) _tpe
                        {-# LINE 14146 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOtpe ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 14151 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _tnOflags ->
                  (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                          _lhsIcat
                          {-# LINE 14156 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                          )) of
                   { _tnOcat ->
                   (case (tn_1 _tnOcat _tnOflags _tnOtpe ) of
                    { ( _tnIannotatedTree) ->
                        (case (ann_ ) of
                         { ( _annIoriginalTree,ann_1) ->
                             (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIflags
                                     {-# LINE 14165 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOflags ->
                              (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _lhsIcat
                                      {-# LINE 14170 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annOcat ->
                               (case (ann_1 _annOcat _annOflags _annOtpe ) of
                                { ( _annIannotatedTree) ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            SimpleTypeName _annIannotatedTree _tnIannotatedTree
                                            {-# LINE 14177 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _annotatedTree ->
                                     (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             _annotatedTree
                                             {-# LINE 14182 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _lhsOannotatedTree ->
                                      (case (({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                                              either (const Nothing) Just _tpe
                                              {-# LINE 14187 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOnamedType ->
                                       (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                               SimpleTypeName _annIoriginalTree _tnIoriginalTree
                                               {-# LINE 14192 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                               )) of
                                        { _originalTree ->
                                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                                _originalTree
                                                {-# LINE 14197 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                                )) of
                                         { _lhsOoriginalTree ->
                                         ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- TypeNameList ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                       TypeCheckingFlags ->
                       ( TypeNameList ,TypeNameList )
data Inh_TypeNameList  = Inh_TypeNameList {cat_Inh_TypeNameList :: Catalog,flags_Inh_TypeNameList :: TypeCheckingFlags}
data Syn_TypeNameList  = Syn_TypeNameList {annotatedTree_Syn_TypeNameList :: TypeNameList ,originalTree_Syn_TypeNameList :: TypeNameList }
wrap_TypeNameList :: T_TypeNameList  ->
                     Inh_TypeNameList  ->
                     Syn_TypeNameList 
wrap_TypeNameList sem (Inh_TypeNameList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_TypeNameList _lhsOannotatedTree _lhsOoriginalTree ))
sem_TypeNameList_Cons :: T_TypeName  ->
                         T_TypeNameList  ->
                         T_TypeNameList 
sem_TypeNameList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 14248 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14253 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tlOflags ->
            (case (tl_ _tlOcat _tlOflags ) of
             { ( _tlIannotatedTree,_tlIoriginalTree) ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 14265 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _hdOflags ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdInamedType,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 14272 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 14277 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 14282 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 14287 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_TypeNameList_Nil :: T_TypeNameList 
sem_TypeNameList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 14297 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14302 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 14307 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 14312 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- VarDef ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                 TypeCheckingFlags ->
                 ( VarDef ,VarDef )
data Inh_VarDef  = Inh_VarDef {cat_Inh_VarDef :: Catalog,flags_Inh_VarDef :: TypeCheckingFlags}
data Syn_VarDef  = Syn_VarDef {annotatedTree_Syn_VarDef :: VarDef ,originalTree_Syn_VarDef :: VarDef }
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_VarDef _lhsOannotatedTree _lhsOoriginalTree ))
sem_VarDef_ParamAlias :: T_Annotation  ->
                         NameComponent ->
                         Integer ->
                         T_VarDef 
sem_VarDef_ParamAlias ann_ name_ i_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 error "missing rule: VarDef.ParamAlias.ann.tpe"
                 {-# LINE 14383 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annOtpe ->
          (case (ann_ ) of
           { ( _annIoriginalTree,ann_1) ->
               (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                       _lhsIflags
                       {-# LINE 14390 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                       )) of
                { _annOflags ->
                (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIcat
                        {-# LINE 14395 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _annOcat ->
                 (case (ann_1 _annOcat _annOflags _annOtpe ) of
                  { ( _annIannotatedTree) ->
                      (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              ParamAlias _annIannotatedTree name_ i_
                              {-# LINE 14402 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annotatedTree ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _annotatedTree
                               {-# LINE 14407 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _lhsOannotatedTree ->
                        (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                ParamAlias _annIoriginalTree name_ i_
                                {-# LINE 14412 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _originalTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 _originalTree
                                 {-# LINE 14417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _lhsOoriginalTree ->
                          ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }))
sem_VarDef_VarAlias :: T_Annotation  ->
                       NameComponent ->
                       T_Name  ->
                       T_VarDef 
sem_VarDef_VarAlias ann_ name_ aliased_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                 error "missing rule: VarDef.VarAlias.aliased.tpe"
                 {-# LINE 14430 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _aliasedOtpe ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: VarDef.VarAlias.ann.tpe"
                  {-# LINE 14435 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (aliased_ ) of
            { ( _aliasedIoriginalTree,aliased_1) ->
                (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                        _lhsIflags
                        {-# LINE 14442 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                        )) of
                 { _aliasedOflags ->
                 (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIcat
                         {-# LINE 14447 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _aliasedOcat ->
                  (case (aliased_1 _aliasedOcat _aliasedOflags _aliasedOtpe ) of
                   { ( _aliasedIannotatedTree) ->
                       (case (ann_ ) of
                        { ( _annIoriginalTree,ann_1) ->
                            (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                    _lhsIflags
                                    {-# LINE 14456 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                    )) of
                             { _annOflags ->
                             (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     _lhsIcat
                                     {-# LINE 14461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annOcat ->
                              (case (ann_1 _annOcat _annOflags _annOtpe ) of
                               { ( _annIannotatedTree) ->
                                   (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                           VarAlias _annIannotatedTree name_ _aliasedIannotatedTree
                                           {-# LINE 14468 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                           )) of
                                    { _annotatedTree ->
                                    (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                            _annotatedTree
                                            {-# LINE 14473 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                            )) of
                                     { _lhsOannotatedTree ->
                                     (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                             VarAlias _annIoriginalTree name_ _aliasedIoriginalTree
                                             {-# LINE 14478 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                             )) of
                                      { _originalTree ->
                                      (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                              _originalTree
                                              {-# LINE 14483 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                              )) of
                                       { _lhsOoriginalTree ->
                                       ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_VarDef_VarDef :: T_Annotation  ->
                     NameComponent ->
                     T_TypeName  ->
                     (Maybe ScalarExpr) ->
                     T_VarDef 
sem_VarDef_VarDef ann_ name_ typ_ value_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 14497 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _typOcat ->
          (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  error "missing rule: VarDef.VarDef.ann.tpe"
                  {-# LINE 14502 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _annOtpe ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14507 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _typOflags ->
            (case (typ_ _typOcat _typOflags ) of
             { ( _typIannotatedTree,_typInamedType,_typIoriginalTree) ->
                 (case (ann_ ) of
                  { ( _annIoriginalTree,ann_1) ->
                      (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                              _lhsIflags
                              {-# LINE 14516 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                              )) of
                       { _annOflags ->
                       (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIcat
                               {-# LINE 14521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOcat ->
                        (case (ann_1 _annOcat _annOflags _annOtpe ) of
                         { ( _annIannotatedTree) ->
                             (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                     VarDef _annIannotatedTree name_ _typIannotatedTree value_
                                     {-# LINE 14528 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                     )) of
                              { _annotatedTree ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      _annotatedTree
                                      {-# LINE 14533 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _lhsOannotatedTree ->
                               (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       VarDef _annIoriginalTree name_ _typIoriginalTree value_
                                       {-# LINE 14538 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _originalTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        _originalTree
                                        {-# LINE 14543 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _lhsOoriginalTree ->
                                 ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }))
-- VarDefList --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                     TypeCheckingFlags ->
                     ( VarDefList ,VarDefList )
data Inh_VarDefList  = Inh_VarDefList {cat_Inh_VarDefList :: Catalog,flags_Inh_VarDefList :: TypeCheckingFlags}
data Syn_VarDefList  = Syn_VarDefList {annotatedTree_Syn_VarDefList :: VarDefList ,originalTree_Syn_VarDefList :: VarDefList }
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_VarDefList _lhsOannotatedTree _lhsOoriginalTree ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIcat
                 {-# LINE 14594 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOcat ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14599 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _hdOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14604 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _tlOflags ->
            (case (tl_ _tlOcat _tlOflags ) of
             { ( _tlIannotatedTree,_tlIoriginalTree) ->
                 (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                         _lhsIflags
                         {-# LINE 14611 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                         )) of
                  { _hdOflags ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 14618 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 14623 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 14628 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 14633 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 14643 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14648 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 14653 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 14658 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))
-- WithQuery ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                    TypeCheckingFlags ->
                    ( WithQuery ,WithQuery )
data Inh_WithQuery  = Inh_WithQuery {cat_Inh_WithQuery :: Catalog,flags_Inh_WithQuery :: TypeCheckingFlags}
data Syn_WithQuery  = Syn_WithQuery {annotatedTree_Syn_WithQuery :: WithQuery ,originalTree_Syn_WithQuery :: WithQuery }
wrap_WithQuery :: T_WithQuery  ->
                  Inh_WithQuery  ->
                  Syn_WithQuery 
wrap_WithQuery sem (Inh_WithQuery _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_WithQuery _lhsOannotatedTree _lhsOoriginalTree ))
sem_WithQuery_WithQuery :: T_Annotation  ->
                           NameComponent ->
                           (Maybe [NameComponent]) ->
                           T_QueryExpr  ->
                           T_WithQuery 
sem_WithQuery_WithQuery ann_ name_ colAliases_ ex_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 14710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _exOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14715 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _exOcat ->
           (case (({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: WithQuery.WithQuery.ann.tpe"
                   {-# LINE 14720 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _annOtpe ->
            (case (({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                    Nothing
                    {-# LINE 14725 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _exOouterDownEnv ->
             (case (ex_ _exOcat _exOflags _exOouterDownEnv ) of
              { ( _exIannotatedTree,_exIoriginalTree,_exIupType) ->
                  (case (ann_ ) of
                   { ( _annIoriginalTree,ann_1) ->
                       (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               _lhsIflags
                               {-# LINE 14734 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annOflags ->
                        (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _lhsIcat
                                {-# LINE 14739 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _annOcat ->
                         (case (ann_1 _annOcat _annOflags _annOtpe ) of
                          { ( _annIannotatedTree) ->
                              (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                      WithQuery _annIannotatedTree name_ colAliases_ _exIannotatedTree
                                      {-# LINE 14746 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                      )) of
                               { _annotatedTree ->
                               (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                       _annotatedTree
                                       {-# LINE 14751 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                       )) of
                                { _lhsOannotatedTree ->
                                (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                        WithQuery _annIoriginalTree name_ colAliases_ _exIoriginalTree
                                        {-# LINE 14756 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                        )) of
                                 { _originalTree ->
                                 (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                         _originalTree
                                         {-# LINE 14761 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                         )) of
                                  { _lhsOoriginalTree ->
                                  ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- WithQueryList -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
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
                        TypeCheckingFlags ->
                        ( WithQueryList ,WithQueryList )
data Inh_WithQueryList  = Inh_WithQueryList {cat_Inh_WithQueryList :: Catalog,flags_Inh_WithQueryList :: TypeCheckingFlags}
data Syn_WithQueryList  = Syn_WithQueryList {annotatedTree_Syn_WithQueryList :: WithQueryList ,originalTree_Syn_WithQueryList :: WithQueryList }
wrap_WithQueryList :: T_WithQueryList  ->
                      Inh_WithQueryList  ->
                      Syn_WithQueryList 
wrap_WithQueryList sem (Inh_WithQueryList _lhsIcat _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags 
     in  (Syn_WithQueryList _lhsOannotatedTree _lhsOoriginalTree ))
sem_WithQueryList_Cons :: T_WithQuery  ->
                          T_WithQueryList  ->
                          T_WithQueryList 
sem_WithQueryList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 _lhsIflags
                 {-# LINE 14812 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _tlOflags ->
          (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14817 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _tlOcat ->
           (case (({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14822 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _hdOflags ->
            (case (({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _lhsIcat
                    {-# LINE 14827 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _hdOcat ->
             (case (tl_ _tlOcat _tlOflags ) of
              { ( _tlIannotatedTree,_tlIoriginalTree) ->
                  (case (hd_ _hdOcat _hdOflags ) of
                   { ( _hdIannotatedTree,_hdIoriginalTree) ->
                       (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                               (:) _hdIannotatedTree _tlIannotatedTree
                               {-# LINE 14836 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                               )) of
                        { _annotatedTree ->
                        (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                _annotatedTree
                                {-# LINE 14841 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                )) of
                         { _lhsOannotatedTree ->
                         (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                 (:) _hdIoriginalTree _tlIoriginalTree
                                 {-# LINE 14846 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                 )) of
                          { _originalTree ->
                          (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                                  _originalTree
                                  {-# LINE 14851 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                                  )) of
                           { _lhsOoriginalTree ->
                           ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }) }) }) }) }) }) }))
sem_WithQueryList_Nil :: T_WithQueryList 
sem_WithQueryList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                 []
                 {-# LINE 14861 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                 )) of
          { _annotatedTree ->
          (case (({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14866 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                  )) of
           { _lhsOannotatedTree ->
           (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 14871 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )) of
            { _originalTree ->
            (case (({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                    _originalTree
                    {-# LINE 14876 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                    )) of
             { _lhsOoriginalTree ->
             ( _lhsOannotatedTree,_lhsOoriginalTree) }) }) }) }))