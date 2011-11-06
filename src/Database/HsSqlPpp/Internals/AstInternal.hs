

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

{-# LINE 178 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}

tcAppLike :: Catalog -> Name -> [Maybe Type] -> Either [TypeError] Type
tcAppLike cat anm args = do
  -- get the types of the arguments
  -- then lookup in TypeConversion.matchApp
  tys <- mapM (maybe (Left []) Right) args
  let Name _ ns = anm
  (_,rt) <- matchApp cat ns tys
  return rt

{-# LINE 417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}

{-# LINE 155 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}


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
         (let _lhsOannotatedTree :: AlterTableAction 
              _lhsOoriginalTree :: AlterTableAction 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _conOcat :: Catalog
              _conOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _conIannotatedTree :: Constraint 
              _conIoriginalTree :: Constraint 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AddConstraint _annIannotatedTree _conIannotatedTree
                   {-# LINE 504 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AddConstraint _annIoriginalTree _conIoriginalTree
                   {-# LINE 510 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 516 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 522 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 528 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 534 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: AlterTableAction.AddConstraint.ann.tpe"
                   {-# LINE 540 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _conOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 546 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _conOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 552 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _conIannotatedTree,_conIoriginalTree) =
                  con_ _conOcat _conOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_AlterTableAction_AlterColumnDefault :: T_Annotation  ->
                                           NameComponent ->
                                           T_ScalarExpr  ->
                                           T_AlterTableAction 
sem_AlterTableAction_AlterColumnDefault ann_ nm_ def_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: AlterTableAction 
              _lhsOoriginalTree :: AlterTableAction 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _defOcat :: Catalog
              _defOdownEnv :: Environment
              _defOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _defIannotatedTree :: ScalarExpr 
              _defIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _defIoriginalTree :: ScalarExpr 
              _defIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterColumnDefault _annIannotatedTree nm_ _defIannotatedTree
                   {-# LINE 584 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterColumnDefault _annIoriginalTree nm_ _defIoriginalTree
                   {-# LINE 590 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 596 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 602 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 608 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 614 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: AlterTableAction.AlterColumnDefault.ann.tpe"
                   {-# LINE 620 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _defOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 626 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _defOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: AlterTableAction.AlterColumnDefault.def.downEnv"
                   {-# LINE 632 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _defOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 638 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _defIannotatedTree,_defIcolExprs,_defIoriginalTree,_defIupType) =
                  def_ _defOcat _defOdownEnv _defOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: AlterTableActionList 
              _lhsOoriginalTree :: AlterTableActionList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: AlterTableAction 
              _hdIoriginalTree :: AlterTableAction 
              _tlIannotatedTree :: AlterTableActionList 
              _tlIoriginalTree :: AlterTableActionList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 704 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 716 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 722 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 728 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 734 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 740 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 746 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_AlterTableActionList_Nil :: T_AlterTableActionList 
sem_AlterTableActionList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: AlterTableActionList 
              _lhsOoriginalTree :: AlterTableActionList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 763 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 769 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 775 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 781 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- Annotation --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
         tpe                  : Either [TypeError] Type
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Annotation:
         child asrc           : {Maybe SourcePosition}
         child atype          : {Maybe Type}
         child errs           : {[TypeError]}
         child implicitCast   : {Maybe Type}
         child stType         : {Maybe ParameterizedStatementType}
         child catUpd         : {[CatalogUpdate]}
         visit 0:
            local annotatedTree : _
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
type T_Annotation  = Catalog ->
                     TypeCheckingFlags ->
                     (Either [TypeError] Type) ->
                     ( Annotation ,Annotation )
data Inh_Annotation  = Inh_Annotation {cat_Inh_Annotation :: Catalog,flags_Inh_Annotation :: TypeCheckingFlags,tpe_Inh_Annotation :: (Either [TypeError] Type)}
data Syn_Annotation  = Syn_Annotation {annotatedTree_Syn_Annotation :: Annotation ,originalTree_Syn_Annotation :: Annotation }
wrap_Annotation :: T_Annotation  ->
                   Inh_Annotation  ->
                   Syn_Annotation 
wrap_Annotation sem (Inh_Annotation _lhsIcat _lhsIflags _lhsItpe )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags _lhsItpe 
     in  (Syn_Annotation _lhsOannotatedTree _lhsOoriginalTree ))
sem_Annotation_Annotation :: (Maybe SourcePosition) ->
                             (Maybe Type) ->
                             ([TypeError]) ->
                             (Maybe Type) ->
                             (Maybe ParameterizedStatementType) ->
                             ([CatalogUpdate]) ->
                             T_Annotation 
sem_Annotation_Annotation asrc_ atype_ errs_ implicitCast_ stType_ catUpd_  =
    (\ _lhsIcat
       _lhsIflags
       _lhsItpe ->
         (let _lhsOannotatedTree :: Annotation 
              _lhsOoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag"(line 103, column 7)
              _lhsOannotatedTree =
                  ({-# LINE 103 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   let t = either (const Nothing) Just _lhsItpe
                       es = either id (const []) _lhsItpe
                   in Annotation asrc_ t es implicitCast_ stType_ catUpd_
                   {-# LINE 845 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Annotation asrc_ atype_ errs_ implicitCast_ stType_ catUpd_
                   {-# LINE 851 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Annotation asrc_ atype_ errs_ implicitCast_ stType_ catUpd_
                   {-# LINE 857 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 863 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: AttributeDef 
              _lhsOoriginalTree :: AttributeDef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _typOcat :: Catalog
              _typOflags :: TypeCheckingFlags
              _defOcat :: Catalog
              _defOdownEnv :: Environment
              _defOflags :: TypeCheckingFlags
              _consOcat :: Catalog
              _consOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _typIannotatedTree :: TypeName 
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName 
              _defIannotatedTree :: MaybeScalarExpr 
              _defIoriginalTree :: MaybeScalarExpr 
              _defIupType :: (Maybe Type)
              _consIannotatedTree :: RowConstraintList 
              _consIoriginalTree :: RowConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AttributeDef _annIannotatedTree name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
                   {-# LINE 940 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AttributeDef _annIoriginalTree name_ _typIoriginalTree _defIoriginalTree _consIoriginalTree
                   {-# LINE 946 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 952 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 958 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 964 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 970 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: AttributeDef.AttributeDef.ann.tpe"
                   {-# LINE 976 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 982 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 988 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _defOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 994 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _defOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: AttributeDef.AttributeDef.def.downEnv"
                   {-# LINE 1000 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _defOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1006 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _consOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1012 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _consOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1018 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  typ_ _typOcat _typOflags 
              ( _defIannotatedTree,_defIoriginalTree,_defIupType) =
                  def_ _defOcat _defOdownEnv _defOflags 
              ( _consIannotatedTree,_consIoriginalTree) =
                  cons_ _consOcat _consOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: AttributeDefList 
              _lhsOoriginalTree :: AttributeDefList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: AttributeDef 
              _hdIoriginalTree :: AttributeDef 
              _tlIannotatedTree :: AttributeDefList 
              _tlIoriginalTree :: AttributeDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 1088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 1094 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1100 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1112 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1118 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1130 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: AttributeDefList 
              _lhsOoriginalTree :: AttributeDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1147 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1153 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1159 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1165 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         thenType             : Maybe Type
         upTypes              : [Maybe Type]
         whenTypes            : [Maybe Type]
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
                                           ( CaseScalarExprListScalarExprPair ,CaseScalarExprListScalarExprPair ,(Maybe Type),([Maybe Type]),([Maybe Type]))
data Inh_CaseScalarExprListScalarExprPair  = Inh_CaseScalarExprListScalarExprPair {cat_Inh_CaseScalarExprListScalarExprPair :: Catalog,downEnv_Inh_CaseScalarExprListScalarExprPair :: Environment,flags_Inh_CaseScalarExprListScalarExprPair :: TypeCheckingFlags}
data Syn_CaseScalarExprListScalarExprPair  = Syn_CaseScalarExprListScalarExprPair {annotatedTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair ,originalTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair ,thenType_Syn_CaseScalarExprListScalarExprPair :: (Maybe Type),upTypes_Syn_CaseScalarExprListScalarExprPair :: ([Maybe Type]),whenTypes_Syn_CaseScalarExprListScalarExprPair :: ([Maybe Type])}
wrap_CaseScalarExprListScalarExprPair :: T_CaseScalarExprListScalarExprPair  ->
                                         Inh_CaseScalarExprListScalarExprPair  ->
                                         Syn_CaseScalarExprListScalarExprPair 
wrap_CaseScalarExprListScalarExprPair sem (Inh_CaseScalarExprListScalarExprPair _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOthenType,_lhsOupTypes,_lhsOwhenTypes) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_CaseScalarExprListScalarExprPair _lhsOannotatedTree _lhsOoriginalTree _lhsOthenType _lhsOupTypes _lhsOwhenTypes ))
sem_CaseScalarExprListScalarExprPair_Tuple :: T_ScalarExprList  ->
                                              T_ScalarExpr  ->
                                              T_CaseScalarExprListScalarExprPair 
sem_CaseScalarExprListScalarExprPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOwhenTypes :: ([Maybe Type])
              _lhsOthenType :: (Maybe Type)
              _lhsOannotatedTree :: CaseScalarExprListScalarExprPair 
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPair 
              _lhsOupTypes :: ([Maybe Type])
              _x1Ocat :: Catalog
              _x1OdownEnv :: Environment
              _x1Oflags :: TypeCheckingFlags
              _x2Ocat :: Catalog
              _x2OdownEnv :: Environment
              _x2Oflags :: TypeCheckingFlags
              _x1IannotatedTree :: ScalarExprList 
              _x1IoriginalTree :: ScalarExprList 
              _x1IupTypes :: ([Maybe Type])
              _x2IannotatedTree :: ScalarExpr 
              _x2IcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _x2IoriginalTree :: ScalarExpr 
              _x2IupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 217, column 13)
              _lhsOwhenTypes =
                  ({-# LINE 217 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _x1IupTypes
                   {-# LINE 1237 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 218, column 13)
              _lhsOthenType =
                  ({-# LINE 218 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _x2IupType
                   {-# LINE 1243 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IannotatedTree,_x2IannotatedTree)
                   {-# LINE 1249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IoriginalTree,_x2IoriginalTree)
                   {-# LINE 1255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1261 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOupTypes =
                  ({-# LINE 222 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _x1IupTypes
                   {-# LINE 1273 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1279 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1OdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 1285 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1291 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1297 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2OdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 1303 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1309 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _x1IannotatedTree,_x1IoriginalTree,_x1IupTypes) =
                  x1_ _x1Ocat _x1OdownEnv _x1Oflags 
              ( _x2IannotatedTree,_x2IcolExprs,_x2IoriginalTree,_x2IupType) =
                  x2_ _x2Ocat _x2OdownEnv _x2Oflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOthenType,_lhsOupTypes,_lhsOwhenTypes)))
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
         thenTypes            : [Maybe Type]
         upTypes              : [Maybe Type]
         whenTypes            : [[Maybe Type]]
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
                                               ( CaseScalarExprListScalarExprPairList ,CaseScalarExprListScalarExprPairList ,([Maybe Type]),([Maybe Type]),([[Maybe Type]]))
data Inh_CaseScalarExprListScalarExprPairList  = Inh_CaseScalarExprListScalarExprPairList {cat_Inh_CaseScalarExprListScalarExprPairList :: Catalog,downEnv_Inh_CaseScalarExprListScalarExprPairList :: Environment,flags_Inh_CaseScalarExprListScalarExprPairList :: TypeCheckingFlags}
data Syn_CaseScalarExprListScalarExprPairList  = Syn_CaseScalarExprListScalarExprPairList {annotatedTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList ,originalTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList ,thenTypes_Syn_CaseScalarExprListScalarExprPairList :: ([Maybe Type]),upTypes_Syn_CaseScalarExprListScalarExprPairList :: ([Maybe Type]),whenTypes_Syn_CaseScalarExprListScalarExprPairList :: ([[Maybe Type]])}
wrap_CaseScalarExprListScalarExprPairList :: T_CaseScalarExprListScalarExprPairList  ->
                                             Inh_CaseScalarExprListScalarExprPairList  ->
                                             Syn_CaseScalarExprListScalarExprPairList 
wrap_CaseScalarExprListScalarExprPairList sem (Inh_CaseScalarExprListScalarExprPairList _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOupTypes,_lhsOwhenTypes) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_CaseScalarExprListScalarExprPairList _lhsOannotatedTree _lhsOoriginalTree _lhsOthenTypes _lhsOupTypes _lhsOwhenTypes ))
sem_CaseScalarExprListScalarExprPairList_Cons :: T_CaseScalarExprListScalarExprPair  ->
                                                 T_CaseScalarExprListScalarExprPairList  ->
                                                 T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOwhenTypes :: ([[Maybe Type]])
              _lhsOthenTypes :: ([Maybe Type])
              _lhsOannotatedTree :: CaseScalarExprListScalarExprPairList 
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPairList 
              _lhsOupTypes :: ([Maybe Type])
              _hdOcat :: Catalog
              _hdOdownEnv :: Environment
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOdownEnv :: Environment
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: CaseScalarExprListScalarExprPair 
              _hdIoriginalTree :: CaseScalarExprListScalarExprPair 
              _hdIthenType :: (Maybe Type)
              _hdIupTypes :: ([Maybe Type])
              _hdIwhenTypes :: ([Maybe Type])
              _tlIannotatedTree :: CaseScalarExprListScalarExprPairList 
              _tlIoriginalTree :: CaseScalarExprListScalarExprPairList 
              _tlIthenTypes :: ([Maybe Type])
              _tlIupTypes :: ([Maybe Type])
              _tlIwhenTypes :: ([[Maybe Type]])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 207, column 10)
              _lhsOwhenTypes =
                  ({-# LINE 207 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _hdIwhenTypes : _tlIwhenTypes
                   {-# LINE 1392 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 208, column 10)
              _lhsOthenTypes =
                  ({-# LINE 208 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _hdIthenType : _tlIthenTypes
                   {-# LINE 1398 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 1404 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 1410 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1416 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOupTypes =
                  ({-# LINE 222 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tlIupTypes
                   {-# LINE 1428 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1434 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOdownEnv =
                  ({-# LINE 221 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 1440 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1446 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1452 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOdownEnv =
                  ({-# LINE 221 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 1458 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1464 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree,_hdIthenType,_hdIupTypes,_hdIwhenTypes) =
                  hd_ _hdOcat _hdOdownEnv _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIthenTypes,_tlIupTypes,_tlIwhenTypes) =
                  tl_ _tlOcat _tlOdownEnv _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOupTypes,_lhsOwhenTypes)))
sem_CaseScalarExprListScalarExprPairList_Nil :: T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList_Nil  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOwhenTypes :: ([[Maybe Type]])
              _lhsOthenTypes :: ([Maybe Type])
              _lhsOannotatedTree :: CaseScalarExprListScalarExprPairList 
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPairList 
              _lhsOupTypes :: ([Maybe Type])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 209, column 9)
              _lhsOwhenTypes =
                  ({-# LINE 209 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   []
                   {-# LINE 1485 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 210, column 9)
              _lhsOthenTypes =
                  ({-# LINE 210 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   []
                   {-# LINE 1491 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1497 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 1503 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1509 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1515 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOupTypes =
                  ({-# LINE 222 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: CaseScalarExprListScalarExprPairList.Nil.lhs.upTypes"
                   {-# LINE 1521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOupTypes,_lhsOwhenTypes)))
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
         (let _lhsOannotatedTree :: Constraint 
              _lhsOoriginalTree :: Constraint 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exprOcat :: Catalog
              _exprOdownEnv :: Environment
              _exprOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exprIannotatedTree :: ScalarExpr 
              _exprIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exprIoriginalTree :: ScalarExpr 
              _exprIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CheckConstraint _annIannotatedTree name_ _exprIannotatedTree
                   {-# LINE 1620 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CheckConstraint _annIoriginalTree name_ _exprIoriginalTree
                   {-# LINE 1626 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1632 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1638 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1644 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1650 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Constraint.CheckConstraint.ann.tpe"
                   {-# LINE 1656 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1662 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exprOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Constraint.CheckConstraint.expr.downEnv"
                   {-# LINE 1668 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1674 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOdownEnv _exprOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Constraint_PrimaryKeyConstraint :: T_Annotation  ->
                                       String ->
                                       ([NameComponent]) ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint ann_ name_ x_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Constraint 
              _lhsOoriginalTree :: Constraint 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PrimaryKeyConstraint _annIannotatedTree name_ x_
                   {-# LINE 1699 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PrimaryKeyConstraint _annIoriginalTree name_ x_
                   {-# LINE 1705 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1711 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1717 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1723 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1729 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Constraint.PrimaryKeyConstraint.ann.tpe"
                   {-# LINE 1735 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: Constraint 
              _lhsOoriginalTree :: Constraint 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _tableOcat :: Catalog
              _tableOflags :: TypeCheckingFlags
              _tableOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReferenceConstraint _annIannotatedTree name_ atts_ _tableIannotatedTree tableAtts_ onUpdate_ onDelete_
                   {-# LINE 1767 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReferenceConstraint _annIoriginalTree name_ atts_ _tableIoriginalTree tableAtts_ onUpdate_ onDelete_
                   {-# LINE 1773 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1779 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1785 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1791 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1797 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Constraint.ReferenceConstraint.ann.tpe"
                   {-# LINE 1803 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1809 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1815 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _tableOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Constraint.ReferenceConstraint.table.tpe"
                   {-# LINE 1821 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat _tableOflags _tableOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Constraint_UniqueConstraint :: T_Annotation  ->
                                   String ->
                                   ([NameComponent]) ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint ann_ name_ x_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Constraint 
              _lhsOoriginalTree :: Constraint 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   UniqueConstraint _annIannotatedTree name_ x_
                   {-# LINE 1846 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   UniqueConstraint _annIoriginalTree name_ x_
                   {-# LINE 1852 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1858 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1864 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1870 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1876 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Constraint.UniqueConstraint.ann.tpe"
                   {-# LINE 1882 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: ConstraintList 
              _lhsOoriginalTree :: ConstraintList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: Constraint 
              _hdIoriginalTree :: Constraint 
              _tlIannotatedTree :: ConstraintList 
              _tlIoriginalTree :: ConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 1946 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 1952 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 1958 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 1964 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1970 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1976 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 1982 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 1988 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: ConstraintList 
              _lhsOoriginalTree :: ConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 2005 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 2011 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2017 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2023 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: FnBody 
              _lhsOoriginalTree :: FnBody 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _blkOcat :: Catalog
              _blkOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _blkIannotatedTree :: Statement 
              _blkIoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PlpgsqlFnBody _annIannotatedTree _blkIannotatedTree
                   {-# LINE 2092 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PlpgsqlFnBody _annIoriginalTree _blkIoriginalTree
                   {-# LINE 2098 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2104 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2110 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2116 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2122 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: FnBody.PlpgsqlFnBody.ann.tpe"
                   {-# LINE 2128 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _blkOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2134 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _blkOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2140 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _blkIannotatedTree,_blkIoriginalTree) =
                  blk_ _blkOcat _blkOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_FnBody_SqlFnBody :: T_Annotation  ->
                        T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody ann_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: FnBody 
              _lhsOoriginalTree :: FnBody 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _stsOcat :: Catalog
              _stsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SqlFnBody _annIannotatedTree _stsIannotatedTree
                   {-# LINE 2168 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SqlFnBody _annIoriginalTree _stsIoriginalTree
                   {-# LINE 2174 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2180 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2186 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2192 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2198 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: FnBody.SqlFnBody.ann.tpe"
                   {-# LINE 2204 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2210 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2216 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat _stsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: InList 
              _lhsOoriginalTree :: InList 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exprsOcat :: Catalog
              _exprsOdownEnv :: Environment
              _exprsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exprsIannotatedTree :: ScalarExprList 
              _exprsIoriginalTree :: ScalarExprList 
              _exprsIupTypes :: ([Maybe Type])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 68, column 9)
              _tpe =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 2296 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InList _annIannotatedTree _exprsIannotatedTree
                   {-# LINE 2302 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InList _annIoriginalTree _exprsIoriginalTree
                   {-# LINE 2308 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2314 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2320 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2326 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2332 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (from local)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _tpe
                   {-# LINE 2338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2344 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprsOdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 2350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exprsIannotatedTree,_exprsIoriginalTree,_exprsIupTypes) =
                  exprs_ _exprsOcat _exprsOdownEnv _exprsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_InList_InQueryExpr :: T_Annotation  ->
                          T_QueryExpr  ->
                          T_InList 
sem_InList_InQueryExpr ann_ sel_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _selOouterDownEnv :: (Maybe Environment)
              _lhsOannotatedTree :: InList 
              _lhsOoriginalTree :: InList 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _selOcat :: Catalog
              _selOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              _selIupType :: (Maybe [(String,Type)])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 30, column 17)
              _selOouterDownEnv =
                  ({-# LINE 30 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Nothing
                   {-# LINE 2387 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 68, column 9)
              _tpe =
                  ({-# LINE 68 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 2393 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InQueryExpr _annIannotatedTree _selIannotatedTree
                   {-# LINE 2399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InQueryExpr _annIoriginalTree _selIoriginalTree
                   {-# LINE 2405 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2411 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2423 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2429 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (from local)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _tpe
                   {-# LINE 2435 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2441 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2447 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _selIannotatedTree,_selIoriginalTree,_selIupType) =
                  sel_ _selOcat _selOflags _selOouterDownEnv 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: JoinExpr 
              _lhsOoriginalTree :: JoinExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exprOcat :: Catalog
              _exprOdownEnv :: Environment
              _exprOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exprIannotatedTree :: ScalarExpr 
              _exprIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exprIoriginalTree :: ScalarExpr 
              _exprIupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 72, column 9)
              _tpe =
                  ({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 2528 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinOn _annIannotatedTree _exprIannotatedTree
                   {-# LINE 2534 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinOn _annIoriginalTree _exprIoriginalTree
                   {-# LINE 2540 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2546 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2552 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2558 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2564 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (from local)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _tpe
                   {-# LINE 2570 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2576 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 2582 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2588 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOdownEnv _exprOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_JoinExpr_JoinUsing :: T_Annotation  ->
                          ([NameComponent]) ->
                          T_JoinExpr 
sem_JoinExpr_JoinUsing ann_ x_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOannotatedTree :: JoinExpr 
              _lhsOoriginalTree :: JoinExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 72, column 9)
              _tpe =
                  ({-# LINE 72 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 2613 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinUsing _annIannotatedTree x_
                   {-# LINE 2619 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinUsing _annIoriginalTree x_
                   {-# LINE 2625 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2631 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2637 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2643 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2649 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (from local)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _tpe
                   {-# LINE 2655 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: MaybeBoolExpr 
              _lhsOoriginalTree :: MaybeBoolExpr 
              _justOcat :: Catalog
              _justOdownEnv :: Environment
              _justOflags :: TypeCheckingFlags
              _justIannotatedTree :: ScalarExpr 
              _justIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _justIoriginalTree :: ScalarExpr 
              _justIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIannotatedTree
                   {-# LINE 2721 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIoriginalTree
                   {-# LINE 2727 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2733 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2739 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2745 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 2751 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2757 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _justIannotatedTree,_justIcolExprs,_justIoriginalTree,_justIupType) =
                  just_ _justOcat _justOdownEnv _justOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_MaybeBoolExpr_Nothing :: T_MaybeBoolExpr 
sem_MaybeBoolExpr_Nothing  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOannotatedTree :: MaybeBoolExpr 
              _lhsOoriginalTree :: MaybeBoolExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2773 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2779 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2785 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2791 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
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
      inherited attributes:
         cat                  : Catalog
         downEnv              : Environment
         flags                : TypeCheckingFlags
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         upType               : Maybe Type
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
                          Environment ->
                          TypeCheckingFlags ->
                          ( MaybeScalarExpr ,MaybeScalarExpr ,(Maybe Type))
data Inh_MaybeScalarExpr  = Inh_MaybeScalarExpr {cat_Inh_MaybeScalarExpr :: Catalog,downEnv_Inh_MaybeScalarExpr :: Environment,flags_Inh_MaybeScalarExpr :: TypeCheckingFlags}
data Syn_MaybeScalarExpr  = Syn_MaybeScalarExpr {annotatedTree_Syn_MaybeScalarExpr :: MaybeScalarExpr ,originalTree_Syn_MaybeScalarExpr :: MaybeScalarExpr ,upType_Syn_MaybeScalarExpr :: (Maybe Type)}
wrap_MaybeScalarExpr :: T_MaybeScalarExpr  ->
                        Inh_MaybeScalarExpr  ->
                        Syn_MaybeScalarExpr 
wrap_MaybeScalarExpr sem (Inh_MaybeScalarExpr _lhsIcat _lhsIdownEnv _lhsIflags )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType) = sem _lhsIcat _lhsIdownEnv _lhsIflags 
     in  (Syn_MaybeScalarExpr _lhsOannotatedTree _lhsOoriginalTree _lhsOupType ))
sem_MaybeScalarExpr_Just :: T_ScalarExpr  ->
                            T_MaybeScalarExpr 
sem_MaybeScalarExpr_Just just_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOannotatedTree :: MaybeScalarExpr 
              _lhsOoriginalTree :: MaybeScalarExpr 
              _lhsOupType :: (Maybe Type)
              _justOcat :: Catalog
              _justOdownEnv :: Environment
              _justOflags :: TypeCheckingFlags
              _justIannotatedTree :: ScalarExpr 
              _justIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _justIoriginalTree :: ScalarExpr 
              _justIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIannotatedTree
                   {-# LINE 2891 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIoriginalTree
                   {-# LINE 2897 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2903 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2909 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOupType =
                  ({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _justIupType
                   {-# LINE 2915 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 2921 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 2927 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 2933 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _justIannotatedTree,_justIcolExprs,_justIoriginalTree,_justIupType) =
                  just_ _justOcat _justOdownEnv _justOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType)))
sem_MaybeScalarExpr_Nothing :: T_MaybeScalarExpr 
sem_MaybeScalarExpr_Nothing  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOupType :: (Maybe Type)
              _lhsOannotatedTree :: MaybeScalarExpr 
              _lhsOoriginalTree :: MaybeScalarExpr 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 39, column 15)
              _lhsOupType =
                  ({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Nothing
                   {-# LINE 2950 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2956 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 2962 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 2968 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 2974 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType)))
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
         (let _lhsOannotatedTree :: MaybeSelectList 
              _lhsOoriginalTree :: MaybeSelectList 
              _justOcat :: Catalog
              _justOdownEnv :: Environment
              _justOflags :: TypeCheckingFlags
              _justIannotatedTree :: SelectList 
              _justIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _justIoriginalTree :: SelectList 
              _justIupEnv :: Environment
              _justIupType :: (Maybe [(String,Type)])
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIannotatedTree
                   {-# LINE 3036 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIoriginalTree
                   {-# LINE 3042 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3048 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3054 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3060 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _justOdownEnv =
                  ({-# LINE 115 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   error "missing rule: MaybeSelectList.Just.just.downEnv"
                   {-# LINE 3066 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3072 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _justIannotatedTree,_justIcolExprs,_justIoriginalTree,_justIupEnv,_justIupType) =
                  just_ _justOcat _justOdownEnv _justOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_MaybeSelectList_Nothing :: T_MaybeSelectList 
sem_MaybeSelectList_Nothing  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: MaybeSelectList 
              _lhsOoriginalTree :: MaybeSelectList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 3087 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 3093 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3099 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3105 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- Name --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         flags                : TypeCheckingFlags
         tpe                  : Either [TypeError] Type
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Name:
         child ann            : Annotation 
         child is             : {[NameComponent]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data Name  = Name (Annotation ) (([NameComponent])) 
           deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Name :: Name  ->
            T_Name 
sem_Name (Name _ann _is )  =
    (sem_Name_Name (sem_Annotation _ann ) _is )
-- semantic domain
type T_Name  = Catalog ->
               TypeCheckingFlags ->
               (Either [TypeError] Type) ->
               ( Name ,Name )
data Inh_Name  = Inh_Name {cat_Inh_Name :: Catalog,flags_Inh_Name :: TypeCheckingFlags,tpe_Inh_Name :: (Either [TypeError] Type)}
data Syn_Name  = Syn_Name {annotatedTree_Syn_Name :: Name ,originalTree_Syn_Name :: Name }
wrap_Name :: T_Name  ->
             Inh_Name  ->
             Syn_Name 
wrap_Name sem (Inh_Name _lhsIcat _lhsIflags _lhsItpe )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) = sem _lhsIcat _lhsIflags _lhsItpe 
     in  (Syn_Name _lhsOannotatedTree _lhsOoriginalTree ))
sem_Name_Name :: T_Annotation  ->
                 ([NameComponent]) ->
                 T_Name 
sem_Name_Name ann_ is_  =
    (\ _lhsIcat
       _lhsIflags
       _lhsItpe ->
         (let _lhsOannotatedTree :: Name 
              _lhsOoriginalTree :: Name 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Name _annIannotatedTree is_
                   {-# LINE 3164 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Name _annIoriginalTree is_
                   {-# LINE 3170 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3176 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3182 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3188 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3194 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsItpe
                   {-# LINE 3200 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
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
         (let _lhsOannotatedTree :: NameTypeNameListPair 
              _lhsOoriginalTree :: NameTypeNameListPair 
              _x1Ocat :: Catalog
              _x1Oflags :: TypeCheckingFlags
              _x1Otpe :: (Either [TypeError] Type)
              _x2Ocat :: Catalog
              _x2Oflags :: TypeCheckingFlags
              _x1IannotatedTree :: Name 
              _x1IoriginalTree :: Name 
              _x2IannotatedTree :: TypeNameList 
              _x2IoriginalTree :: TypeNameList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IannotatedTree,_x2IannotatedTree)
                   {-# LINE 3295 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IoriginalTree,_x2IoriginalTree)
                   {-# LINE 3301 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3307 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3313 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3319 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3325 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _x1Otpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: NameTypeNameListPair.Tuple.x1.tpe"
                   {-# LINE 3331 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3337 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3343 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _x1IannotatedTree,_x1IoriginalTree) =
                  x1_ _x1Ocat _x1Oflags _x1Otpe 
              ( _x2IannotatedTree,_x2IoriginalTree) =
                  x2_ _x2Ocat _x2Oflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: NameTypeNameListPairList 
              _lhsOoriginalTree :: NameTypeNameListPairList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: NameTypeNameListPair 
              _hdIoriginalTree :: NameTypeNameListPair 
              _tlIannotatedTree :: NameTypeNameListPairList 
              _tlIoriginalTree :: NameTypeNameListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 3409 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 3415 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3421 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3427 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3433 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3439 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3445 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3451 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_NameTypeNameListPairList_Nil :: T_NameTypeNameListPairList 
sem_NameTypeNameListPairList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: NameTypeNameListPairList 
              _lhsOoriginalTree :: NameTypeNameListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 3468 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 3474 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3480 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3486 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: OnExpr 
              _lhsOoriginalTree :: OnExpr 
              _justOcat :: Catalog
              _justOdownEnv :: Environment
              _justOflags :: TypeCheckingFlags
              _justIannotatedTree :: JoinExpr 
              _justIoriginalTree :: JoinExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIannotatedTree
                   {-# LINE 3548 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Just _justIoriginalTree
                   {-# LINE 3554 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3560 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3566 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3572 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOdownEnv =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 3578 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _justOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3584 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _justIannotatedTree,_justIoriginalTree) =
                  just_ _justOcat _justOdownEnv _justOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOannotatedTree :: OnExpr 
              _lhsOoriginalTree :: OnExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 3600 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Nothing
                   {-# LINE 3606 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3612 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3618 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: ParamDef 
              _lhsOoriginalTree :: ParamDef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _typOcat :: Catalog
              _typOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _typIannotatedTree :: TypeName 
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamDef _annIannotatedTree name_ _typIannotatedTree
                   {-# LINE 3690 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamDef _annIoriginalTree name_ _typIoriginalTree
                   {-# LINE 3696 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3702 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3708 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3714 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3720 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: ParamDef.ParamDef.ann.tpe"
                   {-# LINE 3726 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3732 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3738 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  typ_ _typOcat _typOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ParamDef_ParamDefTp :: T_Annotation  ->
                           T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp ann_ typ_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: ParamDef 
              _lhsOoriginalTree :: ParamDef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _typOcat :: Catalog
              _typOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _typIannotatedTree :: TypeName 
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamDefTp _annIannotatedTree _typIannotatedTree
                   {-# LINE 3767 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamDefTp _annIoriginalTree _typIoriginalTree
                   {-# LINE 3773 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3779 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3785 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3791 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3797 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: ParamDef.ParamDefTp.ann.tpe"
                   {-# LINE 3803 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3809 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3815 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  typ_ _typOcat _typOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: ParamDefList 
              _lhsOoriginalTree :: ParamDefList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: ParamDef 
              _hdIoriginalTree :: ParamDef 
              _tlIannotatedTree :: ParamDefList 
              _tlIoriginalTree :: ParamDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 3881 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 3887 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3893 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3899 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3905 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3911 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 3917 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 3923 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: ParamDefList 
              _lhsOoriginalTree :: ParamDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 3940 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 3946 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 3952 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 3958 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _tpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: QueryExpr 
              _lhsOoriginalTree :: QueryExpr 
              _lhsOupType :: (Maybe [(String,Type)])
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _sel1Ocat :: Catalog
              _sel1Oflags :: TypeCheckingFlags
              _sel1OouterDownEnv :: (Maybe Environment)
              _sel2Ocat :: Catalog
              _sel2Oflags :: TypeCheckingFlags
              _sel2OouterDownEnv :: (Maybe Environment)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _sel1IannotatedTree :: QueryExpr 
              _sel1IoriginalTree :: QueryExpr 
              _sel1IupType :: (Maybe [(String,Type)])
              _sel2IannotatedTree :: QueryExpr 
              _sel2IoriginalTree :: QueryExpr 
              _sel2IupType :: (Maybe [(String,Type)])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 60, column 9)
              _tpe =
                  ({-# LINE 60 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 4076 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CombineQueryExpr _annIannotatedTree ctype_ _sel1IannotatedTree _sel2IannotatedTree
                   {-# LINE 4082 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CombineQueryExpr _annIoriginalTree ctype_ _sel1IoriginalTree _sel2IoriginalTree
                   {-# LINE 4088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4094 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4100 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOupType =
                  ({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _sel2IupType
                   {-# LINE 4106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4112 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4118 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (from local)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _tpe
                   {-# LINE 4124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sel1Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4130 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sel1Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sel1OouterDownEnv =
                  ({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _lhsIouterDownEnv
                   {-# LINE 4142 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sel2Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4148 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sel2Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4154 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sel2OouterDownEnv =
                  ({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _lhsIouterDownEnv
                   {-# LINE 4160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _sel1IannotatedTree,_sel1IoriginalTree,_sel1IupType) =
                  sel1_ _sel1Ocat _sel1Oflags _sel1OouterDownEnv 
              ( _sel2IannotatedTree,_sel2IoriginalTree,_sel2IupType) =
                  sel2_ _sel2Ocat _sel2Oflags _sel2OouterDownEnv 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType)))
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
         (let _selSelectListOdownEnv :: Environment
              _selGroupByOdownEnv :: Environment
              _selHavingOdownEnv :: Environment
              _selOrderByOdownEnv :: Environment
              _tpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe [(String,Type)])
              _selWhereOdownEnv :: Environment
              _lhsOannotatedTree :: QueryExpr 
              _lhsOoriginalTree :: QueryExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _selSelectListOcat :: Catalog
              _selSelectListOflags :: TypeCheckingFlags
              _selTrefOcat :: Catalog
              _selTrefOflags :: TypeCheckingFlags
              _selWhereOcat :: Catalog
              _selWhereOflags :: TypeCheckingFlags
              _selGroupByOcat :: Catalog
              _selGroupByOflags :: TypeCheckingFlags
              _selHavingOcat :: Catalog
              _selHavingOflags :: TypeCheckingFlags
              _selOrderByOcat :: Catalog
              _selOrderByOflags :: TypeCheckingFlags
              _selLimitOcat :: Catalog
              _selLimitOdownEnv :: Environment
              _selLimitOflags :: TypeCheckingFlags
              _selOffsetOcat :: Catalog
              _selOffsetOdownEnv :: Environment
              _selOffsetOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _selSelectListIannotatedTree :: SelectList 
              _selSelectListIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _selSelectListIoriginalTree :: SelectList 
              _selSelectListIupEnv :: Environment
              _selSelectListIupType :: (Maybe [(String,Type)])
              _selTrefIannotatedTree :: TableRefList 
              _selTrefIoriginalTree :: TableRefList 
              _selTrefIupEnv :: Environment
              _selWhereIannotatedTree :: MaybeBoolExpr 
              _selWhereIoriginalTree :: MaybeBoolExpr 
              _selGroupByIannotatedTree :: ScalarExprList 
              _selGroupByIoriginalTree :: ScalarExprList 
              _selGroupByIupTypes :: ([Maybe Type])
              _selHavingIannotatedTree :: MaybeBoolExpr 
              _selHavingIoriginalTree :: MaybeBoolExpr 
              _selOrderByIannotatedTree :: ScalarExprDirectionPairList 
              _selOrderByIoriginalTree :: ScalarExprDirectionPairList 
              _selLimitIannotatedTree :: MaybeScalarExpr 
              _selLimitIoriginalTree :: MaybeScalarExpr 
              _selLimitIupType :: (Maybe Type)
              _selOffsetIannotatedTree :: MaybeScalarExpr 
              _selOffsetIoriginalTree :: MaybeScalarExpr 
              _selOffsetIupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 34, column 9)
              _selSelectListOdownEnv =
                  ({-# LINE 34 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _selTrefIupEnv
                   {-# LINE 4243 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 35, column 9)
              _selGroupByOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _selTrefIupEnv
                   {-# LINE 4249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 36, column 9)
              _selHavingOdownEnv =
                  ({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _selTrefIupEnv
                   {-# LINE 4255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 37, column 9)
              _selOrderByOdownEnv =
                  ({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _selSelectListIupEnv
                   {-# LINE 4261 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 40, column 9)
              _tpe =
                  ({-# LINE 40 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   maybe (Left []) (Right . CompositeType) _selSelectListIupType
                   {-# LINE 4267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 45, column 9)
              _lhsOupType =
                  ({-# LINE 45 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _selSelectListIupType
                   {-# LINE 4273 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 48, column 9)
              _selWhereOdownEnv =
                  ({-# LINE 48 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   maybe _selTrefIupEnv
                    (\e -> createCorrelatedSubqueryEnvironment e _selTrefIupEnv)
                    _lhsIouterDownEnv
                   {-# LINE 4281 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Select _annIannotatedTree selDistinct_ _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
                   {-# LINE 4287 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Select _annIoriginalTree selDistinct_ _selSelectListIoriginalTree _selTrefIoriginalTree _selWhereIoriginalTree _selGroupByIoriginalTree _selHavingIoriginalTree _selOrderByIoriginalTree _selLimitIoriginalTree _selOffsetIoriginalTree
                   {-# LINE 4293 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4299 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4305 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4311 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4317 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (from local)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _tpe
                   {-# LINE 4323 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selSelectListOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4329 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selSelectListOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4335 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selTrefOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4341 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selTrefOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4347 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selWhereOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4353 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selWhereOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4359 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selGroupByOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4365 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selGroupByOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4371 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selHavingOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4377 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selHavingOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4383 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOrderByOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4389 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOrderByOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4395 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selLimitOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4401 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _selLimitOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: QueryExpr.Select.selLimit.downEnv"
                   {-# LINE 4407 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selLimitOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4413 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOffsetOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4419 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _selOffsetOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: QueryExpr.Select.selOffset.downEnv"
                   {-# LINE 4425 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOffsetOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4431 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _selSelectListIannotatedTree,_selSelectListIcolExprs,_selSelectListIoriginalTree,_selSelectListIupEnv,_selSelectListIupType) =
                  selSelectList_ _selSelectListOcat _selSelectListOdownEnv _selSelectListOflags 
              ( _selTrefIannotatedTree,_selTrefIoriginalTree,_selTrefIupEnv) =
                  selTref_ _selTrefOcat _selTrefOflags 
              ( _selWhereIannotatedTree,_selWhereIoriginalTree) =
                  selWhere_ _selWhereOcat _selWhereOdownEnv _selWhereOflags 
              ( _selGroupByIannotatedTree,_selGroupByIoriginalTree,_selGroupByIupTypes) =
                  selGroupBy_ _selGroupByOcat _selGroupByOdownEnv _selGroupByOflags 
              ( _selHavingIannotatedTree,_selHavingIoriginalTree) =
                  selHaving_ _selHavingOcat _selHavingOdownEnv _selHavingOflags 
              ( _selOrderByIannotatedTree,_selOrderByIoriginalTree) =
                  selOrderBy_ _selOrderByOcat _selOrderByOdownEnv _selOrderByOflags 
              ( _selLimitIannotatedTree,_selLimitIoriginalTree,_selLimitIupType) =
                  selLimit_ _selLimitOcat _selLimitOdownEnv _selLimitOflags 
              ( _selOffsetIannotatedTree,_selOffsetIoriginalTree,_selOffsetIupType) =
                  selOffset_ _selOffsetOcat _selOffsetOdownEnv _selOffsetOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType)))
sem_QueryExpr_Values :: T_Annotation  ->
                        T_ScalarExprListList  ->
                        T_QueryExpr 
sem_QueryExpr_Values ann_ vll_  =
    (\ _lhsIcat
       _lhsIflags
       _lhsIouterDownEnv ->
         (let _tpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: QueryExpr 
              _lhsOoriginalTree :: QueryExpr 
              _lhsOupType :: (Maybe [(String,Type)])
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _vllOcat :: Catalog
              _vllOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _vllIannotatedTree :: ScalarExprListList 
              _vllIoriginalTree :: ScalarExprListList 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 60, column 9)
              _tpe =
                  ({-# LINE 60 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 4476 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Values _annIannotatedTree _vllIannotatedTree
                   {-# LINE 4482 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Values _annIoriginalTree _vllIoriginalTree
                   {-# LINE 4488 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4494 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4500 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOupType =
                  ({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: QueryExpr.Values.lhs.upType"
                   {-# LINE 4506 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4512 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4518 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (from local)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _tpe
                   {-# LINE 4524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _vllOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4530 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _vllOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4536 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _vllIannotatedTree,_vllIoriginalTree) =
                  vll_ _vllOcat _vllOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType)))
sem_QueryExpr_WithQueryExpr :: T_Annotation  ->
                               T_WithQueryList  ->
                               T_QueryExpr  ->
                               T_QueryExpr 
sem_QueryExpr_WithQueryExpr ann_ withs_ ex_  =
    (\ _lhsIcat
       _lhsIflags
       _lhsIouterDownEnv ->
         (let _tpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: QueryExpr 
              _lhsOoriginalTree :: QueryExpr 
              _lhsOupType :: (Maybe [(String,Type)])
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _withsOcat :: Catalog
              _withsOflags :: TypeCheckingFlags
              _exOcat :: Catalog
              _exOflags :: TypeCheckingFlags
              _exOouterDownEnv :: (Maybe Environment)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _withsIannotatedTree :: WithQueryList 
              _withsIoriginalTree :: WithQueryList 
              _exIannotatedTree :: QueryExpr 
              _exIoriginalTree :: QueryExpr 
              _exIupType :: (Maybe [(String,Type)])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 60, column 9)
              _tpe =
                  ({-# LINE 60 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Left []
                   {-# LINE 4574 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WithQueryExpr _annIannotatedTree _withsIannotatedTree _exIannotatedTree
                   {-# LINE 4580 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WithQueryExpr _annIoriginalTree _withsIoriginalTree _exIoriginalTree
                   {-# LINE 4586 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4592 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4598 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOupType =
                  ({-# LINE 18 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _exIupType
                   {-# LINE 4604 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4610 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4616 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (from local)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _tpe
                   {-# LINE 4622 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _withsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4628 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _withsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4634 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4640 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4646 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOouterDownEnv =
                  ({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _lhsIouterDownEnv
                   {-# LINE 4652 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _withsIannotatedTree,_withsIoriginalTree) =
                  withs_ _withsOcat _withsOflags 
              ( _exIannotatedTree,_exIoriginalTree,_exIupType) =
                  ex_ _exOcat _exOflags _exOouterDownEnv 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupType)))
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
         (let _lhsOannotatedTree :: Root 
              _lhsOoriginalTree :: Root 
              _statementsOcat :: Catalog
              _statementsOflags :: TypeCheckingFlags
              _statementsIannotatedTree :: StatementList 
              _statementsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Root _statementsIannotatedTree
                   {-# LINE 4711 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Root _statementsIoriginalTree
                   {-# LINE 4717 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4723 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4729 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _statementsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4735 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _statementsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4741 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _statementsIannotatedTree,_statementsIoriginalTree) =
                  statements_ _statementsOcat _statementsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NotNullConstraint _annIannotatedTree name_
                   {-# LINE 4849 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NotNullConstraint _annIoriginalTree name_
                   {-# LINE 4855 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4861 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4867 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4873 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4879 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: RowConstraint.NotNullConstraint.ann.tpe"
                   {-# LINE 4885 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_NullConstraint :: T_Annotation  ->
                                    String ->
                                    T_RowConstraint 
sem_RowConstraint_NullConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullConstraint _annIannotatedTree name_
                   {-# LINE 4907 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullConstraint _annIoriginalTree name_
                   {-# LINE 4913 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4919 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4925 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4931 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 4937 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: RowConstraint.NullConstraint.ann.tpe"
                   {-# LINE 4943 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowCheckConstraint :: T_Annotation  ->
                                        String ->
                                        T_ScalarExpr  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exprOcat :: Catalog
              _exprOdownEnv :: Environment
              _exprOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exprIannotatedTree :: ScalarExpr 
              _exprIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exprIoriginalTree :: ScalarExpr 
              _exprIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowCheckConstraint _annIannotatedTree name_ _exprIannotatedTree
                   {-# LINE 4973 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowCheckConstraint _annIoriginalTree name_ _exprIoriginalTree
                   {-# LINE 4979 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 4985 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 4991 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 4997 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5003 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: RowConstraint.RowCheckConstraint.ann.tpe"
                   {-# LINE 5009 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5015 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exprOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: RowConstraint.RowCheckConstraint.expr.downEnv"
                   {-# LINE 5021 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5027 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOdownEnv _exprOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowPrimaryKeyConstraint :: T_Annotation  ->
                                             String ->
                                             T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowPrimaryKeyConstraint _annIannotatedTree name_
                   {-# LINE 5051 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowPrimaryKeyConstraint _annIoriginalTree name_
                   {-# LINE 5057 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5063 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5069 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5075 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5081 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: RowConstraint.RowPrimaryKeyConstraint.ann.tpe"
                   {-# LINE 5087 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _tableOcat :: Catalog
              _tableOflags :: TypeCheckingFlags
              _tableOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowReferenceConstraint _annIannotatedTree name_ _tableIannotatedTree att_ onUpdate_ onDelete_
                   {-# LINE 5118 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowReferenceConstraint _annIoriginalTree name_ _tableIoriginalTree att_ onUpdate_ onDelete_
                   {-# LINE 5124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5130 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5142 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5148 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: RowConstraint.RowReferenceConstraint.ann.tpe"
                   {-# LINE 5154 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5166 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _tableOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: RowConstraint.RowReferenceConstraint.table.tpe"
                   {-# LINE 5172 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat _tableOflags _tableOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowUniqueConstraint :: T_Annotation  ->
                                         String ->
                                         T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: RowConstraint 
              _lhsOoriginalTree :: RowConstraint 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowUniqueConstraint _annIannotatedTree name_
                   {-# LINE 5196 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   RowUniqueConstraint _annIoriginalTree name_
                   {-# LINE 5202 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5208 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5214 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5220 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5226 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: RowConstraint.RowUniqueConstraint.ann.tpe"
                   {-# LINE 5232 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: RowConstraintList 
              _lhsOoriginalTree :: RowConstraintList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: RowConstraint 
              _hdIoriginalTree :: RowConstraint 
              _tlIannotatedTree :: RowConstraintList 
              _tlIoriginalTree :: RowConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 5296 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 5302 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5308 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5314 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5320 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5326 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5332 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: RowConstraintList 
              _lhsOoriginalTree :: RowConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5355 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 5361 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5367 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5373 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative AntiScalarExpr:
         child string         : {String}
         visit 0:
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative App:
         child ann            : Annotation 
         child funName        : Name 
         child args           : ScalarExprList 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative BinaryOp:
         child ann            : Annotation 
         child opName         : Name 
         child arg0           : ScalarExpr 
         child arg1           : ScalarExpr 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative BooleanLit:
         child ann            : Annotation 
         child b              : {Bool}
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Case:
         child ann            : Annotation 
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local upType      : _
            local whenTypes   : _
            local thenTypes   : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative CaseSimple:
         child ann            : Annotation 
         child value          : ScalarExpr 
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local upType      : _
            local whenTypes   : _
            local thenTypes   : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Cast:
         child ann            : Annotation 
         child expr           : ScalarExpr 
         child tn             : TypeName 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Exists:
         child ann            : Annotation 
         child sel            : QueryExpr 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Extract:
         child ann            : Annotation 
         child field          : {ExtractField}
         child e              : ScalarExpr 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Identifier:
         child ann            : Annotation 
         child i              : {NameComponent}
         visit 0:
            local upType      : _
            local elkp        : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative InPredicate:
         child ann            : Annotation 
         child expr           : ScalarExpr 
         child i              : {Bool}
         child list           : InList 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Interval:
         child ann            : Annotation 
         child value          : {String}
         child field          : {IntervalField}
         child prec           : {Maybe Int}
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative LiftApp:
         child ann            : Annotation 
         child oper           : Name 
         child flav           : {LiftFlavour}
         child args           : ScalarExprList 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative NullLit:
         child ann            : Annotation 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative NumberLit:
         child ann            : Annotation 
         child d              : {String}
         visit 0:
            local upType      : _
            local tpe         : _
            local digChars    : _
            local annotatedTree : _
            local originalTree : _
      alternative Placeholder:
         child ann            : Annotation 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative PositionalArg:
         child ann            : Annotation 
         child p              : {Integer}
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative PostfixOp:
         child ann            : Annotation 
         child opName         : Name 
         child arg            : ScalarExpr 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative PrefixOp:
         child ann            : Annotation 
         child opName         : Name 
         child arg            : ScalarExpr 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative QIdentifier:
         child ann            : Annotation 
         child is             : {[NameComponent]}
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative QStar:
         child ann            : Annotation 
         child q              : {NameComponent}
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative ScalarSubQuery:
         child ann            : Annotation 
         child sel            : QueryExpr 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative SpecialOp:
         child ann            : Annotation 
         child opName         : Name 
         child args           : ScalarExprList 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative Star:
         child ann            : Annotation 
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative StringLit:
         child ann            : Annotation 
         child value          : {String}
         visit 0:
            local upType      : _
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
      alternative TypedStringLit:
         child ann            : Annotation 
         child tn             : TypeName 
         child value          : {String}
         visit 0:
            local upType      : _
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
            local upType      : _
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
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _fnOcat :: Catalog
              _fnOdownEnv :: Environment
              _fnOflags :: TypeCheckingFlags
              _orderByOcat :: Catalog
              _orderByOdownEnv :: Environment
              _orderByOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _fnIannotatedTree :: ScalarExpr 
              _fnIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _fnIoriginalTree :: ScalarExpr 
              _fnIupType :: (Maybe Type)
              _orderByIannotatedTree :: ScalarExprDirectionPairList 
              _orderByIoriginalTree :: ScalarExprDirectionPairList 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 5773 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 5779 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 5785 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 172, column 10)
              _tpe =
                  ({-# LINE 172 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   case _fnIoriginalTree of
                     (App _ fnm@(Name _ [Nmc "count"]) [Star _]) ->
                         tcAppLike _lhsIcat fnm [Just UnknownType]
                     _ -> maybe (Left []) Right _fnIupType
                   {-# LINE 5794 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 5811 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AggregateApp _annIannotatedTree aggDistinct_ _fnIannotatedTree _orderByIannotatedTree
                   {-# LINE 5817 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AggregateApp _annIoriginalTree aggDistinct_ _fnIoriginalTree _orderByIoriginalTree
                   {-# LINE 5823 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5829 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5835 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5841 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5847 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5853 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 5859 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5865 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _orderByOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 5871 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _orderByOdownEnv =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 5877 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _orderByOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 5883 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _fnIannotatedTree,_fnIcolExprs,_fnIoriginalTree,_fnIupType) =
                  fn_ _fnOcat _fnOdownEnv _fnOflags 
              ( _orderByIannotatedTree,_orderByIoriginalTree) =
                  orderBy_ _orderByOcat _orderByOdownEnv _orderByOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_AntiScalarExpr :: String ->
                                 T_ScalarExpr 
sem_ScalarExpr_AntiScalarExpr string_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOupType :: (Maybe Type)
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 31, column 9)
              _lhsOupType =
                  ({-# LINE 31 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 5906 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 292, column 9)
              _tpe =
                  ({-# LINE 292 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 5912 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiScalarExpr string_
                   {-# LINE 5918 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiScalarExpr string_
                   {-# LINE 5924 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 5930 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 5936 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOcolExprs =
                  ({-# LINE 31 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   error "missing rule: ScalarExpr.AntiScalarExpr.lhs.colExprs"
                   {-# LINE 5942 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_App :: T_Annotation  ->
                      T_Name  ->
                      T_ScalarExprList  ->
                      T_ScalarExpr 
sem_ScalarExpr_App ann_ funName_ args_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _funNameOtpe :: (Either [TypeError] Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _funNameOcat :: Catalog
              _funNameOflags :: TypeCheckingFlags
              _argsOcat :: Catalog
              _argsOdownEnv :: Environment
              _argsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _funNameIannotatedTree :: Name 
              _funNameIoriginalTree :: Name 
              _argsIannotatedTree :: ScalarExprList 
              _argsIoriginalTree :: ScalarExprList 
              _argsIupTypes :: ([Maybe Type])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 5977 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 5983 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 5989 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 56, column 9)
              _funNameOtpe =
                  ({-# LINE 56 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 5995 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 158, column 10)
              _tpe =
                  ({-# LINE 158 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   case _originalTree of
                     (App _ (Name _ [Nmc "count"]) [Star _]) ->
                         tcAppLike _lhsIcat _funNameIoriginalTree [Just UnknownType]
                     _ -> tcAppLike _lhsIcat _funNameIoriginalTree _argsIupTypes
                   {-# LINE 6004 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 6021 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   App _annIannotatedTree _funNameIannotatedTree _argsIannotatedTree
                   {-# LINE 6027 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   App _annIoriginalTree _funNameIoriginalTree _argsIoriginalTree
                   {-# LINE 6033 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6039 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6045 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6051 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6057 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _funNameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6063 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _funNameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6069 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6075 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6081 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6087 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _funNameIannotatedTree,_funNameIoriginalTree) =
                  funName_ _funNameOcat _funNameOflags _funNameOtpe 
              ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) =
                  args_ _argsOcat _argsOdownEnv _argsOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_BinaryOp :: T_Annotation  ->
                           T_Name  ->
                           T_ScalarExpr  ->
                           T_ScalarExpr  ->
                           T_ScalarExpr 
sem_ScalarExpr_BinaryOp ann_ opName_ arg0_ arg1_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _opNameOtpe :: (Either [TypeError] Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _opNameOcat :: Catalog
              _opNameOflags :: TypeCheckingFlags
              _arg0Ocat :: Catalog
              _arg0OdownEnv :: Environment
              _arg0Oflags :: TypeCheckingFlags
              _arg1Ocat :: Catalog
              _arg1OdownEnv :: Environment
              _arg1Oflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _opNameIannotatedTree :: Name 
              _opNameIoriginalTree :: Name 
              _arg0IannotatedTree :: ScalarExpr 
              _arg0IcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _arg0IoriginalTree :: ScalarExpr 
              _arg0IupType :: (Maybe Type)
              _arg1IannotatedTree :: ScalarExpr 
              _arg1IcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _arg1IoriginalTree :: ScalarExpr 
              _arg1IupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 6137 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 6143 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 6149 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 58, column 9)
              _opNameOtpe =
                  ({-# LINE 58 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 6155 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 164, column 10)
              _tpe =
                  ({-# LINE 164 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   tcAppLike _lhsIcat _opNameIoriginalTree [_arg0IupType,_arg1IupType]
                   {-# LINE 6161 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 6178 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   BinaryOp _annIannotatedTree _opNameIannotatedTree _arg0IannotatedTree _arg1IannotatedTree
                   {-# LINE 6184 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   BinaryOp _annIoriginalTree _opNameIoriginalTree _arg0IoriginalTree _arg1IoriginalTree
                   {-# LINE 6190 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6196 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6202 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6208 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6214 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _opNameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6220 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _opNameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6226 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _arg0Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6232 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _arg0OdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6238 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _arg0Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6244 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _arg1Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6250 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _arg1OdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6256 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _arg1Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6262 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _opNameIannotatedTree,_opNameIoriginalTree) =
                  opName_ _opNameOcat _opNameOflags _opNameOtpe 
              ( _arg0IannotatedTree,_arg0IcolExprs,_arg0IoriginalTree,_arg0IupType) =
                  arg0_ _arg0Ocat _arg0OdownEnv _arg0Oflags 
              ( _arg1IannotatedTree,_arg1IcolExprs,_arg1IoriginalTree,_arg1IupType) =
                  arg1_ _arg1Ocat _arg1OdownEnv _arg1Oflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_BooleanLit :: T_Annotation  ->
                             Bool ->
                             T_ScalarExpr 
sem_ScalarExpr_BooleanLit ann_ b_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 6293 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 6299 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 6305 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 85, column 9)
              _tpe =
                  ({-# LINE 85 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Right typeBool
                   {-# LINE 6311 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 6328 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   BooleanLit _annIannotatedTree b_
                   {-# LINE 6334 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   BooleanLit _annIoriginalTree b_
                   {-# LINE 6340 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6346 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6352 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6358 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6364 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_Case :: T_Annotation  ->
                       T_CaseScalarExprListScalarExprPairList  ->
                       T_MaybeScalarExpr  ->
                       T_ScalarExpr 
sem_ScalarExpr_Case ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _casesOcat :: Catalog
              _casesOdownEnv :: Environment
              _casesOflags :: TypeCheckingFlags
              _elsOcat :: Catalog
              _elsOdownEnv :: Environment
              _elsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _casesIannotatedTree :: CaseScalarExprListScalarExprPairList 
              _casesIoriginalTree :: CaseScalarExprListScalarExprPairList 
              _casesIthenTypes :: ([Maybe Type])
              _casesIupTypes :: ([Maybe Type])
              _casesIwhenTypes :: ([[Maybe Type]])
              _elsIannotatedTree :: MaybeScalarExpr 
              _elsIoriginalTree :: MaybeScalarExpr 
              _elsIupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 6404 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 6410 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 6416 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 226, column 9)
              _whenTypes =
                  ({-# LINE 226 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _casesIwhenTypes
                   {-# LINE 6422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 227, column 9)
              _thenTypes =
                  ({-# LINE 227 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIupType
                   {-# LINE 6428 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 230, column 9)
              _tpe =
                  ({-# LINE 230 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   do
                   wt <- mapM (maybe (Left []) Right) $ concat _whenTypes
                   errorWhen (any (/= typeBool) wt)
                       [WrongTypes typeBool wt]
                   tt <- mapM (maybe (Left []) Right) _thenTypes
                   resolveResultSetType _lhsIcat tt
                   {-# LINE 6439 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 6456 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Case _annIannotatedTree _casesIannotatedTree _elsIannotatedTree
                   {-# LINE 6462 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Case _annIoriginalTree _casesIoriginalTree _elsIoriginalTree
                   {-# LINE 6468 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6474 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6480 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6486 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6492 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6498 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOdownEnv =
                  ({-# LINE 221 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6504 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6510 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6516 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6522 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6528 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _casesIannotatedTree,_casesIoriginalTree,_casesIthenTypes,_casesIupTypes,_casesIwhenTypes) =
                  cases_ _casesOcat _casesOdownEnv _casesOflags 
              ( _elsIannotatedTree,_elsIoriginalTree,_elsIupType) =
                  els_ _elsOcat _elsOdownEnv _elsOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_CaseSimple :: T_Annotation  ->
                             T_ScalarExpr  ->
                             T_CaseScalarExprListScalarExprPairList  ->
                             T_MaybeScalarExpr  ->
                             T_ScalarExpr 
sem_ScalarExpr_CaseSimple ann_ value_ cases_ els_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _valueOcat :: Catalog
              _valueOdownEnv :: Environment
              _valueOflags :: TypeCheckingFlags
              _casesOcat :: Catalog
              _casesOdownEnv :: Environment
              _casesOflags :: TypeCheckingFlags
              _elsOcat :: Catalog
              _elsOdownEnv :: Environment
              _elsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _valueIannotatedTree :: ScalarExpr 
              _valueIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _valueIoriginalTree :: ScalarExpr 
              _valueIupType :: (Maybe Type)
              _casesIannotatedTree :: CaseScalarExprListScalarExprPairList 
              _casesIoriginalTree :: CaseScalarExprListScalarExprPairList 
              _casesIthenTypes :: ([Maybe Type])
              _casesIupTypes :: ([Maybe Type])
              _casesIwhenTypes :: ([[Maybe Type]])
              _elsIannotatedTree :: MaybeScalarExpr 
              _elsIoriginalTree :: MaybeScalarExpr 
              _elsIupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 6580 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 6586 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 6592 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 226, column 9)
              _whenTypes =
                  ({-# LINE 226 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _casesIwhenTypes
                   {-# LINE 6598 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 227, column 9)
              _thenTypes =
                  ({-# LINE 227 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIupType
                   {-# LINE 6604 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 239, column 9)
              _tpe =
                  ({-# LINE 239 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   do
                   wt <- mapM (maybe (Left []) Right) $ concat _whenTypes
                   vt <- maybe (Left []) Right _valueIupType
                   _ <- resolveResultSetType _lhsIcat (vt : wt)
                   tt <- mapM (maybe (Left []) Right) _thenTypes
                   resolveResultSetType _lhsIcat tt
                   {-# LINE 6615 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 6632 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseSimple _annIannotatedTree _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                   {-# LINE 6638 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseSimple _annIoriginalTree _valueIoriginalTree _casesIoriginalTree _elsIoriginalTree
                   {-# LINE 6644 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6650 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6656 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6662 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6668 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valueOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6674 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valueOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6680 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valueOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6686 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6692 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOdownEnv =
                  ({-# LINE 221 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6698 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6704 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6716 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6722 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _valueIannotatedTree,_valueIcolExprs,_valueIoriginalTree,_valueIupType) =
                  value_ _valueOcat _valueOdownEnv _valueOflags 
              ( _casesIannotatedTree,_casesIoriginalTree,_casesIthenTypes,_casesIupTypes,_casesIwhenTypes) =
                  cases_ _casesOcat _casesOdownEnv _casesOflags 
              ( _elsIannotatedTree,_elsIoriginalTree,_elsIupType) =
                  els_ _elsOcat _elsOdownEnv _elsOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_Cast :: T_Annotation  ->
                       T_ScalarExpr  ->
                       T_TypeName  ->
                       T_ScalarExpr 
sem_ScalarExpr_Cast ann_ expr_ tn_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _exprOcat :: Catalog
              _exprOdownEnv :: Environment
              _exprOflags :: TypeCheckingFlags
              _tnOcat :: Catalog
              _tnOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exprIannotatedTree :: ScalarExpr 
              _exprIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exprIoriginalTree :: ScalarExpr 
              _exprIupType :: (Maybe Type)
              _tnIannotatedTree :: TypeName 
              _tnInamedType :: (Maybe Type)
              _tnIoriginalTree :: TypeName 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 6766 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 6772 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 6778 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 114, column 10)
              _tpe =
                  ({-# LINE 114 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   maybe (Left []) Right _tnInamedType
                   {-# LINE 6784 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 6801 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Cast _annIannotatedTree _exprIannotatedTree _tnIannotatedTree
                   {-# LINE 6807 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Cast _annIoriginalTree _exprIoriginalTree _tnIoriginalTree
                   {-# LINE 6813 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6819 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6825 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6831 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6837 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6843 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 6849 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6855 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6861 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6867 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOdownEnv _exprOflags 
              ( _tnIannotatedTree,_tnInamedType,_tnIoriginalTree) =
                  tn_ _tnOcat _tnOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_Exists :: T_Annotation  ->
                         T_QueryExpr  ->
                         T_ScalarExpr 
sem_ScalarExpr_Exists ann_ sel_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _selOouterDownEnv :: (Maybe Environment)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _selOcat :: Catalog
              _selOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              _selIupType :: (Maybe [(String,Type)])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 6902 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 6908 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 6914 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 259, column 9)
              _tpe =
                  ({-# LINE 259 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Right typeBool
                   {-# LINE 6920 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 280, column 9)
              _selOouterDownEnv =
                  ({-# LINE 280 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Just _lhsIdownEnv
                   {-# LINE 6926 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 6943 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Exists _annIannotatedTree _selIannotatedTree
                   {-# LINE 6949 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Exists _annIoriginalTree _selIoriginalTree
                   {-# LINE 6955 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 6961 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 6967 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6973 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6979 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 6985 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 6991 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _selIannotatedTree,_selIoriginalTree,_selIupType) =
                  sel_ _selOcat _selOflags _selOouterDownEnv 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_Extract :: T_Annotation  ->
                          ExtractField ->
                          T_ScalarExpr  ->
                          T_ScalarExpr 
sem_ScalarExpr_Extract ann_ field_ e_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _eOcat :: Catalog
              _eOdownEnv :: Environment
              _eOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _eIannotatedTree :: ScalarExpr 
              _eIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _eIoriginalTree :: ScalarExpr 
              _eIupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 7026 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 7032 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 7038 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 118, column 10)
              _tpe =
                  ({-# LINE 118 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   do
                   x <- maybe (Left []) Right _eIupType
                   if x == typeDate
                     then Right typeFloat8
                     else Left [NoMatchingOperator "extract" [x]]
                   {-# LINE 7048 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 7065 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Extract _annIannotatedTree field_ _eIannotatedTree
                   {-# LINE 7071 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Extract _annIoriginalTree field_ _eIoriginalTree
                   {-# LINE 7077 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7083 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7089 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7095 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7101 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _eOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7107 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _eOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 7113 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _eOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7119 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _eIannotatedTree,_eIcolExprs,_eIoriginalTree,_eIupType) =
                  e_ _eOcat _eOdownEnv _eOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_Identifier :: T_Annotation  ->
                             NameComponent ->
                             T_ScalarExpr 
sem_ScalarExpr_Identifier ann_ i_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 7146 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 7152 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 7158 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 133, column 10)
              _elkp =
                  ({-# LINE 133 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   envLookupIdentifier [i_] _lhsIdownEnv
                   {-# LINE 7164 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 134, column 10)
              _tpe =
                  ({-# LINE 134 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   fmap snd _elkp
                   {-# LINE 7170 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 135, column 10)
              _lhsOannotatedTree =
                  ({-# LINE 135 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   fromMaybe _annotatedTree $ do
                       case tcfAddQualifiers _lhsIflags of
                         False -> Nothing
                         True -> do
                                ((q,i),_) <- either (const Nothing) Just _elkp
                                return $ QIdentifier _annIannotatedTree [Nmc q, Nmc i]
                   {-# LINE 7181 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 7198 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Identifier _annIannotatedTree i_
                   {-# LINE 7204 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Identifier _annIoriginalTree i_
                   {-# LINE 7210 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7216 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7222 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7228 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_InPredicate :: T_Annotation  ->
                              T_ScalarExpr  ->
                              Bool ->
                              T_InList  ->
                              T_ScalarExpr 
sem_ScalarExpr_InPredicate ann_ expr_ i_ list_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _exprOcat :: Catalog
              _exprOdownEnv :: Environment
              _exprOflags :: TypeCheckingFlags
              _listOcat :: Catalog
              _listOdownEnv :: Environment
              _listOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exprIannotatedTree :: ScalarExpr 
              _exprIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exprIoriginalTree :: ScalarExpr 
              _exprIupType :: (Maybe Type)
              _listIannotatedTree :: InList 
              _listIoriginalTree :: InList 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 7267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 7273 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 7279 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 292, column 9)
              _tpe =
                  ({-# LINE 292 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 7285 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 7302 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InPredicate _annIannotatedTree _exprIannotatedTree i_ _listIannotatedTree
                   {-# LINE 7308 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   InPredicate _annIoriginalTree _exprIoriginalTree i_ _listIoriginalTree
                   {-# LINE 7314 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7320 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7326 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7332 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7344 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 7350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _listOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7362 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _listOdownEnv =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 7368 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _listOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7374 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOdownEnv _exprOflags 
              ( _listIannotatedTree,_listIoriginalTree) =
                  list_ _listOcat _listOdownEnv _listOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_Interval :: T_Annotation  ->
                           String ->
                           IntervalField ->
                           (Maybe Int) ->
                           T_ScalarExpr 
sem_ScalarExpr_Interval ann_ value_ field_ prec_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 7405 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 7411 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 7417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 116, column 10)
              _tpe =
                  ({-# LINE 116 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Right $ ScalarType "interval"
                   {-# LINE 7423 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 7440 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Interval _annIannotatedTree value_ field_ prec_
                   {-# LINE 7446 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Interval _annIoriginalTree value_ field_ prec_
                   {-# LINE 7452 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7458 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7464 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7470 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7476 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_LiftApp :: T_Annotation  ->
                          T_Name  ->
                          LiftFlavour ->
                          T_ScalarExprList  ->
                          T_ScalarExpr 
sem_ScalarExpr_LiftApp ann_ oper_ flav_ args_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _operOcat :: Catalog
              _operOflags :: TypeCheckingFlags
              _operOtpe :: (Either [TypeError] Type)
              _argsOcat :: Catalog
              _argsOdownEnv :: Environment
              _argsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _operIannotatedTree :: Name 
              _operIoriginalTree :: Name 
              _argsIannotatedTree :: ScalarExprList 
              _argsIoriginalTree :: ScalarExprList 
              _argsIupTypes :: ([Maybe Type])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 7514 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 7520 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 7526 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 292, column 9)
              _tpe =
                  ({-# LINE 292 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 7532 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 7549 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   LiftApp _annIannotatedTree _operIannotatedTree flav_ _argsIannotatedTree
                   {-# LINE 7555 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   LiftApp _annIoriginalTree _operIoriginalTree flav_ _argsIoriginalTree
                   {-# LINE 7561 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7567 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7573 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7579 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7585 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _operOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7591 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _operOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7597 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (from local)
              _operOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   _tpe
                   {-# LINE 7603 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7609 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 7615 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7621 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _operIannotatedTree,_operIoriginalTree) =
                  oper_ _operOcat _operOflags _operOtpe 
              ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) =
                  args_ _argsOcat _argsOdownEnv _argsOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_NullLit :: T_Annotation  ->
                          T_ScalarExpr 
sem_ScalarExpr_NullLit ann_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 7649 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 7655 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 7661 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 104, column 10)
              _tpe =
                  ({-# LINE 104 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Right UnknownType
                   {-# LINE 7667 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 7684 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullLit _annIannotatedTree
                   {-# LINE 7690 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullLit _annIoriginalTree
                   {-# LINE 7696 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7702 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7708 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7714 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7720 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_NumberLit :: T_Annotation  ->
                            String ->
                            T_ScalarExpr 
sem_ScalarExpr_NumberLit ann_ d_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 7745 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 7751 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 7757 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 94, column 9)
              _tpe =
                  ({-# LINE 94 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Right $ if all (`elem` _digChars    ) d_
                           then typeInt
                           else typeNumeric
                   {-# LINE 7765 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 97, column 9)
              _digChars =
                  ({-# LINE 97 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   concatMap show [(0::Int)..9]
                   {-# LINE 7771 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 7788 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NumberLit _annIannotatedTree d_
                   {-# LINE 7794 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NumberLit _annIoriginalTree d_
                   {-# LINE 7800 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7806 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7812 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7818 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7824 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_Placeholder :: T_Annotation  ->
                              T_ScalarExpr 
sem_ScalarExpr_Placeholder ann_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 7848 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 7854 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 7860 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 125, column 10)
              _tpe =
                  ({-# LINE 125 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Right UnknownType
                   {-# LINE 7866 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 7883 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Placeholder _annIannotatedTree
                   {-# LINE 7889 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Placeholder _annIoriginalTree
                   {-# LINE 7895 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7901 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 7907 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 7913 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 7919 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_PositionalArg :: T_Annotation  ->
                                Integer ->
                                T_ScalarExpr 
sem_ScalarExpr_PositionalArg ann_ p_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 7944 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 7950 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 7956 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 292, column 9)
              _tpe =
                  ({-# LINE 292 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 7962 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 7979 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PositionalArg _annIannotatedTree p_
                   {-# LINE 7985 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PositionalArg _annIoriginalTree p_
                   {-# LINE 7991 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 7997 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8003 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8009 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8015 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_PostfixOp :: T_Annotation  ->
                            T_Name  ->
                            T_ScalarExpr  ->
                            T_ScalarExpr 
sem_ScalarExpr_PostfixOp ann_ opName_ arg_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _opNameOtpe :: (Either [TypeError] Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _opNameOcat :: Catalog
              _opNameOflags :: TypeCheckingFlags
              _argOcat :: Catalog
              _argOdownEnv :: Environment
              _argOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _opNameIannotatedTree :: Name 
              _opNameIoriginalTree :: Name 
              _argIannotatedTree :: ScalarExpr 
              _argIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _argIoriginalTree :: ScalarExpr 
              _argIupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 8053 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 8059 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 8065 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 58, column 9)
              _opNameOtpe =
                  ({-# LINE 58 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 8071 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 168, column 10)
              _tpe =
                  ({-# LINE 168 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   tcAppLike _lhsIcat _opNameIoriginalTree [_argIupType]
                   {-# LINE 8077 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 8094 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PostfixOp _annIannotatedTree _opNameIannotatedTree _argIannotatedTree
                   {-# LINE 8100 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PostfixOp _annIoriginalTree _opNameIoriginalTree _argIoriginalTree
                   {-# LINE 8106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8112 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8118 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8130 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _opNameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _opNameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8142 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8148 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 8154 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _opNameIannotatedTree,_opNameIoriginalTree) =
                  opName_ _opNameOcat _opNameOflags _opNameOtpe 
              ( _argIannotatedTree,_argIcolExprs,_argIoriginalTree,_argIupType) =
                  arg_ _argOcat _argOdownEnv _argOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_PrefixOp :: T_Annotation  ->
                           T_Name  ->
                           T_ScalarExpr  ->
                           T_ScalarExpr 
sem_ScalarExpr_PrefixOp ann_ opName_ arg_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _opNameOtpe :: (Either [TypeError] Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _opNameOcat :: Catalog
              _opNameOflags :: TypeCheckingFlags
              _argOcat :: Catalog
              _argOdownEnv :: Environment
              _argOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _opNameIannotatedTree :: Name 
              _opNameIoriginalTree :: Name 
              _argIannotatedTree :: ScalarExpr 
              _argIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _argIoriginalTree :: ScalarExpr 
              _argIupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 8202 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 8208 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 8214 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 58, column 9)
              _opNameOtpe =
                  ({-# LINE 58 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 8220 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 166, column 10)
              _tpe =
                  ({-# LINE 166 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   tcAppLike _lhsIcat _opNameIoriginalTree [_argIupType]
                   {-# LINE 8226 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 8243 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PrefixOp _annIannotatedTree _opNameIannotatedTree _argIannotatedTree
                   {-# LINE 8249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PrefixOp _annIoriginalTree _opNameIoriginalTree _argIoriginalTree
                   {-# LINE 8255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8261 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8273 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8279 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _opNameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8285 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _opNameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8291 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8297 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 8303 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8309 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _opNameIannotatedTree,_opNameIoriginalTree) =
                  opName_ _opNameOcat _opNameOflags _opNameOtpe 
              ( _argIannotatedTree,_argIcolExprs,_argIoriginalTree,_argIupType) =
                  arg_ _argOcat _argOdownEnv _argOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_QIdentifier :: T_Annotation  ->
                              ([NameComponent]) ->
                              T_ScalarExpr 
sem_ScalarExpr_QIdentifier ann_ is_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 8338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 8344 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 8350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 145, column 10)
              _tpe =
                  ({-# LINE 145 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   fmap snd $ envLookupIdentifier is_ _lhsIdownEnv
                   {-# LINE 8356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 8373 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QIdentifier _annIannotatedTree is_
                   {-# LINE 8379 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QIdentifier _annIoriginalTree is_
                   {-# LINE 8385 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8391 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8397 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8403 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8409 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_QStar :: T_Annotation  ->
                        NameComponent ->
                        T_ScalarExpr 
sem_ScalarExpr_QStar ann_ q_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 8434 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 8440 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 8446 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 292, column 9)
              _tpe =
                  ({-# LINE 292 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 8452 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 8469 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QStar _annIannotatedTree q_
                   {-# LINE 8475 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QStar _annIoriginalTree q_
                   {-# LINE 8481 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8487 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8493 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8499 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8505 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_ScalarSubQuery :: T_Annotation  ->
                                 T_QueryExpr  ->
                                 T_ScalarExpr 
sem_ScalarExpr_ScalarSubQuery ann_ sel_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _selOouterDownEnv :: (Maybe Environment)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _selOcat :: Catalog
              _selOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              _selIupType :: (Maybe [(String,Type)])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 8536 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 8542 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 8548 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 269, column 9)
              _tpe =
                  ({-# LINE 269 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   do
                   selType <- maybe (Left []) Right _selIupType
                   case length selType of
                     0 -> Left [InternalError "no columns in scalar subquery?"]
                     1 -> Right $ snd $ head selType
                     _ -> Right $ AnonymousCompositeType $ map snd selType
                   {-# LINE 8559 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 278, column 9)
              _selOouterDownEnv =
                  ({-# LINE 278 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Just _lhsIdownEnv
                   {-# LINE 8565 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 8582 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ScalarSubQuery _annIannotatedTree _selIannotatedTree
                   {-# LINE 8588 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ScalarSubQuery _annIoriginalTree _selIoriginalTree
                   {-# LINE 8594 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8600 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8606 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8612 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8618 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8624 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8630 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _selIannotatedTree,_selIoriginalTree,_selIupType) =
                  sel_ _selOcat _selOflags _selOouterDownEnv 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_SpecialOp :: T_Annotation  ->
                            T_Name  ->
                            T_ScalarExprList  ->
                            T_ScalarExpr 
sem_ScalarExpr_SpecialOp ann_ opName_ args_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _opNameOtpe :: (Either [TypeError] Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _opNameOcat :: Catalog
              _opNameOflags :: TypeCheckingFlags
              _argsOcat :: Catalog
              _argsOdownEnv :: Environment
              _argsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _opNameIannotatedTree :: Name 
              _opNameIoriginalTree :: Name 
              _argsIannotatedTree :: ScalarExprList 
              _argsIoriginalTree :: ScalarExprList 
              _argsIupTypes :: ([Maybe Type])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 8669 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 8675 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 8681 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 58, column 9)
              _opNameOtpe =
                  ({-# LINE 58 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 8687 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 170, column 10)
              _tpe =
                  ({-# LINE 170 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   tcAppLike _lhsIcat _opNameIoriginalTree _argsIupTypes
                   {-# LINE 8693 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 8710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SpecialOp _annIannotatedTree _opNameIannotatedTree _argsIannotatedTree
                   {-# LINE 8716 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SpecialOp _annIoriginalTree _opNameIoriginalTree _argsIoriginalTree
                   {-# LINE 8722 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8728 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8734 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8740 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8746 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _opNameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8752 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _opNameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8758 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8764 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 8770 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8776 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _opNameIannotatedTree,_opNameIoriginalTree) =
                  opName_ _opNameOcat _opNameOflags _opNameOtpe 
              ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) =
                  args_ _argsOcat _argsOdownEnv _argsOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_Star :: T_Annotation  ->
                       T_ScalarExpr 
sem_ScalarExpr_Star ann_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 8804 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 8810 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 8816 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 292, column 9)
              _tpe =
                  ({-# LINE 292 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 8822 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 8839 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Star _annIannotatedTree
                   {-# LINE 8845 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Star _annIoriginalTree
                   {-# LINE 8851 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8857 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8863 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8869 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8875 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_StringLit :: T_Annotation  ->
                            String ->
                            T_ScalarExpr 
sem_ScalarExpr_StringLit ann_ value_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 8900 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 8906 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 8912 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 101, column 9)
              _tpe =
                  ({-# LINE 101 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Right UnknownType
                   {-# LINE 8918 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 8935 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   StringLit _annIannotatedTree value_
                   {-# LINE 8941 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   StringLit _annIoriginalTree value_
                   {-# LINE 8947 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 8953 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 8959 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 8965 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 8971 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
sem_ScalarExpr_TypedStringLit :: T_Annotation  ->
                                 T_TypeName  ->
                                 String ->
                                 T_ScalarExpr 
sem_ScalarExpr_TypedStringLit ann_ tn_ value_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _tnOcat :: Catalog
              _tnOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tnIannotatedTree :: TypeName 
              _tnInamedType :: (Maybe Type)
              _tnIoriginalTree :: TypeName 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 9002 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 9008 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 9014 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 114, column 10)
              _tpe =
                  ({-# LINE 114 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   maybe (Left []) Right _tnInamedType
                   {-# LINE 9020 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 9037 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TypedStringLit _annIannotatedTree _tnIannotatedTree value_
                   {-# LINE 9043 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TypedStringLit _annIoriginalTree _tnIoriginalTree value_
                   {-# LINE 9049 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9055 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9061 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9067 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9073 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9079 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9085 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tnIannotatedTree,_tnInamedType,_tnIoriginalTree) =
                  tn_ _tnOcat _tnOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
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
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupType :: (Maybe Type)
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOannotatedTree :: ScalarExpr 
              _lhsOoriginalTree :: ScalarExpr 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _fnOcat :: Catalog
              _fnOdownEnv :: Environment
              _fnOflags :: TypeCheckingFlags
              _partitionByOcat :: Catalog
              _partitionByOdownEnv :: Environment
              _partitionByOflags :: TypeCheckingFlags
              _orderByOcat :: Catalog
              _orderByOdownEnv :: Environment
              _orderByOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _fnIannotatedTree :: ScalarExpr 
              _fnIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _fnIoriginalTree :: ScalarExpr 
              _fnIupType :: (Maybe Type)
              _partitionByIannotatedTree :: ScalarExprList 
              _partitionByIoriginalTree :: ScalarExprList 
              _partitionByIupTypes :: ([Maybe Type])
              _orderByIannotatedTree :: ScalarExprDirectionPairList 
              _orderByIoriginalTree :: ScalarExprDirectionPairList 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 25, column 9)
              _annOtpe =
                  ({-# LINE 25 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _tpe
                   {-# LINE 9133 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 26, column 9)
              _upType =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 9139 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 27, column 9)
              _lhsOupType =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _upType
                   {-# LINE 9145 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 292, column 9)
              _tpe =
                  ({-# LINE 292 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   Left []
                   {-# LINE 9151 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 41, column 7)
              _lhsOcolExprs =
                  ({-# LINE 41 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
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
                   {-# LINE 9168 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WindowApp _annIannotatedTree _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree frm_
                   {-# LINE 9174 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WindowApp _annIoriginalTree _fnIoriginalTree _partitionByIoriginalTree _orderByIoriginalTree frm_
                   {-# LINE 9180 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9186 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9192 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9198 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9204 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9210 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 9216 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9222 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _partitionByOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9228 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _partitionByOdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 9234 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _partitionByOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9240 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _orderByOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9246 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _orderByOdownEnv =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 9252 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _orderByOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9258 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _fnIannotatedTree,_fnIcolExprs,_fnIoriginalTree,_fnIupType) =
                  fn_ _fnOcat _fnOdownEnv _fnOflags 
              ( _partitionByIannotatedTree,_partitionByIoriginalTree,_partitionByIupTypes) =
                  partitionBy_ _partitionByOcat _partitionByOdownEnv _partitionByOflags 
              ( _orderByIannotatedTree,_orderByIoriginalTree) =
                  orderBy_ _orderByOcat _orderByOdownEnv _orderByOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupType)))
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
         (let _lhsOannotatedTree :: ScalarExprDirectionPair 
              _lhsOoriginalTree :: ScalarExprDirectionPair 
              _x1Ocat :: Catalog
              _x1OdownEnv :: Environment
              _x1Oflags :: TypeCheckingFlags
              _x1IannotatedTree :: ScalarExpr 
              _x1IcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _x1IoriginalTree :: ScalarExpr 
              _x1IupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IannotatedTree,x2_)
                   {-# LINE 9326 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IoriginalTree,x2_)
                   {-# LINE 9332 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9344 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1OdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 9356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9362 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _x1IannotatedTree,_x1IcolExprs,_x1IoriginalTree,_x1IupType) =
                  x1_ _x1Ocat _x1OdownEnv _x1Oflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: ScalarExprDirectionPairList 
              _lhsOoriginalTree :: ScalarExprDirectionPairList 
              _hdOcat :: Catalog
              _hdOdownEnv :: Environment
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOdownEnv :: Environment
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: ScalarExprDirectionPair 
              _hdIoriginalTree :: ScalarExprDirectionPair 
              _tlIannotatedTree :: ScalarExprDirectionPairList 
              _tlIoriginalTree :: ScalarExprDirectionPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 9431 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 9437 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9443 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9449 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9455 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOdownEnv =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 9461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9467 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9473 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOdownEnv =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 9479 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9485 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOdownEnv _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOdownEnv _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExprDirectionPairList_Nil :: T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList_Nil  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOannotatedTree :: ScalarExprDirectionPairList 
              _lhsOoriginalTree :: ScalarExprDirectionPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9503 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9509 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9515 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOupTypes :: ([Maybe Type])
              _lhsOannotatedTree :: ScalarExprList 
              _lhsOoriginalTree :: ScalarExprList 
              _hdOcat :: Catalog
              _hdOdownEnv :: Environment
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOdownEnv :: Environment
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: ScalarExpr 
              _hdIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _hdIoriginalTree :: ScalarExpr 
              _hdIupType :: (Maybe Type)
              _tlIannotatedTree :: ScalarExprList 
              _tlIoriginalTree :: ScalarExprList 
              _tlIupTypes :: ([Maybe Type])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 48, column 12)
              _lhsOupTypes =
                  ({-# LINE 48 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _hdIupType : _tlIupTypes
                   {-# LINE 9593 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 9599 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 9605 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9611 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9617 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9623 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 9629 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9635 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9641 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 9647 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9653 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIcolExprs,_hdIoriginalTree,_hdIupType) =
                  hd_ _hdOcat _hdOdownEnv _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIupTypes) =
                  tl_ _tlOcat _tlOdownEnv _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupTypes)))
sem_ScalarExprList_Nil :: T_ScalarExprList 
sem_ScalarExprList_Nil  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOupTypes :: ([Maybe Type])
              _lhsOannotatedTree :: ScalarExprList 
              _lhsOoriginalTree :: ScalarExprList 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag"(line 49, column 11)
              _lhsOupTypes =
                  ({-# LINE 49 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   []
                   {-# LINE 9672 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9678 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9684 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9690 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9696 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupTypes)))
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
         (let _lhsOannotatedTree :: ScalarExprListList 
              _lhsOoriginalTree :: ScalarExprListList 
              _hdOcat :: Catalog
              _hdOdownEnv :: Environment
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: ScalarExprList 
              _hdIoriginalTree :: ScalarExprList 
              _hdIupTypes :: ([Maybe Type])
              _tlIannotatedTree :: ScalarExprListList 
              _tlIoriginalTree :: ScalarExprListList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 9760 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 9766 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9772 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9778 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9784 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _hdOdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: ScalarExprListList.Cons.hd.downEnv"
                   {-# LINE 9790 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9796 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9802 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9808 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree,_hdIupTypes) =
                  hd_ _hdOcat _hdOdownEnv _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExprListList_Nil :: T_ScalarExprListList 
sem_ScalarExprListList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: ScalarExprListList 
              _lhsOoriginalTree :: ScalarExprListList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9825 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 9831 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9837 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9843 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: ScalarExprListStatementListPair 
              _lhsOoriginalTree :: ScalarExprListStatementListPair 
              _x1Ocat :: Catalog
              _x1OdownEnv :: Environment
              _x1Oflags :: TypeCheckingFlags
              _x2Ocat :: Catalog
              _x2Oflags :: TypeCheckingFlags
              _x1IannotatedTree :: ScalarExprList 
              _x1IoriginalTree :: ScalarExprList 
              _x1IupTypes :: ([Maybe Type])
              _x2IannotatedTree :: StatementList 
              _x2IoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IannotatedTree,_x2IannotatedTree)
                   {-# LINE 9903 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IoriginalTree,_x2IoriginalTree)
                   {-# LINE 9909 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 9915 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 9921 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9927 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _x1OdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: ScalarExprListStatementListPair.Tuple.x1.downEnv"
                   {-# LINE 9933 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9939 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 9945 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 9951 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _x1IannotatedTree,_x1IoriginalTree,_x1IupTypes) =
                  x1_ _x1Ocat _x1OdownEnv _x1Oflags 
              ( _x2IannotatedTree,_x2IoriginalTree) =
                  x2_ _x2Ocat _x2Oflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: ScalarExprListStatementListPairList 
              _lhsOoriginalTree :: ScalarExprListStatementListPairList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: ScalarExprListStatementListPair 
              _hdIoriginalTree :: ScalarExprListStatementListPair 
              _tlIannotatedTree :: ScalarExprListStatementListPairList 
              _tlIoriginalTree :: ScalarExprListStatementListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 10017 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 10023 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10029 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10035 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10041 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10047 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10053 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10059 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExprListStatementListPairList_Nil :: T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: ScalarExprListStatementListPairList 
              _lhsOoriginalTree :: ScalarExprListStatementListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10076 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10082 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10094 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: ScalarExprRoot 
              _lhsOoriginalTree :: ScalarExprRoot 
              _exprOcat :: Catalog
              _exprOdownEnv :: Environment
              _exprOflags :: TypeCheckingFlags
              _exprIannotatedTree :: ScalarExpr 
              _exprIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exprIoriginalTree :: ScalarExpr 
              _exprIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ScalarExprRoot _exprIannotatedTree
                   {-# LINE 10150 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ScalarExprRoot _exprIoriginalTree
                   {-# LINE 10156 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10162 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10168 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10174 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exprOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: ScalarExprRoot.ScalarExprRoot.expr.downEnv"
                   {-# LINE 10180 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10186 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOdownEnv _exprOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: ScalarExprStatementListPair 
              _lhsOoriginalTree :: ScalarExprStatementListPair 
              _x1Ocat :: Catalog
              _x1OdownEnv :: Environment
              _x1Oflags :: TypeCheckingFlags
              _x2Ocat :: Catalog
              _x2Oflags :: TypeCheckingFlags
              _x1IannotatedTree :: ScalarExpr 
              _x1IcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _x1IoriginalTree :: ScalarExpr 
              _x1IupType :: (Maybe Type)
              _x2IannotatedTree :: StatementList 
              _x2IoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IannotatedTree,_x2IannotatedTree)
                   {-# LINE 10249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (_x1IoriginalTree,_x2IoriginalTree)
                   {-# LINE 10255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10261 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10273 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _x1OdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: ScalarExprStatementListPair.Tuple.x1.downEnv"
                   {-# LINE 10279 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x1Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10285 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10291 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _x2Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10297 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _x1IannotatedTree,_x1IcolExprs,_x1IoriginalTree,_x1IupType) =
                  x1_ _x1Ocat _x1OdownEnv _x1Oflags 
              ( _x2IannotatedTree,_x2IoriginalTree) =
                  x2_ _x2Ocat _x2Oflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: ScalarExprStatementListPairList 
              _lhsOoriginalTree :: ScalarExprStatementListPairList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: ScalarExprStatementListPair 
              _hdIoriginalTree :: ScalarExprStatementListPair 
              _tlIannotatedTree :: ScalarExprStatementListPairList 
              _tlIoriginalTree :: ScalarExprStatementListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 10363 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 10369 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10381 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10387 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10393 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10405 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ScalarExprStatementListPairList_Nil :: T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: ScalarExprStatementListPairList 
              _lhsOoriginalTree :: ScalarExprStatementListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10428 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10434 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10440 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: SelectItem 
              _annOtpe :: (Either [TypeError] Type)
              _lhsOoriginalTree :: SelectItem 
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _exOcat :: Catalog
              _exOdownEnv :: Environment
              _exOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exIannotatedTree :: ScalarExpr 
              _exIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exIoriginalTree :: ScalarExpr 
              _exIupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 78, column 7)
              _lhsOannotatedTree =
                  ({-# LINE 78 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   if tcfAddSelectItemAliases _lhsIflags
                   then case _exIcolExprs of
                          [(n,_,_)] -> SelectItem _annIannotatedTree _exIannotatedTree (Nmc n)
                          _ ->
                               _annotatedTree
                   else _annotatedTree
                   {-# LINE 10523 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 153, column 7)
              _annOtpe =
                  ({-# LINE 153 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   Left []
                   {-# LINE 10529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelExp _annIannotatedTree _exIannotatedTree
                   {-# LINE 10535 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelExp _annIoriginalTree _exIoriginalTree
                   {-# LINE 10541 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10547 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOcolExprs =
                  ({-# LINE 111 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _exIcolExprs
                   {-# LINE 10553 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10559 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10565 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10571 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 10577 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10583 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exIannotatedTree,_exIcolExprs,_exIoriginalTree,_exIupType) =
                  ex_ _exOcat _exOdownEnv _exOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree)))
sem_SelectItem_SelectItem :: T_Annotation  ->
                             T_ScalarExpr  ->
                             NameComponent ->
                             T_SelectItem 
sem_SelectItem_SelectItem ann_ ex_ name_  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _annOtpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: SelectItem 
              _lhsOoriginalTree :: SelectItem 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _exOcat :: Catalog
              _exOdownEnv :: Environment
              _exOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exIannotatedTree :: ScalarExpr 
              _exIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exIoriginalTree :: ScalarExpr 
              _exIupType :: (Maybe Type)
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 73, column 7)
              _lhsOcolExprs =
                  ({-# LINE 73 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   case _exIcolExprs of
                     [(_,t,e)] -> [(ncStr name_, t,e)]
                     x -> x
                   {-# LINE 10619 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 153, column 7)
              _annOtpe =
                  ({-# LINE 153 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   Left []
                   {-# LINE 10625 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelectItem _annIannotatedTree _exIannotatedTree name_
                   {-# LINE 10631 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelectItem _annIoriginalTree _exIoriginalTree name_
                   {-# LINE 10637 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 10643 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10649 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10655 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10661 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10667 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 10673 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10679 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exIannotatedTree,_exIcolExprs,_exIoriginalTree,_exIupType) =
                  ex_ _exOcat _exOdownEnv _exOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree)))
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
            local colExprs    : _
            local upType      : _
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local upType      : _
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
         (let _lhsOannotatedTree :: SelectItemList 
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOupType :: (Maybe [(String,Type)])
              _lhsOupEnv :: Environment
              _lhsOoriginalTree :: SelectItemList 
              _hdOcat :: Catalog
              _hdOdownEnv :: Environment
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOdownEnv :: Environment
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: SelectItem 
              _hdIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _hdIoriginalTree :: SelectItem 
              _tlIannotatedTree :: SelectItemList 
              _tlIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _tlIoriginalTree :: SelectItemList 
              _tlIupEnv :: Environment
              _tlIupType :: (Maybe [(String,Type)])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 93, column 10)
              _lhsOannotatedTree =
                  ({-# LINE 93 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   if tcfExpandStars _lhsIflags
                   then flip map _colExprs $ \(n,_t,e) ->
                         if n == columnName e
                         then SelExp emptyAnnotation e
                         else SelectItem emptyAnnotation e (Nmc n)
                   else _annotatedTree
                   {-# LINE 10768 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 134, column 9)
              _colExprs =
                  ({-# LINE 134 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _hdIcolExprs ++ _tlIcolExprs
                   {-# LINE 10774 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 135, column 9)
              _lhsOcolExprs =
                  ({-# LINE 135 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _colExprs
                   {-# LINE 10780 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 136, column 9)
              _upType =
                  ({-# LINE 136 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   sequence $ flip map _colExprs
                   $ \(n,t,_) -> fmap (n,) t
                   {-# LINE 10787 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 139, column 7)
              _lhsOupType =
                  ({-# LINE 139 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _upType
                   {-# LINE 10793 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 140, column 7)
              _lhsOupEnv =
                  ({-# LINE 140 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   case fmap envSelectListEnvironment _upType     of
                     (Just (Right e)) -> e
                     _ -> emptyEnvironment
                   {-# LINE 10801 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 10807 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 10813 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10819 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10825 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOdownEnv =
                  ({-# LINE 112 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 10831 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10837 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 10843 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOdownEnv =
                  ({-# LINE 115 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 10849 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 10855 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIcolExprs,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOdownEnv _hdOflags 
              ( _tlIannotatedTree,_tlIcolExprs,_tlIoriginalTree,_tlIupEnv,_tlIupType) =
                  tl_ _tlOcat _tlOdownEnv _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupEnv,_lhsOupType)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIcat
       _lhsIdownEnv
       _lhsIflags ->
         (let _lhsOannotatedTree :: SelectItemList 
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOupType :: (Maybe [(String,Type)])
              _lhsOupEnv :: Environment
              _lhsOoriginalTree :: SelectItemList 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 92, column 9)
              _lhsOannotatedTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   []
                   {-# LINE 10876 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 131, column 9)
              _lhsOcolExprs =
                  ({-# LINE 131 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   []
                   {-# LINE 10882 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 132, column 9)
              _upType =
                  ({-# LINE 132 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   Nothing
                   {-# LINE 10888 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 139, column 7)
              _lhsOupType =
                  ({-# LINE 139 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _upType
                   {-# LINE 10894 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 140, column 7)
              _lhsOupEnv =
                  ({-# LINE 140 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   case fmap envSelectListEnvironment _upType     of
                     (Just (Right e)) -> e
                     _ -> emptyEnvironment
                   {-# LINE 10902 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10908 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 10914 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 10920 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupEnv,_lhsOupType)))
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
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: SelectList 
              _lhsOoriginalTree :: SelectList 
              _lhsOcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _lhsOupEnv :: Environment
              _lhsOupType :: (Maybe [(String,Type)])
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _itemsOcat :: Catalog
              _itemsOdownEnv :: Environment
              _itemsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _itemsIannotatedTree :: SelectItemList 
              _itemsIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _itemsIoriginalTree :: SelectItemList 
              _itemsIupEnv :: Environment
              _itemsIupType :: (Maybe [(String,Type)])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag"(line 150, column 7)
              _annOtpe =
                  ({-# LINE 150 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   Left []
                   {-# LINE 10993 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelectList _annIannotatedTree _itemsIannotatedTree
                   {-# LINE 10999 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SelectList _annIoriginalTree _itemsIoriginalTree
                   {-# LINE 11005 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 11011 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 11017 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOcolExprs =
                  ({-# LINE 116 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _itemsIcolExprs
                   {-# LINE 11023 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOupEnv =
                  ({-# LINE 118 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _itemsIupEnv
                   {-# LINE 11029 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (up)
              _lhsOupType =
                  ({-# LINE 117 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _itemsIupType
                   {-# LINE 11035 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11041 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11047 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _itemsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11053 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _itemsOdownEnv =
                  ({-# LINE 115 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag" #-}
                   _lhsIdownEnv
                   {-# LINE 11059 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _itemsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11065 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _itemsIannotatedTree,_itemsIcolExprs,_itemsIoriginalTree,_itemsIupEnv,_itemsIupType) =
                  items_ _itemsOcat _itemsOdownEnv _itemsOflags 
          in  ( _lhsOannotatedTree,_lhsOcolExprs,_lhsOoriginalTree,_lhsOupEnv,_lhsOupType)))
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
         (let _lhsOannotatedTree :: SetClause 
              _lhsOoriginalTree :: SetClause 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exOcat :: Catalog
              _exOdownEnv :: Environment
              _exOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exIannotatedTree :: ScalarExpr 
              _exIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exIoriginalTree :: ScalarExpr 
              _exIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   MultiSetClause _annIannotatedTree setTargets_ _exIannotatedTree
                   {-# LINE 11144 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   MultiSetClause _annIoriginalTree setTargets_ _exIoriginalTree
                   {-# LINE 11150 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 11156 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 11162 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11168 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11174 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: SetClause.MultiSetClause.ann.tpe"
                   {-# LINE 11180 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11186 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: SetClause.MultiSetClause.ex.downEnv"
                   {-# LINE 11192 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11198 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exIannotatedTree,_exIcolExprs,_exIoriginalTree,_exIupType) =
                  ex_ _exOcat _exOdownEnv _exOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_SetClause_SetClause :: T_Annotation  ->
                           NameComponent ->
                           T_ScalarExpr  ->
                           T_SetClause 
sem_SetClause_SetClause ann_ setTarget_ ex_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: SetClause 
              _lhsOoriginalTree :: SetClause 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exOcat :: Catalog
              _exOdownEnv :: Environment
              _exOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exIannotatedTree :: ScalarExpr 
              _exIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exIoriginalTree :: ScalarExpr 
              _exIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SetClause _annIannotatedTree setTarget_ _exIannotatedTree
                   {-# LINE 11230 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SetClause _annIoriginalTree setTarget_ _exIoriginalTree
                   {-# LINE 11236 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 11242 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 11248 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11254 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11260 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: SetClause.SetClause.ann.tpe"
                   {-# LINE 11266 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11272 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: SetClause.SetClause.ex.downEnv"
                   {-# LINE 11278 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11284 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exIannotatedTree,_exIcolExprs,_exIoriginalTree,_exIupType) =
                  ex_ _exOcat _exOdownEnv _exOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: SetClauseList 
              _lhsOoriginalTree :: SetClauseList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: SetClause 
              _hdIoriginalTree :: SetClause 
              _tlIannotatedTree :: SetClauseList 
              _tlIoriginalTree :: SetClauseList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 11350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 11356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 11362 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 11368 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11374 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11380 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11386 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11392 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: SetClauseList 
              _lhsOoriginalTree :: SetClauseList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 11409 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 11415 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 11421 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 11427 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _nameOcat :: Catalog
              _nameOflags :: TypeCheckingFlags
              _nameOtpe :: (Either [TypeError] Type)
              _ownedByOcat :: Catalog
              _ownedByOflags :: TypeCheckingFlags
              _ownedByOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _ownedByIannotatedTree :: Name 
              _ownedByIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterSequence _annIannotatedTree _nameIannotatedTree _ownedByIannotatedTree
                   {-# LINE 11930 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterSequence _annIoriginalTree _nameIoriginalTree _ownedByIoriginalTree
                   {-# LINE 11936 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 11942 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 11948 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11954 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11960 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.AlterSequence.ann.tpe"
                   {-# LINE 11966 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11972 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11978 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _nameOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.AlterSequence.name.tpe"
                   {-# LINE 11984 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _ownedByOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 11990 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _ownedByOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 11996 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _ownedByOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.AlterSequence.ownedBy.tpe"
                   {-# LINE 12002 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat _nameOflags _nameOtpe 
              ( _ownedByIannotatedTree,_ownedByIoriginalTree) =
                  ownedBy_ _ownedByOcat _ownedByOflags _ownedByOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_AlterTable :: T_Annotation  ->
                            T_Name  ->
                            T_AlterTableActionList  ->
                            T_Statement 
sem_Statement_AlterTable ann_ name_ actions_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _nameOcat :: Catalog
              _nameOflags :: TypeCheckingFlags
              _nameOtpe :: (Either [TypeError] Type)
              _actionsOcat :: Catalog
              _actionsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _actionsIannotatedTree :: AlterTableActionList 
              _actionsIoriginalTree :: AlterTableActionList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterTable _annIannotatedTree _nameIannotatedTree _actionsIannotatedTree
                   {-# LINE 12038 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AlterTable _annIoriginalTree _nameIoriginalTree _actionsIoriginalTree
                   {-# LINE 12044 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12050 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12056 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12062 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12068 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.AlterTable.ann.tpe"
                   {-# LINE 12074 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12080 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12086 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _nameOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.AlterTable.name.tpe"
                   {-# LINE 12092 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _actionsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12098 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _actionsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12104 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat _nameOflags _nameOtpe 
              ( _actionsIannotatedTree,_actionsIoriginalTree) =
                  actions_ _actionsOcat _actionsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_AntiStatement :: String ->
                               T_Statement 
sem_Statement_AntiStatement string_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiStatement string_
                   {-# LINE 12124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   AntiStatement string_
                   {-# LINE 12130 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12142 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Assignment :: T_Annotation  ->
                            T_Name  ->
                            T_ScalarExpr  ->
                            T_Statement 
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _targetOcat :: Catalog
              _targetOflags :: TypeCheckingFlags
              _targetOtpe :: (Either [TypeError] Type)
              _valueOcat :: Catalog
              _valueOdownEnv :: Environment
              _valueOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _targetIannotatedTree :: Name 
              _targetIoriginalTree :: Name 
              _valueIannotatedTree :: ScalarExpr 
              _valueIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _valueIoriginalTree :: ScalarExpr 
              _valueIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Assignment _annIannotatedTree _targetIannotatedTree _valueIannotatedTree
                   {-# LINE 12175 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Assignment _annIoriginalTree _targetIoriginalTree _valueIoriginalTree
                   {-# LINE 12181 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12187 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12193 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12199 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12205 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Assignment.ann.tpe"
                   {-# LINE 12211 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _targetOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12217 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _targetOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12223 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _targetOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.Assignment.target.tpe"
                   {-# LINE 12229 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valueOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12235 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _valueOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.Assignment.value.downEnv"
                   {-# LINE 12241 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valueOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12247 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _targetIannotatedTree,_targetIoriginalTree) =
                  target_ _targetOcat _targetOflags _targetOtpe 
              ( _valueIannotatedTree,_valueIcolExprs,_valueIoriginalTree,_valueIupType) =
                  value_ _valueOcat _valueOdownEnv _valueOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Block :: T_Annotation  ->
                       (Maybe String) ->
                       T_VarDefList  ->
                       T_StatementList  ->
                       T_Statement 
sem_Statement_Block ann_ lb_ vars_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _varsOcat :: Catalog
              _varsOflags :: TypeCheckingFlags
              _stsOcat :: Catalog
              _stsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _varsIannotatedTree :: VarDefList 
              _varsIoriginalTree :: VarDefList 
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Block _annIannotatedTree lb_ _varsIannotatedTree _stsIannotatedTree
                   {-# LINE 12283 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Block _annIoriginalTree lb_ _varsIoriginalTree _stsIoriginalTree
                   {-# LINE 12289 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12295 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12301 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12307 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12313 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Block.ann.tpe"
                   {-# LINE 12319 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _varsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12325 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _varsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12331 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12337 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12343 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _varsIannotatedTree,_varsIoriginalTree) =
                  vars_ _varsOcat _varsOflags 
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat _stsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CaseStatement :: T_Annotation  ->
                               T_ScalarExprListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _casesOcat :: Catalog
              _casesOflags :: TypeCheckingFlags
              _elsOcat :: Catalog
              _elsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _casesIannotatedTree :: ScalarExprListStatementListPairList 
              _casesIoriginalTree :: ScalarExprListStatementListPairList 
              _elsIannotatedTree :: StatementList 
              _elsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseStatement _annIannotatedTree _casesIannotatedTree _elsIannotatedTree
                   {-# LINE 12378 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseStatement _annIoriginalTree _casesIoriginalTree _elsIoriginalTree
                   {-# LINE 12384 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12390 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12396 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12402 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12408 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CaseStatement.ann.tpe"
                   {-# LINE 12414 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12420 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12426 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12432 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12438 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  cases_ _casesOcat _casesOflags 
              ( _elsIannotatedTree,_elsIoriginalTree) =
                  els_ _elsOcat _elsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CaseStatementSimple :: T_Annotation  ->
                                     T_ScalarExpr  ->
                                     T_ScalarExprListStatementListPairList  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_CaseStatementSimple ann_ val_ cases_ els_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _valOcat :: Catalog
              _valOdownEnv :: Environment
              _valOflags :: TypeCheckingFlags
              _casesOcat :: Catalog
              _casesOflags :: TypeCheckingFlags
              _elsOcat :: Catalog
              _elsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _valIannotatedTree :: ScalarExpr 
              _valIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _valIoriginalTree :: ScalarExpr 
              _valIupType :: (Maybe Type)
              _casesIannotatedTree :: ScalarExprListStatementListPairList 
              _casesIoriginalTree :: ScalarExprListStatementListPairList 
              _elsIannotatedTree :: StatementList 
              _elsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseStatementSimple _annIannotatedTree _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                   {-# LINE 12481 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CaseStatementSimple _annIoriginalTree _valIoriginalTree _casesIoriginalTree _elsIoriginalTree
                   {-# LINE 12487 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12493 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12499 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12505 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12511 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CaseStatementSimple.ann.tpe"
                   {-# LINE 12517 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12523 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _valOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.CaseStatementSimple.val.downEnv"
                   {-# LINE 12529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12535 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12541 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12547 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12553 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12559 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _valIannotatedTree,_valIcolExprs,_valIoriginalTree,_valIupType) =
                  val_ _valOcat _valOdownEnv _valOflags 
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  cases_ _casesOcat _casesOflags 
              ( _elsIannotatedTree,_elsIoriginalTree) =
                  els_ _elsOcat _elsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ContinueStatement :: T_Annotation  ->
                                   (Maybe String) ->
                                   T_Statement 
sem_Statement_ContinueStatement ann_ lb_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ContinueStatement _annIannotatedTree lb_
                   {-# LINE 12587 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ContinueStatement _annIoriginalTree lb_
                   {-# LINE 12593 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12599 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12605 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12611 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12617 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.ContinueStatement.ann.tpe"
                   {-# LINE 12623 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Copy :: T_Annotation  ->
                      T_Name  ->
                      ([NameComponent]) ->
                      CopySource ->
                      T_Statement 
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _tableOcat :: Catalog
              _tableOflags :: TypeCheckingFlags
              _tableOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Copy _annIannotatedTree _tableIannotatedTree targetCols_ source_
                   {-# LINE 12652 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Copy _annIoriginalTree _tableIoriginalTree targetCols_ source_
                   {-# LINE 12658 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12664 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12670 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12676 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12682 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Copy.ann.tpe"
                   {-# LINE 12688 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12694 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12700 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _tableOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.Copy.table.tpe"
                   {-# LINE 12706 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat _tableOflags _tableOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CopyData :: T_Annotation  ->
                          String ->
                          T_Statement 
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CopyData _annIannotatedTree insData_
                   {-# LINE 12730 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CopyData _annIoriginalTree insData_
                   {-# LINE 12736 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12742 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12748 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12754 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12760 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CopyData.ann.tpe"
                   {-# LINE 12766 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateDomain :: T_Annotation  ->
                              T_Name  ->
                              T_TypeName  ->
                              String ->
                              T_MaybeBoolExpr  ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ constraintName_ check_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _nameOcat :: Catalog
              _nameOflags :: TypeCheckingFlags
              _nameOtpe :: (Either [TypeError] Type)
              _typOcat :: Catalog
              _typOflags :: TypeCheckingFlags
              _checkOcat :: Catalog
              _checkOdownEnv :: Environment
              _checkOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _typIannotatedTree :: TypeName 
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName 
              _checkIannotatedTree :: MaybeBoolExpr 
              _checkIoriginalTree :: MaybeBoolExpr 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateDomain _annIannotatedTree _nameIannotatedTree _typIannotatedTree constraintName_ _checkIannotatedTree
                   {-# LINE 12806 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateDomain _annIoriginalTree _nameIoriginalTree _typIoriginalTree constraintName_ _checkIoriginalTree
                   {-# LINE 12812 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12818 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12824 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12830 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12836 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateDomain.ann.tpe"
                   {-# LINE 12842 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12848 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12854 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _nameOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateDomain.name.tpe"
                   {-# LINE 12860 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12866 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12872 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _checkOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12878 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _checkOdownEnv =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: Statement.CreateDomain.check.downEnv"
                   {-# LINE 12884 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _checkOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12890 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat _nameOflags _nameOtpe 
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  typ_ _typOcat _typOflags 
              ( _checkIannotatedTree,_checkIoriginalTree) =
                  check_ _checkOcat _checkOdownEnv _checkOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _nameOcat :: Catalog
              _nameOflags :: TypeCheckingFlags
              _nameOtpe :: (Either [TypeError] Type)
              _paramsOcat :: Catalog
              _paramsOflags :: TypeCheckingFlags
              _rettypeOcat :: Catalog
              _rettypeOflags :: TypeCheckingFlags
              _bodyOcat :: Catalog
              _bodyOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _paramsIannotatedTree :: ParamDefList 
              _paramsIoriginalTree :: ParamDefList 
              _rettypeIannotatedTree :: TypeName 
              _rettypeInamedType :: (Maybe Type)
              _rettypeIoriginalTree :: TypeName 
              _bodyIannotatedTree :: FnBody 
              _bodyIoriginalTree :: FnBody 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateFunction _annIannotatedTree _nameIannotatedTree _paramsIannotatedTree _rettypeIannotatedTree rep_ lang_ _bodyIannotatedTree vol_
                   {-# LINE 12942 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateFunction _annIoriginalTree _nameIoriginalTree _paramsIoriginalTree _rettypeIoriginalTree rep_ lang_ _bodyIoriginalTree vol_
                   {-# LINE 12948 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 12954 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 12960 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12966 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12972 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateFunction.ann.tpe"
                   {-# LINE 12978 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 12984 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 12990 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _nameOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateFunction.name.tpe"
                   {-# LINE 12996 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _paramsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13002 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _paramsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13008 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _rettypeOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13014 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _rettypeOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13020 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _bodyOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13026 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _bodyOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13032 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat _nameOflags _nameOtpe 
              ( _paramsIannotatedTree,_paramsIoriginalTree) =
                  params_ _paramsOcat _paramsOflags 
              ( _rettypeIannotatedTree,_rettypeInamedType,_rettypeIoriginalTree) =
                  rettype_ _rettypeOcat _rettypeOflags 
              ( _bodyIannotatedTree,_bodyIoriginalTree) =
                  body_ _bodyOcat _bodyOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateLanguage :: T_Annotation  ->
                                String ->
                                T_Statement 
sem_Statement_CreateLanguage ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateLanguage _annIannotatedTree name_
                   {-# LINE 13062 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateLanguage _annIoriginalTree name_
                   {-# LINE 13068 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 13074 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 13080 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13086 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13092 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateLanguage.ann.tpe"
                   {-# LINE 13098 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _nameOcat :: Catalog
              _nameOflags :: TypeCheckingFlags
              _nameOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateSequence _annIannotatedTree _nameIannotatedTree incr_ min_ max_ start_ cache_
                   {-# LINE 13130 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateSequence _annIoriginalTree _nameIoriginalTree incr_ min_ max_ start_ cache_
                   {-# LINE 13136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 13142 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 13148 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13154 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13160 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateSequence.ann.tpe"
                   {-# LINE 13166 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13172 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13178 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _nameOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateSequence.name.tpe"
                   {-# LINE 13184 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat _nameOflags _nameOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateTable :: T_Annotation  ->
                             T_Name  ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _nameOcat :: Catalog
              _nameOflags :: TypeCheckingFlags
              _nameOtpe :: (Either [TypeError] Type)
              _attsOcat :: Catalog
              _attsOflags :: TypeCheckingFlags
              _consOcat :: Catalog
              _consOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _attsIannotatedTree :: AttributeDefList 
              _attsIoriginalTree :: AttributeDefList 
              _consIannotatedTree :: ConstraintList 
              _consIoriginalTree :: ConstraintList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTable _annIannotatedTree _nameIannotatedTree _attsIannotatedTree _consIannotatedTree
                   {-# LINE 13223 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTable _annIoriginalTree _nameIoriginalTree _attsIoriginalTree _consIoriginalTree
                   {-# LINE 13229 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 13235 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 13241 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13247 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13253 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateTable.ann.tpe"
                   {-# LINE 13259 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13265 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13271 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _nameOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateTable.name.tpe"
                   {-# LINE 13277 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _attsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13283 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _attsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13289 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _consOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13295 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _consOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13301 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat _nameOflags _nameOtpe 
              ( _attsIannotatedTree,_attsIoriginalTree) =
                  atts_ _attsOcat _attsOflags 
              ( _consIannotatedTree,_consIoriginalTree) =
                  cons_ _consOcat _consOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateTableAs :: T_Annotation  ->
                               T_Name  ->
                               T_QueryExpr  ->
                               T_Statement 
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _nameOcat :: Catalog
              _nameOflags :: TypeCheckingFlags
              _nameOtpe :: (Either [TypeError] Type)
              _exprOcat :: Catalog
              _exprOflags :: TypeCheckingFlags
              _exprOouterDownEnv :: (Maybe Environment)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _exprIannotatedTree :: QueryExpr 
              _exprIoriginalTree :: QueryExpr 
              _exprIupType :: (Maybe [(String,Type)])
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTableAs _annIannotatedTree _nameIannotatedTree _exprIannotatedTree
                   {-# LINE 13341 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTableAs _annIoriginalTree _nameIoriginalTree _exprIoriginalTree
                   {-# LINE 13347 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 13353 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 13359 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13365 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13371 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateTableAs.ann.tpe"
                   {-# LINE 13377 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13383 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13389 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _nameOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateTableAs.name.tpe"
                   {-# LINE 13395 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13401 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13407 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exprOouterDownEnv =
                  ({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: Statement.CreateTableAs.expr.outerDownEnv"
                   {-# LINE 13413 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat _nameOflags _nameOtpe 
              ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOflags _exprOouterDownEnv 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _tblOcat :: Catalog
              _tblOflags :: TypeCheckingFlags
              _tblOtpe :: (Either [TypeError] Type)
              _fnNameOcat :: Catalog
              _fnNameOflags :: TypeCheckingFlags
              _fnNameOtpe :: (Either [TypeError] Type)
              _fnArgsOcat :: Catalog
              _fnArgsOdownEnv :: Environment
              _fnArgsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tblIannotatedTree :: Name 
              _tblIoriginalTree :: Name 
              _fnNameIannotatedTree :: Name 
              _fnNameIoriginalTree :: Name 
              _fnArgsIannotatedTree :: ScalarExprList 
              _fnArgsIoriginalTree :: ScalarExprList 
              _fnArgsIupTypes :: ([Maybe Type])
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTrigger _annIannotatedTree name_ wh_ events_ _tblIannotatedTree firing_ _fnNameIannotatedTree _fnArgsIannotatedTree
                   {-# LINE 13461 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateTrigger _annIoriginalTree name_ wh_ events_ _tblIoriginalTree firing_ _fnNameIoriginalTree _fnArgsIoriginalTree
                   {-# LINE 13467 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 13473 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 13479 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13485 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13491 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateTrigger.ann.tpe"
                   {-# LINE 13497 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tblOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13503 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tblOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13509 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _tblOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateTrigger.tbl.tpe"
                   {-# LINE 13515 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnNameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnNameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13527 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _fnNameOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateTrigger.fnName.tpe"
                   {-# LINE 13533 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnArgsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13539 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _fnArgsOdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.CreateTrigger.fnArgs.downEnv"
                   {-# LINE 13545 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnArgsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tblIannotatedTree,_tblIoriginalTree) =
                  tbl_ _tblOcat _tblOflags _tblOtpe 
              ( _fnNameIannotatedTree,_fnNameIoriginalTree) =
                  fnName_ _fnNameOcat _fnNameOflags _fnNameOtpe 
              ( _fnArgsIannotatedTree,_fnArgsIoriginalTree,_fnArgsIupTypes) =
                  fnArgs_ _fnArgsOcat _fnArgsOdownEnv _fnArgsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateType :: T_Annotation  ->
                            T_Name  ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _nameOcat :: Catalog
              _nameOflags :: TypeCheckingFlags
              _nameOtpe :: (Either [TypeError] Type)
              _attsOcat :: Catalog
              _attsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _attsIannotatedTree :: TypeAttributeDefList 
              _attsIoriginalTree :: TypeAttributeDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateType _annIannotatedTree _nameIannotatedTree _attsIannotatedTree
                   {-# LINE 13589 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateType _annIoriginalTree _nameIoriginalTree _attsIoriginalTree
                   {-# LINE 13595 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 13601 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 13607 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13613 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13619 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateType.ann.tpe"
                   {-# LINE 13625 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13631 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13637 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _nameOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateType.name.tpe"
                   {-# LINE 13643 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _attsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13649 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _attsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13655 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat _nameOflags _nameOtpe 
              ( _attsIannotatedTree,_attsIoriginalTree) =
                  atts_ _attsOcat _attsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_CreateView :: T_Annotation  ->
                            T_Name  ->
                            MaybeNameComponentList ->
                            T_QueryExpr  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ colNames_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _nameOcat :: Catalog
              _nameOflags :: TypeCheckingFlags
              _nameOtpe :: (Either [TypeError] Type)
              _exprOcat :: Catalog
              _exprOflags :: TypeCheckingFlags
              _exprOouterDownEnv :: (Maybe Environment)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _nameIannotatedTree :: Name 
              _nameIoriginalTree :: Name 
              _exprIannotatedTree :: QueryExpr 
              _exprIoriginalTree :: QueryExpr 
              _exprIupType :: (Maybe [(String,Type)])
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateView _annIannotatedTree _nameIannotatedTree colNames_ _exprIannotatedTree
                   {-# LINE 13694 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   CreateView _annIoriginalTree _nameIoriginalTree colNames_ _exprIoriginalTree
                   {-# LINE 13700 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 13706 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 13712 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13718 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13724 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.CreateView.ann.tpe"
                   {-# LINE 13730 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13736 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _nameOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13742 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _nameOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.CreateView.name.tpe"
                   {-# LINE 13748 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13754 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13760 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exprOouterDownEnv =
                  ({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: Statement.CreateView.expr.outerDownEnv"
                   {-# LINE 13766 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _nameIannotatedTree,_nameIoriginalTree) =
                  name_ _nameOcat _nameOflags _nameOtpe 
              ( _exprIannotatedTree,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOflags _exprOouterDownEnv 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Delete :: T_Annotation  ->
                        T_Name  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Delete ann_ table_ using_ whr_ returning_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _tableOcat :: Catalog
              _tableOflags :: TypeCheckingFlags
              _tableOtpe :: (Either [TypeError] Type)
              _usingOcat :: Catalog
              _usingOflags :: TypeCheckingFlags
              _whrOcat :: Catalog
              _whrOdownEnv :: Environment
              _whrOflags :: TypeCheckingFlags
              _returningOcat :: Catalog
              _returningOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              _usingIannotatedTree :: TableRefList 
              _usingIoriginalTree :: TableRefList 
              _usingIupEnv :: Environment
              _whrIannotatedTree :: MaybeBoolExpr 
              _whrIoriginalTree :: MaybeBoolExpr 
              _returningIannotatedTree :: MaybeSelectList 
              _returningIoriginalTree :: MaybeSelectList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Delete _annIannotatedTree _tableIannotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
                   {-# LINE 13814 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Delete _annIoriginalTree _tableIoriginalTree _usingIoriginalTree _whrIoriginalTree _returningIoriginalTree
                   {-# LINE 13820 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 13826 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 13832 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13838 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13844 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Delete.ann.tpe"
                   {-# LINE 13850 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13856 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13862 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _tableOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.Delete.table.tpe"
                   {-# LINE 13868 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _usingOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13874 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _usingOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13880 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _whrOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13886 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _whrOdownEnv =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: Statement.Delete.whr.downEnv"
                   {-# LINE 13892 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _whrOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13898 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _returningOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13904 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _returningOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13910 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat _tableOflags _tableOtpe 
              ( _usingIannotatedTree,_usingIoriginalTree,_usingIupEnv) =
                  using_ _usingOcat _usingOflags 
              ( _whrIannotatedTree,_whrIoriginalTree) =
                  whr_ _whrOcat _whrOdownEnv _whrOflags 
              ( _returningIannotatedTree,_returningIoriginalTree) =
                  returning_ _returningOcat _returningOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_DropFunction :: T_Annotation  ->
                              IfExists ->
                              T_NameTypeNameListPairList  ->
                              Cascade ->
                              T_Statement 
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _sigsOcat :: Catalog
              _sigsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _sigsIannotatedTree :: NameTypeNameListPairList 
              _sigsIoriginalTree :: NameTypeNameListPairList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   DropFunction _annIannotatedTree ifE_ _sigsIannotatedTree cascade_
                   {-# LINE 13946 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   DropFunction _annIoriginalTree ifE_ _sigsIoriginalTree cascade_
                   {-# LINE 13952 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 13958 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 13964 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13970 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13976 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.DropFunction.ann.tpe"
                   {-# LINE 13982 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sigsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 13988 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _sigsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 13994 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _sigsIannotatedTree,_sigsIoriginalTree) =
                  sigs_ _sigsOcat _sigsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_DropSomething :: T_Annotation  ->
                               DropType ->
                               IfExists ->
                               ([Name]) ->
                               Cascade ->
                               T_Statement 
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   DropSomething _annIannotatedTree dropType_ ifE_ names_ cascade_
                   {-# LINE 14021 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   DropSomething _annIoriginalTree dropType_ ifE_ names_ cascade_
                   {-# LINE 14027 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14033 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14039 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14045 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14051 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.DropSomething.ann.tpe"
                   {-# LINE 14057 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Execute :: T_Annotation  ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exprOcat :: Catalog
              _exprOdownEnv :: Environment
              _exprOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exprIannotatedTree :: ScalarExpr 
              _exprIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exprIoriginalTree :: ScalarExpr 
              _exprIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Execute _annIannotatedTree _exprIannotatedTree
                   {-# LINE 14086 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Execute _annIoriginalTree _exprIoriginalTree
                   {-# LINE 14092 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14098 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14104 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14110 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14116 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Execute.ann.tpe"
                   {-# LINE 14122 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14128 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exprOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.Execute.expr.downEnv"
                   {-# LINE 14134 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14140 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOdownEnv _exprOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ExitStatement :: T_Annotation  ->
                               (Maybe String) ->
                               T_Statement 
sem_Statement_ExitStatement ann_ lb_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ExitStatement _annIannotatedTree lb_
                   {-# LINE 14164 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ExitStatement _annIoriginalTree lb_
                   {-# LINE 14170 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14176 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14182 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14188 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14194 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.ExitStatement.ann.tpe"
                   {-# LINE 14200 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _fromOcat :: Catalog
              _fromOdownEnv :: Environment
              _fromOflags :: TypeCheckingFlags
              _toOcat :: Catalog
              _toOdownEnv :: Environment
              _toOflags :: TypeCheckingFlags
              _stsOcat :: Catalog
              _stsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _fromIannotatedTree :: ScalarExpr 
              _fromIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _fromIoriginalTree :: ScalarExpr 
              _fromIupType :: (Maybe Type)
              _toIannotatedTree :: ScalarExpr 
              _toIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _toIoriginalTree :: ScalarExpr 
              _toIupType :: (Maybe Type)
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ForIntegerStatement _annIannotatedTree lb_ var_ _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                   {-# LINE 14244 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ForIntegerStatement _annIoriginalTree lb_ var_ _fromIoriginalTree _toIoriginalTree _stsIoriginalTree
                   {-# LINE 14250 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14256 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14262 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14268 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14274 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.ForIntegerStatement.ann.tpe"
                   {-# LINE 14280 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fromOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14286 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _fromOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.ForIntegerStatement.from.downEnv"
                   {-# LINE 14292 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fromOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14298 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _toOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14304 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _toOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.ForIntegerStatement.to.downEnv"
                   {-# LINE 14310 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _toOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14316 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14322 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14328 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _fromIannotatedTree,_fromIcolExprs,_fromIoriginalTree,_fromIupType) =
                  from_ _fromOcat _fromOdownEnv _fromOflags 
              ( _toIannotatedTree,_toIcolExprs,_toIoriginalTree,_toIupType) =
                  to_ _toOcat _toOdownEnv _toOflags 
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat _stsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ForQueryStatement :: T_Annotation  ->
                                   (Maybe String) ->
                                   NameComponent ->
                                   T_QueryExpr  ->
                                   T_StatementList  ->
                                   T_Statement 
sem_Statement_ForQueryStatement ann_ lb_ var_ sel_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _selOcat :: Catalog
              _selOflags :: TypeCheckingFlags
              _selOouterDownEnv :: (Maybe Environment)
              _stsOcat :: Catalog
              _stsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              _selIupType :: (Maybe [(String,Type)])
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ForQueryStatement _annIannotatedTree lb_ var_ _selIannotatedTree _stsIannotatedTree
                   {-# LINE 14369 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ForQueryStatement _annIoriginalTree lb_ var_ _selIoriginalTree _stsIoriginalTree
                   {-# LINE 14375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14381 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14387 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14393 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14399 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.ForQueryStatement.ann.tpe"
                   {-# LINE 14405 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14411 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14417 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _selOouterDownEnv =
                  ({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: Statement.ForQueryStatement.sel.outerDownEnv"
                   {-# LINE 14423 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14429 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14435 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _selIannotatedTree,_selIoriginalTree,_selIupType) =
                  sel_ _selOcat _selOflags _selOouterDownEnv 
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat _stsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_If :: T_Annotation  ->
                    T_ScalarExprStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _casesOcat :: Catalog
              _casesOflags :: TypeCheckingFlags
              _elsOcat :: Catalog
              _elsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _casesIannotatedTree :: ScalarExprStatementListPairList 
              _casesIoriginalTree :: ScalarExprStatementListPairList 
              _elsIannotatedTree :: StatementList 
              _elsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   If _annIannotatedTree _casesIannotatedTree _elsIannotatedTree
                   {-# LINE 14470 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   If _annIoriginalTree _casesIoriginalTree _elsIoriginalTree
                   {-# LINE 14476 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14482 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14488 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14494 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14500 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.If.ann.tpe"
                   {-# LINE 14506 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14512 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _casesOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14518 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14524 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _elsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14530 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  cases_ _casesOcat _casesOflags 
              ( _elsIannotatedTree,_elsIoriginalTree) =
                  els_ _elsOcat _elsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Insert :: T_Annotation  ->
                        T_Name  ->
                        ([NameComponent]) ->
                        T_QueryExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _tableOcat :: Catalog
              _tableOflags :: TypeCheckingFlags
              _tableOtpe :: (Either [TypeError] Type)
              _insDataOcat :: Catalog
              _insDataOflags :: TypeCheckingFlags
              _insDataOouterDownEnv :: (Maybe Environment)
              _returningOcat :: Catalog
              _returningOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              _insDataIannotatedTree :: QueryExpr 
              _insDataIoriginalTree :: QueryExpr 
              _insDataIupType :: (Maybe [(String,Type)])
              _returningIannotatedTree :: MaybeSelectList 
              _returningIoriginalTree :: MaybeSelectList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Insert _annIannotatedTree _tableIannotatedTree targetCols_ _insDataIannotatedTree _returningIannotatedTree
                   {-# LINE 14574 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Insert _annIoriginalTree _tableIoriginalTree targetCols_ _insDataIoriginalTree _returningIoriginalTree
                   {-# LINE 14580 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14586 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14592 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14598 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14604 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Insert.ann.tpe"
                   {-# LINE 14610 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14616 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14622 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _tableOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.Insert.table.tpe"
                   {-# LINE 14628 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _insDataOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14634 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _insDataOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14640 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _insDataOouterDownEnv =
                  ({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: Statement.Insert.insData.outerDownEnv"
                   {-# LINE 14646 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _returningOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14652 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _returningOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14658 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat _tableOflags _tableOtpe 
              ( _insDataIannotatedTree,_insDataIoriginalTree,_insDataIupType) =
                  insData_ _insDataOcat _insDataOflags _insDataOouterDownEnv 
              ( _returningIannotatedTree,_returningIoriginalTree) =
                  returning_ _returningOcat _returningOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Into :: T_Annotation  ->
                      Bool ->
                      ([Name]) ->
                      T_Statement  ->
                      T_Statement 
sem_Statement_Into ann_ strict_ into_ stmt_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _stmtOcat :: Catalog
              _stmtOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _stmtIannotatedTree :: Statement 
              _stmtIoriginalTree :: Statement 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Into _annIannotatedTree strict_ into_ _stmtIannotatedTree
                   {-# LINE 14692 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Into _annIoriginalTree strict_ into_ _stmtIoriginalTree
                   {-# LINE 14698 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14704 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14710 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14716 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14722 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Into.ann.tpe"
                   {-# LINE 14728 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stmtOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14734 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stmtOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14740 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _stmtIannotatedTree,_stmtIoriginalTree) =
                  stmt_ _stmtOcat _stmtOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_LoopStatement :: T_Annotation  ->
                               (Maybe String) ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_LoopStatement ann_ lb_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _stsOcat :: Catalog
              _stsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   LoopStatement _annIannotatedTree lb_ _stsIannotatedTree
                   {-# LINE 14769 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   LoopStatement _annIoriginalTree lb_ _stsIoriginalTree
                   {-# LINE 14775 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14781 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14787 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14793 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14799 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.LoopStatement.ann.tpe"
                   {-# LINE 14805 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14811 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14817 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat _stsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Notify :: T_Annotation  ->
                        String ->
                        T_Statement 
sem_Statement_Notify ann_ name_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Notify _annIannotatedTree name_
                   {-# LINE 14841 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Notify _annIoriginalTree name_
                   {-# LINE 14847 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14853 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14859 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14865 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14871 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Notify.ann.tpe"
                   {-# LINE 14877 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_NullStatement :: T_Annotation  ->
                               T_Statement 
sem_Statement_NullStatement ann_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullStatement _annIannotatedTree
                   {-# LINE 14898 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NullStatement _annIoriginalTree
                   {-# LINE 14904 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14910 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14916 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14922 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14928 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.NullStatement.ann.tpe"
                   {-# LINE 14934 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Perform :: T_Annotation  ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exprOcat :: Catalog
              _exprOdownEnv :: Environment
              _exprOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exprIannotatedTree :: ScalarExpr 
              _exprIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exprIoriginalTree :: ScalarExpr 
              _exprIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Perform _annIannotatedTree _exprIannotatedTree
                   {-# LINE 14963 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Perform _annIoriginalTree _exprIoriginalTree
                   {-# LINE 14969 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 14975 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 14981 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 14987 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 14993 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Perform.ann.tpe"
                   {-# LINE 14999 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15005 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exprOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.Perform.expr.downEnv"
                   {-# LINE 15011 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15017 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOdownEnv _exprOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_QueryStatement :: T_Annotation  ->
                                T_QueryExpr  ->
                                T_Statement 
sem_Statement_QueryStatement ann_ ex_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _exOouterDownEnv :: (Maybe Environment)
              _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exOcat :: Catalog
              _exOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exIannotatedTree :: QueryExpr 
              _exIoriginalTree :: QueryExpr 
              _exIupType :: (Maybe [(String,Type)])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 24, column 20)
              _exOouterDownEnv =
                  ({-# LINE 24 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Nothing
                   {-# LINE 15047 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QueryStatement _annIannotatedTree _exIannotatedTree
                   {-# LINE 15053 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   QueryStatement _annIoriginalTree _exIoriginalTree
                   {-# LINE 15059 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15065 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15071 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15077 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15083 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.QueryStatement.ann.tpe"
                   {-# LINE 15089 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15095 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15101 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exIannotatedTree,_exIoriginalTree,_exIupType) =
                  ex_ _exOcat _exOflags _exOouterDownEnv 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Raise :: T_Annotation  ->
                       RaiseType ->
                       String ->
                       T_ScalarExprList  ->
                       T_Statement 
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _argsOcat :: Catalog
              _argsOdownEnv :: Environment
              _argsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _argsIannotatedTree :: ScalarExprList 
              _argsIoriginalTree :: ScalarExprList 
              _argsIupTypes :: ([Maybe Type])
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Raise _annIannotatedTree level_ message_ _argsIannotatedTree
                   {-# LINE 15133 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Raise _annIoriginalTree level_ message_ _argsIoriginalTree
                   {-# LINE 15139 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15145 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15151 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15157 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15163 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Raise.ann.tpe"
                   {-# LINE 15169 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15175 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _argsOdownEnv =
                  ({-# LINE 44 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.Raise.args.downEnv"
                   {-# LINE 15181 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _argsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15187 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _argsIannotatedTree,_argsIoriginalTree,_argsIupTypes) =
                  args_ _argsOcat _argsOdownEnv _argsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Return :: T_Annotation  ->
                        T_MaybeScalarExpr  ->
                        T_Statement 
sem_Statement_Return ann_ value_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _valueOcat :: Catalog
              _valueOdownEnv :: Environment
              _valueOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _valueIannotatedTree :: MaybeScalarExpr 
              _valueIoriginalTree :: MaybeScalarExpr 
              _valueIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Return _annIannotatedTree _valueIannotatedTree
                   {-# LINE 15217 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Return _annIoriginalTree _valueIoriginalTree
                   {-# LINE 15223 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15229 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15235 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15241 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15247 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Return.ann.tpe"
                   {-# LINE 15253 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valueOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15259 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _valueOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.Return.value.downEnv"
                   {-# LINE 15265 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _valueOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15271 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _valueIannotatedTree,_valueIoriginalTree,_valueIupType) =
                  value_ _valueOcat _valueOdownEnv _valueOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ReturnNext :: T_Annotation  ->
                            T_ScalarExpr  ->
                            T_Statement 
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exprOcat :: Catalog
              _exprOdownEnv :: Environment
              _exprOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exprIannotatedTree :: ScalarExpr 
              _exprIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exprIoriginalTree :: ScalarExpr 
              _exprIupType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReturnNext _annIannotatedTree _exprIannotatedTree
                   {-# LINE 15302 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReturnNext _annIoriginalTree _exprIoriginalTree
                   {-# LINE 15308 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15314 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15320 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15326 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15332 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.ReturnNext.ann.tpe"
                   {-# LINE 15338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15344 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exprOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.ReturnNext.expr.downEnv"
                   {-# LINE 15350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15356 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOdownEnv _exprOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_ReturnQuery :: T_Annotation  ->
                             T_QueryExpr  ->
                             T_Statement 
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _selOcat :: Catalog
              _selOflags :: TypeCheckingFlags
              _selOouterDownEnv :: (Maybe Environment)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              _selIupType :: (Maybe [(String,Type)])
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReturnQuery _annIannotatedTree _selIannotatedTree
                   {-# LINE 15386 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ReturnQuery _annIoriginalTree _selIoriginalTree
                   {-# LINE 15392 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15398 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15404 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15410 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15416 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.ReturnQuery.ann.tpe"
                   {-# LINE 15422 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15428 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15434 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _selOouterDownEnv =
                  ({-# LINE 22 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: Statement.ReturnQuery.sel.outerDownEnv"
                   {-# LINE 15440 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _selIannotatedTree,_selIoriginalTree,_selIupType) =
                  sel_ _selOcat _selOflags _selOouterDownEnv 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Set :: T_Annotation  ->
                     String ->
                     ([SetValue]) ->
                     T_Statement 
sem_Statement_Set ann_ name_ values_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Set _annIannotatedTree name_ values_
                   {-# LINE 15465 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Set _annIoriginalTree name_ values_
                   {-# LINE 15471 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15477 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15483 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15489 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15495 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Set.ann.tpe"
                   {-# LINE 15501 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_Truncate :: T_Annotation  ->
                          ([Name]) ->
                          RestartIdentity ->
                          Cascade ->
                          T_Statement 
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Truncate _annIannotatedTree tables_ restartIdentity_ cascade_
                   {-# LINE 15525 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Truncate _annIoriginalTree tables_ restartIdentity_ cascade_
                   {-# LINE 15531 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15537 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15543 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15549 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15555 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Truncate.ann.tpe"
                   {-# LINE 15561 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _tableOcat :: Catalog
              _tableOflags :: TypeCheckingFlags
              _tableOtpe :: (Either [TypeError] Type)
              _assignsOcat :: Catalog
              _assignsOflags :: TypeCheckingFlags
              _fromListOcat :: Catalog
              _fromListOflags :: TypeCheckingFlags
              _whrOcat :: Catalog
              _whrOdownEnv :: Environment
              _whrOflags :: TypeCheckingFlags
              _returningOcat :: Catalog
              _returningOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tableIannotatedTree :: Name 
              _tableIoriginalTree :: Name 
              _assignsIannotatedTree :: SetClauseList 
              _assignsIoriginalTree :: SetClauseList 
              _fromListIannotatedTree :: TableRefList 
              _fromListIoriginalTree :: TableRefList 
              _fromListIupEnv :: Environment
              _whrIannotatedTree :: MaybeBoolExpr 
              _whrIoriginalTree :: MaybeBoolExpr 
              _returningIannotatedTree :: MaybeSelectList 
              _returningIoriginalTree :: MaybeSelectList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Update _annIannotatedTree _tableIannotatedTree _assignsIannotatedTree _fromListIannotatedTree _whrIannotatedTree _returningIannotatedTree
                   {-# LINE 15610 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Update _annIoriginalTree _tableIoriginalTree _assignsIoriginalTree _fromListIoriginalTree _whrIoriginalTree _returningIoriginalTree
                   {-# LINE 15616 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15622 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15628 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15634 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15640 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.Update.ann.tpe"
                   {-# LINE 15646 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15652 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tableOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15658 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _tableOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: Statement.Update.table.tpe"
                   {-# LINE 15664 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _assignsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15670 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _assignsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15676 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fromListOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15682 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fromListOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15688 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _whrOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15694 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _whrOdownEnv =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   error "missing rule: Statement.Update.whr.downEnv"
                   {-# LINE 15700 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _whrOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15706 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _returningOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15712 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _returningOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15718 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tableIannotatedTree,_tableIoriginalTree) =
                  table_ _tableOcat _tableOflags _tableOtpe 
              ( _assignsIannotatedTree,_assignsIoriginalTree) =
                  assigns_ _assignsOcat _assignsOflags 
              ( _fromListIannotatedTree,_fromListIoriginalTree,_fromListIupEnv) =
                  fromList_ _fromListOcat _fromListOflags 
              ( _whrIannotatedTree,_whrIoriginalTree) =
                  whr_ _whrOcat _whrOdownEnv _whrOflags 
              ( _returningIannotatedTree,_returningIoriginalTree) =
                  returning_ _returningOcat _returningOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Statement_WhileStatement :: T_Annotation  ->
                                (Maybe String) ->
                                T_ScalarExpr  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement ann_ lb_ expr_ sts_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: Statement 
              _lhsOoriginalTree :: Statement 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exprOcat :: Catalog
              _exprOdownEnv :: Environment
              _exprOflags :: TypeCheckingFlags
              _stsOcat :: Catalog
              _stsOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exprIannotatedTree :: ScalarExpr 
              _exprIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _exprIoriginalTree :: ScalarExpr 
              _exprIupType :: (Maybe Type)
              _stsIannotatedTree :: StatementList 
              _stsIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WhileStatement _annIannotatedTree lb_ _exprIannotatedTree _stsIannotatedTree
                   {-# LINE 15763 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WhileStatement _annIoriginalTree lb_ _exprIoriginalTree _stsIoriginalTree
                   {-# LINE 15769 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15775 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15781 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15787 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15793 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: Statement.WhileStatement.ann.tpe"
                   {-# LINE 15799 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15805 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _exprOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: Statement.WhileStatement.expr.downEnv"
                   {-# LINE 15811 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15817 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15823 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _stsOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15829 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exprIannotatedTree,_exprIcolExprs,_exprIoriginalTree,_exprIupType) =
                  expr_ _exprOcat _exprOdownEnv _exprOflags 
              ( _stsIannotatedTree,_stsIoriginalTree) =
                  sts_ _stsOcat _stsOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: StatementList 
              _lhsOoriginalTree :: StatementList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: Statement 
              _hdIoriginalTree :: Statement 
              _tlIannotatedTree :: StatementList 
              _tlIoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 15897 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 15903 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15909 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15915 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15921 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15927 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 15933 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 15939 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: StatementList 
              _lhsOoriginalTree :: StatementList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 15956 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 15962 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 15968 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 15974 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TableAlias 
              _lhsOoriginalTree :: TableAlias 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 75, column 9)
              _annOtpe =
                  ({-# LINE 75 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   Left []
                   {-# LINE 16049 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   FullAlias _annIannotatedTree tb_ cols_
                   {-# LINE 16055 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   FullAlias _annIoriginalTree tb_ cols_
                   {-# LINE 16061 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 16067 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 16073 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16079 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16085 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TableAlias_NoAlias :: T_Annotation  ->
                          T_TableAlias 
sem_TableAlias_NoAlias ann_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TableAlias 
              _lhsOoriginalTree :: TableAlias 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 75, column 9)
              _annOtpe =
                  ({-# LINE 75 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   Left []
                   {-# LINE 16106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NoAlias _annIannotatedTree
                   {-# LINE 16112 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   NoAlias _annIoriginalTree
                   {-# LINE 16118 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 16124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 16130 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16136 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16142 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TableAlias_TableAlias :: T_Annotation  ->
                             NameComponent ->
                             T_TableAlias 
sem_TableAlias_TableAlias ann_ tb_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TableAlias 
              _lhsOoriginalTree :: TableAlias 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 75, column 9)
              _annOtpe =
                  ({-# LINE 75 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   Left []
                   {-# LINE 16164 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TableAlias _annIannotatedTree tb_
                   {-# LINE 16170 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TableAlias _annIoriginalTree tb_
                   {-# LINE 16176 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 16182 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 16188 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16194 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16200 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
            local eEnv2       : _
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
            local eEnv2       : _
            local eEnv        : {Either [TypeError] Environment}
            local annotatedTree : _
            local originalTree : _
      alternative SubTref:
         child ann            : Annotation 
         child sel            : QueryExpr 
         child alias          : TableAlias 
         visit 0:
            local eEnv2       : _
            local eEnv        : {Either [TypeError] Environment}
            local annotatedTree : _
            local originalTree : _
      alternative Tref:
         child ann            : Annotation 
         child tbl            : Name 
         child alias          : TableAlias 
         visit 0:
            local eEnv2       : _
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
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupEnv :: Environment
              _eEnv :: (Either [TypeError] Environment)
              _lhsOannotatedTree :: TableRef 
              _lhsOoriginalTree :: TableRef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _fnOcat :: Catalog
              _fnOdownEnv :: Environment
              _fnOflags :: TypeCheckingFlags
              _aliasOcat :: Catalog
              _aliasOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _fnIannotatedTree :: ScalarExpr 
              _fnIcolExprs :: ([(String,Maybe Type,ScalarExpr)])
              _fnIoriginalTree :: ScalarExpr 
              _fnIupType :: (Maybe Type)
              _aliasIannotatedTree :: TableAlias 
              _aliasIoriginalTree :: TableAlias 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 27, column 7)
              _eEnv2 =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   case _aliasIoriginalTree of
                     NoAlias _ -> _eEnv
                     TableAlias _ t ->
                       fmap (createTrefAliasedEnvironment (ncStr t) Nothing)
                            _eEnv
                     FullAlias _ t cs ->
                       fmap (createTrefAliasedEnvironment (ncStr t)
                                                          (Just $ map ncStr cs))
                            _eEnv
                   {-# LINE 16324 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 38, column 7)
              _annOtpe =
                  ({-# LINE 38 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   either Left (const $ Left []) _eEnv
                   {-# LINE 16330 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 40, column 7)
              _lhsOupEnv =
                  ({-# LINE 40 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   either (const emptyEnvironment) id _eEnv2
                   {-# LINE 16336 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 71, column 7)
              _eEnv =
                  ({-# LINE 71 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   Left []
                   {-# LINE 16342 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   FunTref _annIannotatedTree _fnIannotatedTree _aliasIannotatedTree
                   {-# LINE 16348 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   FunTref _annIoriginalTree _fnIoriginalTree _aliasIoriginalTree
                   {-# LINE 16354 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 16360 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 16366 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16372 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16378 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16384 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _fnOdownEnv =
                  ({-# LINE 35 "src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs.ag" #-}
                   error "missing rule: TableRef.FunTref.fn.downEnv"
                   {-# LINE 16390 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _fnOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16396 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16402 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16408 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _fnIannotatedTree,_fnIcolExprs,_fnIoriginalTree,_fnIupType) =
                  fn_ _fnOcat _fnOdownEnv _fnOflags 
              ( _aliasIannotatedTree,_aliasIoriginalTree) =
                  alias_ _aliasOcat _aliasOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv)))
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
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupEnv :: Environment
              _eEnv :: (Either [TypeError] Environment)
              _onExprOdownEnv :: Environment
              _lhsOannotatedTree :: TableRef 
              _lhsOoriginalTree :: TableRef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _tbl0Ocat :: Catalog
              _tbl0Oflags :: TypeCheckingFlags
              _tbl1Ocat :: Catalog
              _tbl1Oflags :: TypeCheckingFlags
              _onExprOcat :: Catalog
              _onExprOflags :: TypeCheckingFlags
              _aliasOcat :: Catalog
              _aliasOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tbl0IannotatedTree :: TableRef 
              _tbl0IoriginalTree :: TableRef 
              _tbl0IupEnv :: Environment
              _tbl1IannotatedTree :: TableRef 
              _tbl1IoriginalTree :: TableRef 
              _tbl1IupEnv :: Environment
              _onExprIannotatedTree :: OnExpr 
              _onExprIoriginalTree :: OnExpr 
              _aliasIannotatedTree :: TableAlias 
              _aliasIoriginalTree :: TableAlias 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 27, column 7)
              _eEnv2 =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   case _aliasIoriginalTree of
                     NoAlias _ -> _eEnv
                     TableAlias _ t ->
                       fmap (createTrefAliasedEnvironment (ncStr t) Nothing)
                            _eEnv
                     FullAlias _ t cs ->
                       fmap (createTrefAliasedEnvironment (ncStr t)
                                                          (Just $ map ncStr cs))
                            _eEnv
                   {-# LINE 16468 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 38, column 7)
              _annOtpe =
                  ({-# LINE 38 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   either Left (const $ Left []) _eEnv
                   {-# LINE 16474 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 40, column 7)
              _lhsOupEnv =
                  ({-# LINE 40 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   either (const emptyEnvironment) id _eEnv2
                   {-# LINE 16480 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 52, column 7)
              _eEnv =
                  ({-# LINE 52 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   createJoinTrefEnvironment _lhsIcat
                     _tbl0IupEnv _tbl1IupEnv
                     $ case (joinType_,_onExprIoriginalTree) of
                        (x,Nothing) | x /= Cross -> Nothing
                        (_,Just (JoinUsing _ nms)) -> Just nms
                        _ -> Just []
                   {-# LINE 16491 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 62, column 7)
              _onExprOdownEnv =
                  ({-# LINE 62 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   either (const emptyEnvironment) id _eEnv
                   {-# LINE 16497 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinTref _annIannotatedTree _tbl0IannotatedTree nat_ joinType_ _tbl1IannotatedTree _onExprIannotatedTree _aliasIannotatedTree
                   {-# LINE 16503 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   JoinTref _annIoriginalTree _tbl0IoriginalTree nat_ joinType_ _tbl1IoriginalTree _onExprIoriginalTree _aliasIoriginalTree
                   {-# LINE 16509 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 16515 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 16521 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16527 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16533 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tbl0Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16539 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tbl0Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16545 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tbl1Ocat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16551 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tbl1Oflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16557 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _onExprOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16563 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _onExprOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16569 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16575 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16581 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tbl0IannotatedTree,_tbl0IoriginalTree,_tbl0IupEnv) =
                  tbl0_ _tbl0Ocat _tbl0Oflags 
              ( _tbl1IannotatedTree,_tbl1IoriginalTree,_tbl1IupEnv) =
                  tbl1_ _tbl1Ocat _tbl1Oflags 
              ( _onExprIannotatedTree,_onExprIoriginalTree) =
                  onExpr_ _onExprOcat _onExprOdownEnv _onExprOflags 
              ( _aliasIannotatedTree,_aliasIoriginalTree) =
                  alias_ _aliasOcat _aliasOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv)))
sem_TableRef_SubTref :: T_Annotation  ->
                        T_QueryExpr  ->
                        T_TableAlias  ->
                        T_TableRef 
sem_TableRef_SubTref ann_ sel_ alias_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _selOouterDownEnv :: (Maybe Environment)
              _annOtpe :: (Either [TypeError] Type)
              _lhsOupEnv :: Environment
              _eEnv :: (Either [TypeError] Environment)
              _lhsOannotatedTree :: TableRef 
              _lhsOoriginalTree :: TableRef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _selOcat :: Catalog
              _selOflags :: TypeCheckingFlags
              _aliasOcat :: Catalog
              _aliasOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _selIannotatedTree :: QueryExpr 
              _selIoriginalTree :: QueryExpr 
              _selIupType :: (Maybe [(String,Type)])
              _aliasIannotatedTree :: TableAlias 
              _aliasIoriginalTree :: TableAlias 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 26, column 13)
              _selOouterDownEnv =
                  ({-# LINE 26 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Nothing
                   {-# LINE 16624 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 27, column 7)
              _eEnv2 =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   case _aliasIoriginalTree of
                     NoAlias _ -> _eEnv
                     TableAlias _ t ->
                       fmap (createTrefAliasedEnvironment (ncStr t) Nothing)
                            _eEnv
                     FullAlias _ t cs ->
                       fmap (createTrefAliasedEnvironment (ncStr t)
                                                          (Just $ map ncStr cs))
                            _eEnv
                   {-# LINE 16638 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 38, column 7)
              _annOtpe =
                  ({-# LINE 38 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   either Left (const $ Left []) _eEnv
                   {-# LINE 16644 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 40, column 7)
              _lhsOupEnv =
                  ({-# LINE 40 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   either (const emptyEnvironment) id _eEnv2
                   {-# LINE 16650 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 64, column 7)
              _eEnv =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   maybe (Left []) id
                   $ fmap envSelectListEnvironment _selIupType
                   {-# LINE 16657 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SubTref _annIannotatedTree _selIannotatedTree _aliasIannotatedTree
                   {-# LINE 16663 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SubTref _annIoriginalTree _selIoriginalTree _aliasIoriginalTree
                   {-# LINE 16669 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 16675 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 16681 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16687 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16693 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16699 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _selOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16705 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16711 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16717 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _selIannotatedTree,_selIoriginalTree,_selIupType) =
                  sel_ _selOcat _selOflags _selOouterDownEnv 
              ( _aliasIannotatedTree,_aliasIoriginalTree) =
                  alias_ _aliasOcat _aliasOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv)))
sem_TableRef_Tref :: T_Annotation  ->
                     T_Name  ->
                     T_TableAlias  ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_ alias_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _annOtpe :: (Either [TypeError] Type)
              _lhsOupEnv :: Environment
              _eEnv :: (Either [TypeError] Environment)
              _tblOtpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TableRef 
              _lhsOoriginalTree :: TableRef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _tblOcat :: Catalog
              _tblOflags :: TypeCheckingFlags
              _aliasOcat :: Catalog
              _aliasOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tblIannotatedTree :: Name 
              _tblIoriginalTree :: Name 
              _aliasIannotatedTree :: TableAlias 
              _aliasIoriginalTree :: TableAlias 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 27, column 7)
              _eEnv2 =
                  ({-# LINE 27 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   case _aliasIoriginalTree of
                     NoAlias _ -> _eEnv
                     TableAlias _ t ->
                       fmap (createTrefAliasedEnvironment (ncStr t) Nothing)
                            _eEnv
                     FullAlias _ t cs ->
                       fmap (createTrefAliasedEnvironment (ncStr t)
                                                          (Just $ map ncStr cs))
                            _eEnv
                   {-# LINE 16763 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 38, column 7)
              _annOtpe =
                  ({-# LINE 38 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   either Left (const $ Left []) _eEnv
                   {-# LINE 16769 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 40, column 7)
              _lhsOupEnv =
                  ({-# LINE 40 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   either (const emptyEnvironment) id _eEnv2
                   {-# LINE 16775 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 48, column 7)
              _eEnv =
                  ({-# LINE 48 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   envCreateTrefEnvironment _lhsIcat (nameComponents _tblIoriginalTree)
                   {-# LINE 16781 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 49, column 7)
              _tblOtpe =
                  ({-# LINE 49 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   Left []
                   {-# LINE 16787 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Tref _annIannotatedTree _tblIannotatedTree _aliasIannotatedTree
                   {-# LINE 16793 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Tref _annIoriginalTree _tblIoriginalTree _aliasIoriginalTree
                   {-# LINE 16799 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 16805 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 16811 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16817 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16823 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tblOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16829 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tblOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16835 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16841 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16847 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tblIannotatedTree,_tblIoriginalTree) =
                  tbl_ _tblOcat _tblOflags _tblOtpe 
              ( _aliasIannotatedTree,_aliasIoriginalTree) =
                  alias_ _aliasOcat _aliasOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv)))
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
         (let _lhsOupEnv :: Environment
              _lhsOannotatedTree :: TableRefList 
              _lhsOoriginalTree :: TableRefList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: TableRef 
              _hdIoriginalTree :: TableRef 
              _hdIupEnv :: Environment
              _tlIannotatedTree :: TableRefList 
              _tlIoriginalTree :: TableRefList 
              _tlIupEnv :: Environment
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 15, column 7)
              _lhsOupEnv =
                  ({-# LINE 15 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   if isEmptyEnv _tlIupEnv
                   then _hdIupEnv
                   else
                          either (error . show) id $
                          createJoinTrefEnvironment _lhsIcat _hdIupEnv _tlIupEnv $ Just []
                   {-# LINE 16923 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 16929 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 16935 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 16941 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 16947 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16953 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16959 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 16965 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 16971 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree,_hdIupEnv) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIupEnv) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv)))
sem_TableRefList_Nil :: T_TableRefList 
sem_TableRefList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOupEnv :: Environment
              _lhsOannotatedTree :: TableRefList 
              _lhsOoriginalTree :: TableRefList 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag"(line 13, column 9)
              _lhsOupEnv =
                  ({-# LINE 13 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag" #-}
                   emptyEnvironment
                   {-# LINE 16989 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 16995 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 17001 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17007 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17013 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOupEnv)))
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
         (let _lhsOannotatedTree :: TypeAttributeDef 
              _lhsOoriginalTree :: TypeAttributeDef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _typOcat :: Catalog
              _typOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _typIannotatedTree :: TypeName 
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TypeAttDef _annIannotatedTree name_ _typIannotatedTree
                   {-# LINE 17076 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   TypeAttDef _annIoriginalTree name_ _typIoriginalTree
                   {-# LINE 17082 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17088 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17094 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17100 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17106 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: TypeAttributeDef.TypeAttDef.ann.tpe"
                   {-# LINE 17112 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17118 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17124 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  typ_ _typOcat _typOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: TypeAttributeDefList 
              _lhsOoriginalTree :: TypeAttributeDefList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: TypeAttributeDef 
              _hdIoriginalTree :: TypeAttributeDef 
              _tlIannotatedTree :: TypeAttributeDefList 
              _tlIoriginalTree :: TypeAttributeDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 17190 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 17196 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17202 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17208 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17214 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17220 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17226 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17232 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: TypeAttributeDefList 
              _lhsOoriginalTree :: TypeAttributeDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 17249 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 17255 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17261 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17267 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOnamedType :: (Maybe Type)
              _annOtpe :: (Either [TypeError] Type)
              _tpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName 
              _lhsOoriginalTree :: TypeName 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _typOcat :: Catalog
              _typOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _typIannotatedTree :: TypeName 
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 36, column 10)
              _lhsOnamedType =
                  ({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 17375 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 37, column 10)
              _annOtpe =
                  ({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   either Left (const $ Left []) _tpe
                   {-# LINE 17381 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 58, column 9)
              _tpe =
                  ({-# LINE 58 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   maybe (Left []) Right _typInamedType
                   >>=  Right . ArrayType
                   {-# LINE 17388 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ArrayTypeName _annIannotatedTree _typIannotatedTree
                   {-# LINE 17394 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ArrayTypeName _annIoriginalTree _typIoriginalTree
                   {-# LINE 17400 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17406 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17412 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17418 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17424 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17430 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17436 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  typ_ _typOcat _typOflags 
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_Prec2TypeName :: T_Annotation  ->
                              T_Name  ->
                              Integer ->
                              Integer ->
                              T_TypeName 
sem_TypeName_Prec2TypeName ann_ tn_ prec_ prec1_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOnamedType :: (Maybe Type)
              _annOtpe :: (Either [TypeError] Type)
              _tnOtpe :: (Either [TypeError] Type)
              _tpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName 
              _lhsOoriginalTree :: TypeName 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _tnOcat :: Catalog
              _tnOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tnIannotatedTree :: Name 
              _tnIoriginalTree :: Name 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 36, column 10)
              _lhsOnamedType =
                  ({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 17469 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 37, column 10)
              _annOtpe =
                  ({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   either Left (const $ Left []) _tpe
                   {-# LINE 17475 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 39, column 10)
              _tnOtpe =
                  ({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   Left []
                   {-# LINE 17481 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 66, column 9)
              _tpe =
                  ({-# LINE 66 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   catLookupType _lhsIcat (nameComponents _tnIoriginalTree)
                   {-# LINE 17487 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Prec2TypeName _annIannotatedTree _tnIannotatedTree prec_ prec1_
                   {-# LINE 17493 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   Prec2TypeName _annIoriginalTree _tnIoriginalTree prec_ prec1_
                   {-# LINE 17499 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17505 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17511 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17517 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17523 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17529 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17535 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tnIannotatedTree,_tnIoriginalTree) =
                  tn_ _tnOcat _tnOflags _tnOtpe 
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_PrecTypeName :: T_Annotation  ->
                             T_Name  ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName ann_ tn_ prec_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOnamedType :: (Maybe Type)
              _annOtpe :: (Either [TypeError] Type)
              _tnOtpe :: (Either [TypeError] Type)
              _tpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName 
              _lhsOoriginalTree :: TypeName 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _tnOcat :: Catalog
              _tnOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tnIannotatedTree :: Name 
              _tnIoriginalTree :: Name 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 36, column 10)
              _lhsOnamedType =
                  ({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 17567 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 37, column 10)
              _annOtpe =
                  ({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   either Left (const $ Left []) _tpe
                   {-# LINE 17573 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 39, column 10)
              _tnOtpe =
                  ({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   Left []
                   {-# LINE 17579 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 64, column 9)
              _tpe =
                  ({-# LINE 64 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   catLookupType _lhsIcat (nameComponents _tnIoriginalTree)
                   {-# LINE 17585 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PrecTypeName _annIannotatedTree _tnIannotatedTree prec_
                   {-# LINE 17591 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   PrecTypeName _annIoriginalTree _tnIoriginalTree prec_
                   {-# LINE 17597 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17603 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17609 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17615 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17621 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17627 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17633 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tnIannotatedTree,_tnIoriginalTree) =
                  tn_ _tnOcat _tnOflags _tnOtpe 
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_SetOfTypeName :: T_Annotation  ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName ann_ typ_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOnamedType :: (Maybe Type)
              _annOtpe :: (Either [TypeError] Type)
              _tpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName 
              _lhsOoriginalTree :: TypeName 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _typOcat :: Catalog
              _typOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _typIannotatedTree :: TypeName 
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 36, column 10)
              _lhsOnamedType =
                  ({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 17664 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 37, column 10)
              _annOtpe =
                  ({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   either Left (const $ Left []) _tpe
                   {-# LINE 17670 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 61, column 9)
              _tpe =
                  ({-# LINE 61 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   maybe (Left []) Right _typInamedType
                   >>=  Right . Pseudo . SetOfType
                   {-# LINE 17677 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SetOfTypeName _annIannotatedTree _typIannotatedTree
                   {-# LINE 17683 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SetOfTypeName _annIoriginalTree _typIoriginalTree
                   {-# LINE 17689 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17695 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17701 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17707 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17713 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17719 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17725 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  typ_ _typOcat _typOflags 
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_SimpleTypeName :: T_Annotation  ->
                               T_Name  ->
                               T_TypeName 
sem_TypeName_SimpleTypeName ann_ tn_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOnamedType :: (Maybe Type)
              _annOtpe :: (Either [TypeError] Type)
              _tnOtpe :: (Either [TypeError] Type)
              _tpe :: (Either [TypeError] Type)
              _lhsOannotatedTree :: TypeName 
              _lhsOoriginalTree :: TypeName 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _tnOcat :: Catalog
              _tnOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _tnIannotatedTree :: Name 
              _tnIoriginalTree :: Name 
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 36, column 10)
              _lhsOnamedType =
                  ({-# LINE 36 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   either (const Nothing) Just _tpe
                   {-# LINE 17756 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 37, column 10)
              _annOtpe =
                  ({-# LINE 37 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   either Left (const $ Left []) _tpe
                   {-# LINE 17762 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 39, column 10)
              _tnOtpe =
                  ({-# LINE 39 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   Left []
                   {-# LINE 17768 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag"(line 50, column 10)
              _tpe =
                  ({-# LINE 50 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   catLookupType _lhsIcat (nameComponents _tnIoriginalTree)
                   {-# LINE 17774 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SimpleTypeName _annIannotatedTree _tnIannotatedTree
                   {-# LINE 17780 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   SimpleTypeName _annIoriginalTree _tnIoriginalTree
                   {-# LINE 17786 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17792 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17798 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17804 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17810 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17816 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tnOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17822 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _tnIannotatedTree,_tnIoriginalTree) =
                  tn_ _tnOcat _tnOflags _tnOtpe 
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: TypeNameList 
              _lhsOoriginalTree :: TypeNameList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: TypeName 
              _hdInamedType :: (Maybe Type)
              _hdIoriginalTree :: TypeName 
              _tlIannotatedTree :: TypeNameList 
              _tlIoriginalTree :: TypeNameList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 17889 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 17895 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17901 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17907 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17913 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17919 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 17925 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 17931 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdInamedType,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_TypeNameList_Nil :: T_TypeNameList 
sem_TypeNameList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: TypeNameList 
              _lhsOoriginalTree :: TypeNameList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 17948 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 17954 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 17960 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 17966 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: VarDef 
              _lhsOoriginalTree :: VarDef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamAlias _annIannotatedTree name_ i_
                   {-# LINE 18045 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   ParamAlias _annIoriginalTree name_ i_
                   {-# LINE 18051 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 18057 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 18063 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18069 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18075 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: VarDef.ParamAlias.ann.tpe"
                   {-# LINE 18081 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_VarDef_VarAlias :: T_Annotation  ->
                       NameComponent ->
                       T_Name  ->
                       T_VarDef 
sem_VarDef_VarAlias ann_ name_ aliased_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: VarDef 
              _lhsOoriginalTree :: VarDef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _aliasedOcat :: Catalog
              _aliasedOflags :: TypeCheckingFlags
              _aliasedOtpe :: (Either [TypeError] Type)
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _aliasedIannotatedTree :: Name 
              _aliasedIoriginalTree :: Name 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   VarAlias _annIannotatedTree name_ _aliasedIannotatedTree
                   {-# LINE 18109 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   VarAlias _annIoriginalTree name_ _aliasedIoriginalTree
                   {-# LINE 18115 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 18121 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 18127 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18133 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18139 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: VarDef.VarAlias.ann.tpe"
                   {-# LINE 18145 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasedOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18151 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _aliasedOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18157 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _aliasedOtpe =
                  ({-# LINE 46 "src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag" #-}
                   error "missing rule: VarDef.VarAlias.aliased.tpe"
                   {-# LINE 18163 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _aliasedIannotatedTree,_aliasedIoriginalTree) =
                  aliased_ _aliasedOcat _aliasedOflags _aliasedOtpe 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_VarDef_VarDef :: T_Annotation  ->
                     NameComponent ->
                     T_TypeName  ->
                     (Maybe ScalarExpr) ->
                     T_VarDef 
sem_VarDef_VarDef ann_ name_ typ_ value_  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: VarDef 
              _lhsOoriginalTree :: VarDef 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _typOcat :: Catalog
              _typOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _typIannotatedTree :: TypeName 
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   VarDef _annIannotatedTree name_ _typIannotatedTree value_
                   {-# LINE 18194 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   VarDef _annIoriginalTree name_ _typIoriginalTree value_
                   {-# LINE 18200 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 18206 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 18212 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18218 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18224 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: VarDef.VarDef.ann.tpe"
                   {-# LINE 18230 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18236 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _typOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18242 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  typ_ _typOcat _typOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: VarDefList 
              _lhsOoriginalTree :: VarDefList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: VarDef 
              _hdIoriginalTree :: VarDef 
              _tlIannotatedTree :: VarDefList 
              _tlIoriginalTree :: VarDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 18308 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 18314 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 18320 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 18326 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18332 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18338 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18344 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18350 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: VarDefList 
              _lhsOoriginalTree :: VarDefList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 18367 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 18373 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 18379 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 18385 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _exOouterDownEnv :: (Maybe Environment)
              _lhsOannotatedTree :: WithQuery 
              _lhsOoriginalTree :: WithQuery 
              _annOcat :: Catalog
              _annOflags :: TypeCheckingFlags
              _annOtpe :: (Either [TypeError] Type)
              _exOcat :: Catalog
              _exOflags :: TypeCheckingFlags
              _annIannotatedTree :: Annotation 
              _annIoriginalTree :: Annotation 
              _exIannotatedTree :: QueryExpr 
              _exIoriginalTree :: QueryExpr 
              _exIupType :: (Maybe [(String,Type)])
              -- "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag"(line 28, column 15)
              _exOouterDownEnv =
                  ({-# LINE 28 "src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryExprs.ag" #-}
                   Nothing
                   {-# LINE 18451 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WithQuery _annIannotatedTree name_ colAliases_ _exIannotatedTree
                   {-# LINE 18457 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   WithQuery _annIoriginalTree name_ colAliases_ _exIoriginalTree
                   {-# LINE 18463 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 18469 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 18475 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18481 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _annOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18487 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (chain)
              _annOtpe =
                  ({-# LINE 99 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   error "missing rule: WithQuery.WithQuery.ann.tpe"
                   {-# LINE 18493 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18499 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _exOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18505 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _annIannotatedTree,_annIoriginalTree) =
                  ann_ _annOcat _annOflags _annOtpe 
              ( _exIannotatedTree,_exIoriginalTree,_exIupType) =
                  ex_ _exOcat _exOflags _exOouterDownEnv 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         (let _lhsOannotatedTree :: WithQueryList 
              _lhsOoriginalTree :: WithQueryList 
              _hdOcat :: Catalog
              _hdOflags :: TypeCheckingFlags
              _tlOcat :: Catalog
              _tlOflags :: TypeCheckingFlags
              _hdIannotatedTree :: WithQuery 
              _hdIoriginalTree :: WithQuery 
              _tlIannotatedTree :: WithQueryList 
              _tlIoriginalTree :: WithQueryList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIannotatedTree _tlIannotatedTree
                   {-# LINE 18571 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   (:) _hdIoriginalTree _tlIoriginalTree
                   {-# LINE 18577 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 18583 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 18589 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18595 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _hdOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18601 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 89 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIcat
                   {-# LINE 18607 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- copy rule (down)
              _tlOflags =
                  ({-# LINE 90 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _lhsIflags
                   {-# LINE 18613 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  hd_ _hdOcat _hdOflags 
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  tl_ _tlOcat _tlOflags 
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_WithQueryList_Nil :: T_WithQueryList 
sem_WithQueryList_Nil  =
    (\ _lhsIcat
       _lhsIflags ->
         (let _lhsOannotatedTree :: WithQueryList 
              _lhsOoriginalTree :: WithQueryList 
              -- self rule
              _annotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 18630 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _originalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   []
                   {-# LINE 18636 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOannotatedTree =
                  ({-# LINE 91 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _annotatedTree
                   {-# LINE 18642 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
              -- self rule
              _lhsOoriginalTree =
                  ({-# LINE 92 "src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag" #-}
                   _originalTree
                   {-# LINE 18648 "src/Database/HsSqlPpp/Internals/AstInternal.hs" #-}
                   )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))